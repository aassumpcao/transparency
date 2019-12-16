### active and passive transparency
# this script merges data across active/passive transparency, performance, and
#  sanctions. it creates an incomplete panel of brazilian municipalities part of
# the paper's sample.
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com
rm(list = ls())
# import statements
library(fuzzyjoin)
library(magrittr)
library(tidyverse)

# set seed for control group sampling
set.seed(20151219)

# load datasets
for (file in list.files('data_output', full.names = TRUE)) {load(file)}

# define function
# calculate power for sampling strategy
power <- function(n = 5570, alpha = .1, H0 = 0, H1 = .05, sig = 1) {
  # args:
  #   n:     sample size
  #   alpha: significance level (one-sided)
  #   H0:    mean of hypothesis zero
  #   H1:    mean of alternative hypothesis
  #   sig:   variance of sample distribution

  # returns:
  #   power calculation

  # body:
  #   find critical value of z
  z_alpha <- qnorm(p = alpha, mean = 0, sd = 1, lower.tail = FALSE)

  #   find the ybar_critical value
  y_bar <- z_alpha * (sig / sqrt(n)) + H0

  #   calculate the power under h1.
  power <- pnorm(q = y_bar, mean = H1, sd = sig / sqrt(n), lower.tail = FALSE)

  #   report the power.
  return(power)
}

# wrangle each dataset separately. the goal is to create data in panel format
#  for all variables and municipalities in this sample. the unit of analysis is
#  the municipality-year pair
# 1: wrangle the data on crackdown operations
crackdown %<>%
  transmute(
    mun_id             = str_sub(mun.id, 1, 6),
    crackdown_year     = crackdown_year,
    crackdown_outcome  = crackdown_outcome,
    conviction_outcome = conviction_outcome
  ) %>%
  mutate_all(as.character)

# 2: wrangle the data on police investigations
rde %<>%
  transmute(
    mun_id      = str_sub(Cod_Mun_IBGE, 1, 6),
    rde_year    = as.integer(rde.year),
    rde_outcome = 1
  ) %>%
  mutate_all(as.character)

# 3: wrangle the ibge-munic data on performance
munic %<>%
  transmute(
    mun_id      = str_sub(mun_id, 1, 6),
    mdp_year    = year,
    mdp_outcome = mdp_outcome
  ) %>%
  mutate_all(as.character)

# 4: wrangle ifdm performance data
ifdm %<>%
  transmute(
    mun_id     = str_sub(ibge, 1, 6),
    ifdm_year  = year,
    ifdm_class = index,
    ifdm_score = score
  ) %>%
  mutate_all(as.character)

# 5: wrangle ebt data (passive transparency)
ebt %<>%
  filter(nchar(mun.id) > 2) %>%
  mutate_at(
    vars(matches('outcome')),
    list(~ifelse(str_detect(., 'Sim|SIM') & !is.na(.), TRUE, FALSE))
  ) %>%
  transmute(
    mun_id   = str_sub(mun.id, 1, 6),
    ebt_year = case_when(ebt.id == '1' ~ 2015, ebt.id == '2' ~ 2016,
                         ebt.id == '3' ~ 2017
    ),
    ebt_treatment   = 1,
    ebttime_outcome = ifelse(
      health.outcome1 | education.outcome1 |
      social.outcome1 | information.outcome1, 1, 0
    ),
    ebtquality_outcome = ifelse(
      health.outcome2 | education.outcome2 |
      social.outcome2 | information.outcome2, 1, 0
    )
  ) %>%
  mutate_all(as.character)

# 6: wrangle ebt data (active transparency)
audit %<>%
  transmute(
    mun_id             = str_sub(mun_id, 1, 6),
    audit_year         = audit_year,
    audit_id           = audit_id,
    so_id              = so_number,
    corruption_outcome = ifelse(str_detect(audit_outcome, 'Formal'), 0, 1),
    so_amount          = so_amount
  ) %>%
  group_by(mun_id, audit_id) %>%
  summarize(
    audit_year            = first(audit_year),
    audit_amount          = sum(as.double(so_amount), na.rm = TRUE),
    mismanagement_outcome = sum(corruption_outcome == 0),
    audit_treatment       = 1,
    corruption_outcome    = sum(corruption_outcome == 1),
    count_outcome         = n()) %>%
  ungroup() %>%
  select(contains('id'), contains('audit'), contains('outcome'))

# create a massive panel of data points for every municipality-year pair
years <- rep(seq(2003, 2017), each = 5568)
municipalities <- list(crackdown, rde, munic, ifdm, ebt, audit) %>%
                  lapply(function(x){x %$% unique(mun_id)}) %>%
                  unlist() %>%
                  unique() %>%
                  rep(15)

# create (empty) analysis dataset
analysis <- tibble(mun_id = municipalities, obs_year = as.character(years))

# merge all performance data onto the same dataset
analysis %<>%
  full_join(crackdown, by = matchkey[1:2]) %>%
  full_join(rde, by = matchkey[c(1,3)]) %>%
  full_join(munic, by = matchkey[c(1,4)]) %>%
  full_join(filter(ifdm, ifdm_class == 'aggregate'), by = matchkey[c(1,5)]) %>%
  mutate(
    rde_outcome        = ifelse(is.na(rde_outcome), 0, rde_outcome),
    conviction_outcome = replace_na(conviction_outcome, 0),
    sanction_outcome   = ifelse(
      crackdown_outcome == 1 | conviction_outcome == 1 | rde_outcome == 1, 1, 0
    )
  ) %>%
  select(-conviction_outcome, -crackdown_outcome, -rde_outcome, -ifdm_class) %>%
  rename(ifdm_outcome = ifdm_score) %>%
  mutate_all(as.character)

# create final dataset with information starting in 2006 and ending in 2017
transparency <- ebt %>%
  full_join(audit, 'mun_id') %>%
  mutate(
    active_treatment  = ifelse(is.na(audit_id), 0, 1),
    passive_treatment = ifelse(audit_year > 2012 | is.na(audit_year), 1, 0)
  ) %>%
  mutate_all(as.character)

# create list of unique municipalities for control group
control_pool <- setdiff(unique(analysis$mun_id), unique(transparency$mun_id))

# extract outcomes for control group. i sample 900 data
control_pool <- analysis %>%
  filter(mun_id %in% control & obs_year < 2012) %>%
  filter_at(vars(matches('outcome')), all_vars(!is.na(.)))

 # merge onto ebt information with inexact date matching. we find the closest
# # performance date with respect to the ebt date
# passive_transparency <- analysis %>%
#   full_join(ebt, 'mun_id') %>%
#   filter_at(vars(matches('^e(.)*come')), all_vars(!is.na(.))) %>%
#   filter(
#     (obs_year %in% 2013:2015 & ebt_year == 2015) |
#     (obs_year == 2016 & ebt_year == 2016) |
#     (obs_year >= 2017 & ebt_year == 2017)
#   ) %>%
#   distinct() %>%
#   rename(passive_year = obs_year) %>%
#   select(-ebt_year)
# ebt$ebt_year %>% table()
# # merge onto audit information with inexact date matching. we find the closest
# # performance date with respect to the audit date
# active_transparency <- analysis %>%
#   full_join(audit, 'mun_id') %>%
#   filter_at(vars(matches('^(misma|corru|count)')), all_vars(!is.na(.))) %>%
#   mutate(
#     ifdm_outcome = as.double(ifdm_outcome) %>%
#                    {ifelse(is.na(.), median(., na.rm = TRUE), .)}
#   ) %>%
#   filter(!is.na(mdp_outcome)) %>%
#   filter(
#     (obs_year == 2005 & audit_year == 2006) |
#     (obs_year == 2008 & audit_year == 2007) |
#     (obs_year == 2008 & audit_year == 2008) |
#     (obs_year == 2009 & audit_year == 2009) |
#     (obs_year == 2009 & audit_year == 2010) |
#     (obs_year == 2012 & audit_year == 2011) |
#     (obs_year == 2012 & audit_year == 2012) |
#     (obs_year == 2013 & audit_year == 2013) |
#     (obs_year == 2013 & audit_year == 2014) |
#     (obs_year == 2015 & audit_year == 2015)
#   ) %>%
#   rename(active_year = obs_year) %>%
#   select(-audit_year)

# full_join(passive_transparency, active_transparency, 'mun_id') %$%
#   table(is.na(active_year), is.na(passive_year))


# replace crackdown outcome, edit treatment variable, and drop empty rows
analysis %<>%
  mutate(
    audit_treatment = ifelse(is.na(audit_id), 0, 1),
    ebt_treatment   = ifelse(obs_year < 2012, 0, 1)
  ) %>%
  mutate_all(as.character) %>%
  select(matches('_id'), obs_year, matches('_tre|amount'), matches('_out'))

# write to disk
save(transparency, file = 'data_output/transparency_analysis.Rda')
save(control_pool, file = 'data_output/control_pool.Rda')

# remove everything for serial sourcing
rm(list = ls())
