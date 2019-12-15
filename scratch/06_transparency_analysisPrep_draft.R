### active and passive transparency
# this script merges data across active/passive transparency, performance, and
#  sanctions. it creates an incomplete panel of brazilian municipalities part of
# the paper's sample.
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com
rm(list = ls())
# import statements
library(tidyverse)
library(magrittr)

# load datasets
for (file in list.files('data_output', full.names = TRUE)) {load(file)}

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
  mutate_at(vars(matches('outcome')), ~ifelse(str_detect(., '^S'), 1, 0)) %>%
  transmute(
    mun_id   = str_sub(mun.id, 1, 6),
    ebt_year = case_when(ebt.id == '1' ~ 2015, ebt.id == '2' ~ 2016,
                         ebt.id == '3' ~ 2017
    ),
    ebt_treatment   = 1,
    ebttime_outcome    = ifelse(health.outcome1 | education.outcome1 |
                                social.outcome1 | information.outcome1, 1, 0
    ),
    ebtquality_outcome = ifelse(health.outcome2 | education.outcome2 |
                                social.outcome2 | information.outcome2, 1, 0
    )
  ) %>%
  mutate_all(as.character)
  # group_by(mun_id) %>%
  # slice(which.min(ebt_year)) %>%
  # ungroup()

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

# merge all data into one panel
# create a massive panel of data points for every municipality-year pair
years <- rep(seq(2003, 2017), each = 5568)
municipalities <- list(crackdown, rde, munic, ifdm, ebt, audit) %>%
                  lapply(function(x){x %$% unique(mun_id)}) %>%
                  unlist() %>%
                  unique() %>%
                  rep(15)

# create (empty) analysis dataset
analysis <- tibble(mun_id = municipalities, obs_year = as.character(years))
matchkey <- c('mun_id', 'obs_year' = 'crackdown_year', 'obs_year' = 'rde_year',
              'obs_year' = 'mdp_year', 'obs_year' = 'ifdm_year',
              'obs_year' = 'ebt_year', 'obs_year' = 'audit_year'
            )

# merge all data onto the same dataset
analysis %<>%
  full_join(crackdown, by = matchkey[1:2]) %>%
  full_join(rde, by = matchkey[c(1,3)]) %>%
  full_join(munic, by = matchkey[c(1,4)]) %>%
  full_join(filter(ifdm, ifdm_class == 'aggregate'), by = matchkey[c(1,5)]) %>%
  full_join(ebt, by = matchkey[c(1,6)]) %>%
  full_join(audit, by = matchkey[c(1,7)])

# replace crackdown outcome, edit treatment variable, and drop empty rows
analysis %<>%
  mutate(
    rde_outcome        = ifelse(is.na(rde_outcome), 0, rde_outcome),
    conviction_outcome = replace_na(conviction_outcome, 0),
    sanction_outcome   = ifelse(
      crackdown_outcome == 1 | conviction_outcome == 1 | rde_outcome == 1, 1, 0
    )
  ) %>%
  select(-conviction_outcome, -crackdown_outcome, -rde_outcome, -ifdm_class) %>%
  rename(ifdm_outcome = ifdm_score) %>%
  mutate(
    audit_treatment = ifelse(is.na(audit_id), 0, 1),
    ebt_treatment   = ifelse(obs_year < 2012, 0, 1)
  ) %>%
  mutate_all(as.character) %>%
  select(matches('_id'), obs_year, matches('_tre|amount'), matches('_out'))

# join active/passive transparency outcomes data: ebt and audits. i first define
# unique sets of audited municipalities, both before and after lai
audited_mun <- audit %$% unique(mun_id)
lai_mun     <- ebt   %$% unique(mun_id)

# split audit dataset for before and after 2012
audit_prelai  <- filter(audit, audit_year < 2012)
audit_postlai <- filter(audit, audit_year > 2011)

# split ebt dataset for yes and no audit
ebt_no  <- filter(ebt, !(mun_id %in% audited_mun))
ebt_yes <- filter(ebt, mun_id %in% audited_mun)

# audited before lai
audit_prelai %>% left_join(ebt, by = c('mun_id', 'audit_year' = 'ebt_year'))

# audited after lai
audit_postlai %<>%
  full_join(ebt_yes, by = 'mun_id') %>%
  mutate(obs_year = ifelse(is.na(ebt_year), audit_year, ebt_year))

# not audited post lai
ebt_no %<>% left_join(audit, by = c('mun_id', 'ebt_year' = 'audit_year'))

# bind transparency dataset
transparency <- bind_rows(audit_prelai, audit_postlai, ebt_no) %>%
                mutate(obs_id = row_number(mun_id))

# join performance outcomes onto active/passive transparency data
transparency %<>%
  left_join(munic, by = 'mun_id') %>%
  select(state_id, matches('_id$'), matches('year'), matches('outcome')) %>%
  mutate(obs.year = ifelse(is.na(ebt_year), audit_year, ebt_year)) %>%
  group_by(obs_id) %>%
  slice(which.min(abs(obs_year - as.integer(mdp_year)))) %>%
  ungroup() %>%
  select(obs_id, state_id, mun_id, everything(), -audit_year, -ebt_year) %>%
  mutate_all(as.character)

# join crackdown/conviction and rde datasets
sanctions <- crackdown %>%
  left_join(rde, by = c('mun_id', 'crackdown_year' = 'rde_year')) %>%
  mutate(rde_outcome = ifelse(is.na(rde_outcome), 0, rde_outcome)) %>%
  select(-state_id.y) %>%
  mutate(conviction_outcome = replace_na(conviction_outcome, 0)) %>%
  mutate(sanction_outcome = ifelse(crackdown_outcome == 1 |
    conviction_outcome == 1 | rde_outcome == 1, 1, 0)
  ) %>%
  rename(state_id = state_id.x)

# join sanctions onto transparency dataset
transparency %>%
  left_join(
    mutate_at(sanctions, vars(crackdown_year), as.character),
    by = c('mun_id', 'obs_year' = 'crackdown_year')
  ) %>%
  mutate(audit_treatment = ifelse(is.na(audit_id), 0, 1)) %>%
  mutate(ebt_treatment   = ifelse(obs_year < 2012, 0, 1)) %>%
  select(-state_id.y, state_id = state_id.x, 1:6, matches('treatment')) %>%
  filter(!duplicated(obs_id))

# remove unnecessary objects
rm(list = objects(pattern = '\\.'))

# write to disk
save(transparency, file = 'transparency_analysis.Rda')
