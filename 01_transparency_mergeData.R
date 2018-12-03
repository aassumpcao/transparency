################################################################################
# active passive transparency paper
# merge and create panel of municipality data

# this script merges data across active/passive transparency, performance, and
# sanctions. it creates an incomplete panel of brazilian municipalities part of
# the paper's sample.

# by andre.assumpcao@gmail.com

# remove everything
rm(list = ls())

# import statements
library(here)
library(tidyverse)
library(magrittr)

# load datasets
load('00_audit.Rda')
load('00_crackdown.Rda')
load('00_ebt.Rda')
load('00_performance.Rda')
load('00_rde.Rda')

################################################################################
# wrangle each dataset separately. the goal is to create data in panel format
# for all variables and municipalities in this sample

# start off with crackdown operations
crackdown %<>%
  transmute(state.id = substr(mun.id, 1, 2), mun.id = as.character(mun.id),
    crackdown.year = crackdown.year, crackdown.outcome = crackdown.outcome,
    conviction.outcome = conviction.outcome)

# include police investigations variables
rde %<>%
  transmute(state.id = substr(Cod_Mun_IBGE, 1, 2), mun.id = Cod_Mun_IBGE,
    rde.year = as.integer(rde.year), rde.outcome = 1)

# include performance variables
performance %<>%
  gather(contains('mdp.outcome'), key = 'mdp.year', value = 'mdp.outcome') %>%
  mutate(mdp.year = as.integer(str_sub(mdp.year, -4, -1)),
    state.id = substr(mun.id, 1, 2)) %>%
  select(state.id, mun.id, mdp.year, mdp.outcome)

# include ebt variables
ebt %<>%
  filter(nchar(mun.id) > 2) %>%
  mutate_at(
    vars(matches('outcome')), funs(ifelse(. %in% c('Sim', 'SIM'), TRUE, FALSE))
  ) %>%
  transmute(state.id = substr(mun.id, 1, 2), mun.id = mun.id,
    ebt.year = case_when(ebt.id == '1' ~ 2015, ebt.id == '2' ~ 2016,
      ebt.id == '3' ~ 2017), ebt.treatment = 1,
    ebttime.outcome = ifelse(health.outcome1 | education.outcome1 |
      social.outcome1 | information.outcome1, 1, 0),
    ebtquality.outcome = ifelse(health.outcome2 | education.outcome2 |
      social.outcome2 | information.outcome2, 1, 0))

# include audit data
audit %<>%
  transmute(state.id = substr(mun.id, 1, 2), mun.id = mun.id,
    audit.id = audit.id, audit.year = as.integer(audit.year), so.id = so.number,
    corruption.outcome = ifelse(str_detect(audit.outcome, 'Formal'), 0, 1),
    so.amount = so.amount) %>%
  group_by(mun.id, audit.id) %>%
  summarize(state.id = first(state.id), audit.year = first(audit.year),
    audit.amount = sum(as.double(so.amount), na.rm = TRUE),
    mismanagement.outcome = sum(corruption.outcome == 0), audit.treatment = 1,
    corruption.outcome = sum(corruption.outcome == 1), count.outcome = n()) %>%
  ungroup() %>%
  select(contains('id'), contains('audit'), contains('outcome'))

################################################################################
# merge all data into one panel
# join active/passive transparency outcomes data: ebt and audits
transparency <- ebt %>%
  full_join(audit, by = c('mun.id')) %>%
  mutate(obs.year = ifelse(!is.na(audit.year), audit.year, ebt.year),
    ebttime.outcome = ifelse(obs.year < 2012, NA, ebttime.outcome),
    ebtquality.outcome = ifelse(obs.year < 2012, NA, ebtquality.outcome),
    obs.id = row_number(obs.year))

# join performance outcomes onto active/passive transparency data
transparency %<>%
  left_join(performance, by = c('mun.id')) %>%
  group_by(obs.id) %>%
  select(state.id, matches('\\.id$'), matches('year'), matches('outcome')) %>%
  filter(abs(obs.year - mdp.year) == min(abs(obs.year - mdp.year))) %>%
  ungroup() %>%
  filter(!duplicated(obs.id)) %>%
  select(obs.id, state.id, mun.id, everything(), -audit.year, -ebt.year)

# join crackdown/conviction and rde datasets
sanctions <- crackdown %>%
  left_join(rde, by = c('mun.id', 'crackdown.year' = 'rde.year')) %>%
  mutate(rde.outcome = ifelse(is.na(rde.outcome), 0, rde.outcome)) %>%
  select(-state.id.y) %>%
  mutate(conviction.outcome = replace_na(conviction.outcome, 0)) %>%
  mutate(sanction.outcome = ifelse(crackdown.outcome == 1 |
    conviction.outcome == 1 | rde.outcome == 1, 1, 0)) %>%
  rename(state.id = state.id.x)

# join sanctions onto transparency dataset
transparency %>%
  left_join(sanctions, by = c('mun.id', 'obs.year' = 'crackdown.year')) %>%
  mutate(audit.treatment = ifelse(!is.na(audit.id), 1, 0)) %>%
  mutate(ebt.treatment   = ifelse( obs.year < 2012, 0, 1)) %>%
  select(state.id = state.id.x, 2:7, matches('trea'), everything(), -state.id.y)



