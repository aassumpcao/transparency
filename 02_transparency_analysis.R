################################################################################
# active passive transparency paper
# preliminary analysis for paper proposal

# this script produces the preliminary analysis for dissertation paper three. it
# generates descriptive statistics tables, experimental conditions tabulation,
# and four tables containing the results for the four main outcomes.

# by andre.assumpcao@gmail.com

# import statements
library(here)
library(tidyverse)
library(magrittr)
library(AER)
library(stargazer)

# load datasets
load('01_transparency.Rda')
load('01_municipalCovariates.Rda')

# define functions:
#   calculate corrected SEs for OLS regression
cse <- function(reg) {
  # Args:
  #   reg: regression object

  # Returns:
  #   matrix of robust standard errors

  # Body:
  #   call to vcovHC
  rob <- sqrt(diag(sandwich::vcovHC(reg, type = "HC1")))

  #   return matrix
  return(rob)
}

################################################################################
# merge municipal covariates on transparency data
analysis <- transparency %>%
  mutate(mun.id = str_sub(mun.id, 1, 6)) %>%
  left_join(mutate(mun.data, mun.id = as.character(ibge.id)), by = 'mun.id') %>%
  select(-ibge.id) %>%
  mutate(double.treatment = ifelse(audit.treatment==1 & ebt.treatment==1, 1, 0))

################################################################################
# define labels for descriptive statistics and regression tables
# subset outcomes and create labels
outcomes <- names(analysis) %>% str_subset('\\.outcome') %>% .[c(1:6, 10)]
o.labels <- c('Acts of Mismanagement (ln)', 'Acts of Corruption (ln)',
              'Number of Irregularities (ln)', 'FOI Request (time)',
              'FOI Request (quality)', 'MDP Adoption', 'Sanctioned')

# subset municipal covariates and create labels
covariates <- names(analysis) %>% str_subset('mun\\.(?!id$)')
cov.labels <- c('Share Urban (Pop.)', 'Share Female (Pop.)', 'Illiteracy Rate',
                'Income Per Capita (ln)', 'Gini Coefficient',
                'Human Development Index','Share Poor (Pop.)',
                'Presence of AM Radio', 'Presence of Health Council',
                'Presence of Education Council', 'Seat of Judiciary Branch')

# subset political covariates and create labels

# subset treatment assignment indicators and create labels
treatment <- names(analysis) %>% str_subset('\\.treatment$')
t.labels  <- c('Active Transparency', 'Passive Transparency',
               'Active and Passive Transparency')

################################################################################
# create dir for prospectus
dir.create('./proposal')

# subset dataframe in three
analysis %$%
  {t.test(mun.urbanpop ~ .$ebt.treatment,
          var.equal = FALSE,
          conf.level = .90)} %>%
  str()