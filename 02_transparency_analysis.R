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
dir.create('./proposal3')

# produce means, difference in means and p.values for all variables so that i
# print the descriptive statistics table
for (i in seq(covariates)) {

  # run ttest for each treatment arm independently
  column1 <- paste0(covariates[i], '~ audit.treatment') %>%
             as.formula() %>%
             t.test(data = mutate(analysis, mun.income = log(mun.income)),
                    conf.level = .95) %>%
             .[c('estimate', 'p.value')] %>%
             unlist() %>%
             unname()

  # run ttest for each treatment arm independently
  column2 <- paste0(covariates[i], '~ ebt.treatment') %>%
             as.formula() %>%
             t.test(data = mutate(analysis, mun.income = log(mun.income)),
                    conf.level = .95) %>%
             .[c('estimate', 'p.value')] %>%
             unlist() %>%
             unname()

  # run ttest for each treatment arm independently
  column3 <- paste0(covariates[i], '~ double.treatment') %>%
             as.formula() %>%
             t.test(data = mutate(analysis, mun.income = log(mun.income)),
                    conf.level = .95) %>%
             .[c('estimate', 'p.value')] %>%
             unlist() %>%
             unname()

  # compute difference in means and keep p-values
  column1[3:4] <- c(column1[1] - column1[2], column1[3])
  column2[3:4] <- c(column2[1] - column2[2], column2[3])
  column3[3:4] <- c(column3[1] - column3[2], column3[3])

  # build row with such information
  row1 <- c(column1[1:3], column2[1:3], column3[1:3])
  row2 <- c('', '', column1[4], '', '', column2[4], '', '', column3[4])

  # bind into dataset
  if (i == 1) {table <- tibble(row1, row2)}
  else        {table <- bind_cols(table, row = row1, row = row2)}

  # transpose table at the end of the loop
  if (i == length(covariates)){table <- as.tibble(data.table::transpose(table))}
}

# remove unnecessary objects
rm(list = objects(pattern = 'column|^i$|row'))

# create vectors with the number of observations for descriptive stats table
audit.obs  <- analysis %$% table(audit.treatment)  %>% unlist() %>% unname()
ebt.obs    <- analysis %$% table(ebt.treatment)    %>% unlist() %>% unname()
double.obs <- analysis %$% table(double.treatment) %>% unlist() %>% unname()

# create additional rows (labels and number of observations)
labels.row <- rep(c('Control', 'Treatment', 'Difference'), 3)
sample.row <- c(audit.obs, '', ebt.obs, '', double.obs, '') %>%
              {setNames(as.list(.), paste0('V', 1:9))} %>%
              as.tibble()

# insert rows
table %<>%
  mutate_all(as.numeric) %>%
  mutate_all(round, digits = 3) %>%
  mutate_all(as.character) %>%
  bind_rows(sample.row)

# set col and row names
names(table) <- labels.row
table$Variables <- c(cov.labels[1], NA, cov.labels[2], NA, cov.labels[3], NA,
                     cov.labels[4], NA, cov.labels[5], NA, cov.labels[6], NA,
                     cov.labels[7], NA, cov.labels[8], NA, cov.labels[9], NA,
                     cov.labels[10], NA, cov.labels[11], NA, 'Sample Size')

# print table
xtable::xtable(
  # table object
  table,
  # styling arguments
  caption = 'Descriptive Statistics by Treatment Condition',
  label = 'tab:descriptivestats3',
  align = rep('r', 11),
  digits = 3,
  display = rep('s', 11)
) %>%
# xtable::print.xtable(
#   # styling arguments
#   file = './proposal3/tab_sumstats.tex',
#   table.placement = '!htbp',
#   caption.placement = 'top',
#   hline.after = c(-1, -1, 0, 22, 23, 23),
#   print.results = TRUE
# )