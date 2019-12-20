### active and passive transparency
# this script produces the analysis in dissertation paper three. it
#  generates descriptive statistics tables and plots.
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com
rm(list = ls())
# import statements
library(tidyverse)
library(magrittr)
library(AER)
library(stargazer)
library(lfe)

# load datasets
load('data_output/01_ifdm.Rda')
load('data_output/10_control_pool.Rda')
load('data_output/10_municipal_covariates.Rda')
load('data_output/10_performance.Rda')
load('data_output/10_transparency.Rda')

# set seed for control group sampling
set.seed(20151219)

# define functions
# calculate robust SEs for OLS regression
cse <- function(reg) {return(sqrt(diag(sandwich::vcovHC(reg, type = 'HC1'))))}

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

# change variable names in municipal dataset
names(municipal_data) <- str_replace_all(names(municipal_data), '\\.', '_')

# sample control municipalities from donor pool of ~7,500 observations. these
# performance measures come from pre-2012 and municipalities that were never
# audited by cgu
control_pool %<>%
  group_by(obs_year) %>%
  sample_n(650, replace = TRUE) %>%
  ungroup()

# check statistical power for the final sample, based on the number of unique
# municipalities in our sample
n <- length(unique(transparency$mun_id)) + length(unique(control_pool$mun_id))

# we are aiming for 90% power
power(n, alpha = .025)

# fill in the missing values for mdp performance in years where municipal survey
# was not conducted
performance %<>%
  group_by(mun_id) %>%
  fill(mdp_outcome, .direction = 'updown') %>%
  fill(ifdm_outcome, .direction = 'down') %>%
  filter(obs_year > 2005 & obs_year < 2018) %>%
  ungroup()

# find performance measures in performance dataset for treated municipalities
transparency %<>%
  left_join(performance, by = c('mun_id', 'obs_year')) %>%
  filter_at(vars(matches('mdp|ifdm|sanction')), all_vars(!is.na(.)))

# match to control pool
analysis <- bind_rows(control_pool, transparency) %>%
            replace_na(list(active_treatment = 0, passive_treatment = 0)) %>%
            select(-audit_treatment, -ebt_treatment) %>%
            mutate(state_id = str_sub(mun_id, 1, 2))

# merge municipal covariates on transparency data
analysis %<>%
  left_join(mutate(municipal_data, mun_id = as.character(ibge_id)),'mun_id') %>%
  select(-ibge_id) %>%
  mutate(
    double_treatment = ifelse(
      active_treatment == 1 & passive_treatment == 1, 1, 0
    )
  ) %>%
  mutate_at(vars(matches('^ebt.*outcome')), list(~ifelse(is.na(.), 0, .)))

# define labels for descriptive statistics and regression tables
# subset outcomes and create labels for each outcome
outcomes   <- names(analysis) %>% .[{which(str_detect(., '_outcome'))}]
out_labels <- c('MUDP Adoption', 'Municipal Development Index (MDI)',
  'Sanctions Imposed', 'FOI Request (time)', 'FOI Request (accuracy)',
  'Acts of Mismanagement (ln)', 'Acts of Corruption (ln)',
  'Number of Irregularities (ln)'
)

# subset municipal covariates and create labels
covariates <- names(analysis) %>% .[{which(str_detect(., 'mun_(?!id|idhm$)'))}]
cov_labels <- c(
  'Share Urban (Pop.)', 'Share Female (Pop.)', 'Share Illiterate',
  'Income Per Capita (ln)', 'Gini Coefficient', 'Share Poor (Pop.)',
  'Presence of AM Radio', 'Presence of Health Council',
  'Presence of Education Council', 'Seat of Judiciary Branch'
)

# subset treatment assignment indicators and create labels
treatment     <- names(analysis) %>% .[{which(str_detect(., 'treatment$'))}]
treat_labels  <- c(
  'Active Transparency','Passive Transparency','Active and Passive Transparency'
)

# make last changes to data before analysis
# first, i log income and corruption outcomes (as in avis, ferraz, finan (2018))
analysis %<>%
  mutate_at(vars(matches('mism|orr|count|inc')), as.integer)%>%
  mutate_at(vars(matches('mism|orr|count|inc')), list(~ifelse(. == 0, 1, .)))%>%
  mutate_at(vars(matches('mism|orr|count|inc')), log)

# define function to produce sample size for latex tables
produce_samples <- function(group = 1, dataset = analysis) {
  # define regex group
  if (group == 1)      {regex_group <- 'mdp|ifdm|sanction'}
  else if (group == 2) {regex_group <- 'corru|mism|count'}
  else                 {regex_group <- '^ebt.*outcome$'}

  # define full sample size
  full_obs <- analysis %>%
    filter_at(vars(matches(regex_group)), all_vars(!is.na(.))) %$%
    table(active_treatment, passive_treatment) %>%
    unname()

  # define unique observations sample size
  unique_obs <- analysis %>%
    group_by(mun_id) %>%
    slice(1) %>%
    filter_at(vars(matches(regex_group)), all_vars(!is.na(.))) %$%
    table(active_treatment, passive_treatment) %>%
    unname()

  # return call for final object
  return(list(full_obs = full_obs, unique_obs = unique_obs))
}

# use custom function to extract sample sizes
n_performance <- produce_samples(1)
n_corruption  <- produce_samples(2)
n_information <- produce_samples(3)

# list full and unique obs
lapply(list(n_performance, n_corruption, n_information), '[[', 1)
lapply(list(n_performance, n_corruption, n_information), '[[', 2)

### table: hypothesis in experimental design sample size
# manually included in table

### table: summary statistics
# produce means, difference in means and p.values for all variables so that i
# print the descriptive statistics table
column1 <- vector()
column2 <- vector()
column3 <- vector()
column4 <- vector()

# create function to produce means across all covariates
calculate_means <- function(active = 1, passive = 1) {
  # filter dataset to contain municipal level data
  dataset <- analysis %>%
    filter(active_treatment == active & passive_treatment == passive) %>%
    group_by(mun_id)

  # produce vector of means
  dataset %>%
    select(mun_id, covariates) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup() %>%
    select(-mun_id) %>%
    as.list() %>%
    lapply(mean, na.rm = TRUE) %>%
    lapply(round, 3) %>%
    unlist() %>%
    c(n = paste0('[', nrow(dataset),';', nrow(attr(dataset, 'group')),']')) %>%
    return()
}

# calculate means for all covariates and all groups
actpass <- calculate_means(active = 1, passive = 1)
active  <- calculate_means(active = 1, passive = 0)
passive <- calculate_means(active = 0, passive = 1)
control <- calculate_means(active = 0, passive = 0)

# create function to regress covariate means across all sampling groups
calculate_pvalue <- function(variable) {
  # group dataset so that observations are evaluated at the group level
  data <- group_by(analysis, mun_id) %>% slice(1)

  # calculate difference in means for active + passive treatment group
  actpass <- paste0(variable, ' ~ double_treatment') %>%
             as.formula() %>%
             t.test(formula = ., data = data) %$%
             c(estimate[2] - estimate[1], p.value) %>%
             sapply(round, digits = 3)

  # calculate difference in means for active treatment group
  active  <- paste0(variable, ' ~ active_treatment') %>%
             as.formula() %>%
             t.test(formula = ., data = filter(data, obs_year > 2011)) %$%
             c(estimate[2] - estimate[1], p.value) %>%
             sapply(round, digits = 3)

  # calculate difference in means for passive treatment group
  passive <- paste0(variable, ' ~ passive_treatment') %>%
             as.formula() %>%
             t.test(formula = ., data = filter(data, !is.na(audit_id))) %$%
             c(estimate[2] - estimate[1], p.value) %>%
             sapply(round, digits = 3)

  # put t-tests together
  mean_diff <- c(actpass, active, passive)

  # name all elements in vector
  names(mean_diff) <- rep(c('diff', 'pvalue'), 3)

  # return difference in means
  return(mean_diff)
}

# apply function to extract all p-values and assign names to
covariates_pvalue <- lapply(covariates, calculate_pvalue)
names(covariates_pvalue) <- covariates

# bind cols, transpose, format varnames and finally create a good dataset
covariates_pvalue <- t(bind_cols(covariates_pvalue, n = rep('', 6)))
covariates_pvalue %<>% as_tibble(.name_repair = 'universal')

# create final table
tibble(
  var = c(cov_labels, 'n'),
  actpass, diff1 = covariates_pvalue$`...1`, pvalue1 = covariates_pvalue$`...2`,
  active,  diff2 = covariates_pvalue$`...3`, pvalue2 = covariates_pvalue$`...4`,
  passive, diff3 = covariates_pvalue$`...5`, pvalue3 = covariates_pvalue$`...6`
) %>%
xtable::xtable(
  caption = 'Descriptive Statistics by Treatment Condition',
  label   = 'tab:descriptivestats3',
  align   = rep('r', 11),
  display = rep('s', 11)
) %>%
xtable::print.xtable(
  # file = 'tables/tab_sumstats.tex',
  table.placement   = '!htbp',
  caption.placement = 'top',
  hline.after       = c(-1, -1, 0, 10, 11, 11),
  print.results     = TRUE,
  include.rownames  = FALSE
)

### Note: Manual editing required after this process

### table: information outcomes
# create formula with no covariates
information.reg0 <- outcomes[4:5] %>%
  paste0(' ~ audit.treatment')

# create formula for covariates and fixed-effects
information.reg1 <- outcomes[4:5] %>%
  paste0(' ~ audit.treatment + ') %>%
  paste0(paste0(covariates, collapse = ' + ')) %>%
  paste0(' + factor(obs.year)')

# create formulas for all six regresions
active0.infotime <- lm(as.formula(information.reg0[1]), data = info.ds)
active1.infotime <- lm(as.formula(information.reg1[1]), data = info.ds)
active0.infoqual <- lm(as.formula(information.reg0[2]), data = info.ds)
active1.infoqual <- lm(as.formula(information.reg1[2]), data = info.ds)

# produce table two: information outcomes
stargazer(

  # regressions with outcome 1: outcome.elected
  list(active0.infotime, active1.infotime, active0.infoqual, active1.infoqual),

  # table cosmetics
  type = 'text',
  title = 'The Effect of Active Transparency on Information Requests',
  style = 'default',
  # out = './proposal3/tab_transparency2.tex',
  out.header = FALSE,
  column.labels = o.labels[4:5],
  column.separate = rep(2, 2),
  covariate.labels = c(t.labels[1], cov.labels),
  dep.var.caption = '',
  dep.var.labels.include = FALSE,
  align = TRUE,
  se = list(cse(active0.infotime), cse(active1.infotime),
            cse(active0.infoqual), cse(active1.infoqual)),
  column.sep.width = '-2pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  df = TRUE,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = c('audit'),
  label = 'tab:transparency2',
  no.space = FALSE,
  omit = c('mun\\.', 'obs\\.year'),
  omit.labels = c('Municipal Controls', 'Year Fixed-Effects'),
  omit.yes.no = c('Yes', '-'),
  omit.stat = c('ser', 'f'),
  table.placement = '!htbp'
)

###
# table two: corruption outcomes
# create formula with no covariates
corruption.reg0 <- outcomes[c(1:3, 6:7)] %>%
  paste0(' ~ ebt.treatment')

# create formula for covariates and fixed-effects
corruption.reg1 <- outcomes[c(1:3, 6:7)] %>%
  paste0(' ~ ebt.treatment + ') %>%
  paste0(paste0(covariates, collapse = ' + '))

# create formulas for all six regressions
passive0.corruption <- lm(as.formula(corruption.reg0[2]), data = corrup.ds)
passive1.corruption <- lm(as.formula(corruption.reg1[2]), data = corrup.ds)
passive0.mismanagmt <- lm(as.formula(corruption.reg0[1]), data = corrup.ds)
passive1.mismanagmt <- lm(as.formula(corruption.reg1[1]), data = corrup.ds)
passive0.irregtotal <- lm(as.formula(corruption.reg0[3]), data = corrup.ds)
passive1.irregtotal <- lm(as.formula(corruption.reg1[3]), data = corrup.ds)

# produce table one: corruption outcomes
stargazer(

  # regressions with outcome 1: outcome.elected
  list(passive0.corruption, passive1.corruption, passive0.mismanagmt,
       passive1.mismanagmt, passive0.irregtotal, passive1.irregtotal),

  # table cosmetics
  type = 'text',
  title = 'The Effect of Passive Transparency on Corruption Irregularities',
  style = 'default',
  # out = './proposal3/tab_corruption1.tex',
  out.header = FALSE,
  column.labels = o.labels[c(2, 1, 3)],
  column.separate = rep(2, 3),
  covariate.labels = c(t.labels[2], cov.labels),
  dep.var.caption = '',
  dep.var.labels.include = FALSE,
  align = TRUE,
  se = list(cse(passive0.corruption), cse(passive1.corruption),
            cse(passive0.mismanagmt), cse(passive1.mismanagmt),
            cse(passive0.irregtotal), cse(passive1.irregtotal)),
  column.sep.width = '-2pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  df = FALSE,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = c('ebt'),
  label = 'tab:corruption1',
  no.space = FALSE,
  omit = 'mun\\.',
  omit.labels = 'Municipal Controls',
  omit.yes.no = c('Yes', '-'),
  omit.stat = c('ser', 'f'),
  table.placement = '!htbp'
)

###
# table three: performance and sanction outcomes
# create formula with no covariates
performance.reg0 <- outcomes[6:7] %>%
  paste0(' ~ double.treatment')

# create formula for covariates and fixed-effects
performance.reg1 <- outcomes[6:7] %>%
  paste0(' ~ double.treatment + ') %>%
  paste0(paste0(covariates, collapse = ' + ')) %>%
  paste0(' + factor(obs.year)')

# create formulas for all six regresions
performance0.mdp       <- lm(as.formula(performance.reg0[1]), data = perf.ds)
performance1.mdp       <- lm(as.formula(performance.reg1[1]), data = perf.ds)
performance0.sanctions <- lm(as.formula(performance.reg0[2]), data = perf.ds)
performance1.sanctions <- lm(as.formula(performance.reg1[2]), data = perf.ds)

# produce table three: performance and sanction outcomes
stargazer(

  # regressions with outcome 1: outcome.elected
  list(performance0.mdp, performance1.mdp, performance0.sanctions,
       performance1.sanctions),

  # table cosmetics
  type = 'text',
  title = paste0('The Effect of Active and Passive Transparency on Performance',
                 ' and Sanctions'),
  style = 'default',
  # out = './proposal3/tab_performance3.tex',
  out.header = FALSE,
  column.labels = o.labels[6:7],
  column.separate = rep(2, 2),
  covariate.labels = c(t.labels[3], cov.labels),
  dep.var.caption = '',
  dep.var.labels.include = FALSE,
  align = TRUE,
  se = list(cse(performance0.mdp), cse(performance1.mdp),
            cse(performance0.sanctions), cse(performance1.sanctions)),
  column.sep.width = '-2pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  df = TRUE,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = c('double'),
  label = 'tab:performance3',
  no.space = FALSE,
  omit = c('mun\\.', 'obs\\.year'),
  omit.labels = c('Municipal Controls', 'Year Fixed-Effects'),
  omit.yes.no = c('Yes', '-'),
  omit.stat = c('ser', 'f'),
  table.placement = '!htbp'
)