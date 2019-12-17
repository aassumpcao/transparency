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

# define function to produce sample size for latex tables
produce_samples <- function(group = 1, dataset = analysis) {
  # define regex group
  if (group == 1)        {regex_group <- 'mdp|ifdm|sanction'
  } else if (group == 2) {regex_group <- 'corru|mism|count'
  } else                 {regex_group <- '^ebt.*outcome$'
  }

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
            select(-audit_treatment, -ebt_treatment)

# merge municipal covariates on transparency data
analysis %<>%
  left_join(mutate(municipal_data, mun_id = as.character(ibge_id)),'mun_id') %>%
  select(-ibge_id) %>%
  mutate(
    double_treatment = ifelse(
      active_treatment == 1 & passive_treatment == 1, 1, 0
    )
  )

# define labels for descriptive statistics and regression tables
# subset outcomes and create labels for each outcome
outcomes   <- names(analysis) %>% .[{which(str_detect(., '_outcome'))}]
out_labels <- c('MUDP Adoption', 'Municipal Development Index (MDI)',
  'Sanctions Imposed', 'FOI Request (time)', 'FOI Request (accuracy)',
  'Acts of Mismanagement (ln)', 'Acts of Corruption (ln)',
  'Number of Irregularities (ln)'
)

# subset municipal covariates and create labels
covariates <- names(analysis) %>% .[{which(str_detect(., 'mun_(?!id$)'))}]
cov_labels <- c(
  'Share Urban (Pop.)', 'Share Female (Pop.)', 'Share Illiterate',
  'Income Per Capita (ln)', 'Gini Coefficient', 'Human Development Index',
  'Share Poor (Pop.)', 'Presence of AM Radio', 'Presence of Health Council',
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

# use custom function to extract sample sizes
n_performance <- produce_samples(1)
n_corruption  <- produce_samples(2)
n_information <- produce_samples(3)

# list full obs
lapply(list(n_performance, n_corruption, n_information), '[[', 1)

### manually include in tables

# return call for final object
return(list(full_obs = full_obs, unique_obs = unique_obs))

# check the total number of observations for performance experiment
analysis %>%
  filter_at(vars(matches('mdp|ifdm|sanction')), all_vars(!is.na(.))) %$%
  table(active_treatment, passive_treatment)

# check the total number of observations for corruption experiment
analysis %>%
  filter_at(vars(matches('corru|mism|count')), all_vars(!is.na(.))) %$%
  table(active_treatment, passive_treatment)

# check the total number of observations for information experiment
analysis %>%
  filter_at(vars(matches('^ebt.*outcome$')), all_vars(!is.na(.))) %$%
  table(active_treatment, passive_treatment)

# find the number of unique observations in each group for table
outcome <- c('ebtquality_outcome')
dataset <- filter_at(analysis, vars(outcome), all_vars(!is.na(.)))


dataset <- list(
  dataset %$% table(double_treatment),
  analysis %$% table(ebttime_outcome, !is.na(audit_id))
  filter(dataset, double_treatment == 0 & audit_treatment == 1),
  filter(dataset, double_treatment == 0 & obs_year > 2011),
  filter_at(dataset, vars(matches('treat')), all_vars(. == 0))
)

vector <- lapply(dataset, function(x){
            {c(mun_unique = length(unique(x$mun_id)), mun_total = nrow(x))}
          })

names(vector) <- c('double', 'active', 'passive', 'control');vector


###### CAGOU TUDO ####


# double transparency
double  <- analysis %>%
           filter(double_treatment == 1) %>%
           filter_at(vars(mdp_outcome, ifdm_outcome), all_vars(!is.na(.))) %>%
           {c(mun_unique = length(unique(.$mun_id)), mun_total = nrow(.))}

# active transparency
active  <- analysis %>%
           filter(!is.na(audit_id)) %>%
           filter_at(vars(mdp_outcome, ifdm_outcome), all_vars(!is.na(.))) %>%
           {c(mun_unique = length(unique(.$mun_id)), mun_total = nrow(.))}

# passive transparency
passive <- analysis %>%
           filter(obs_year > 2011 & is.na(audit_id) & double_treatment == 0) %>%
           filter_at(vars(mdp_outcome, ifdm_outcome), all_vars(!is.na(.))) %>%
           {c(mun_unique = length(unique(.$mun_id)), mun_total = nrow(.))}

# control
control <- analysis %>%
           filter(obs_year < 2012 & is.na(audit_id)) %>%
           filter_at(vars(mdp_outcome, ifdm_outcome), all_vars(!is.na(.))) %>%
           {c(mun_unique = length(unique(.$mun_id)), mun_total = nrow(.))}

### produce means, difference in means and p.values for all variables so that i
# print the descriptive statistics table
column1 <- vector()
column2 <- vector()
column3 <- vector()

# loop over covariates and create table
for (i in seq(covariates)) {

  # compute means for each treatment group
  column1[1] <- filter(analysis, ebt.treatment == 0) %$%
                mean(get(covariates[i]), na.rm = TRUE)
  column1[2] <- filter(analysis, ebt.treatment == 1) %$%
                mean(get(covariates[i]), na.rm = TRUE)

  # compute predicted difference across treatment groups
  column1[3:4] <- covariates[i] %>%
    paste0(' ~ ebt.treatment + factor(audit.id) | 0 | 0 | state.id') %>%
    as.formula() %>%
    felm(data = analysis) %>%
    {c(.[['coefficients']][[2, 1]], .[['cse']][[2]])}

  # compute means for each treatment group
  column2[1] <- filter(analysis, audit.treatment == 0) %$%
                mean(get(covariates[i]), na.rm = TRUE)
  column2[2] <- filter(analysis, audit.treatment == 1) %$%
                mean(get(covariates[i]), na.rm = TRUE)

  # compute predicted difference across treatment groups
  column2[3:4] <- covariates[i] %>%
    paste0(' ~ audit.treatment + factor(obs.year) | 0 | 0 | state.id') %>%
    as.formula() %>%
    felm(data = analysis) %>%
    {c(.[['coefficients']][[2, 1]], .[['cse']][[2]])}

  # compute means for each treatment group
  column3[1] <- filter(analysis, double.treatment == 0) %$%
                mean(get(covariates[i]), na.rm = TRUE)
  column3[2] <- filter(analysis, double.treatment == 1) %$%
                mean(get(covariates[i]), na.rm = TRUE)

  # compute predicted difference across treatment groups
  column3[3:4] <- covariates[i] %>%
    paste0(' ~ double.treatment + factor(obs.year) | 0 | 0 | state.id') %>%
    as.formula() %>%
    felm(data = analysis) %>%
    {c(.[['coefficients']][[2, 1]], .[['cse']][[2]])}

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

# calculate statistically significance by hand and paste onto table
table %>%
  select(V3, V6, V9) %>%
  data.table::transpose() %>%
  as.tibble() %>%
  mutate_all(as.double) %>%
  transmute(
    pvalue1  =  V1 / V2,  pvalue2  =  V3 / V4,  pvalue3 =  V5 / V6,
    pvalue4  =  V7 / V8,  pvalue5  =  V9 / V10, pvalue6 = V11 / V12,
    pvalue7  = V13 / V14, pvalue8  = V15 / V16, pvalue9 = V17 / V18,
    pvalue10 = V19 / V20, pvalue11 = V21 / V22) %>%
  mutate_all(funs(case_when(abs(.) >= 2.576 ~ '***',
                            abs(.) < 2.576 & abs(.) >= 1.960 ~ '**',
                            abs(.) < 1.960 & abs(.) >= 1.645 ~ '*',
                            abs(.) < 1.645 ~ NA_character_))) %>%
  data.table::transpose()

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
# xtable::xtable(
#   # table object
#   table,
#   # styling arguments
#   caption = 'Descriptive Statistics by Treatment Condition',
#   label = 'tab:descriptivestats3',
#   align = rep('r', 11),
#   digits = 3,
#   display = rep('s', 11)
# ) %>%
# xtable::print.xtable(
#   # styling arguments
#   file = './proposal3/tab_sumstats1.tex',
#   table.placement = '!htbp',
#   caption.placement = 'top',
#   hline.after = c(-1, -1, 0, 22, 23, 23),
#   print.results = TRUE
# )

# produce tabulation with sample sizes
# wrangle data, print tabulation, and manually pass values to latex
analysis %$% table(audit.treatment, ebt.treatment)

###
# graph one: plot power curve
# clear graphical device
dev.off()

# two-sided power calculations
pcurv.10 <- lapply(X = 1:5570, FUN = power, alpha = .050)
pcurv.05 <- lapply(X = 1:5570, FUN = power, alpha = .025)
pcurv.01 <- lapply(X = 1:5570, FUN = power, alpha = .005)

# find 90% power using two-sided .05 alpha (.025 on each side)
power(n = 4203)

# define graphical file and font family argument
# pdf(file = './proposal3/power.pdf')
par(family = 'LM Roman 10')

# plot the .1 alpha result (two-sided @ .05)
plot(x = 1:5570, y = pcurv.10, type = 'l', xlab = 'Sample Size (n)',
  ylim = c(0,1), ylab = expression(paste('Power When ', mu, ' = 5570')),
  col = 'black', lwd = 2)

# add horizontal and vertical lines
abline(h = .9,   lty = 1, lwd = 2, col = 'darksalmon')
abline(v = 4203, lty = 1, lwd = 2, col = 'darksalmon')

# merge .05 and .01 alphas
points(x = 1:5570, y = pcurv.10, type = 'l', col = 'black', lwd = 2)
points(x = 1:5570, y = pcurv.05, type = 'l', col = 4, lwd = 2)
points(x = 1:5570, y = pcurv.01, type = 'l', col = 'grey', lwd = 2)

# insert grid
grid(col = gray(.3))

# insert label point at power = 90%
text(x = 4500, y = .5, labels = 'n = 4203', cex = .75)

# include legend
legend(x = 3000, y = .4,  col = c('black' , 4, 'grey'), cex = .75, lwd = 2,
  legend = c('two-sided alpha = .10', 'two-sided alpha = .05',
  'two-sided alpha = .01'))

# remove table to avoid confusion
rm(list = objects(pattern = '^table$|sample|labels\\.row$|pcurv'))

###
# table: frequencies for sampling strategy
# display sample size by year
filter(analysis, obs.year < 2012) %$%
  table(obs.year) %>%
  prop.table() %>%
  as.tibble() %>%
  transmute(year = obs.year, frequency = n * 100, mun.n = round(n * 916, 0))

###
# table one: information outcomes
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
