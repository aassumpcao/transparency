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
power <- function(n = 5568, alpha = .1, H0 = 0, H1 = .05, sig = 1) {
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
    ),
    time_trend = as.integer(obs_year) - 2005,
    time_trend_sqrt = time_trend^2,
  ) %>%
  mutate_at(vars(matches('^ebt.*outcome')), list(~ifelse(is.na(.), 0, .)))

# define labels for descriptive statistics and regression tables
# subset outcomes and create labels for each outcome
outcomes   <- names(analysis) %>% .[{which(str_detect(., '_outcome'))}]
out_labels <- c('MUDP Adoption', 'Municipal Human Development Index (HDI)',
  'Sanctions Imposed', 'FOIA Request (time)', 'FOIA Request (accuracy)',
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
treatment    <- names(analysis) %>% .[{which(str_detect(., 'treatment$'))}]
treat_labels <- c(
  'Active Transparency','Passive Transparency','Active + Passive Transparency'
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

# create function to produce means across all covariates
calculate_means <- function(active = 1, passive = 1, vars = covariates) {
  # filter dataset to contain municipal level data
  dataset <- analysis %>%
    filter(active_treatment == active & passive_treatment == passive) %>%
    group_by(mun_id)

  # produce vector of means
  dataset %>%
    select(mun_id, vars) %>%
    mutate_at(vars(-group_cols()), as.numeric) %>%
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
# variables <- c(covariates, outcomes[1:3])
actpass <- calculate_means(active = 1, passive = 1)
active  <- calculate_means(active = 1, passive = 0)
passive <- calculate_means(active = 0, passive = 1)
control <- calculate_means(active = 0, passive = 0)

# create function to regress covariate means across all sampling groups
calculate_pvalue <- function(variable, covs = TRUE) {
  # define grouping variables
  if (covs == TRUE) {
    data <- group_by(analysis, mun_id) %>% slice(1)
  } else {
    data <- group_by(analysis, obs_year) %>%
            mutate_at(vars(-mun_id, obs_year), as.numeric)
  }

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
### Note: Manual editing required after this process
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

### table: performance outcomes
# create tests for difference-in-differences assumptions
# 1. parallel trends
# 2. stable composition of groups
# 3. no simultaneous, external shock

# parallel trends
pre_treatment <- analysis %>%
  filter(obs_year > 2005 & obs_year < 2012) %>%
  select(obs_year, active_treatment, matches('mdp|ifdm|sanction')) %>%
  mutate_all(as.numeric)

# create function
formulas <- paste0(names(pre_treatment)[3:5], ' ~ active_treatment') %>%
            sapply(as.formula)

# test difference in means across pre-treatment levels
pretreatment_levels <- lapply(formulas, t.test, data = pre_treatment) %>%
                       lapply(function(x){x %$% c(estimate, p.value)}) %>%
                       unname() %>%
                       lapply(bind_rows)

# redefine element names
names <- c('Treatment', 'Control', 'p-value')
for (i in 1:3) {names(pretreatment_levels[[i]]) <- names}

# produce table
pretreatment_levels %>%
  bind_rows() %>%
  transmute(
    Treatment = Treatment, Control = Control,
    `Mean Diff.` = Treatment - Control, `p-value` = `p-value`
  )

#


### include time_trends

# stable composition of groups
### no change in control or treatment

# no simultaneous, external shock
### use args in favor of audit program

### table: performance outcomes
# create formula with no covariates
performance_reg0 <- outcomes[1:3] %>%
  paste0(' ~ active_treatment * passive_treatment * time_trend') %>%
  sapply(FUN = as.formula)

# create formula for covariates and fixed-effects
performance_reg1 <- outcomes[1:3] %>%
  paste0(' ~ active_treatment * passive_treatment * time_trend + ') %>%
  paste0(paste0(covariates, collapse = ' + ')) %>%
  paste0(' | 0 | 0 | mun_id') %>%
  sapply(FUN = as.formula)

# create formulas for all six regressions
performance_results <- c(performance_reg0, performance_reg1) %>%
                       lapply(felm, data = analysis, exactDOF = TRUE)

# extract cluster robust standard errors
performance_cse <- lapply(performance_results, summary, robust = TRUE) %>%
                   lapply(function(x){x$coefficients[,2]})

# produce table two: performance outcomes
stargazer(

  # regressions with performance outcomes
  performance_results[c(1,4,2,5,3,6)],

  # table cosmetics
  type = 'text',
  title = 'The Effect of Active Transparency on Performance',
  style = 'default',
  # out = 'tables/tab_transparency1.tex',
  out.header = FALSE,
  column.labels = out_labels[1:3],
  column.separate = rep(2, 3),
  covariate.labels = c(treat_labels[c(3,1,2)]),
  order = c('\\:', 'active', 'passive'),
  dep.var.caption = '',
  dep.var.labels.include = FALSE,
  se = performance_cse[c(1,4,2,5,3,6)],
  align = TRUE,
  column.sep.width = '3pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  df = TRUE,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = 'treatment1$',
  label = 'tab:transparency1',
  no.space =  FALSE,
  omit = c('time_trend', 'mun_'),
  omit.labels = c('Time Trend Interactions', 'Municipal Controls'),
  omit.yes.no = c('Yes', '-'),
  omit.stat = c('ser', 'adj.rsq', 'rsq'),
  table.placement = 'H'
)

# spit f-stat out
lapply(performance_results, function(x){summary(x)$fstat}) %>%
.[c(1,4,2,5,3,6)] %>%
sapply(round, 1) %>%
paste0('^{***} ', collapse = '& ') %>%
{paste0(cat('\\emph{F}-stat & '), ., ' \\')}

### table: corruption outcomes
# create info dataset
corrup_ds <- filter(analysis, !is.na(audit_id)) %>%
             mutate(time_trend = time_trend - 7)

# create formula with no covariates
corruption_reg0 <- outcomes[6:8] %>%
  paste0(' ~ passive_treatment * time_trend') %>%
  sapply(FUN = as.formula)

# create formula for covariates and fixed-effects
corruption_reg1 <- outcomes[6:8] %>%
  paste0(' ~ passive_treatment * time_trend +') %>%
  paste0(paste0(covariates, collapse = ' + ')) %>%
  paste0(' | 0 | 0 | mun_id') %>%
  sapply(FUN = as.formula)

# create formulas for all six regressions
corruption_results <- c(corruption_reg0, corruption_reg1) %>%
                      lapply(felm, data = corrup_ds, exactDOF = TRUE)

# extract cluster robust standard errors
corruption_cse <- lapply(corruption_results, summary, robust = TRUE) %>%
                  lapply(function(x){x$coefficients[,2]})

# produce table two: corruption outcomes
stargazer(

  # regressions with corruption outcomes
  corruption_results[c(1,4,2,5,3,6)],

  # table cosmetics
  type = 'text',
  title = 'The Effect of Passive Transparency on Corruption',
  style = 'default',
  # out = 'tables/tab_transparency2.tex',
  out.header = FALSE,
  column.labels = out_labels[6:8],
  column.separate = rep(2, 3),
  covariate.labels = treat_labels[2],
  dep.var.caption = '',
  dep.var.labels.include = FALSE,
  se = corruption_cse[c(1,4,2,5,3,6)],
  align = TRUE,
  column.sep.width = '3pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  df = TRUE,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  keep = 'treatment1$',
  label = 'tab:transparency2',
  no.space = FALSE,
  omit = c('time_trend', 'mun_'),
  omit.labels = c('Time Trend Interactions', 'Municipal Controls'),
  omit.yes.no = c('Yes', '-'),
  omit.stat = c('ser', 'adj.rsq', 'rsq'),
  table.placement = 'H'
)

# spit f-stat out
lapply(corruption_results, function(x){summary(x)$fstat}) %>%
.[c(1,4,2,5,3,6)] %>%
sapply(round, 1) %>%
paste0('^{***} ', collapse = '& ') %>%
{paste0(cat('\\emph{F}-stat & '), ., ' \\')}

### table: information outcomes
# create info dataset
info_ds <- filter(analysis, obs_year > 2011)

### outcomes
# create formula with no covariates
information_reg0 <- outcomes[4:5] %>%
  paste0(' ~ active_treatment * time_trend ') %>%
  sapply(FUN = as.formula)

# create formula for covariates and fixed-effects
information_reg1 <- outcomes[4:5] %>%
  paste0(' ~ active_treatment * time_trend + ') %>%
  paste0(paste0(covariates, collapse = ' + ')) %>%
  paste0(' | 0 | 0 | mun_id') %>%
  sapply(FUN = as.formula)

# create formulas for all six regressions
information_results <- c(information_reg0, information_reg1) %>%
                       lapply(felm, data = info_ds, exactDOF = TRUE)

# extract cluster robust standard errors
information_cse <- lapply(information_results, summary, robust = TRUE) %>%
                   lapply(function(x){x$coefficients[,2]})

# produce table two: information outcomes
stargazer(

  # regressions with information outcomes
  information_results[c(1,3,2,4)],

  # table cosmetics
  type = 'text',
  title = 'The Effect of Active Transparency on Information',
  style = 'default',
  # out = 'tables/tab_transparency3.tex',
  out.header = FALSE,
  column.labels = out_labels[4:5],
  column.separate = rep(2, 2),
  covariate.labels = treat_labels[1],
  dep.var.caption = '',
  dep.var.labels.include = FALSE,
  se = information_cse[c(1,3,2,4)],
  align = TRUE,
  column.sep.width = '3pt',
  digit.separate = 3,
  digits = 3,
  digits.extra = 0,
  df = TRUE,
  font.size = 'scriptsize',
  header = FALSE,
  initial.zero = FALSE,
  model.names = FALSE,
  # keep = 'treatment1$',
  label = 'tab:transparency3',
  no.space = FALSE,
  omit = c('time_trend', 'mun_'),
  omit.labels = c('Time Trend Interactions', 'Municipal Controls'),
  omit.yes.no = c('Yes', '-'),
  omit.stat = c('ser', 'adj.rsq', 'rsq'),
  table.placement = '!htbp'
)

# spit f-stat out
lapply(information_results, function(x){summary(x)$fstat}) %>%
.[c(1,3,2,4)] %>%
sapply(round, 1) %>%
paste0('^{***} ', collapse = '& ') %>%
{paste0(cat('\\emph{F}-stat & '), ., ' \\')}

### table: simulation of performance outcomes
# here, i randomly draw municipalities from sample, run the regression, store
# coefficients and plot them back to check sensitivity of results to different
# samples
bootstrap_coefs <- function(data, size, coef, reg) {
  sample_n(data, size) %>%
  {felm(formula = reg, data = ., exactDOF = TRUE)} %>%
  broom::tidy() %>%
  filter(str_detect(term, coef)) %>%
  {bind_cols(., sample = rep(size, nrow(.)))} %>%
  invisible()
}

# create sequence of sample sizes for performance outcomes
breaks <- ceiling(nrow(analysis)/10)
sample_sizes <- c(seq(breaks, nrow(analysis), breaks), nrow(analysis))

# create regex pattern for coefficient search
regex <- '_treatment1$'

# calculate coefficient estimates for active transparency
mudp <- lapply(sample_sizes, function(x){
  bootstrap_coefs(analysis, x, regex, performance_reg1[[1]])
})
hdi <- lapply(sample_sizes, function(x){
  bootstrap_coefs(analysis, x, regex, performance_reg1[[2]])
})
sanctions <- lapply(sample_sizes, function(x){
  bootstrap_coefs(analysis, x, regex, performance_reg1[[3]])
})

# create dataset of bootstrapped effects
boostrapped_effects <- bind_rows(mudp, hdi, sanctions) %>%
                       mutate(
                         Outcome = rep(c('MUDP', 'HDI', 'Sanctions'), each = 30),
                         ci_lower = estimate - qnorm(.025) * std.error,
                         ci_upper = estimate + qnorm(.025) * std.error
                       )

plot_bootstrap <- function(data, file, x_breaks = sample_sizes) {
  # create plot
  p <- ggplot(data, aes(y = estimate, x = sample, color = Outcome)) +
    geom_hline(aes(yintercept = 0), linetype = 'dashed') +
    geom_point(position = position_dodge(250)) +
    geom_pointrange(
      aes(ymin = ci_lower, ymax = ci_upper), position = position_dodge(250)
    ) +
    scale_x_continuous(breaks = sample_sizes) +
    scale_color_manual(
      name = 'Outcome:',
      values = c('MUDP' = 'grey12', 'HDI' = 'yellow4', 'Sanctions' = 'grey59')
    ) +
    labs(y = 'Point Estimates and CIs', x = 'Sample Size') +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12),
      axis.title.y = element_text(size = 16, margin = margin(r = 12)),
      axis.title.x = element_text(size = 16, margin = margin(t = 12)),
      axis.text.y = element_text(size = 14, lineheight = 1.1, face = 'bold'),
      axis.text.x = element_text(size = 14, lineheight = 1.1, face = 'bold'),
      text = element_text(family = 'LM Roman 10'),
      panel.border = element_rect(color = 'black', size = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_line(color = 'grey79', linetype = 'dashed'),
      panel.grid.major.y = element_line(color = 'grey79', linetype = 'dashed'),
      legend.text = element_text(size = 16, lineheight = 1.1),
      legend.title = element_text(size = 16, lineheight = 1.1),
      legend.position = 'top'
    )

    # save plot
    ggsave(
      filename = paste0(file, '.pdf'), plot = p, device = cairo_pdf,
      path = 'plots', dpi = 100, width = 8, height = 5
    )
}

# create data for graphs
active_bootstrap  <- filter(boostrapped_effects, term == 'active_treatment1')
passive_bootstrap <- filter(boostrapped_effects, term == 'passive_treatment1')
joint_bootstrap   <- filter(boostrapped_effects, str_detect(term, '\\:'))

# save graphs for each intervention
plot_bootstrap(active_bootstrap, 'active')
plot_bootstrap(passive_bootstrap, 'passive')
plot_bootstrap(joint_bootstrap, 'joint')

# define vector of results to be used in plots
results <- c(performance_results, corruption_results, information_results)

# define vector of results
plot_effects <- function(
  outcome, label, treat, y_breaks = NULL, reg = results, year_0 = 2012,
  year_final = 2017, save = TRUE
){
  # create sequence of years to display
  years_ <- seq(0, year_final - year_0)

  # define search patterns to find regression results
  pattern <- paste0(outcome, '.*mun_')
  groups <- c(paste0(treat, '|^time'), '^time')

  # find regression results that match outcome of interest
  regression <- reg[[min(str_which(names(reg), pattern))]]

  # convert results into table and select significant estimates
  table <- summary(regression)$coefficients
  table <- table[which(table[,'Pr(>|t|)'] <= .05),]

  # define coefficients for time and treat effects
  if (str_detect(pattern, 'corr')) {

    # define groups for each trend line
    group1 <- table[str_which(rownames(table), 'pass|Inter'),]

    # define coefficients for time and treat effects
    timet1 <- 0
    treat1 <- group1[2, 'Estimate']
    timet2 <- 0

    # calculate effects for all years
    e1 <- c(treat1, rep(0, length(years_)-1))
    e2 <- sapply(years_, function(x){sum(timet2 * x)})
    diff <- e1 - e2

  } else {
    # define groups for each trend line
    group1 <- table[str_which(rownames(table), groups[1]),]
    group2 <- table[str_which(rownames(table), groups[2]),]

    # define coefficients for time and treat effects
    timet1 <- group1[str_which(rownames(group1), 'time'), 'Estimate']
    treat1 <- group1[str_which(rownames(group1), 'time', TRUE), 'Estimate']
    timet2 <- group2[1]

    # calculate effects for all years
    e1 <- sapply(years_, function(x){sum(timet1 * x) + sum(treat1)})
    e2 <- sapply(years_, function(x){sum(timet2 * x)})
    diff <- e1 - e2
  }

  # crate data for plot
  data <- tibble(
    effect = c(cumsum(e1), cumsum(e2), cumsum(diff)),
    treat = factor(rep(c(1,0,2), each = length(years_)), levels = c(0, 1, 2)),
    time = rep(years_+1, 3)
  )
  # build plot
  p <- ggplot(data, aes(x = time, y = effect, group = treat, color = treat))

  # build scale if not provided by user
  if (is.null(y_breaks)) {
    # extract min and max for y scale
    y_min <- min(ggplot_build(p)$data[[1]]['y']) * 0.9
    y_max <- max(ggplot_build(p)$data[[1]]['y']) * 1.1

    # y-breaks
    y_breaks <- round(seq(y_min, y_max, by = (y_max - y_min) / 10), 3)
  }

  # limits for count outcome
  if (str_detect(outcome, 'count')) {limits <- NULL}
  else                              {limits <- c(min(y_breaks), max(y_breaks))}

  # build remaining part of graph
  p <- p +
    geom_point() +
    geom_line() +
    scale_y_continuous(breaks = y_breaks, limits = limits) +
    scale_x_continuous(breaks = years_+1) +
    scale_color_manual(
      name = element_blank(),
      values = c('0' = 'grey59', '1' = 'grey12', '2' = 'dodgerblue2'),
      labels = c('0' = 'Control', '1' = 'Treatment', '2' = 'Difference')
    ) +
    labs(y = label, x = 'Years Since Policy Adoption') +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12),
      axis.title.y = element_text(size = 16, margin = margin(r = 12)),
      axis.title.x = element_text(size = 16, margin = margin(t = 12)),
      axis.text.y = element_text(size = 14, lineheight = 1.1, face = 'bold'),
      axis.text.x = element_text(size = 14, lineheight = 1.1, face = 'bold'),
      text = element_text(family = 'LM Roman 10'),
      panel.border = element_rect(color = 'black', size = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(color = 'grey79', linetype = 'dashed'),
      legend.background = element_rect(colour = 'black', fill = 'grey99'),
      legend.direction = 'horizontal',
      legend.text = element_text(size = 16, lineheight = 1.1),
      legend.position = c(.3, .95)
    )

  # condition to save file
  if (save == TRUE) {

    # determine filename
    files <- list.files('plots', pattern = 'outcome\\.pdf', full.names = TRUE)
    if (length(files) == 6) {unlink(files)}
    files <- list.files('plots', pattern = 'outcome\\.pdf', full.names = TRUE)

    # create filename
    file_ <- paste0(length(files) + 1, outcome, '.pdf')

    # save plot
    ggsave(
      filename = file_, plot = p, device = cairo_pdf,
      path = 'plots', dpi = 100, width = 8, height = 5
    )

    # return
    print('plot saved')

  } else {

    # return plot if asked
    return(p)
  }
}

# produce plots
# produce y_breaks for plots
y_breaks1 <- seq(0, 1.5, .15)
y_breaks2 <- seq(0, 0.8, .1)
y_breaks3 <- seq(-.3, .1, .04)
y_breaks4 <- seq(0, 7.5, .75)

# take in outcomes, labels, and treament arm of interest
plot_effects(outcomes[1], 'MUDP Update', '^active', y_breaks1)
plot_effects(outcomes[1], 'MUDP Update', '^passive', y_breaks1)
plot_effects(outcomes[2], 'Municipal HDI', '^active|^passive', y_breaks2)
plot_effects(outcomes[2], 'Municipal HDI', '^passive', y_breaks2)
plot_effects(outcomes[7], 'Acts of Corruption (ln)', '^pass', y_breaks3)
plot_effects(outcomes[5], 'FOI Request (Accuracy)', '^active', y_breaks4)

