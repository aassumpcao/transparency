### active and passive transparency
# this script wrangles data from the ibge munic dataset. the goal here is to
#  produce performance variables from munic info
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# import statements
library(tidyverse)
library(magrittr)

# load dataset
load('data_input/ibge_dataset.Rda')

# load two performance variables per dataset
# outcome: variable municipal development plan
#   file1  A30; year 2004
#   file2  A77; year 2005
#   file3  -  ; year 2006
#   file4  A79; year 2008
#   file5  A56; year 2009
#   file6  -  ; year 2011
#   file7  A36; year 2012
#   file8  A34; year 2013
#   file9  -  ; year 2014
#   file10 A17; year 2015

# list csv files
folder <- 'data_input/'
files <- list.files(folder, pattern = 'file.*\\.csv')
years <- c(2004:2005, 2008:2009, 2012:2013, 2015)

# create loop for datasets
for (i in seq(files)) {
  # extract origin file
  file <- paste0(folder, files[i])
  # conditions for creating or appending data
  if (i == 1) {
    # create dataset for first loop
    performance <- read_csv(file, col_names = FALSE) %>%
                   mutate(year = years[i]) %>%
                   slice(-1)
  } else {
    # create dataset for other sequences of loop
    append      <- read_csv(file, col_names = FALSE) %>%
                   mutate(year = years[i]) %>%
                   slice(-1)
    # append dataset for other sequences of loop
    performance <- rbind(performance, append)
  }
  if (i == length(files)) {rm(append, i)}
}

# spread data
munic <- performance %>%
  transmute(
    mun_id = str_extract(X1, '[0-9]{1,6}'),
    mdp.outcome = ifelse(X2 == 'Sim', 1, 0),
    mdp.year = year
  ) %>%
  filter(!is.na(mun_id)) %>%
  spread(key = mdp.year, value = mdp.outcome) %>%
  rename(
    mdp.outcome2004 = `2004`, mdp.outcome2005 = `2005`,
    mdp.outcome2008 = `2008`, mdp.outcome2009 = `2009`,
    mdp.outcome2012 = `2012`, mdp.outcome2013 = `2013`,
    mdp.outcome2015 = `2015`
  ) %>%
  left_join(
    mutate(ibge_dataset, mun_id = as.character(Codmun)), by = c('mun_id')
  ) %>%
  select(mun_id = Codmundv, 2:8) %>%
  mutate(mun_id = as.character(mun_id))

# gather data
munic %<>%
  pivot_longer(
    cols = matches('^mdp'), names_to = 'year', names_prefix = 'mdp.outcome',
    values_to = 'mdp_outcome'
  )

# write to disk
save(munic, file = 'data_output/04_munic.Rda')

# remove everything for serial sourcing
rm(list = ls())
