### active and passive transparency
# this file downloads and wrangles ifdm data used in the paper
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# import statements
library(magrittr)
library(tidyverse)
library(rvest)

# IFDM: municipal development index
# visit page, find and save links
urls <- read_html('https://www.firjan.com.br/ifdm/downloads/') %>%
        html_nodes(xpath = '//*[contains(@href, "Evolu")]') %>%
        {list(href = html_attr(., 'href'), text = html_text(.))}

# check if links exist; skip data wrangling if true
if (all(is.na(urls$href))) {

  # print message if downloads are not available
  print('links are not available')

} else {

  # redefine links so that we can save them in a proper format
  urls$href %<>% str_replace('\\.\\./\\.\\.', 'https://www.firjan.com.br') %>%
                 str_replace_all(' ', '%20')
  urls$text %<>% str_extract('(?<=IFDM ).*(?= - )') %>%
                 stringi::stri_trans_general('Latin-ASCII') %>%
                 str_to_lower() %>%
                 {paste0('data_input/ifdm_', ., '.xlsx')}

  # save to file
  mapply(download.file, urls$href, urls$text)

  # load, clean, and save as csv into another folder
  files <- list.files('data_input', pattern = 'ifdm_', full.names = TRUE)
  datasets <- lapply(files, readxl::read_excel, skip = 2)

  # create vector of variables names
  variables1 <- c('ibge', 'region', 'state', 'municipality')
  variables2 <- paste0(rep(c('score', 'rank'), 11), rep(2005:2016, each = 2))

  # assign variable names to datasets
  for (i in 1:4) {names(datasets[[i]]) <- c(variables1, variables2)}

  # create indicators for all indexes
  index <- rep(c('education', 'labor', 'aggregate', 'health'), each = 5568)

  # convert all datasets to character and bind them
  for (i in 1:4) {datasets[[i]] %<>% mutate_all(as.character)}
  ifdm <- bind_rows(datasets)

  # create indicator for each separate dataset and rearrange columns
  ifdm$index <- index
  ifdm %<>% select(ibge, region, state, municipality, index, everything())

  # reshape variables to wide
  ifdm %<>%
    pivot_longer(
      cols = matches('^(score|rank)'), names_to = 'year',
      names_prefix = '^(score|rank)', values_to = c('score', 'rank')
    ) %>%
    mutate_at(vars(score, rank), list(~as.character(as.numeric(.))))

  # save to R dataset
  save(ifdm, file = 'data_output/01_ifdm.Rda')
}

# remove everything for serial sourcing
rm(list = ls())
