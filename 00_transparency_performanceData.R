################################################################################
# active passive transparency paper
# performance data wrangling

# this script wrangles data from the munic database with ibge. the goal here is
# to produce performance variables from munic info

# by andre.assumpcao@gmail.com

# import statements
library(here)
library(tidyverse)
library(magrittr)
library(readxl)

################################################################################
# unzip all files
files  <- list.files(pattern = 'munic20(0[4-9]|1[1-7])?\\.zip')
folder <- './munic/'

# create for loop renaming files
for (i in seq(files)) {
  # extract origin file
  file <- paste0('./', files[i])
  # unzip file into folder
  unzip(file, exdir = folder)
  # wait for a few seconds before files are unzipped
  Sys.sleep(5)
  # rename file in folder
  folder %>%
    list.files(pattern = '(^[bB]|mu)(.)+\\.xls') %>%
    {paste0(folder, .)} %>%
    file.rename(paste0('./munic/file', i, '.xls'))
}

### warning: R couldn't load munic files, so i had to manually convert all files
### before importing them here

################################################################################
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


