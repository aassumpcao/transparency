### active and passive transparency analysis script
# to be updated
# by andre.assumpcao

# import statements
library(here)
library(tidyverse)
library(magrittr)
library(readxl)
library(AER)
library(stargazer)
library(lfe)

# wrangle audit data
source('00_transparency_auditData.R')

# wrangle ebt data
source('00_transparency_ebtData.R')

# wrangle performance data
source('00_transparency_performanceData.R')

# wrangle sanctions data
source('00_transparency_sanctionsData.R')

# merge data across time and space creating panel of municipalities
source('01_transparency_mergeData.R')

# produce paper analysis
source('02_transparency_analysis.R')