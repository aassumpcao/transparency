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

################################################################################