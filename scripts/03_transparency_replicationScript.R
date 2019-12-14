### active and passive transparency: substitutes or complements?
# master script
#  this is the master script for the reproduction of the third chapter of my
#  dissertation. it contains two large groups of scripts: data wrangling
#  (or munging) and analysis. i indicate below the execution times for either
#  group when scripts took longer than 15 minutes to execute. if you have r and
#  rstudio installed on your computer, you can source this script from the top.
#  if you would like further clarification on how to go about
#  these scripts, please email me at the address below.
# author: andre assumpcao
# by: andre.assumpcao@gmail.com

# import statements (== packages required to run all scripts in R)
if (!require(AER))       {install.packages('AER')}
if (!require(extrafont)) {install.packages('extrafont')}
if (!require(lfe))       {install.packages('lfe')}
if (!require(magrittr))  {install.packages('magrittr')}
if (!require(stargazer)) {install.packages('stargazer')}
if (!require(tidyverse)) {install.packages('tidyverse')}

# load rproj (comment out if using another R IDE)
rstudioapi::openProject('2019 Active and Passive Transparency.Rproj')

### wrangling scripts
#  these scripts wrangle all data used in this paper. you should not run them
#  as they will take a long time to process. you are better off using the final datasets than producing these
#  scripts; nonetheless, i include all files for replication and transparency
#  purposes if you are interested in a particular step taken.

# wrangle ifdm (performance) data
source('01_transparency_ifdmData')

# wrangle audit data
source('02_transparency_auditData.R')

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

### analysis scripts
# these scripts, however, should be executed. they produce the paper analysis
# with the datasets that have been wrangled/munged by the wrangling scripts.
