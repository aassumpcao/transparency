################################################################################
# active passive transparency paper
# sanctions data wrangling

# this script wrangles sanctions data used for all municipalities in our sample.
# there are four sources of sanction data: (1) police requests made to CGU for
# the investigation of corruption and misconduct in public office; (2) CGU/Feds
# corruption crackdowns; (3) companies currently impeded from contracting with
# the public sector; (4) officials removed from public office for misconduct in
# public office

# by andre.assumpcao@gmail.com

# import statements
library(here)
library(tidyverse)
library(magrittr)
library(readxl)

################################################################################
#