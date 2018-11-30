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
# load data on police cooperation with CGU
rde <- read_excel('LAI 00075.001622-2018-19.xlsx')

# create year of investigation variable. we extract the year from the case
#   unique id. if the case unique id is missing, i recover from the year of
#   investigation variable
# extract year when case id is not standardized to 17-digit id
date.fix1 <- rde %>%
  filter(nchar(Nr_Processo) < 17) %>%
  mutate(rde.year = substr(Nr_Ordem_Servico, 1, 4))

# extract year when the case is perfectly coded as the standard 17-digit id
date.fix2 <- rde %>%
  filter(nchar(Nr_Processo) == 17) %>%
  mutate(rde.year = substr(Nr_Processo, 12, 15)) %>%
  filter(rde.year %in% c(2003:2018))

# extract year when case does have 17 digits but does not match standard
date.fix3 <- rde %>%
  filter(nchar(Nr_Processo) == 17) %>%
  filter(!(substr(Nr_Processo, 12, 15) %in% c(2003:2018))) %>%
  mutate(rde.year = substr(Nr_Ordem_Servico, 1, 4))

# bind them together. in the process, we drop the reports on the state capitals
# that have nothing to do with police investigations.
rde <- rbind(date.fix1, date.fix2, date.fix3)

# remove previous temporary objects
rm(list = objects(pattern = '\\.fix'))

# drop reports that are not originated by police investigations
rde %<>% filter(!is.na(Demanda))