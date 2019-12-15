### active and passive transparency
# this script wrangles sanctions data used for all municipalities in our sample.
#  there are three sources of sanction data: (1) police requests made to cgu for
#  the investigation of corruption and misconduct in public office; (2) cgu/feds
#  in public office
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# import statements
library(magrittr)
library(readxl)
library(tidyverse)

# load datasets
load('data_input/ibge_dataset.Rda')

# wrangle legal authorities cooperation dataset
rde <- readxl::read_excel('data_input/LAI 00075.001622-2018-19.xlsx')

# create year of investigation variable. we extract the year from the unique
#  case id. if the unique case id is missing, i recover the date from the year
#  of investigation.
# extract year when case id is not standardized to 17-digit id
date_fix1 <- rde %>%
  filter(nchar(Nr_Processo) < 17) %>%
  mutate(rde.year = substr(Nr_Ordem_Servico, 1, 4))

# extract year when the case is perfectly coded as the standard 17-digit id
date_fix2 <- rde %>%
  filter(nchar(Nr_Processo) == 17) %>%
  mutate(rde.year = substr(Nr_Processo, 12, 15)) %>%
  filter(rde.year %in% c(2003:2018))

# extract year when the case id has 17 digits but does not match standard
date_fix3 <- rde %>%
  filter(nchar(Nr_Processo) == 17) %>%
  filter(!(substr(Nr_Processo, 12, 15) %in% c(2003:2018))) %>%
  mutate(rde.year = substr(Nr_Ordem_Servico, 1, 4))

# bind them together. in the process, we drop the reports of the state capitals
#  that have nothing to do with police investigations.
rde <- rbind(date_fix1, date_fix2, date_fix3)

# drop reports that are not originated by police investigations
rde %<>% filter(!is.na(Demanda))

# # filter to reports from 2012, download them from the internet and check the
# # date they were conducted to narrow in on their status before or after lai
# dir.create('./rdereports/')

# # define destination files for download
# pdf.names <- rde %>%
#   filter(rde.year == 2012) %$%
#   unique(IdRelatorioPublicacao) %>%
#   paste0('.pdf') %>%
#   {paste0('data_input/rdereports/', .)}

# # download all files
# rde %>%
#   filter(rde.year == 2012) %$%
#   unique(IdRelatorioPublicacao) %>%
#   {paste0('https://auditoria.cgu.gov.br/download/', ., '.pdf')} %>%
#   {mapply(download.file, ., destfile = pdf.names)}

# save rde to file
save(rde, file = 'data_output/rde.Rda')

# wrangle federal policy and CGU crackdown operations dataset
# import from avis, ferraz, and finan (2018) @ the jpe (2003-2015)
crackdown1 <- haven::read_stata('data_input/table4-1.dta') %>%
              mutate_at(vars(cod_munic), as.character)

# import from cgu (2016-2018)
crackdown2 <- read_excel('data_input/Operacoes_Especiais_20181001.xlsx') %>%
              filter(!(is.na(str_count(uf, ';')) | is.na(str_count(mun, ';'))))

# fix the number of state and municipality entries for each crackdown operation
# split sample where everything is correct
crackdown2_1 <- crackdown2 %>%
  filter(!is.na(mun)) %>%
  filter(!str_detect(mun, 'Estado')) %>%
  filter(str_count(uf, ';') == str_count(mun, ';'))

# split sample when all municipalities are in the same state
crackdown2_2 <- crackdown2 %>%
  filter(!is.na(mun)) %>%
  filter(!str_detect(mun, 'Estado')) %>%
  filter(str_count(uf, ';') < str_count(mun, ';')) %>%
  filter(str_count(uf, ';') == 0) %>%
  mutate(uf = paste0(uf, strrep(paste0(';', uf), str_count(mun, ';'))))

# split sample for the case where there is mix of municipalities and states
crackdown2_3 <- crackdown2 %>%
  filter(!is.na(mun)) %>%
  filter(!str_detect(mun, 'Estado')) %>%
  filter(str_count(uf, ';') < str_count(mun, ';')) %>%
  filter(str_count(uf, ';') > 0)

# fill states in manually
crackdown2_3$uf <- c('MS;MS;PR;PR;SP;SP', 'PB;PB;PB;RN;PE', 'MS;MT;MT;SP',
  'MA;MA;TO;TO;GO', 'MA;MA;TO;TO;GO', 'GO;GO;GO;PR;PR;SC;DF',
  'PR;PR;PR;PR;PR;PR;RJ;RJ', 'AL;AL;AL;AL;AL;AL;AL;AL;AL;AL;AL;AL;PE;PE',
  'PR;PR;MS;MS;RN', 'MG;MG;MG;GO;MG;MG;MG', 'SC;SC;DF'
)

# bind them together
crackdown2 <- rbind(crackdown2_1, crackdown2_2, crackdown2_3)

# expand rows by the number of municipalities audited
crackdown2 %<>% separate_rows(uf, mun, sep = ';')

# create id variable to check when we join ibge id below
crackdown2 %<>% mutate(operation.id = 1:nrow(crackdown2))

# correct municipality names and states
crackdown2[64,  'uf' ] <- 'RS'
crackdown2[84,  'mun'] <- 'Balneário Arroio do Silva'
crackdown2[109, 'mun'] <- 'Santarém'
crackdown2[138, 'mun'] <- 'Barueri'
crackdown2[153, 'uf' ] <- 'PR'
crackdown2[194, 'mun'] <- 'Abaetetuba'
crackdown2[242, c('uf', 'mun')] <- c('RJ', 'Rio de Janeiro')
crackdown2[247, 'mun'] <- 'São Luis do Quitunde'
crackdown2[248, 'mun'] <- 'Anadia'

# fix one 2017 operation (lateronis) that occurred in multiple towns
crackdown2 %<>%
  filter(str_detect(nome_op, 'Lateronis')) %>%
  mutate(uf = c('BA;BA;BA;BA;BA;BA'), mun = c(
    'Cândido Sales;Encruzilhada;Itambé;Piripá;Ipirá;Formosa do Rio Preto')) %>%
  separate_rows(uf, mun, sep = ';') %>%
  {bind_rows(filter(crackdown2, !str_detect(nome_op, 'Lateronis')), .)}

# find ibge id for crackdown2 municipalities
# create abbreviation for IBGE states
fullname <- ibge_dataset %$% unique(UF) %>% sort()
partname <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT',
              'MS', 'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS', 'RO',
              'RR', 'SC', 'SP', 'SE', 'TO')

# create dataset
states <- tibble(fullname, partname)

# merge state IDs onto ibge data, convert case in municipality name, change
# encoding before fuzzy matching with crackdown dataset
ibge_dataset %<>%
  left_join(states, by = c('UF' = 'fullname')) %>%
  mutate(mun = str_to_title(NomeMunic), uf = partname) %>%
  mutate(mun = stringi::stri_trans_general(mun, 'Latin-ASCII')) %>%
  select(1:5, uf, mun, everything())

# change encoding before fuzzy matching with ibge dataset
crackdown2 %<>%
  mutate(mun = stringi::stri_trans_general(mun, 'Latin-ASCII')) %>%
  select(1:2, uf, mun, everything())

# run fuzzy match on municipality name and compute levenshtein distance. this
# process yields 1-to-many matches since there's no match on states. max lv
# distance is two because anything beyond one lv distance becomes too messy.
crackdown_fuzzy <- crackdown2 %>%
  fuzzyjoin::stringdist_left_join(ibge_dataset, by = c('mun'), max_dist = 2,
    distance_col = 'distance', method = 'lv'
  )

# filter down to within-state matches before manually solving the last few
# municipalities with no match
crackdown_fuzzy %<>%
  filter(uf.x == uf.y) %>%
  arrange(distance) %>%
  select(1:4, uf.y, mun.y, distance, everything())

# solve easier conflicts
crackdown_easy <- crackdown_fuzzy %$%
  table(operation.id) %>%
  .[. == 1] %>%
  dimnames() %>%
  unlist() %>%
  {filter(crackdown_fuzzy, operation.id %in% .)} %>%
  select(operation.id, everything()) %>%
  arrange(operation.id, distance)

# # (manually) check for problems
# crackdown_easy %>% View()
# one municipality's name was misspelled (lagoa do carmo == lagoa do carro)

# solve harder conflicts
crackdown_hard <- crackdown_fuzzy %$%
  table(operation.id) %>%
  .[. > 1] %>%
  dimnames() %>%
  unlist() %>%
  {filter(crackdown_fuzzy, operation.id %in% .)} %>%
  filter(distance == 0 | str_detect(mun.y, '^(Goiania)')) %>%
  select(operation.id, everything())

# obs: there are three operations not investigating municipalities that are
#      still in the crackdown2 dataset.

# bind all
crackdown2 <- rbind(crackdown_easy, crackdown_hard) %>%
              mutate_at(vars(Codmundv), as.character)

# create vector of municipalities
mun.id <- ibge_dataset %>%
  select(Codmundv) %>%
  mutate_all(as.character) %>%
  unlist() %>%
  as.vector()

# create vector of crackdown years
crackdown_year <- rep(2003:2018, length(mun.id))

# expand vector of municipalities
mun.id %<>% rep(each = length(2003:2018))

# create dataset
crackdown <- tibble(mun.id = mun.id, crackdown_year = crackdown_year)

# merge both datasets
crackdown %<>%
  left_join(
    mutate(crackdown1, year = as.integer(year)),
    by = c('mun.id' = 'cod_munic', 'crackdown_year' = 'year')
  ) %>%
  mutate(crackdown_outcome = operacoes, conviction_outcome = dconviction) %>%
  select(1:2, 8:9) %>%
  left_join(
    mutate(crackdown2, ano = as.integer(ano), crackdown = 1),
    by = c('mun.id' = 'Codmundv', 'crackdown_year' = 'ano')
  ) %>%
  mutate(crackdown_outcome = ifelse(!is.na(crackdown),1, crackdown_outcome)) %>%
  mutate(crackdown_outcome = replace_na(crackdown_outcome, 0))

# write to disk
save(crackdown, file = 'data_output/crackdown.Rda')

# remove everything for serial sourcing
rm(list = ls())
