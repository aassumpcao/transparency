# preliminary tests for third dissertation paper
# by andre.assumpcao@gmail.com

# import statements
library(tidyverse)
library(magrittr)
library(readxl)

# load datasets
load('ibge.dataset.Rda')
load('audit.dataset.Rda')
load('brasil.transparente.Rda')

# create abbreviation for IBGE states
fullname <- ibge.dataset %$% unique(UF) %>% sort()
partname <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT',
              'MS', 'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS', 'RO',
              'RR', 'SC', 'SP', 'SE', 'TO')

# create dataset
states <- tibble(fullname, partname)

################################################################################
# how many observations do we have in each subgroup?
# merge state IDs onto ibge data
ibge.dataset %<>% left_join(states, by = c('UF' = 'fullname'))

# find IBGE id for audit dataset
ibge.munID <- audit.dataset %>%
  transmute(municipio = str_to_upper(Municipio), UF = UF) %>%
  left_join(ibge.dataset, by = c('UF' = 'partname', 'municipio' = 'NomeMunic'))

# find municipalities with missing ID
missing <- which(is.na(ibge.munID$Codmundv))
munic   <- ibge.munID[missing, 'municipio'] %$% unique(municipio)

# mun ids from manual search
municID <- c('3515004', '1600154', '1714203', '5105309', '2300150', '2412559',
             '5000609')

# create dataset
municipalities <- tibble(munic, municID)

# join missing municipality names and IDs
ibge.munID %<>%
  left_join(municipalities, by = c('municipio' = 'munic')) %>%
  transmute(
    ibgeID = ifelse(!is.na(Codmundv), Codmundv, municID),
    munName = municipio, stateID = UF)

# join onto audit dataset
audit.dataset <- bind_cols(audit.dataset, ibge.munID)

# run tabulation on municipalities and year
audit.treatment <- audit.dataset %>%
  group_by(munName) %>%
  summarize(ibgeID = first(ibgeID),
            state  = first(stateID),
            year   = first(Ano_Sorteio)
  )

foi.treatment <- brasil.transparente %>%
  mutate(year = substr(dt_inicio_avaliacao, 7, 10)) %>%
  group_by(municipio, rodada) %>%
  summarize(ibgeID = first(cod_ibge), state = first(uf), year = first(year)) %>%
  spread(rodada, year) %>%
  mutate(rodada1 = ifelse(is.na(`1`), 0, 1), rodada2 = ifelse(is.na(`2`), 0, 1),
         rodada3 = ifelse(is.na(`3`), 0, 1)
  ) %>%
  select(c(1:3, 7:9)) %>%
  mutate(ibgeID = as.character(ibgeID)) %>%
  ungroup()

# workable group
transparency.dataset <- full_join(foi.treatment, audit.treatment,
  by = c('ibgeID' = 'ibgeID')) %>%
  transmute(ibgeID = ibgeID, ebt2015 = rodada1, ebt2016 = rodada2,
            ebt2017 = rodada3, state = state.x, audit.year = year,
            audit = ifelse(is.na(year), 0, 1),
            ebt = ifelse(is.na(ebt2015) & is.na(ebt2016) & is.na(ebt2017), 0,
                         ifelse(any(ebt2015, ebt2016, ebt2017) > 0, 1, 0))
  )

# build table of factorial treatment
transparency.dataset %>% filter(state == 'SP') %$% table(audit.year)

# build table of audit year by ebt
transparency.dataset %$% table(ebt, audit.year)

################################################################################
# what outcomes can we find for all four subgroups?
# (1) municipal performance from IBGE's municipal profile databases

# write out file information
mainURL <- 'ftp://ftp.ibge.gov.br/Perfil_Municipios'
years   <- sort(c(2001, 2002, 2004:2006, 2008, 2009, 2011:2015, 2017), TRUE)
files   <- c('Base_de_Dados/Base_MUNIC_2017_xls.zip',
             'Base_de_Dados/Base_MUNIC_2015_xls.zip',
             'base_MUNIC_xls_2014.zip', 'base_MUNIC_xls_2013.zip',
             'base_MUNIC_xls_2012.zip', 'base_MUNIC_xls_2011.zip',
             'base_MUNIC_2009.zip', 'Base2008.zip', 'base_MUNIC_2006.zip',
             'base_MUNIC_2005.zip', 'base_MUNIC_2004.zip', 'Tabelas_2002.zip',
             'Tabelas_2001.zip')

# download all municipal profile datasets
mapply(download.file, paste(mainURL, years, files, sep = '/'),
       destfile = paste0('../2018 TSE Databank/munic', years, '.zip'))



audit.dataset %>% names()
audit.dataset %>% View()
audit.dataset %$% table(Ed_Sorteio, Ano_Sorteio)

total.audits %>% names()

total.audits %$% table(linhadeatuação)

rm(total.audits)
rm(audit.date)

(.7*65)+(.3*25)
(.7*75)+(.3*15)

(.5*65)+(.5*25)
(.5*75)+(.5*15)



(90-2*(11.25+11.25))*11.25
(90-2*(11.25+11.25))*11.25569

(506.25-450)/(569.5312-506.25)


