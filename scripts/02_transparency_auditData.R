### active and passive transparency
# this script wrangles audit data used in avis, ferraz, and finan (2018) paper.
#  what i do here is basically clean up the data and produce a dataset version
#  which aggregates everything by municipality.
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# import statements
library(tidyverse)
library(magrittr)

# load datasets
load('data_input/audit_dataset.Rda')
load('data_input/ibge_dataset.Rda')

# import ibge dataset to find municipal IDs for audit data
fullname <- ibge_dataset %$% unique(UF) %>% sort()
partname <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT',
              'MS', 'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS', 'RO',
              'RR', 'SC', 'SP', 'SE', 'TO')

# create states dataset
states <- tibble(fullname, partname)

# merge state IDs onto ibge data
ibge_dataset %<>% left_join(states, by = c('UF' = 'fullname'))

# find IBGE id for audit dataset
ibge_munID <- audit_dataset %>%
  transmute(municipio = str_to_upper(Municipio), UF = UF) %>%
  left_join(ibge_dataset, by = c('UF' = 'partname', 'municipio' = 'NomeMunic'))

# find municipalities with missing ID
missing <- which(is.na(ibge_munID$Codmundv))
munic   <- ibge_munID[missing, 'municipio'] %$% unique(municipio)

# mun ids from manual search
municID <- c('3515004', '1600154', '1714203', '5105309', '2300150', '2412559',
             '5000609')

# create dataset
municipalities <- tibble(munic, municID)

# join missing municipality names and IDs
ibge_munID %<>%
  left_join(municipalities, by = c('municipio' = 'munic')) %>%
  transmute(
    ibgeID = ifelse(!is.na(Codmundv), Codmundv, municID),
    munName = municipio, stateID = UF
  )

# join onto audit dataset
audit_dataset <- bind_cols(audit_dataset, ibge_munID) %>%
                 select(ibgeID, everything())

# clean up other variables
# filter lotteries up to 40
audit_dataset %<>% filter(!(Ed_Sorteio %in% paste0('V0', 1:4)))

# rename variables and recode them as string
audit <- audit_dataset %>%
  select(-Municipio, -UF, -descricao_sumaria) %>%
  select(
    mun_id = ibgeID, state_id = stateID, mun_name = munName,
    audit_id = Ed_Sorteio, audit_year = Ano_Sorteio, so_number = Nr_OS,
    so_amount = Montante_fisc, so_min = Orgao_Sup, so_program = Funcao,
    so_subprogram = Subfuncao, program_name = Programa, subprogram_name = Acao,
    audit_outcome = Tipo_constatacao
  ) %>%
  mutate_all(as.character)

# write to disk
save(audit, file = 'data_output/02_cgu.Rda')

# remove everything for serial sourcing
rm(list = ls())
