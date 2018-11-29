################################################################################
# active passive transparency paper
# audit data wrangling

# this script wrangles audit data used for Avis, Ferraz, and Finan (2018) paper.
# what we do here is basically clean up the data and produce a dataset version
# which aggregates everything by municipality

# by andre.assumpcao@gmail.com

# import statements
library(here)
library(tidyverse)
library(magrittr)
library(readxl)

# load datasets
load('audit.dataset.Rda')
load('ibge.dataset.Rda')

################################################################################
# import ibge dataset to find municipal IDs for audit data
fullname <- ibge.dataset %$% unique(UF) %>% sort()
partname <- c('AC', 'AL', 'AP', 'AM', 'BA', 'CE', 'DF', 'ES', 'GO', 'MA', 'MT',
              'MS', 'MG', 'PA', 'PB', 'PR', 'PE', 'PI', 'RJ', 'RN', 'RS', 'RO',
              'RR', 'SC', 'SP', 'SE', 'TO')

# create states dataset
states <- tibble(fullname, partname)

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
  transmute(ibgeID = ifelse(!is.na(Codmundv), Codmundv, municID),
            munName = municipio, stateID = UF)

# join onto audit dataset
audit.dataset <- bind_cols(audit.dataset, ibge.munID) %>%
                 select(ibgeID, everything())

################################################################################
# clean up other variables
# filter lotteries up to 40
audit.dataset %<>% filter(!(Ed_Sorteio %in% paste0('V0', 1:4)))

# rename variables and recode them as string
audit.dataset %<>%
  select(-Municipio, -UF, -descricao_sumaria) %>%
  select(mun.id = ibgeID, state.id = stateID, mun.name = munName,
    lottery.id = Ed_Sorteio, lottery.year = Ano_Sorteio,
    so.amount = Montante_fisc, so.min = Orgao_Sup, so.program = Funcao,
    so.subprogram = Subfuncao, program.name = Programa, subprogram.name = Acao,
    so.corruption = Tipo_constatacao) %>%
  mutate_all(as.character)

# change dataset new
assign('audits', audit.dataset)

# write to disk
save(audits, file = '00_audit.Rda')

# quit
q('no')