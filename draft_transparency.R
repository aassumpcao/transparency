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

brasil.transparente %>% names()

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

performance %>%
  filter(row_number() %in% c(22254:22260))

performance %$% table(nchar(X1))

?str_extract

(90-2*(11.25+11.25))*11.25
(90-2*(11.25+11.25))*11.25569

(506.25-450)/(569.5312-506.25)

audits %>% names()

brasil.transparente %$% table(municipio)

brasil.transparente %>% names()
brasil.transparente %>% View()

audits %$% table(nchar(mun.id))

rde %$% table(nchar(Nr_Ordem_Servico))
rde %>%
  filter(nchar(Nr_Processo) == 17) %>%
  mutate(date = str_extract(Nr_Ordem_Servico, '^[0-9]{4,4}')) %$%
  table(date)


date.fix1 <- rde %>%
  filter(nchar(Nr_Processo) < 17) %>%
  mutate(rde.year = substr(Nr_Ordem_Servico, 1, 4))

date.fix2 <- rde %>%
  filter(nchar(Nr_Processo) == 17) %>%
  mutate(rde.year = substr(Nr_Processo, 12, 15)) %>%
  filter(rde.year %in% c(2003:2018))

date.fix3 <- rde %>%
  filter(nchar(Nr_Processo) == 17) %>%
  filter(!(substr(Nr_Processo, 12, 15) %in% c(2003:2018))) %>%
  mutate(rde.year = substr(Nr_Ordem_Servico, 1, 4))

rde %$% table(Demanda)

rde %>%
  filter(rde.year == 2012) %>%
  View()


test <- pdf_text('./rdereports/5881.pdf') %>%
        .[1:4] %>%
        str_replace_all('\n', ' ') %>%
        str_replace_all('( ){1,}', ' ') %>%
        str_split('\\.') %>%
        unlist()

str_detect(test, '(trabalh)+(.)*(.)(real)+')

# fix the number of state and municipality entries for each crackdown operation
# split sample where everything is correct
crackdown2.1 <- crackdown2 %>%
  filter(!is.na(mun)) %>%
  select(uf, mun) %>%
  filter(str_count(uf, ';') == str_count(mun, ';'))

# split sample where all municipalities are in the same state
crackdown2.2 <- crackdown2 %>%
  filter(!is.na(mun)) %>%
  select(uf, mun) %>%
  filter(str_count(uf, ';') < str_count(mun, ';')) %>%
  filter(str_count(uf, ';') == 0) %>%
  mutate(uf = paste0(uf, strrep(paste0(';', uf), str_count(mun, ';'))))

# split sample for the case where there is mix of municipalities and states
crackdown2.3 <- crackdown2 %>%
  filter(!is.na(mun)) %>%
  select(uf, mun) %>%
  filter(str_count(uf, ';') < str_count(mun, ';')) %>%
  filter(str_count(uf, ';') > 0)

# fill states in manually
crackdown2.3$uf <- c('MS;MS;PR;PR;SP;SP', 'PB;PB;PB;RN;PE', 'MS;MT;MT;SP',
  'MA;MA;TO;TO;GO', 'MA;MA;TO;TO;GO', 'GO;GO;GO;PR;PR;SC;DF',
  'PR;PR;PR;PR;PR;PR;RJ;RJ', 'AL;AL;AL;AL;AL;AL;AL;AL;AL;AL;AL;AL;PE;PE',
  'PR;PR;MS;MS;RN', 'MG;MG;MG;GO;MG;GO;GO', 'SC;SC;DF')


# operations remaining
operations.remaining <- crackdown2 %>%
  filter(!(operation.id %in% operations.found)) %>% View()
  slice(-c(1:4, 6, 8:15)) %>%
  select(operation.id) %>%
  unlist() %>%
  as.vector()

crackdown.easy %>%
  filter(str_detect(nome_op, 'Betsa')) %>% View()



crackdown2

232+35

# 35 municipios

# Cândido Sales, ba

# Encruzilhada, ba

# Itambé, ba

# Piripá, ba


# Ipirá, ba

# Formosa do Rio Preto, ba


# create vector of municipalities
mun.id <- ibge.dataset %>% select(Codmundv) %>%  unlist() %>%  as.vector()

# create vector of crackdown years
crackdown.year <- rep(2003:2018, length(mun.id))

# expand vector of municipalities
mun.id %<>% rep(each = length(2003:2018))

# create dataset
crackdown <- tibble(mun.id = mun.id, crackdown.year = crackdown.year)

# join everything
crackdown %>%
  left_join(mutate(crackdown1, year = as.integer(year)),
    by = c('mun.id' = 'cod_munic', 'crackdown.year' = 'year')) %>%
  mutate(crackdown.outcome = operacoes, conviction.outcome = dconviction) %>%
  select(1:2, 8:9) %>%
  left_join(mutate(crackdown2, ano = as.integer(ano), crackdown = 1),
    by = c('mun.id' = 'Codmundv', 'crackdown.year' = 'ano')) %>%
  mutate(crackdown.outcome = ifelse(!is.na(crackdown),1, crackdown.outcome)) %>%
  mutate(crackdown.outcome = replace_na(crackdown.outcome, 0))




# include police investigations
rde %<>%
  transmute(state.id = substr(Cod_Mun_IBGE, 1, 2), mun.id = Cod_Mun_IBGE,
    rde.year = as.integer(rde.year), rde.outcome = 1)

# include performance variables
performance %<>%
  mutate(mdp.outcome2003 = NA_real_, mdp.outcome2006 = NA_real_,
    mdp.outcome2007 = NA_real_, mdp.outcome2010 = NA_real_,
    mdp.outcome2011 = NA_real_, mdp.outcome2006 = NA_real_,
    mdp.outcome2014 = NA_real_, mdp.outcome2016 = NA_real_,
    mdp.outcome2017 = NA_real_,
    mdp.outcome2018 = NA_real_) %>%
  gather(contains('mdp.outcome'), key = 'mdp.year', value ='mdp.outcome') %>%
  mutate(mdp.year = as.integer(str_sub(mdp.year, -4, -1)),
    state.id = substr(mun.id, 1, 2)) %>%
  select(state.id, mun.id, mdp.year, mdp.outcome)

# include ebt variables
ebt %<>%
  filter(nchar(mun.id) > 2) %>%
  mutate_at(
    vars(matches('outcome')), funs(ifelse(. %in% c('Sim', 'SIM'), TRUE, FALSE))
  ) %>%
  transmute(state.id = substr(mun.id, 1, 2), mun.id = mun.id,
    ebt.year = case_when(ebt.id == '1' ~ 2015, ebt.id == '2' ~ 2016,
      ebt.id == '3' ~ 2017),
    ebttime.outcome = ifelse(health.outcome1 | education.outcome1 |
      social.outcome1 | information.outcome1, 1, 0),
    ebtquality.outcome = ifelse(health.outcome2 | education.outcome2 |
      social.outcome2 | information.outcome2, 1, 0)
  )

# include audit data
audit %<>%
  transmute(state.id = substr(mun.id, 1, 2), mun.id = mun.id,
    audit.year = as.integer(audit.year), audit.id = audit.id,
    corruption.outcome = ifelse(str_detect(audit.outcome, 'Formal'), 0, 1),
    so.id = so.number, so.amount = so.amount) %>%
  group_by(mun.id, audit.id) %>%
  summarize(state.id = first(state.id), audit.year = first(audit.year),
    audit.amount = sum(as.double(so.amount), na.rm = TRUE), audit.treatment = 1,
    mismanagement.outcome = sum(corruption.outcome == 0),
    corruption.outcome = sum(corruption.outcome == 1), count.outcome = n()) %>%
  ungroup() %>%
  select(contains('id'), contains('audit'), contains('outcome'))

# merge all data into one panel
performance %<>%
  left_join(rde, by = c('mun.id', 'mdp.year' = 'rde.year')) %>%
  left_join(ebt, by = c('mun.id', 'mdp.year' = 'ebt.year')) %>%
  left_join(audit, by = c('mun.id', 'mdp.year' = 'audit.year')) %>%
  left_join(crackdown, by = c('mun.id', 'mdp.year' = 'crackdown.year')) %>%
  select(-matches('\\.id\\.')) %>%
  mutate(state.id = substr(mun.id, 1, 2)) %>%
  select(state.id, mun.id, audit.id, year = mdp.year, matches('treatment'),
     mdp.outcome, crackdown.outcome, conviction.outcome, everything()) %>%
  mutate_at(vars(6, 8, 9), funs(replace_na(., 0))) %>%
  mutate(ebt.treatment = ifelse(year < 2013, 0, 1))


performance %$% table(audit.treatment, year)
performance %$% table(audit.treatment, ebt.treatment)
performance %$% table(audit.treatment, conviction.outcome)
performance %$% table(ebt.treatment, conviction.outcome)
performance %$% table(crackdown.outcome, conviction.outcome)
performance %$% table(audit.treatment, mdp.outcome)

rde %$% table(rde.year)

transparency %>% names()

# join active/passive transparency outcomes data: ebt and audits
ebt %>%
  full_join(audit, by = c('mun.id')) %>%
  mutate(obs.year = ifelse(!is.na(audit.year), audit.year, ebt.year),
    ebttime.outcome = ifelse(obs.year < 2012, NA, ebttime.outcome),
    ebtquality.outcome = ifelse(obs.year < 2012, NA, ebtquality.outcome),
    obs.id = row_number(obs.year))


# join sanctions onto transparency dataset
transparency %>%
  left_join(sanctions, by = c('mun.id', 'obs.year' = 'crackdown.year')) %>%
  mutate(audit.treatment = ifelse(!is.na(audit.id), 1, 0)) %>%
  mutate(ebt.treatment   = ifelse( obs.year < 2012, 0, 1)) %>%
  select(state.id = state.id.x, 2:7, matches('trea'), everything(), -state.id.y) %$%
  table(ebt.treatment, obs.year)

transparency %$% table(sanction.outcome, obs.year)