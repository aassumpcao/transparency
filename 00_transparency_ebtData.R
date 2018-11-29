################################################################################
# active passive transparency paper
# audit data wrangling

# this script wrangles data from the 'escala brasil transparente (ebt)' (the
# transparent brazil scale). what I do here is basically rename variables and
# produce a dataset.

# by andre.assumpcao@gmail.com

# import statements
library(here)
library(tidyverse)
library(magrittr)
library(readxl)

# load datasets
load('audit.dataset.Rda')
load('ibge.dataset.Rda')
load('brasil.transparente.Rda')

################################################################################
# rename variables
ebt <- brasil.transparente %>%
  transmute(mun.id = cod_ibge, state.id = uf, mun.name = municipio,
    ebt.id = rodada, ebt.ranking = posicao_ebt, ebt.score = nota,
    ebt.startdate = dt_inicio_avaliacao, ebt.enddate = dt_fim_avaliacao,
    mun.population = populacao, health.intime = resposta_no_prazo_pergunta_1,
    education.intime     = resposta_no_prazo_pergunta_2,
    social.intime        = resposta_no_prazo_pergunta_3,
    information.intime   = resposta_no_prazo_pergunta_4,
    health.answered      = respondeu_pergunta_1,
    education.answered   = respondeu_pergunta_2,
    social.answered      = respondeu_pergunta_3,
    information.answered = respondeu_pergunta_4) %>%
  mutate_all(as.character)

# write to disk
save(ebt, file = '00_ebt.Rda')
