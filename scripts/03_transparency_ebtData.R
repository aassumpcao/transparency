### active and passive transparency
# this script wrangles data from the 'escala brasil transparente (ebt)' (the
#  transparent brazil scale). what i do here is basically rename variables and
#  produce a dataset.
# author: andre assumpcao
# email:  andre.assumpcao@gmail.com

# import statements
library(tidyverse)
library(magrittr)

# load dataset
load('data_input/brasil_transparente.Rda')

# rename variables
ebt <- brasil_transparente %>%
  transmute(mun.id = cod_ibge, state.id = uf, mun.name = municipio,
    ebt.id = rodada, ebt.ranking = posicao_ebt, ebt.score = nota,
    ebt.startdate = dt_inicio_avaliacao, ebt.enddate = dt_fim_avaliacao,
    mun.population = populacao,
    health.outcome1      = resposta_no_prazo_pergunta_1,
    education.outcome1   = resposta_no_prazo_pergunta_2,
    social.outcome1      = resposta_no_prazo_pergunta_3,
    information.outcome1 = resposta_no_prazo_pergunta_4,
    health.outcome2      = respondeu_pergunta_1,
    education.outcome2   = respondeu_pergunta_2,
    social.outcome2      = respondeu_pergunta_3,
    information.outcome2 = respondeu_pergunta_4
  ) %>%
  mutate_all(as.character)

# write to disk
save(ebt, file = 'data_output/03_ebt.Rda')

# remove everything for serial sourcing
rm(list = ls())
