# Banco para STM ####
# Pacotes
library(tidyverse)
library(here)
library(stringi)
library(textcat)
library(future)
library(furrr)
library(stm)

# Abrir dados
dados_stm <- readRDS(file = "01_dados/dados_pre-stm.RDS")

dados_stm <- dados
# Banco para STM
dados <- dados_stm |>
  count(DOC_ID, WORD, name = "N")


# Matriz esparsa
dados <- dados |> tidytext::cast_sparse(DOC_ID, WORD, N) #matriz para análise

# Covariável do modelo
covars <- dados_stm |>
  dplyr::distinct(DOC_ID, AN_BASE)

#
plan(multisession)
muitos_k <- tibble(K = c(40, 50, 60, 70, 80, 90, 100)) |>
  mutate(
    topic_model = purrr::map(
      K,
      ~ stm(
        dados,
        K = .,
        prevalence = ~AN_BASE,
        seed = 4016325, # RANDOM.ORG - Timestamp: 2026-05-07 16:45:08 UTC
        data = covars,
        init.type = "Spectral"
      )
    )
  )
