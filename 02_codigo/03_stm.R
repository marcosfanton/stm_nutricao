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


heldout <- make.heldout(dados)

k_result <- muitos_k |> # Cria banco com resultados de cada tópico
  mutate(
    exclusivity = map(topic_model, exclusivity),
    semantic_coherence = map(topic_model, semanticCoherence, dados),
    eval_heldout = map(topic_model, eval.heldout, heldout$missing),
    residual = map(topic_model, checkResiduals, dados),
    bound = map_dbl(topic_model, function(x) max(x$convergence$bound)),
    lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
    lbound = bound + lfact,
    iterations = map_dbl(topic_model, function(x) length(x$convergence$bound))
  )

k_result |>
  transmute(
    K,
    `Lower bound` = lbound,
    `Residual` = map_dbl(residual, "dispersion"),
    `Semantic coherence` = map_dbl(semantic_coherence, mean),
    `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")
  ) |>
  gather(Metric, Value, -K) |>
  ggplot(aes(K, Value, color = Metric)) +
  geom_point(size = 2) +
  geom_line(linewidth = 1.5, alpha = 0.9, show.legend = FALSE) +
  theme_classic() +
  facet_wrap(~Metric, scales = "free_y")
