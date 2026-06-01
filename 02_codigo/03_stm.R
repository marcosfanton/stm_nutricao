# Banco para STM ####
# Pacotes
library(tidyverse)
library(here)
library(stringi)
library(textcat)
library(future)
library(furrr)
library(stm)
library(tidyr)
library(tidytext)

# Abrir dados
dados_stm <- readRDS(file = "01_dados/dados_prestm.RDS")

# Banco para STM
dados <- dados_stm |>
  count(DOC_ID, WORD, name = "N")

# Matriz esparsa
dados <- dados |> tidytext::cast_sparse(DOC_ID, WORD, N) #matriz para análise

# Covariável do modelo
covars <- dados_stm |>
  dplyr::distinct(DOC_ID, AN_BASE)

#
muitos_k <- tibble(K = c(60, 65, 70, 75, 80)) |>
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

# Modelo STM ####
stm_nutricao <- stm(
  dados,
  K = 70,
  prevalence = ~AN_BASE,
  seed = 4016325, # RANDOM.ORG - Timestamp: 2026-05-07 16:45:08 UTC
  data = covars,
  init.type = "Spectral"
)

# Salvar Análise
saveRDS(stm_nutricao, file = "01_dados/stm70.RDS")

# Tabela com Tópico, FREX, BETA, GAMMA ####

# BETA
beta_tb <- tidy(stm_nutricao, matrix = "beta") |>
  mutate(topic = topic) |>
  slice_max(beta, n = 10, by = topic) |>
  summarise(
    BETA = paste(term, collapse = ", "),
    .by = topic
  )


# FREX
frex_tb <- tidy(stm_nutricao, matrix = "frex") |>
  mutate(topic = topic) |>
  slice_head(n = 10, by = topic) |>
  summarise(
    FREX = paste(term, collapse = ", "),
    .by = topic
  )

# GAMMA
gamma_tb <- tidy(stm_nutricao, matrix = "gamma") |>
  mutate(topic = topic) |>
  summarise(
    GAMMA = mean(gamma),
    .by = topic
  )

# TABELA TÓPICOS
tabela_topicos <- frex_tb |>
  left_join(beta_tb, by = "topic") |>
  left_join(gamma_tb, by = "topic") |>
  arrange(desc(GAMMA))

# Salvar Tabela
write_csv(tabela_topicos, "01_dados/tabela_70stm.csv")
saveRDS(tabela_topicos, "01_dados/tabela_70stm.rds")
