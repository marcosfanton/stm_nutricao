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
library(tidystm)

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
saveRDS(muitos_k, file = "01_dados/stm65-80.RDS")

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

# Gráfico de diagnóstico
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
  K = 65,
  prevalence = ~AN_BASE,
  seed = 4016325, # RANDOM.ORG - Timestamp: 2026-05-07 16:45:08 UTC
  data = covars,
  init.type = "Spectral"
)

# Salvar Análise
saveRDS(stm_nutricao, file = "01_dados/stm65-2406.RDS")

# tbl TÓPICO | FREX | BETA | GAMMA ####
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
write_csv(tabela_topicos, "01_dados/tabela_65stm-2406.csv")
saveRDS(tabela_topicos, "01_dados/tabela_65stm.rds")


# TABELA RESUMOS
dados_resumo <- readRDS(file = "01_dados/dados_resumos.RDS")

tabela_resumos <- tidy(stm_nutricao, matrix = "gamma") |>
  slice_max(gamma, n = 5, by = topic) |>
  left_join(dados_resumo, by = c("document" = "DOC_ID")) |>
  left_join(tabela_topicos, by = "topic") |>
  arrange(topic, desc(gamma)) |>
  select(topic, FREX, DS_RESUMO, document)

# Salvar Tabela Resumos
write_csv(tabela_resumos, "01_dados/tabela_resumos-2406.csv")
saveRDS(tabela_resumos, "01_dados/tabela_resumos.rds")

# Efeito ano ####
stm_efeitoano <- stm::estimateEffect(
  1:65 ~ s(AN_BASE, k = 3),
  stmobj = stm_nutricao,
  metadata = covars
)

stm_ano <- tidystm::extract.estimateEffect(
  x = stm_efeitoano,
  covariate = "AN_BASE",
  model = stm_nutricao,
  method = "continuous",
  labeltype = "frex",
  n = 4
)

# Gráfico
ggplot(
  stm_ano,
  aes(
    x = covariate.value,
    y = estimate,
    ymin = ci.lower,
    ymax = ci.upper
  )
) +
  facet_wrap(~label, nrow = 5) +
  geom_ribbon(alpha = .5) +
  geom_line()
