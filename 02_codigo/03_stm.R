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
metadados <- readRDS("01_dados/dados_resumos.RDS")

# Banco para STM
dfm <- dados_stm |>
  count(DOC_ID, WORD, name = "N")

# Matriz esparsa
matriz <- dfm |>
  tidytext::cast_sparse(DOC_ID, WORD, N) # matriz para análise

matriz_id <- as.integer(rownames(matriz))

# Metadados do modelo
metadados <- metadados |>
  dplyr::distinct(DOC_ID, AN_BASE) |>
  slice(match(matriz_id, DOC_ID))

# Checar bancos
stopifnot(
  !anyNA(metadados$DOC_ID),
  identical(matriz_id, as.integer(metadados$DOC_ID)),
  nrow(matriz) == nrow(metadados)
)

# Heldout
set.seed(4016325) # RANDOM.ORG - Timestamp: 2026-05-07 16:45:08 UTC

heldout <- make.heldout(
  documents = matriz
)

# MODELOS PARA COMPARAÇÃO
muitos_k <- tibble(K = c(60, 65, 70, 75, 80)) |>
  mutate(
    topic_model = purrr::map(
      K,
      \(k) {
        stm(
          documents = heldout$documents,
          vocab = heldout$vocab,
          K = k,
          prevalence = ~AN_BASE,
          seed = 4016325, # RANDOM.ORG - Timestamp: 2026-05-07 16:45:08 UTC
          data = metadados,
          init.type = "Spectral"
        )
      }
    )
  )

saveRDS(muitos_k, file = "01_dados/stm65-80.RDS")
muitos_k <- readRDS("01_dados/stm65-80.RDS")

resultado_k <- muitos_k |> # Cria banco com resultados de cada tópico
  mutate(
    exclusivity = map(topic_model, exclusivity),
    semantic_coherence = map(topic_model, semanticCoherence, heldout$documents),
    eval_heldout = map(topic_model, eval.heldout, heldout$missing),
    residual = map(topic_model, checkResiduals, heldout$documents),
    bound = map_dbl(topic_model, function(x) max(x$convergence$bound)),
    lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
    lbound = bound + lfact,
    iterations = map_dbl(topic_model, function(x) length(x$convergence$bound))
  )

# Gráfico de diagnóstico
resultado_k |>
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
  facet_wrap(~Metric, scales = "free_y") +
  labs(
    x = "K (Número de Tópicos)",
    y = "Valores",
    colour = "Métricas",
    title = "Comparação entre modelos",
    subtitle = "Modelo de 65 Tópicos é o mais apropriado"
  )

# Gráfico de diagnóstico - Coerência Semântica x Exclusividade
resultado_k |>
  select(K, exclusivity, semantic_coherence) |>
  filter(K %in% c(60, 65, 70, 75, 80)) |>
  unnest() |>
  mutate(K = as.factor(K)) |>
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    x = "Coerência Semântica",
    y = "Exclusividade",
    title = "Comparação entre exclusividade e coerência semântica"
  )

# Modelo STM: 65 Tópicos ####
stm_nutricao <- stm(
  documents = matriz,
  K = 65,
  prevalence = ~ s(AN_BASE),
  seed = 4016325, # RANDOM.ORG - Timestamp: 2026-05-07 16:45:08 UTC
  data = metadados,
  init.type = "Spectral"
)

# Salvar Análise
saveRDS(stm_nutricao, file = "01_dados/stm65.RDS")

# tbl TÓPICO | FREX | BETA | GAMMA ####
stm_nutricao <- readRDS(file = "01_dados/stm65.RDS")

# BETA
beta_tb <- tidy(stm_nutricao, matrix = "beta") |>
  group_by(topic) |>
  slice_max(beta, n = 5) |>
  summarise(
    BETA = paste(term, collapse = ", "),
    .groups = "drop"
  )

# FREX
frex_tb <- tidy(stm_nutricao, matrix = "frex") |>
  group_by(topic) |>
  slice_head(n = 5) |>
  summarise(
    FREX = paste(term, collapse = ", "),
    .groups = "drop"
  )

# GAMMA
gamma_tb <- tidy(stm_nutricao, matrix = "gamma") |>
  group_by(topic) |>
  summarise(
    GAMMA = mean(gamma),
    .groups = "drop"
  )

# TABELA TÓPICOS
tabela_topicos <- frex_tb |>
  left_join(beta_tb, by = "topic") |>
  left_join(gamma_tb, by = "topic") |>
  arrange(desc(GAMMA))

# Salvar Tabela
write_csv(tabela_topicos, "01_dados/tabela_65stm.csv")
saveRDS(tabela_topicos, "01_dados/tabela_65stm.rds")

# TABELA RESUMOS
dados_resumo <- readRDS(file = "01_dados/dados_resumos.RDS")

# 1. Gamma por documento
gamma_docs <- tidy(stm_nutricao, matrix = "gamma") |>
  group_by(document) |>
  slice_max(gamma, n = 1, with_ties = FALSE) |>
  ungroup()

# 3. Selecionar documentos mais representativos por tópico
tabela_resumos <- gamma_docs |>
  group_by(topic) |>
  slice_max(gamma, n = 3, with_ties = FALSE) |>
  ungroup() |>
  left_join(dados_resumo, by = c("document" = "DOC_ID")) |>
  left_join(tabela_topicos, by = "topic") |>
  arrange(topic, desc(gamma)) |>
  select(topic, FREX, DS_RESUMO, document)

# Salvar Tabela Resumos
write_csv(tabela_resumos, "01_dados/tabela_resumos-65stm.csv")
saveRDS(tabela_resumos, "01_dados/tabela_resumos-65stm.rds")

# Efeito ano ####
stm_efeitoano <- stm::estimateEffect(
  1:65 ~ s(AN_BASE),
  stmobj = stm_nutricao,
  metadata = metadados
)
saveRDS(stm_efeitoano, "01_dados/efeitoano_65stm.rds")

stm_ano <- tidystm::extract.estimateEffect(
  x = stm_efeitoano,
  covariate = "AN_BASE",
  model = stm_nutricao,
  method = "continuous",
  labeltype = "frex",
  n = 2
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
