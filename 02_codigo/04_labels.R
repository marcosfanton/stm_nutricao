# Pacotes
library(tidyverse)
library(here)
library(uwot)

tabela_topicos <- readRDS(here::here("01_dados", "tabela_65stm.rds"))
stm_nutricao <- readRDS(here::here("01_dados", "stm65.RDS"))
labels <- read.csv2(here::here("01_dados", "tabela_labels-10.csv"))
metadados <- readRDS("01_dados/dados_resumos.RDS")

# UMAP 1 - Documentos ####
# Exclusão dos tópicos 5 e 55
topicos <- labels |>
  filter_out(categoria == "Excluído") |>
  pull(topic)

# Matriz Gamma
gamma <- stm_nutricao$theta |>
  as_tibble(.name_repair = "minimal") |>
  set_names(as.character(1:65)) |>
  select(all_of(as.character(topicos)))

# UMAP #
umap_docs <- gamma |>
  uwot::umap(
    n_neighbors = 15,
    min_dist = 0.1,
    metric = "cosine",
    seed = 10657 # RANDOM.ORG 2026-09-25 18:11:09 UTC
  ) |>
  as_tibble(.name_repair = ~ c("UMAP1", "UMAP2"))

# Tópicos Dominantes por Documento
gamma_docs <- gamma |>
  mutate(document = row_number()) |>
  pivot_longer(-document, names_to = "topic", values_to = "gamma") |>
  mutate(topic = parse_number(topic)) |>
  slice_max(gamma, n = 1, with_ties = FALSE, by = document)

# Adicionar categoria e label
gamma_docs <- gamma_docs |>
  left_join(
    labels |>
      select(topic, categoria, label),
    by = "topic"
  )

# Banco final com coordenadas UMAP
umap <- gamma_docs |>
  bind_cols(umap_docs)

umap |>
  ggplot(
    aes(
      x = UMAP1,
      y = UMAP2,
      color = categoria
    )
  ) +
  geom_point(
    alpha = 0.4,
    size = 3
  ) +
  labs(
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Categoria"
  ) +
  theme_minimal()


# UMAP POR CATEGORIAS ####
# Gamma com categorias
gamma_categorias <- gamma |>
  mutate(DOC_ID = metadados$DOC_ID, .before = 1) |>
  pivot_longer(
    -DOC_ID,
    names_to = "topic",
    values_to = "gamma"
  ) |>
  mutate(
    topic = parse_number(topic)
  ) |>
  left_join(
    labels |>
      select(topic, categoria),
    by = "topic"
  ) |>
  summarise(
    gamma = sum(gamma),
    .by = c(DOC_ID, categoria)
  )

# Categorias Dominantes em cada documento
categorias_doc <- gamma_categorias |>
  slice_max(
    gamma,
    n = 1,
    with_ties = FALSE,
    by = DOC_ID
  ) |>
  select(
    DOC_ID,
    categoria_dominante = categoria,
    gamma_dominante = gamma
  )

gamma_wide <- gamma_categorias |>
  pivot_wider(
    names_from = categoria,
    values_from = gamma
  )

umap_categorias <- gamma_wide |>
  select(-DOC_ID) |>
  uwot::umap(
    n_neighbors = 15,
    min_dist = 0.1,
    metric = "cosine",
    seed = 10657
  ) |>
  as_tibble(
    .name_repair = ~ c("UMAP1", "UMAP2")
  )

umap_categorias <- gamma_wide |>
  select(DOC_ID) |>
  bind_cols(umap_categorias) |>
  left_join(
    categorias_doc,
    by = "DOC_ID"
  )

umap_categorias |>
  ggplot(
    aes(
      x = UMAP1,
      y = UMAP2,
      color = categoria_dominante
    )
  ) +
  geom_point(
    alpha = 0.4,
    size = 1
  ) +
  labs(
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Categoria"
  ) +
  theme_minimal()
