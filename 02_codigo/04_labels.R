# Pacotes
library(tidyverse)
library(here)
library(uwot)

stm_nutricao <- readRDS(here::here("01_dados", "stm65.RDS"))
rotulos <- read.csv2(here::here("01_dados", "tabela_labels-10.csv"))
metadados <- readRDS("01_dados/dados_resumos.RDS")
########################### MODIFICAR LABEL POR ROTULO ############
# UMAP - Documentos ####
# Matriz Gamma
gamma <- stm_nutricao$theta |>
  as_tibble(.name_repair = "minimal") |>
  set_names(as.character(1:65)) |>
  mutate(DOC_ID = metadados$DOC_ID, .before = 1)

# Tópicos Dominantes por Documento
gamma_docs <- gamma |>
  pivot_longer(
    -DOC_ID,
    names_to = "topic",
    values_to = "gamma",
    names_transform = as.integer
  ) |>
  mutate(gamma = gamma / sum(gamma), .by = DOC_ID) |>
  left_join(rotulos |> select(topic, categoria, label), by = "topic")

# Categoria Dominante em cada Documento
categorias <- gamma_docs |>
  summarise(gamma_categoria = sum(gamma), .by = c(DOC_ID, categoria)) |>
  slice_max(gamma_categoria, n = 1, with_ties = FALSE, by = DOC_ID) |>
  rename(categoria_dominante = categoria)

# Tópico Dominante em cada Documento
topicos <- gamma_docs |>
  slice_max(gamma, n = 1, with_ties = FALSE, by = DOC_ID) |>
  select(DOC_ID, topico_dominante = topic, gamma_topico = gamma)

# UMAP sobre tópicos ####
umap_docs <- gamma |>
  select(-DOC_ID) |>
  as.matrix() |>
  uwot::umap(
    n_neighbors = 15,
    min_dist = 0.1,
    metric = "cosine",
    seed = 10657, # RANDOM.ORG 2026-09-25 18:11:09 UTC
    n_threads = 1,
    n_sgd_threads = 1
  )

# UMAP dataset #
umap_topic <- umap_docs |>
  as_tibble(.name_repair = ~ c("UMAP1", "UMAP2")) |>
  mutate(DOC_ID = gamma$DOC_ID, .before = 1) |>
  left_join(categorias, by = "DOC_ID") |>
  left_join(topicos, by = "DOC_ID")

umap_topic |>
  ggplot(
    aes(
      x = UMAP1,
      y = UMAP2,
      color = categoria_dominante
    )
  ) +
  geom_point(
    #alpha = 0.4,
    size = 2
  ) +
  labs(
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Categoria"
  ) +
  theme_minimal()


##########################################################################
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
    rotulos |>
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
    alpha = 0.5,
    size = 2.5
  ) +
  labs(
    x = "UMAP 1",
    y = "UMAP 2",
    color = "Categoria"
  ) +
  theme_minimal()


# Adicionar categoria e label
gamma_docs <- gamma_docs |>
  left_join(
    rotulos |>
      select(topic, categoria, label),
    by = "topic"
  )
