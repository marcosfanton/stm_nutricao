library(tidyverse)
library(here)
library(stm)
library(tidystm)
library(gt)

# Bancos
stm_nutricao <- readRDS(here::here("01_dados", "stm65.RDS"))
rotulos <- read.csv(here::here("01_dados", "tabela_rotulos.csv"))
metadados <- readRDS("01_dados/dados_resumos.RDS")

# Efeito ano
efeito_ano <- stm::estimateEffect(
  1:65 ~ s(AN_BASE, df = 3),
  stmobj = stm_nutricao,
  metadata = metadados
)

tidy_ano <- tidystm::extract.estimateEffect(
  x = efeito_ano,
  covariate = "AN_BASE",
  model = stm_nutricao,
  method = "continuous",
  labeltype = "frex",
  n = 2
)

# Gráfico
ggplot(
  tidy_ano,
  aes(covariate.value, indice, ymin = ci.lower, ymax = ci.upper)
) +
  facet_wrap(
    ~topic,
    scales = "free_y"
  ) +
  geom_ribbon(alpha = .5) +
  geom_line() +
  labs(x = "Ano", y = "Proporção esperada do tópico")

# Normalização de cada tópico
tidy_ano <- tidy_ano |>
  mutate(
    media = mean(estimate),
    indice = estimate / media,
    indice_lower = ci.lower / media,
    indice_upper = ci.upper / media,
    .by = topic
  )


# Inclusão de rótulos na tabela tidy
tidy_ano <- tidy_ano |>
  left_join(rotulos, by = "topic")

# Gráfico
ggplot(
  tidy_ano,
  aes(covariate.value, indice, ymin = indice_lower, ymax = indice_upper)
) +
  geom_ribbon(alpha = .3) +
  geom_line() +
  facet_wrap(~topic) +
  coord_cartesian(ylim = c(-0.5, 3)) +
  labs(x = "Ano", y = "Proporção relativa à média do tópico (média = 1)")

# TABELA COM 10 TÓPICOS MAIS PREVALENTES POR ANO ####
theta_ano <- as_tibble(
  stm_nutricao$theta,
  .name_repair = ~ paste0("Topic", seq_along(.))
) |>
  mutate(AN_BASE = metadados$AN_BASE) |>
  pivot_longer(-AN_BASE, names_to = "topic", values_to = "gamma") |>
  mutate(topic = as.integer(parse_number(topic))) |>
  left_join(rotulos, by = "topic")

# Tabela com Tópicos mais prevalentes
top10_ano <- theta_ano |>
  summarise(gamma_medio = mean(gamma), .by = c(AN_BASE, rotulo)) |>
  slice_max(gamma_medio, n = 10, by = AN_BASE, with_ties = FALSE) |>
  arrange(AN_BASE, desc(gamma_medio))

top10_ano |> gt()

# Tabela com Categorias mais prevalentes
topcat_ano <- theta_ano |>
  summarise(gamma_topic = mean(gamma), .by = c(AN_BASE, topic, categoria)) |>
  summarise(gamma_cat = sum(gamma_topic) * 100, .by = c(AN_BASE, categoria)) |>
  arrange(AN_BASE, desc(gamma_cat))

# Gráfico
topcat_ano |>
  filter_out(categoria == "Excluído") |>
  ggplot(aes(AN_BASE, gamma_cat, color = categoria)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Ano",
    y = "",
    color = NULL,
    title = "Prevalência Estimada das Categorias por Ano"
  ) +
  theme_minimal()
