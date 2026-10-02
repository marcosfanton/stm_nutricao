library(tidyverse)
library(here)
library(stm)
library(tidyr)
library(tidystm)
library(tidytext)
library(gt)

# Bancos
stm_nutricao <- readRDS(here::here("01_dados", "stm65.RDS"))
rotulos <- read.csv(here::here("01_dados", "tabela_rotulos.csv"))
metadados <- readRDS("01_dados/dados_resumos.RDS")

# Descrição Prevalência (gamma) Total ####
gamma_tb <- tidy(stm_nutricao, matrix = "gamma") |>
  group_by(topic) |>
  summarise(
    GAMMA = (mean(gamma) * 100),
    .groups = "drop"
  ) |>
  left_join(rotulos, by = "topic") |>
  mutate(total = sum(GAMMA), .by = categoria) |>
  arrange(desc(total), desc(GAMMA))

topicos_tb <- gamma_tb |>
  mutate(tipo = "topico")

categorias_tb <- gamma_tb |>
  distinct(categoria, total) |>
  mutate(
    rotulo = categoria,
    GAMMA = total,
    topic = NA_integer_,
    tipo = "categoria"
  )

tab3_gamma <- bind_rows(categorias_tb, topicos_tb) |>
  arrange(desc(total), categoria, tipo, desc(GAMMA)) |>
  select(rotulo, topic, GAMMA, tipo) |>
  gt() |>
  cols_hide(tipo) |>
  cols_merge(columns = c(rotulo, topic), pattern = "{1}<< ({2})>>") |>
  cols_label(
    rotulo = "Categoria/Tópico",
    GAMMA = "γ(%)"
  ) |>
  fmt_number(columns = GAMMA, decimals = 2, dec_mark = ",", sep_mark = ".") |>
  tab_style(
    style = list(cell_text(weight = "bold"), cell_fill(color = "gray95")),
    locations = cells_body(rows = tipo == "categoria")
  ) |>
  tab_style(
    style = cell_text(indent = px(16)),
    locations = cells_body(columns = rotulo, rows = tipo == "topico")
  )

# Salvar Tabela 3
gtsave(tab3_gamma, here("04_relatorio", "tab3_gamma.html"))


# EFEITO ANO ####
efeito_ano <- stm::estimateEffect(
  1:65 ~ s(AN_BASE, df = 3),
  stmobj = stm_nutricao,
  metadata = metadados
)

# Extração dos efeitos
tidy_ano <- tidystm::extract.estimateEffect(
  x = efeito_ano,
  covariate = "AN_BASE",
  model = stm_nutricao,
  method = "continuous",
  labeltype = "frex",
  n = 2
)

# Normalização de cada tópico
tidy_ano <- tidy_ano |>
  mutate(
    media = mean(estimate),
    indice = estimate / media,
    indice_lower = ci.lower / media,
    indice_upper = ci.upper / media,
    .by = topic
  ) |>
  left_join(rotulos, by = "topic")

# Gráfico
fig3 <- tidy_ano |>
  filter_out(categoria == "Excluído") |>
  mutate(
    topic = forcats::fct_reorder(factor(topic), as.integer(factor(categoria)))
  ) |>
  ggplot(
    aes(
      covariate.value,
      indice,
      ymin = indice_lower,
      ymax = indice_upper,
    )
  ) +
  geom_ribbon(alpha = .2) +
  geom_line(aes(color = categoria), linewidth = .8) +
  facet_wrap(~topic) +
  coord_cartesian(ylim = c(-0.5, 2.5)) +
  labs(x = "Ano", y = "") +
  theme(legend.position = "top")


tidy_ano |>

  ggplot(aes(
    covariate.value,
    indice,
    ymin = indice_lower,
    ymax = indice_upper
  )) +
  geom_ribbon(alpha = .3) +
  geom_line(aes(color = categoria)) +
  facet_wrap(~topic) +
  coord_cartesian(ylim = c(-0.5, 3)) +
  labs(
    x = "Ano",
    y = "Proporção relativa à média do tópico (média = 1)",
    color = "Categoria"
  ) +
  theme(legend.position = "bottom")


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
