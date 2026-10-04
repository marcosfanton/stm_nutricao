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
# Tabela Gamma
gamma_doc <- tidy(stm_nutricao, matrix = "gamma") |>
  left_join(
    metadados,
    by = c("document" = "DOC_ID")
  ) |>
  left_join(rotulos, by = "topic")

gamma_tb <- gamma_doc |>
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

# Tabelão
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
gtsave(tab3_gamma, here("04_relatorio", "tab3_gamma.docx"))


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

tidy_ano <- tidy_ano |>
  left_join(rotulos, by = "topic")

# Gráfico sem padronização
fig3_free <- tidy_ano |>
  filter_out(categoria == "Excluído") |>
  mutate(
    topic = forcats::fct_reorder(factor(topic), as.integer(factor(categoria)))
  ) |>
  ggplot(
    aes(
      covariate.value,
      estimate,
      ymin = ci.lower,
      ymax = ci.upper,
    )
  ) +
  geom_ribbon(alpha = .2) +
  geom_line(aes(color = categoria), linewidth = .8) +
  scale_color_manual(values = unname(palette.colors(palette = "Tableau 10"))) +
  facet_wrap(~topic) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  labs(x = "Ano", y = "") +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )

# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "fig3_efeitoano_sempadronizacao.png"),
  plot = fig3_free,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
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
  scale_color_manual(values = unname(palette.colors(palette = "Tableau 10"))) +
  facet_wrap(~topic) +
  coord_cartesian(ylim = c(-0.5, 2.5)) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  labs(x = "Ano", y = "") +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )

# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "fig3_efeitoano.png"),
  plot = fig3,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
)

# TABELA COM CATEGORIAS POR ANO ####
topcats <- gamma_doc |>
  summarise(gamma_topic = mean(gamma), .by = c(AN_BASE, topic, categoria)) |>
  summarise(
    gamma_cat = (sum(gamma_topic) * 100),
    .by = c(AN_BASE, categoria)
  ) |>
  arrange(AN_BASE, desc(gamma_cat))

# Gráfico
topcats |>
  filter_out(categoria == "Excluído") |>
  ggplot(aes(AN_BASE, gamma_cat, color = categoria)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = unname(palette.colors(palette = "Tableau 10"))) +
  labs(
    x = "Ano",
    y = "%",
    color = NULL,
    title = "Prevalência Estimada das Categorias por Ano"
  ) +
  theme_minimal()

# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "fig4_catsano.png"),
  plot = fig3,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
)

# TABELA COM 10 TÓPICOS MAIS PREVALENTES POR ANO ####
top10 <- gamma_doc |>
  summarise(
    GAMMA = (mean(gamma) * 100),
    .by = c(AN_BASE, topic, rotulo, categoria)
  ) |>
  slice_max(GAMMA, n = 10, by = AN_BASE) |>
  mutate(posicao = row_number(), .by = AN_BASE) |>
  arrange(AN_BASE, posicao)


# Tabela Completa
top10_tab1 <- top10 |> gt()

# Números apenas
top10_tab2 <- top10 |>
  select(AN_BASE, posicao, topic) |>
  pivot_wider(names_from = AN_BASE, values_from = topic) |>
  gt() |>
  cols_label(posicao = "Posição") |>
  cols_align(align = "center") |>
  tab_header(title = "Dez tópicos mais prevalentes por ano")

# Salvar Tabela 5
# Completa
gtsave(top10_tab2, here("04_relatorio", "top10n.html"))
