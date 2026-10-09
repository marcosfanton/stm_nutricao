library(tidyverse)
library(here)
library(stm)
library(tidyr)
library(tidystm)
library(tidytext)
library(gt)
library(patchwork)

# Bancos
stm_nutricao <- readRDS(here::here("01_dados", "stm65.RDS"))
metadados <- readRDS("01_dados/dados_resumos.RDS")
rotulos <- read.csv(here::here("01_dados", "tabela_rotulos.csv")) |>
  mutate(categoria = str_squish(categoria))
paleta_categorias <- readRDS(here::here("03_figs", "paleta_categorias.RDS"))

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


# TABELA TOP10 TÓPICOS ####
top10_topicos <- gamma_tb |>
  mutate(categoria = str_squish(categoria)) |>
  filter_out(categoria == "Excluído") |>
  slice_max(GAMMA, n = 10, with_ties = FALSE) |>
  pull(topic)

rotulos_top10 <- tidy_ano |>
  distinct(topic, rotulo) |>
  mutate(rotulo = str_wrap(str_squish(rotulo), 30)) |>
  tibble::deframe()


tidy_ano |>
  filter(topic %in% top10_topicos) |>
  mutate(
    categoria = factor(
      str_squish(categoria),
      levels = names(paleta_categorias)
    ),
    topic = factor(topic, levels = top10_topicos)
  ) |>
  ggplot(
    aes(
      covariate.value,
      estimate,
      ymin = ci.lower,
      ymax = ci.upper
    )
  ) +
  geom_ribbon(aes(fill = categoria), alpha = .2) +
  geom_line(aes(color = categoria), linewidth = 1) +
  scale_color_manual(
    values = paleta_categorias,
    aesthetics = c("color", "fill")
  ) +
  facet_wrap(
    ~topic,
    ncol = 5,
    scales = "free_y",
    labeller = as_labeller(rotulos_top10)
  ) +
  guides(
    color = guide_legend(
      ncol = 3,
      override.aes = list(linewidth = 3, alpha = 1)
    ),
    fill = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Efeito do ano sobre os dez tópicos mais prevalentes",
    subtitle = paste0(
      anos[1],
      " a ",
      anos[2],
      " | escala vertical própria de cada painel"
    ),
    caption = "Fonte: Catálogo de Teses e Dissertações da CAPES"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8),
    strip.text = element_text(
      size = 8,
      hjust = 0,
      lineheight = 0.9,
      margin = margin(2, 0, 2, 0)
    ),
    panel.spacing = unit(8, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey40", hjust = 1)
  )

top10_tb <- gamma_tb |>
  mutate(categoria = str_squish(categoria)) |>
  filter_out(categoria == "Excluído") |>
  slice_max(GAMMA, n = 10, with_ties = FALSE) |>
  select(rotulo, categoria, GAMMA)

# Tabela

top10_tb |>
  gt(locale = "pt") |>
  fmt_number(columns = GAMMA, decimals = 2) |>
  cols_label(
    rotulo = "Tópico",
    categoria = "Categoria",
    GAMMA = "γ (%)"
  ) |>
  tab_header(title = "Dez tópicos mais prevalentes no período") |>
  tab_source_note("Fonte: Catálogo de Teses e Dissertações da CAPES") |>
  cols_align(align = "left", columns = c(rotulo, categoria)) |>
  cols_align(align = "right", columns = GAMMA) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_title(groups = "title"), cells_column_labels())
  ) |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes()
  ) |>
  tab_style(
    style = cell_fill(color = "grey95"),
    locations = cells_body(rows = seq(1, nrow(top10_tb), by = 2))
  ) |>
  data_color(
    columns = categoria,
    target_columns = everything(),
    fn = \(x) unname(paleta_categorias[x]),
    alpha = 0.5,
    autocolor_text = FALSE
  ) |>
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) |>
  tab_options(
    data_row.padding = px(4),
    table.font.size = px(14),
    column_labels.border.bottom.color = "grey40",
    table_body.hlines.color = "grey90"
  )

# EFEITO ANO ####
set.seed(64377) # 2026-10-05 13:12:48 UTC
efeito_ano <- stm::estimateEffect(
  1:65 ~ s(AN_BASE, df = 3),
  stmobj = stm_nutricao,
  metadata = metadados
)
# Salvar Análise
saveRDS(efeito_ano, here::here("01_dados", "efeito_ano.RDS"))

set.seed(64377)
# Extração dos efeitos
tidy_ano <- tidystm::extract.estimateEffect(
  x = efeito_ano,
  covariate = "AN_BASE",
  model = stm_nutricao,
  method = "continuous",
  labeltype = "frex",
  n = 2
)
# Salvar análise
saveRDS(tidy_ano, here::here("01_dados", "efeito_ano-tidy.RDS"))

#
tidy_ano <- readRDS(here::here("01_dados", "efeito_ano-tidy.RDS"))

# Inclusão de rótulos
tidy_ano <- tidy_ano |>
  left_join(rotulos, by = "topic")

# Gráfico sem padronização
fig3_free <-
  tidy_ano |>
  mutate(categoria = str_squish(categoria)) |>
  filter_out(categoria == "Excluído") |>
  mutate(
    categoria = factor(categoria, levels = names(paleta_categorias)),
    topic = forcats::fct_reorder(factor(topic), as.integer(categoria))
  ) |>
  ggplot(
    aes(
      covariate.value,
      estimate,
      ymin = ci.lower,
      ymax = ci.upper
    )
  ) +
  geom_ribbon(aes(fill = categoria), alpha = .2) +
  geom_line(aes(color = categoria), linewidth = .8) +
  scale_color_manual(
    values = paleta_categorias,
    labels = \(x) str_wrap(x, 28),
    aesthetics = c("color", "fill")
  ) +
  facet_wrap(
    ~topic,
    ncol = 9,
    scales = "free_y",
    labeller = as_labeller(rotulos_painel)
  ) +
  guides(
    color = guide_legend(
      #  ncol = 3,
      override.aes = list(linewidth = 3, alpha = 1)
    ),
    fill = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Efeito do ano sobre a prevalência dos tópicos",
    subtitle = paste0(
      anos[1],
      " a ",
      anos[2],
      " | escala vertical própria de cada painel"
    ),
    caption = "Fonte: Catálogo de Teses e Dissertações da CAPES"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8),
    legend.key.spacing.y = unit(4, "pt"),
    strip.text = element_text(
      size = 6,
      hjust = 0,
      lineheight = 0.9,
      margin = margin(1, 0, 1, 0)
    ),
    panel.spacing = unit(3, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.text = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey40", hjust = 1)
  )


# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "teste.png"),
  plot = teste,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
)

# Gráfico com apenas os TOP10 ####

# Gráfico
fig4_catsano <- topcats |>
  filter_out(categoria == "Excluído") |>
  ggplot(aes(AN_BASE, gamma_cat, color = categoria)) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = unname(palette.colors(palette = "Tableau 10"))) +
  guides(color = guide_legend(override.aes = list(linewidth = 2))) +
  labs(
    x = "Ano",
    y = "%",
    color = NULL,
    title = "Prevalência Categorias por Ano"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 6)
  )

# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "fig4_catsano.png"),
  plot = fig4_catsano,
  width = 10,
  height = 6,
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

# Gráfico Top10
fig5_top10 <- top10 |>
  ggplot(aes(factor(AN_BASE), posicao, fill = categoria)) +
  geom_tile(color = "white", alpha = .6) +
  geom_text(aes(label = str_wrap(rotulo, 16)), size = 3, lineheight = 1) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks = 1:10, expand = c(0, 0)) +
  scale_fill_manual(values = unname(palette.colors(palette = "Tableau 10"))) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Top10 Tópicos por Ano"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.x.top = element_text(face = "bold", size = 16),
    margin = margin(b = 2)
  )

# Salvar gráfico
ggsave(
  here("04_relatorio", "fig5_top10.png"),
  plot = fig5_top10,
  width = 16,
  height = 9,
  dpi = 300,
  bg = "white"
)
