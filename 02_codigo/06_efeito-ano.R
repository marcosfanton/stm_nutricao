library(tidyverse)
library(here)
library(stm)
library(tidyr)
library(tidystm)
library(tidytext)
library(gt)
library(ggfittext)

# Bancos
stm_nutricao <- readRDS(here::here("01_dados", "stm65.RDS"))
metadados <- readRDS("01_dados/dados_resumos.RDS")
rotulos <- read.csv(here::here("01_dados", "tabela_rotulos.csv")) |>
  mutate(categoria = str_squish(categoria))
paleta_categorias <- readRDS(here::here("03_figs", "paleta_categorias.RDS"))

# EFEITO ANO ####
set.seed(64377) # 2026-10-05 13:12:48 UTC
efeito_ano <- stm::estimateEffect(
  1:65 ~ s(AN_BASE, df = 3),
  stmobj = stm_nutricao,
  metadata = metadados
)
# Salvar Análise
saveRDS(efeito_ano, here::here("01_dados", "efeito_ano.RDS"))

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

# DESCRIÇÃO PREVALÊNCIA (GAMMA) TOTAL DE TÓPICOS ####
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
  filter_out(categoria == "Excluído") |>
  arrange(desc(total), categoria, tipo, desc(GAMMA)) |>
  select(rotulo, GAMMA, tipo) |>
  gt() |>
  cols_hide(tipo) |>
  cols_label(
    rotulo = "Categoria/Tópico",
    GAMMA = "γ(%)"
  ) |>
  fmt_number(columns = GAMMA, decimals = 2, dec_mark = ",", sep_mark = ".") |>
  tab_header(title = "Prevalência Estimada dos Tópicos") |>
  tab_style(
    style = cell_text(size = px(20), weight = "bold"),
    locations = cells_title(groups = "title")
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
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
fgtsave(tab3_gamma, here("04_relatorio", "tab3_gamma.docx"))

# DESCRIÇÃO PREVALÊNCIA (GAMMA) TOTAL DE CATEGORIAS ####
# Tabela com Prevalência Total
cat_tab <- categorias_tb |>
  select(categoria, GAMMA) |>
  arrange(desc(GAMMA))


tab4_categorias <-
  cat_tab |>
  bind_rows(summarise(cat_tab, categoria = "Total", GAMMA = sum(GAMMA))) |>
  gt(locale = "pt") |>
  cols_label(categoria = "Categoria", GAMMA = "γ (%)") |>
  fmt_number(columns = GAMMA, decimals = 2) |>
  tab_header(title = "Prevalência Estimada das Categorias") |>
  cols_align(align = "center", columns = GAMMA) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_title(groups = "title"), cells_column_labels())
  ) |>
  tab_style(
    style = list(cell_text(weight = "bold"), cell_fill(color = "grey95")),
    locations = cells_body(rows = categoria == "Total")
  ) |>
  cols_width(
    categoria ~ px(320),
    GAMMA ~ px(90)
  ) |>
  tab_style(
    style = cell_fill(color = "grey95"),
    locations = cells_body(rows = seq(1, nrow(cat_tab) + 1, by = 2))
  ) |>
  tab_options(
    data_row.padding = px(4),
    table.font.size = px(14),
    heading.title.font.size = px(15),
    column_labels.border.bottom.color = "grey40",
    table_body.hlines.color = "grey90"
  )

# Salvar Tabela 3
gtsave(tab4_categorias, here("04_relatorio", "tab4_categorias.html"))


# Gráfico com Evolução da Prevalência das Categorias ####
evol_categorias <- gamma_doc |>
  summarise(gamma_topic = mean(gamma), .by = c(AN_BASE, topic, categoria)) |>
  summarise(
    gamma_cat = (sum(gamma_topic) * 100),
    .by = c(AN_BASE, categoria)
  ) |>
  arrange(AN_BASE, desc(gamma_cat))

# Gráfico Evolução de Categorias
fig4_evolcategorias <- evol_categorias |>
  filter_out(categoria == "Excluído") |>
  mutate(
    categoria = factor(categoria, levels = names(paleta_categorias))
  ) |>
  ggplot(aes(AN_BASE, gamma_cat, color = categoria)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 1.2, alpha = .8) +
  scale_color_manual(
    values = paleta_categorias,
    labels = \(x) str_wrap(x, 28)
  ) +
  scale_x_continuous(breaks = unique(evol_categorias$AN_BASE)) +
  scale_y_continuous(
    labels = scales::label_number(decimal.mark = ",", suffix = "%")
  ) +
  guides(
    color = guide_legend(
      #ncol = 3,
      override.aes = list(linewidth = 3, alpha = 1),
    )
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Evolução da prevalência das categorias por ano",
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10, face = "bold"),
    legend.key.spacing.y = unit(4, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    plot.title = element_text(face = "bold"),
  )

# Salvar Gráfico Evolução Categorias
ggsave(
  filename = here("04_relatorio", "fig4_evol-categorias.png"),
  plot = fig4_evolcategorias,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# DESCRIÇÃO PREVALÊNCIA ESTIMADA POR ANO ####
# Descrição Efeito Ano
tidy_ano <- readRDS(here::here("01_dados", "efeito_ano-tidy.RDS"))

# INCLUSÃO DE RÓTULOS
tidy_ano <- tidy_ano |>
  left_join(rotulos, by = "topic")

# RÓTULOS PARA GRÁFICO
rotulos_faceta <- rotulos |>
  mutate(rotulo = str_trunc(rotulo, 45, ellipsis = "…") |> str_wrap(25)) |>
  select(topic, rotulo) |>
  deframe()

# GRÁFICO 03 ####
fig3_free <-
  tidy_ano |>
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
    ncol = 11,
    scales = "free_y",
    labeller = as_labeller(rotulos_faceta)
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
    title = "Prevalência Estimada de Tópicos por Ano (2011-2024)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.key.spacing.y = unit(4, "pt"),
    strip.text = element_text(
      size = 9,
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
    plot.caption = element_text(face = "bold", color = "grey40", hjust = 1)
  )

# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "fig3_efeito-ano.png"),
  plot = fig3_free,
  width = 16,
  height = 10,
  dpi = 300,
  bg = "white"
)
# TABELA - TOP 10 TÓPICOS ####
top10_tb <- gamma_tb |>
  mutate(categoria = str_squish(categoria)) |>
  filter_out(categoria == "Excluído") |>
  slice_max(GAMMA, n = 10, with_ties = FALSE) |>
  select(rotulo, categoria, GAMMA)

# Tabela Top 10
tab5_top10 <-
  top10_tb |>
  gt(locale = "pt") |>
  fmt_number(columns = GAMMA, decimals = 2) |>
  cols_label(
    rotulo = "Tópico",
    categoria = "Categoria",
    GAMMA = "γ (%)"
  ) |>
  tab_header(title = "Dez tópicos mais prevalentes") |>
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

# Salvar Tabela 5
gtsave(tab5_top10, here("04_relatorio", "tab5_top10.html"))

# GRÁFICO 4 - TOP 10 TÓPICOS - EVOLUÇÃO ####
top10 <- gamma_tb |>
  slice_max(GAMMA, n = 10) |>
  select(topic, GAMMA)

# Gráfico
fig5_evoltop10 <-
  tidy_ano |>
  inner_join(top10, by = "topic") |>
  mutate(
    categoria = str_squish(categoria) |>
      factor(levels = names(paleta_categorias)),
    rotulo = str_squish(rotulo) |>
      fct_reorder(GAMMA, .desc = TRUE)
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
    labels = \(x) str_wrap(x, 20),
    aesthetics = c("color", "fill")
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(
      min(tidy_ano$covariate.value),
      max(tidy_ano$covariate.value),
      by = 3
    )
  ) +
  facet_wrap(
    ~rotulo,
    ncol = 5,
    labeller = label_wrap_gen(width = 28)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 3, alpha = 1),
      nrow = 1
    ),
    fill = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Evolução dos 10 tópicos mais prevalentes",
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10, face = "bold"),
    strip.text = element_text(
      size = 10,
      face = "bold",
      hjust = 0,
      lineheight = 0.9,
      margin = margin(1, 0, 2, 0)
    ),
    panel.spacing = unit(8, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.text = element_text(size = 10, color = "grey40"),
    axis.text.x = element_text(
      size = 10,
      #color = "grey40",
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey40", hjust = 1)
  )

# Salvar Gráfico Evolução Top10
ggsave(
  filename = here("04_relatorio", "fig5_evoltop10.png"),
  plot = fig5_evoltop10,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# TABELA 5 - TOP 10 TÓPICOS POR ANO ####
top10_ano <- gamma_doc |>
  summarise(
    GAMMA = mean(gamma) * 100,
    .by = c(AN_BASE, topic, rotulo, categoria)
  ) |>
  slice_max(GAMMA, n = 10, by = AN_BASE, with_ties = FALSE) |>
  mutate(
    posicao = min_rank(desc(GAMMA)),
    .by = AN_BASE
  ) |>
  arrange(AN_BASE, posicao)


# Gráfico
fig6_top10ano <-
  top10_ano |>
  mutate(categoria = factor(categoria, levels = names(paleta_categorias))) |>
  ggplot(aes(factor(AN_BASE), posicao, fill = categoria)) +
  geom_tile(color = "white", alpha = .6) +
  geom_text(aes(label = str_wrap(rotulo, 18)), size = 2.6, lineheight = 0.9) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks = 1:10, expand = c(0, 0)) +
  scale_fill_manual(values = paleta_categorias) +
  guides(fill = guide_legend(nrow = 2, override.aes = list(alpha = .6))) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 7.5, face = "bold"),
    panel.grid = element_blank(),
    axis.text.x.top = element_text(
      face = "bold",
      size = 16,
      margin = margin(b = 2)
    ),
    axis.text.y = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "grey40", hjust = 1)
  )

# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "fig6_top10ano.png"),
  plot = fig6_top10ano,
  width = 14,
  height = 10,
  dpi = 300,
  bg = "white"
)

# TOP10 - VARIAÇÃO DE PREVALÊNCIA ####
variacao_topicos <- tidy_ano |>
  filter_out(categoria == "Excluído") |>
  arrange(topic, covariate.value) |>
  summarise(
    inicial = first(estimate),
    final = last(estimate),
    .by = c(topic, rotulo, categoria)
  ) |>
  mutate(variacao = (final - inicial) * 100)

# Top 10 -- 5+ e 5-
top_variacao <- bind_rows(
  slice_max(variacao_topicos, variacao, n = 5, with_ties = FALSE),
  slice_min(variacao_topicos, variacao, n = 5, with_ties = FALSE)
) |>
  mutate(
    rotulo_painel = str_glue(
      "{rotulo} ({scales::number(variacao, 
      accuracy = 0.01, 
      decimal.mark = ',', 
      style_positive = 'plus')} p.p.)"
    )
  ) |>
  select(topic, variacao, rotulo_painel)

# Gráfico Top10 Variação
fig7_variacao <-
  tidy_ano |>
  inner_join(top_variacao, by = "topic") |>
  mutate(
    categoria = str_squish(categoria) |>
      factor(levels = names(paleta_categorias)),
    rotulo_painel = fct_reorder(rotulo_painel, variacao, .desc = TRUE)
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
    labels = \(x) str_wrap(x, 20),
    aesthetics = c("color", "fill")
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1)
  ) +
  scale_x_continuous(
    breaks = seq(
      min(tidy_ano$covariate.value),
      max(tidy_ano$covariate.value),
      by = 3
    )
  ) +
  facet_wrap(
    ~rotulo_painel,
    ncol = 5,
    labeller = label_wrap_gen(width = 28)
  ) +
  guides(
    color = guide_legend(
      override.aes = list(linewidth = 3, alpha = 1),
      nrow = 1
    ),
    fill = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Tópicos com maior variação de prevalência no período"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 10, face = "bold"),
    strip.text = element_text(
      size = 10,
      face = "bold",
      hjust = 0,
      lineheight = 0.9,
      margin = margin(1, 0, 2, 0)
    ),
    panel.spacing = unit(8, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.text = element_text(size = 10, color = "grey40"),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey40", hjust = 1)
  )

# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "fig7_variacao.png"),
  plot = fig7_variacao,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)
