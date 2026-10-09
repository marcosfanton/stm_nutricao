# Pacotes
library(tidyverse)
library(here)
library(gt)
library(geobr)
library(sf)
library()

# Banco -- n:5.280
dados <- readRDS(here::here("01_dados", "catalogo_limpo.RDS"))

# ANO ####
base_ano <- dados |>
  count(AN_BASE, NM_GRAU_ACADEMICO) |>
  pivot_wider(
    names_from = NM_GRAU_ACADEMICO,
    values_from = n,
    values_fill = 0,
    names_glue = "{NM_GRAU_ACADEMICO}_N"
  ) |>
  mutate(AN_BASE = as.character(AN_BASE))

# Gráfico ANO ####
graf_ano <- dados |>
  count(AN_BASE, NM_GRAU_ACADEMICO) |>
  bind_rows(
    dados |>
      count(AN_BASE) |>
      mutate(NM_GRAU_ACADEMICO = "TOTAL")
  )

# Paleta Cores
cores_grau <- c(
  "Total" = "grey25",
  "Mestrado" = "#5b859e",
  "Doutorado" = "#af4f2f"
)

fig1 <- graf_ano |>
  mutate(
    NM_GRAU_ACADEMICO = str_to_title(NM_GRAU_ACADEMICO) |>
      factor(levels = names(cores_grau))
  ) |>
  ggplot(aes(AN_BASE, n, color = NM_GRAU_ACADEMICO)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.2, alpha = 0.8) +
  scale_x_continuous(breaks = unique(graf_ano$AN_BASE)) +
  scale_color_manual(values = cores_grau) +
  labs(
    x = "Ano",
    y = "n",
    color = NULL,
    title = "Evolução de dissertações e teses em Nutrição por ano (n:5280)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3)
  )

# Salvar gráfico
ggsave(
  filename = here("04_relatorio", "fig1_ano.png"),
  plot = fig1,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

# TABELA ANO ####
tab_ano <- dados |>
  count(AN_BASE, NM_GRAU_ACADEMICO) |>
  pivot_wider(
    names_from = NM_GRAU_ACADEMICO,
    values_from = n,
    values_fill = 0,
    names_glue = "{NM_GRAU_ACADEMICO}_N"
  ) |>
  mutate(
    AN_BASE = as.character(AN_BASE),
    TOTAL_N = MESTRADO_N + DOUTORADO_N,
    TOTAL_FREQ = TOTAL_N / sum(TOTAL_N) * 100
  )

tab_ano <- tab_ano |>
  bind_rows(summarise(
    tab_ano,
    AN_BASE = "Total",
    across(where(is.numeric), sum)
  )) |>
  mutate(
    MESTRADO_FREQ = ((MESTRADO_N / TOTAL_N) * 100),
    DOUTORADO_FREQ = ((DOUTORADO_N / TOTAL_N) * 100)
  )

# Tabela
tab1_ano <- tab_ano |>
  gt(locale = "pt") |>
  fmt_integer(columns = ends_with("_N")) |>
  fmt_number(columns = ends_with("_FREQ"), decimals = 1) |>
  cols_merge(
    columns = c(MESTRADO_N, MESTRADO_FREQ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_merge(
    columns = c(DOUTORADO_N, DOUTORADO_FREQ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_merge(
    columns = c(TOTAL_N, TOTAL_FREQ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_move(columns = DOUTORADO_N, after = MESTRADO_N) |>
  cols_label(
    AN_BASE = "Ano",
    MESTRADO_N = "Mestrado",
    DOUTORADO_N = "Doutorado",
    TOTAL_N = "Total"
  ) |>
  tab_header(title = "Evolução de trabalhos em Nutrição por ano | n(%)") |>
  tab_source_note("Fonte: Catálogo de Teses e Dissertações da CAPES") |>
  cols_align(align = "left", columns = AN_BASE) |>
  cols_align(align = "right", columns = c(MESTRADO_N, DOUTORADO_N, TOTAL_N)) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = TOTAL_N)
  ) |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_title(groups = "title"), cells_column_labels())
  ) |>
  opt_row_striping() |>
  tab_options(
    data_row.padding = px(4),
    table.font.size = px(14),
    column_labels.border.bottom.color = "grey40",
    table_body.hlines.color = "grey90",
    row.striping.background_color = "grey95"
  )

# Salvar Tabela 1
gtsave(tab1_ano, here("04_relatorio", "tab1_ano.html"))

# UF ####
base_uf <- dados |>
  count(SG_UF_IES) |>
  mutate(FREQ = n / sum(n) * 100) |>
  arrange(desc(n))

# Tabela UF
#tab2_uf <-
base_uf |>
  bind_rows(
    summarise(base_uf, SG_UF_IES = "Total", n = sum(n), FREQ = sum(FREQ))
  ) |>
  gt(locale = "pt") |>
  fmt_integer(columns = n) |>
  fmt_number(columns = FREQ, decimals = 1) |>
  cols_merge(columns = c(n, FREQ), pattern = "{1} ({2}%)") |>
  cols_label(SG_UF_IES = "UF", n = "Trabalhos") |>
  tab_header(title = "Distribuição de trabalhos por UF") |>
  # tab_source_note("Fonte: Catálogo de Teses e Dissertações da CAPES") |>
  cols_align(align = "center", columns = SG_UF_IES) |>
  cols_align(align = "center", columns = n) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_title(groups = "title"), cells_column_labels())
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = SG_UF_IES == "Total")
  ) |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes()
  ) |>
  tab_style(
    style = cell_fill(color = "grey95"),
    locations = cells_body(rows = seq(1, nrow(base_uf) + 1, by = 2))
  ) |>
  cols_width(
    SG_UF_IES ~ px(70),
    n ~ px(130)
  ) |>
  tab_options(
    data_row.padding = px(4),
    table.font.size = px(14),
    column_labels.border.bottom.color = "grey40",
    table_body.hlines.color = "grey90"
  )

# Salvar Tabela 2 - UF
gtsave(tab2_uf, here("04_relatorio", "tab2_uf.html"))

# Mapa do Brasil
ufs <- read_state(year = 2020, showProgress = FALSE)

mapa_uf <- ufs |>
  left_join(base_uf, by = c("abbrev_state" = "SG_UF_IES"))

# Gráfico
limite <- mean(range(mapa_uf$FREQ, na.rm = TRUE)) # Texto Branco

ggplot(mapa_uf) +
  geom_sf(aes(fill = FREQ), color = "white", linewidth = 0.05) +
  geom_sf_text(
    aes(
      label = ifelse(
        is.na(FREQ),
        "",
        scales::number(FREQ, accuracy = 0.1, decimal.mark = ",")
      ),
      color = FREQ > limite
    ),
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_gradientn(
    colors = met.brewer("Hokusai2"),
    na.value = "grey90",
    labels = scales::label_number(decimal.mark = ",", suffix = "%")
  ) +
  scale_color_manual(
    values = c("TRUE" = "white", "FALSE" = "grey15"),
    na.value = "grey15",
    guide = "none"
  ) +
  guides(
    fill = guide_colorbar(barheight = unit(40, "mm"), barwidth = unit(3, "mm"))
  ) +
  labs(
    title = "Distribuição dos trabalhos em Nutrição por Estado (%)",
    fill = NULL,
    caption = "Fonte: Catálogo de Teses e Dissertações da CAPES"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(color = "grey40", hjust = 1)
  )
