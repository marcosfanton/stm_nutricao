# Pacotes
library(tidyverse)
library(here)
library(gt)
library(geobr)
library(sf)

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

fig1 <- graf_ano |>
  ggplot(aes(AN_BASE, n, color = NM_GRAU_ACADEMICO)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(graf_ano$AN_BASE)) +
  labs(
    x = "Ano",
    y = "N",
    color = NULL,
    title = "Evolução de dissertações e teses por ano"
  ) +
  theme_minimal()

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
  gt() |>
  fmt_number(columns = ends_with("_FREQ"), decimals = 1, dec_mark = ",") |>
  cols_merge(
    columns = c(MESTRADO_N, MESTRADO_FREQ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_merge(
    columns = c(DOUTORADO_N, DOUTORADO_FREQ),
    pattern = "{1} ({2}%)"
  ) |>
  cols_merge(columns = c(TOTAL_N, TOTAL_FREQ), pattern = "{1} ({2}%)") |>
  cols_move(columns = DOUTORADO_N, after = MESTRADO_N) |>
  cols_label(
    AN_BASE = "Ano",
    MESTRADO_N = "Mestrado",
    DOUTORADO_N = "Doutorado",
    TOTAL_N = "Total"
  )

# Salvar Tabela 1
gtsave(tab1_ano, here("04_relatorio", "tab1_ano.html"))

# UF ####
base_uf <- dados |>
  count(SG_UF_IES) |>
  mutate(FREQ = n / sum(n) * 100) |>
  arrange(desc(n))

# Tabela UF
tab2_uf <- base_uf |>
  bind_rows(
    summarise(base_uf, SG_UF_IES = "Total", n = sum(n), FREQ = sum(FREQ))
  ) |>
  gt() |>
  fmt_number(columns = FREQ, decimals = 1, dec_mark = ",") |>
  cols_merge(columns = c(n, FREQ), pattern = "{1} ({2}%)") |>
  cols_label(SG_UF_IES = "UF", n = "Trabalhos") |>
  cols_align(align = "left", columns = SG_UF_IES) |>
  cols_align(align = "right", columns = n) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = SG_UF_IES == "Total")
  )

# Salvar Tabela 2 - UF
gtsave(tab2_uf, here("04_relatorio", "tab2_uf.html"))

# Mapa do Brasil
ufs <- read_state(year = 2020, showProgress = FALSE)

mapa_uf <- ufs |>
  left_join(base_uf, by = c("abbrev_state" = "SG_UF_IES"))

ggplot(mapa_uf) +
  geom_sf(aes(), color = "white", linewidth = 0.2) +
  geom_sf_text(
    aes(
      label = ifelse(
        is.na(FREQ),
        "",
        scales::number(FREQ, accuracy = 0.1, decimal.mark = ",")
      )
    ),
    size = 2
  ) +
  theme_void() +
  theme(legend.position = "right")
