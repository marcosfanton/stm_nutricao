# Pacotes
library(tidyverse)
library(gt)

# Banco -- n:5.280
dados <- readRDS(here::here("01_dados", "catalogo_limpo.RDS"))

# ANO
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

graf_ano |>
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
tab_ano |>
  gt() |>
  fmt_number(columns = ends_with("_FREQ"), decimals = 1, dec_mark = ",") |>
  cols_merge(columns = c(MESTRADO_N, MESTRADO_FREQ), pattern = "{1} ({2}%)") |>
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
