library(tidyverse)
library(here)
library(stm)
library(tidytext)
library(gt)
library(ggfittext)

# Bancos
stm_nutricao <- readRDS(here::here("01_dados", "stm65.RDS"))
metadados <- readRDS("01_dados/dados_resumos.RDS")
rotulos <- read.csv(here::here("01_dados", "tabela_rotulos.csv")) |>
  mutate(categoria = str_squish(categoria))
paleta_categorias <- readRDS(here::here("03_figs", "paleta_categorias.RDS"))

# Tabela Gamma (prevalência de cada tópico em cada documento)
gamma_doc <- tidy(stm_nutricao, matrix = "gamma") |>
  left_join(
    metadados,
    by = c("document" = "DOC_ID")
  ) |>
  left_join(rotulos, by = "topic")


# Número de documentos por região (para o cabeçalho da tabela)
n_regiao <- metadados |>
  count(NM_REGIAO, name = "n_docs")

# Prevalência média (γ) de cada tópico por região
gamma_regiao <- gamma_doc |>
  filter_out(categoria == "Excluído") |>
  summarise(
    GAMMA = mean(gamma) * 100,
    .by = c(NM_REGIAO, topic, rotulo, categoria)
  )

# Top 3 por região
top3_regiao <- gamma_regiao |>
  slice_max(GAMMA, n = 3, by = NM_REGIAO, with_ties = FALSE) |>
  mutate(posicao = min_rank(desc(GAMMA)), .by = NM_REGIAO) |>
  left_join(n_regiao, by = "NM_REGIAO") |>
  mutate(
    regiao_rotulo = str_glue(
      "{str_to_title(NM_REGIAO)} (n = {scales::number(n_docs, big.mark = '.', decimal.mark = ',')})"
    )
  ) |>
  arrange(desc(n_docs), posicao)

# Tabela Top 3 por Região
tab6_top3regiao <-
  top3_regiao |>
  select(regiao_rotulo, posicao, rotulo, categoria, GAMMA) |>
  gt(groupname_col = "regiao_rotulo", locale = "pt") |>
  fmt_number(columns = GAMMA, decimals = 2) |>
  cols_label(
    posicao = "#",
    rotulo = "Tópico",
    categoria = "Categoria",
    GAMMA = "γ (%)"
  ) |>
  tab_header(title = "Três tópicos mais prevalentes por região") |>
  cols_align(align = "center", columns = posicao) |>
  cols_align(align = "left", columns = c(rotulo, categoria)) |>
  cols_align(align = "right", columns = GAMMA) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_title(groups = "title"),
      cells_column_labels(),
      cells_row_groups()
    )
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
    table_body.hlines.color = "grey90",
    row_group.background.color = "grey95"
  )

# Salvar Tabela 6
gtsave(tab6_top3regiao, here("04_relatorio", "tab6_top3regiao.html"))
