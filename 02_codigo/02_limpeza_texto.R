# Pacotes ####
library(tidyverse)
library(here)
library(tidytext)
library(stopwords)

# Abrir dados (n: 5.282)
dados <- readRDS(file = "01_dados/catalogo_limpo.RDS")

# Função de limpeza de texto ####
limpeza_texto <- function(dados, variavel, idioma = "pt") {
  stopwords_pt <- stopwords::stopwords(idioma) |>
    stringr::str_to_lower() |>
    stringi::stri_trans_general("Latin-ASCII")

  dados |>
    dplyr::mutate(
      across(
        {{ variavel }},
        ~ .x |>
          stringr::str_to_lower() |>
          stringi::stri_trans_general("Latin-ASCII") |>
          stringr::str_replace_all("[[:punct:]]", " ") |>
          stringr::str_squish() |>
          stringr::str_split("\\s+") |>
          purrr::map_chr(~ paste(.x[!.x %in% stopwords_pt], collapse = " "))
      )
    )
}
# Limpeza das colunas DS_PALAVRA_CHAVE, DS_RESUMO
dados <- dados |> limpeza_texto(c(DS_PALAVRA_CHAVE, DS_RESUMO))

# Dicionário de NGRAMS ####
ngrams <- read.csv("01_dados/nutrigrams.csv")

dicionario_grams <- ngrams |>
  transmute(
    padrao = paste0("\\b", grams, "\\b"),
    substituicao = str_replace_all(grams, " ", "_")
  ) |>
  deframe()

# dicionario_grams <- set_names(n_grams$substituicao, n_grams$padrao)

# Substituição dos NGRAMS na variável DS_RESUMO
dados <- dados |>
  mutate(
    DS_RESUMO = str_replace_all(DS_RESUMO, dicionario_grams)
  ) |>
  select(DOC_ID, AN_BASE, DS_RESUMO)

saveRDS(dados, file = "01_dados/dados_resumos.RDS")
dados <- readRDS("01_dados/dados_resumos.RDS")

# Banco para STM ####
dados_stm <- dados |>
  tidytext::unnest_tokens(output = WORD, input = DS_RESUMO)

# Remoção de números e palavras com menos de 2 caracteres
dados_stm <- dados_stm |>
  filter_out(
    str_detect(WORD, "^\\d+$") |
      str_length(WORD) <= 2
  )

# Remoção de palavras raras
# calcular frequência de palavras
word_freq <- dados_stm |>
  distinct(DOC_ID, WORD) |>
  count(WORD, name = "FREQ") |>
  arrange(FREQ)

# filtrar palavras raras (n =< 2)
dados_stm <- dados_stm |>
  inner_join(
    word_freq |>
      filter_out(FREQ <= 2),
    by = "WORD"
  ) |>
  select(-FREQ)


# filtrar stopwords científicas
# nutrilixo
lixo <- c(
  "ic95",
  "statistical",
  "package",
  "revisao_literatura",
  "capitulo",
  "original",
  "qui"
)
nutri_lixo <- tibble(WORD = lixo)

dados_stm <- dados_stm |>
  anti_join(nutri_lixo, by = c("WORD" = "WORD"))

# Salvar banco de dados
saveRDS(dados_stm, file = "01_dados/dados_prestm.RDS")
