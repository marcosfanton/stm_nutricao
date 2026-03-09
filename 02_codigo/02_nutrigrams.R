# Pacotes ####
library(tidyverse)
library(here)
library(tidytext)
library(stopwords)

# Abrir dados (n: 5.284)
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

# Trigrams
trigrams <- dados |>
  tidytext::unnest_tokens(trigram, DS_RESUMO, token = "ngrams", n = 3) |>
  tidyr::separate(trigram, into = c("word1", "word2", "word3"), sep = " ") |>
  dplyr::count(word1, word2, word3, sort = TRUE) |>
  dplyr::filter(n >= 50) |>
  tidyr::unite(trigram, word1, word2, word3, sep = "_")

# Salvar arquivo com trigrams
trigrams |>
  readr::write_csv("01_dados/trigrams.csv")

# Bigrams
bigrams <- dados |>
  tidytext::unnest_tokens(bigram, DS_RESUMO, token = "ngrams", n = 2) |>
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") |>
  dplyr::count(word1, word2, sort = TRUE) |>
  dplyr::filter(n >= 50) |>
  tidyr::unite(bigram, word1, word2, sep = "_")

# Salvar arquivo com bigrams
bigrams |>
  readr::write_csv("01_dados/bigrams.csv")

# Análise de expressões acadêmicas por TD-IDF #### 
tf_idf <- dados |>
  tidytext::unnest_tokens(word, DS_RESUMO) |>
  dplyr::filter_out(
    stringr::str_detect(word, "^[0-9]+$"),
    stringr::str_detect(word, "^[a-záéíóúâêôãõç]{3,}$")
  ) |>
  dplyr::count(DOC_ID, word, sort = TRUE) |>
  tidytext::bind_tf_idf(word, DOC_ID, n) |>
  dplyr::arrange(desc(tf_idf))
