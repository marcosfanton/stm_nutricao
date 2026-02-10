# Pacotes ####
library(tidyverse)
library(here)
library(tidytext)

# Abrir dados
dados <- readRDS(file = "01_dados/catalogo_limpo.RDS")

# Limpeza do texto ####
# Função de Limpeza de texto ####
limpeza_texto <- function(dados, variavel, idioma = "pt") {
  stopwords_pt <- stopwords::stopwords(idioma) |>
    stringi::stri_trans_general("Latin-ASCII")

  dados |>
    dplyr::mutate(
      {{ variavel }} := {{ variavel }} |>
        stringr::str_to_lower() |>
        stringi::stri_trans_general("Latin-ASCII") |>
        stringr::str_replace_all("[[:punct:]]", " ") |>
        stringr::str_squish() |>
        stringr::str_split("\\s+") |>
        purrr::map_chr(~ paste(.x[!.x %in% stopwords_pt], collapse = " "))
    )
}

# NGRAMS ####

bigrams <- dados |>
  tidytext::unnest_tokens(BIGRAM, DS_RESUMO, token = "ngrams", n = 2) # Formação da variável bigram com todas palavras do resumo
bigrams_sep <- bidados |>
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") # Separação dos bigrams para remoção de stopwords
filobigrams <- bidados_sep |>
  dplyr::filter(
    !word1 %in% stop_pt$word, # Remoção de stopwords em bigrams
    !word2 %in% stop_pt$word
  ) |> # Remoção de stopwords em bigrams
  tidyr::unite("bigram", c(word1, word2), sep = " ") |> # Unificação dos bigrams novamente
  dplyr::count(bigram, sort = TRUE) |> # Contagem da frequência absoluta de cada bigram
  dplyr::filter(n >= 29) # Filtragem de bigrams com 29 ou mais ocorrências (n = 1019)


bigrams_clean <- catalogo_clean |>
  unnest_tokens(bigram, DS_RESUMO_CLEAN, token = "ngrams", n = 2) |>
  count(bigram, sort = TRUE)


# Manipulação de texto (variável: DS_RESUMO)
# Função para limpeza
limpeza_texto <- function(texto) {
  texto |>
    map_chr(~ .x[!.x %in% stopwords_list] |> paste(collapse = " ")) |> 
    str_to_lower() |> # caixa baixa
    str_remove_all("[[:punct:]]") |> # remove pontuação
    stri_trans_general("Latin-ASCII") |> # remove acentos
    str_squish() # remove espaços extras
}

  texto |>
    str_to_lower() |>
    str_split("\\s+") |>
    map_chr(~ .x[!.x %in% stopwords_list] |> paste(collapse = " "))
}


# Limpeza do texto
catalogo_raw <- catalogo_raw |>
  mutate(across(c(DS_PALAVRA_CHAVE, DS_RESUMO), limpeza_texto))
