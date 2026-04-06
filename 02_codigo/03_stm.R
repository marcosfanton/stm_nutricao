# Banco para STM ####
# Pacotes
library(tidyverse)
library(here)
library(stringi)
library(textcat)

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


teste <- dados |> limpeza_texto(DS_RESUMO)


stop_pt <- tidytext::get_stopwords("pt")
bidados <- dados |>
  tidytext::unnest_tokens(bigram, ds_resumo, token = "ngrams", n = 2) # Formação da variável bigram com todas palavras do resumo
bidados_sep <- bidados |>
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") # Separação dos bigrams para remoção de stopwords
filobigrams <- bidados_sep |>
  dplyr::filter(
    !word1 %in% stop_pt$word, # Remoção de stopwords em bigrams
    !word2 %in% stop_pt$word
  ) |> # Remoção de stopwords em bigrams
  tidyr::unite("bigram", c(word1, word2), sep = " ") |> # Unificação dos bigrams novamente
  dplyr::count(bigram, sort = TRUE) |> # Contagem da frequência absoluta de cada bigram
  dplyr::filter(n >= 29) # Filtragem de bigrams com 29 ou mais ocorrências (n = 1019)


# Manipulação de texto (variável: DS_RESUMO)
# Função para limpeza
limpeza_texto <- function(texto) {
  texto |>
    str_to_lower() |> # caixa baixa
    str_remove_all("[[:punct:]]") |> # remove pontuação
    stri_trans_general("Latin-ASCII") |> # remove acentos
    str_squish() # remove espaços extras
}

# Limpeza do texto
catalogo_raw <- catalogo_raw |>
  mutate(across(c(DS_PALAVRA_CHAVE, DS_RESUMO), limpeza_texto))
