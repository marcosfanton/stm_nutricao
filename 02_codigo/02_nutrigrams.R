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

# NGRAMS ####
# Stopwords em PT
stop_pt <- tidytext::get_stopwords("pt")

# Bigrams
bidados <- dados |>
  select(DOC_ID, DS_RESUMO) |>
  tidytext::unnest_tokens(output = bigram, DS_RESUMO, token = "ngrams", n = 2) # Formação da variável bigram com todas palavras do resumo
bigrams_sep <- bidados |>
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") # Separação dos bigrams para remoção de stopwords
bigrams <- bigrams_sep |>
  tidyr::unite("bigram", c(word1, word2), sep = " ") |> # Unificação dos bigrams novamente
  dplyr::count(bigram, sort = TRUE) |> # Contagem da frequência absoluta de cada bigram
  dplyr::filter(n >= 25) # Bigram com (ou mais de) 25 ocorrências

# Trigrams
tridados <- dados |>
  select(DOC_ID, DS_RESUMO) |>
  tidytext::unnest_tokens(output = trigram, DS_RESUMO, token = "ngrams", n = 3) # Formação da variável bigram com todas palavras do resumo
trigrams_sep <- tridados |>
  tidyr::separate(trigram, into = c("word1", "word2", "word3"), sep = " ") # Separação dos bigrams para remoção de stopwords
trigrams <- trigrams_sep |>
  tidyr::unite("trigram", c(word1, word2, word3), sep = " ") |> # Unificação dos bigrams novamente
  dplyr::count(trigram, sort = TRUE) |> # Contagem da frequência absoluta de cada bigram
  dplyr::filter(n >= 25) # Trigram com (ou mais de) 25 ocorrências

# Tetragrams
n_dados <- dados |>
  select(DOC_ID, DS_RESUMO) |>
  tidytext::unnest_tokens(
    output = n_gram,
    DS_RESUMO,
    token = "ngrams",
    n = 4
  ) # Formação da variável bigram com todas palavras do resumo
n_grams_sep <- n_dados |>
  tidyr::separate(
    n_gram,
    into = c("word1", "word2", "word3", "word4"),
    sep = " "
  ) # Separação dos bigrams para remoção de stopwords
n_grams <- n_grams_sep |>
  tidyr::unite("n_gram", c(word1, word2, word3, word4), sep = " ") |> # Unificação dos bigrams novamente
  dplyr::count(n_gram, sort = TRUE) |> # Contagem da frequência absoluta de cada bigram
  dplyr::filter(n >= 10) # Tetragram com (ou mais de) 10 ocorrências


# Tetragrams
n_dados <- dados |>
  select(DOC_ID, DS_RESUMO) |>
  tidytext::unnest_tokens(
    output = n_gram,
    DS_RESUMO,
    token = "ngrams",
    n = 5
  ) # Formação da variável bigram com todas palavras do resumo
n_grams_sep <- n_dados |>
  tidyr::separate(
    n_gram,
    into = c("word1", "word2", "word3", "word4", "word5"),
    sep = " "
  ) # Separação dos bigrams para remoção de stopwords
n_grams <- n_grams_sep |>
  tidyr::unite("n_gram", c(word1, word2, word3, word4, word5), sep = " ") |> # Unificação dos bigrams novamente
  dplyr::count(n_gram, sort = TRUE) |> # Contagem da frequência absoluta de cada bigram
  dplyr::filter(n >= 20)
