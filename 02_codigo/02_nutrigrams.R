# Pacotes ####
library(tidyverse)
library(here)
library(tidytext)

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

# Bigrams
bigrams <- dados |>
  tidytext::unnest_tokens(bigram, DS_RESUMO, token = "ngrams", n = 2) |>
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") |>
  dplyr::count(word1, word2, sort = TRUE) |>
  dplyr::filter(n >= 50) |>
  tidyr::unite(bigram, word1, word2, sep = "_")


# Contagem de tokens totais
N_unigram <- sum(unigrams$n_uni)

# Bigrams
bigrams <- dados |>
  tidytext::unnest_tokens(bigram, DS_RESUMO, "ngrams", n = 2) |> # Formação da variável bigram com todas palavras do resumo
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") |> # Separação dos bigrams para remoção de stopwords
  dplyr::count(word1, word2, sort = TRUE)

pmi_bigrams <- bigrams |>
  filter(n >= 10) |>
  left_join(unigrams, by = c("word1" = "word")) |>
  left_join(unigrams, by = c("word2" = "word"), suffix = c("_w1", "_w2")) |>
  mutate(
    pmi = log2(
      (n / N_unigram) / ((n_uni_w1 / N_unigram) * (n_uni_w2 / N_unigram))
    )
  ) |>
  filter(pmi >= 5) |>
  arrange(desc(pmi))

# Trigrams
tridados <- dados |>
  select(DOC_ID, DS_RESUMO) |>
  tidytext::unnest_tokens(output = trigram, DS_RESUMO, token = "ngrams", n = 3) # Formação da variável bigram com todas palavras do resumo
trigrams_sep <- tridados |>
  tidyr::separate(trigram, into = c("word1", "word2", "word3"), sep = " ") # Separação dos bigrams para remoção de stopwords
trigrams <- trigrams_sep |>
  tidyr::unite("trigram", c(word1, word2, word3), sep = " ") |> # Unificação dos bigrams novamente
  dplyr::count(trigram, sort = TRUE) |> # Contagem da frequência absoluta de cada bigram
  dplyr::filter(n >= 100) # Trigram com (ou mais de) 25 ocorrências

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
