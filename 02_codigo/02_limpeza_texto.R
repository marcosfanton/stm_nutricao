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

# NGRAMS ####
# Tetragrams
tetragrams <- dados |>
  tidytext::unnest_tokens(tetragram, DS_RESUMO, token = "ngrams", n = 4) |>
  tidyr::separate(
    tetragram,
    into = c("word1", "word2", "word3", "word4"),
    sep = " "
  ) |>
  dplyr::count(word1, word2, word3, word4, sort = TRUE) |>
  dplyr::filter(n >= 50) |>
  tidyr::unite(tetragram, word1, word2, word3, word4, sep = " ")

# Salvar arquivo com trigrams
tetragrams |>
  readr::write_csv("01_dados/tetragrams.csv")

# Trigrams
trigrams <- dados |>
  tidytext::unnest_tokens(trigram, DS_RESUMO, token = "ngrams", n = 3) |>
  tidyr::separate(trigram, into = c("word1", "word2", "word3"), sep = " ") |>
  dplyr::count(word1, word2, word3, sort = TRUE) |>
  dplyr::filter(n >= 50) |>
  tidyr::unite(trigram, word1, word2, word3, sep = " ")
# Salvar arquivo com trigrams
trigrams |>
  readr::write_csv("01_dados/trigrams.csv")

# Bigrams
bigrams <- dados |>
  tidytext::unnest_tokens(bigram, DS_RESUMO, token = "ngrams", n = 2) |>
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") |>
  dplyr::count(word1, word2, sort = TRUE) |>
  dplyr::filter(n >= 50) |>
  tidyr::unite(bigram, word1, word2, sep = " ")
# Salvar arquivo com bigrams
bigrams |>
  readr::write_csv("01_dados/bigrams.csv")

# Análise de expressões acadêmicas por TF-IDF
tfidf <- dados |>
  tidytext::unnest_tokens(word, DS_RESUMO) |>
  dplyr::filter(
    stringr::str_detect(word, "^[a-záéíóúâêôãõç]{3,}$")
  ) |>
  dplyr::count(DOC_ID, word, sort = TRUE) |>
  tidytext::bind_tf_idf(word, DOC_ID, n) |>
  dplyr::arrange(desc(tf_idf))
# Salvar arquivo com análise TF-IDF
tfidf |>
  readr::write_csv("01_dados/tfidf.csv")

# Expressões mais características do corpus
tfidf_corpus <- tfidf |>
  dplyr::summarise(
    tf_idf = mean(tf_idf),
    freq = sum(n),
    docs = n(),
    .by = word
  ) |>
  dplyr::arrange(desc(freq))
# Salvar arquivo com análise TF-IDF
tfidf_corpus |>
  readr::write_csv("01_dados/tfidf_corpus.csv")

# Dicionário de NGRAMS ####
ngrams <- read.csv("01_dados/nutrigrams.csv")

n_grams <- ngrams |>
  transmute(
    padrao = paste0("\\b", grams, "\\b"),
    substituicao = str_replace_all(grams, " ", "_")
  )

dicionario_grams <- set_names(n_grams$substituicao, n_grams$padrao)

# Substituição dos NGRAMS na variável DS_RESUMO
dados <- dados |>
  mutate(
    DS_RESUMO = str_replace_all(DS_RESUMO, dicionario_grams)
  )

# Banco para STM ####
dados <- dados |>
  tidytext::unnest_tokens(output = WORD, input = DS_RESUMO, drop = TRUE) |>
  select(DOC_ID, NM_PRODUCAO, WORD, AN_BASE)

# Remoção de números
dados <- dados |>
  filter_out(
    str_detect(WORD, "^\\d+$")
  )

# Salvar banco de dados
saveRDS(dados, file = "01_dados/dados_prestm.RDS")
