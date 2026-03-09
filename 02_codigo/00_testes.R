# TESTES

# CÓDIGO DE PROGRAMAS #####
teste <- catalogo1124 |>
  select(AN_BASE, CD_PROGRAMA, NM_ENTIDADE_ENSINO, NM_GRAU_ACADEMICO)

teste1 <- catalogo_raw |>
  distinct(AN_BASE, CD_PROGRAMA, NM_ENTIDADE_ENSINO, NM_GRAU_ACADEMICO) |>
  arrange(CD_PROGRAMA)

# RESUMOS #####
teste <- catalogo_raw |>
  dplyr::filter_out(CD_PROGRAMA == "25001019075P0") |> # n: 30
  dplyr::filter_out(NM_GRAU_ACADEMICO == "MESTRADO PROFISSIONAL") |> # n: 325
  dplyr::mutate(DOC_ID = row_number()) |>
  dplyr::filter_out(
    stringi::stri_count_words(DS_RESUMO) > 100 # n: ??
  ) |>
  select(DS_RESUMO, DOC_ID)

teste2 <- catalogo_raw |>
  dplyr::filter_out(CD_PROGRAMA == "25001019075P0") |> # n: 30
  dplyr::filter_out(NM_GRAU_ACADEMICO == "MESTRADO PROFISSIONAL") |> # n: 325
  dplyr::mutate(DOC_ID = row_number()) |>
  dplyr::filter_out(
    stringi::stri_count_words(DS_RESUMO) > 50 # n: ??
  ) |>
  select(DS_RESUMO, DOC_ID)


somente_teste <- teste |>
  anti_join(teste2, by = "DOC_ID")

# PALAVRAS-CHAVES #####

pc <- dados |>
  unnest_tokens(palavra, DS_PALAVRA_CHAVE, token = "words") |>
  count(palavra, sort = TRUE)

# Ver top 20
pc |>
  slice_head(n = 20)

# Visualizar
pc |>
  slice_head(n = 20) |>
  mutate(palavra = fct_reorder(palavra, n)) |>
  ggplot(aes(x = n, y = palavra)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Top 20 Palavras-chave mais frequentes",
    x = "Frequência",
    y = NULL
  ) +
  theme_minimal()


# Idioma dos resumos
teste <- dados |>
  mutate(
    lang = textcat::textcat(DS_RESUMO)
  )


teste <- catalogo_raw |>
  mutate(
    IDIOMA = cld3::detect_language(DS_RESUMO)
  )

# QUANTEDA ####
corp <- corpus(dados, text_field = "DS_RESUMO", docid_field = "DOC_ID")
toks <- tokens(corp, remove_punct = TRUE, remove_numbers = TRUE)
cols <- textstat_collocations(toks, method = "pmi", min_count = 10)


limpeza_texto <- function(
  dados,
  variavel,
  idioma = "pt",
  letras_relevantes = c("d", "c", "b", "a", "e", "k"),
  numeros_relevantes = c("19")
) {
  stopwords_pt <- stopwords::stopwords(idioma) |> # dicionário de stopwords
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
          purrr::map_chr(
            ~ {
              tokens <- .x

              # remove stopwords
              tokens <- tokens[!tokens %in% stopwords_pt]

              # remove números exceto relevantes
              tokens <- tokens[
                !grepl("^\\d+$", tokens) | tokens %in% numeros_relevantes
              ]

              # remove tokens de 1 letra exceto relevantes
              tokens <- tokens[
                nchar(tokens) > 1 | tokens %in% letras_relevantes
              ]

              paste(tokens, collapse = " ")
            }
          )
      )
    )
}



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
