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
