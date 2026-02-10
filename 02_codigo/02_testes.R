# TESTES

teste <- catalogo1124 |>
  select(AN_BASE, CD_PROGRAMA, NM_ENTIDADE_ENSINO, NM_GRAU_ACADEMICO)

teste1 <- catalogo_raw |>
  distinct(AN_BASE, CD_PROGRAMA, NM_ENTIDADE_ENSINO, NM_GRAU_ACADEMICO) |>
  arrange(CD_PROGRAMA)


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
