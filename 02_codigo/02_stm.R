# Banco para STM ####

# Abrir dados
dados <- readRDS(file = "01_dados/catalogo_limpo.RDS")

# Filtragem de observações com DS_RESUMO < 10 (n: 5346)
dados <- dados |>
  dplyr::filter_out(
    stringi::stri_count_words(catalogo_raw$DS_RESUMO) < 10
  ) # n: 67

# Inserção em ID para cada observação/documento
dados <- dados |>
  dplyr::mutate(DOC_ID = row_number())

# NGRAMS e STOPWORDS
