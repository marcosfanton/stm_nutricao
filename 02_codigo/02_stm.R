# Banco para STM ####

# Abrir dados
dados <- readRDS(file = "01_dados/catalogo_limpo.RDS")

# Inserção em ID para cada observação/documento
dados <- dados |>

  # NGRAMS e STOPWORDS

  teste <- dados |>
  filter(stringi::stri_count_words(catalogo_raw$DS_RESUMO) < 20)
