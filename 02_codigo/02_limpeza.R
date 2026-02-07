# STM_NUTRIÇÃO -----------------------------------------
#  Dados Gerais -------------------------------------

# Pacotes ####
library(tidyverse)
library(here)
library(stringi)

# Importação e unificação dos bancos de 2011-2024 --------------------------------------------------------
# Todos os bancos foram baixados em .csv na página Dados Abertos CAPES - Grupo: Catálogo de Teses e Dissertações
# https://dadosabertos.capes.gov.br/dataset/
# Arquivos armazenados em 01_dados/01_dados_originais
#
# Bancos de 2011 e 2012 | n: 116.607
banco1112 <- list.files(
  path = "01_dados/01_dados_originais",
  pattern = "capes",
  full.names = TRUE
) |>
  purrr::map_dfr(
    readr::read_csv2,
    locale = readr::locale(encoding = "ISO-8859-1"),
    na = c("NI", "NA"),
    show_col_types = FALSE
  )

# Bancos de 2013 a 2024 | n: 976.217
banco1324 <- list.files(
  path = "01_dados/01_dados_originais",
  pattern = "dados",
  full.names = TRUE
) |>
  purrr::map_dfr(
    readr::read_csv2,
    locale = readr::locale(encoding = "ISO-8859-1"),
    na = c("NI", "NA"),
    show_col_types = FALSE
  )

# Dicionário de variáveis: nomes antigos (2011 e 2012) => nomes novos (2013 a 2024)
# Variáveis 2011-2012
vars1112 <- c(
  "AnoBase",
  "NomeIes",
  "TituloTese",
  "Nivel",
  "PalavrasChave",
  "Regiao",
  "Uf",
  "AreaConhecimentoCodigo",
  "NumeroPaginas",
  "ResumoTese"
)

vars1324 <- c(
  "AN_BASE",
  "NM_ENTIDADE_ENSINO",
  "NM_PRODUCAO",
  "NM_SUBTIPO_PRODUCAO",
  "NM_GRAU_ACADEMICO",
  "DS_PALAVRA_CHAVE",
  "NM_REGIAO",
  "SG_UF_IES",
  "CD_AREA_CONHECIMENTO",
  "NR_PAGINAS",
  "DS_RESUMO"
)

# Junção de bancos
catalogo1124 <- dplyr::bind_rows(
  banco1112 |>
    dplyr::select(all_of(vars1112)) |>
    dplyr::rename_with(
      .cols = all_of(vars1112),
      ~ vars1324[vars1324 != "NM_GRAU_ACADEMICO"] # Renomeia todas variáveis exceto NM_GRAU_ACADEMICO, que não existe no banco
    ),
  banco1324 |>
    select(all_of(vars1324))
) |>
  dplyr::filter(CD_AREA_CONHECIMENTO == "40500004")

# Salvar arquivo em .csv e .RDS -- n: 5.738
catalogo1124 |>
  readr::write_csv("01_dados/catalogo_raw.csv")

# Salvar banco em .RDS
saveRDS(catalogo1124, file = "01_dados/catalogo_raw.RDS")

# Limpeza do banco --------------------------------------------------------------------------------------
catalogo_raw <- readRDS(file = "01_dados/catalogo_raw.RDS")

# Recodificação do nome das IES
catalogo_raw <- catalogo_raw |>
  mutate(
    NM_ENTIDADE_ENSINO = NM_ENTIDADE_ENSINO |>
      dplyr::replace_values(
        c(
          "UNIVERSIDADE DE SÃO PAULO - CAMPUS RIBEIRÃO PRETO",
          "UNIVERSIDADE DE SÃO PAULO ( RIBEIRÃO PRETO )"
        ) ~
          "UNIVERSIDADE DE SÃO PAULO (RIBEIRÃO PRETO)",
        c(
          "UNIVERSIDADE ESTADUAL DE CAMPINAS (LIMEIRA)",
          "UNIVERSIDADE ESTADUAL DE CAMPINAS - CAMPUS LIMEIRA",
          "UNIVERSIDADE ESTADUAL DE CAMPINAS/LIMEIRA",
          "UNIVERSIDADE ESTADUAL DE CAMPINAS ( LIMEIRA )"
        ) ~
          "UNIVERSIDADE ESTADUAL DE CAMPINAS",
        c(
          "UNIVERSIDADE FEDERAL DA PARAÍBA (JOÃO PESSOA)",
          "UNIVERSIDADE FEDERAL DA PARAÍBA - CAMPUS JOÃO PESSOA",
          "UNIVERSIDADE FEDERAL DA PARAÍBA/JOÃO PESSOA",
          "UNIVERSIDADE FEDERAL DA PARAÍBA ( JOÃO PESSOA )"
        ) ~
          "UNIVERSIDADE FEDERAL DA PARAÍBA",
        "FUNDAÇÃO UNIV. FEDERAL DE CIÊNCIAS DA SAÚDE DE PORTO ALEGRE" ~
          "UNIVERSIDADE FEDERAL DE CIÊNCIAS DA SAÚDE DE PORTO ALEGRE"
      )
  )
# Imputação do Grau Acadêmico dos anos 2011 e 2012 com base no subtipo de produção
catalogo_raw <- catalogo_raw |>
  dplyr::mutate(
    NM_GRAU_ACADEMICO = NM_GRAU_ACADEMICO |>
      dplyr::replace_when(
        AN_BASE <= 2012 & NM_SUBTIPO_PRODUCAO == "Mestrado" ~ "MESTRADO",
        AN_BASE <= 2012 & NM_SUBTIPO_PRODUCAO == "Doutorado" ~ "DOUTORADO"
      )
  )

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

# Exclusão de Mestrado Profissional, Resumos Insuficientes (<15 palavras),
# variáveis irrelevantes e inclusão da variável de identidade de docs (DOC_ID)
catalogo_raw <- catalogo_raw |>
  dplyr::filter_out(NM_GRAU_ACADEMICO == "mestrado profissional") |> #n:
  dplyr::filter_out(
    stringi::stri_count_words(catalogo_raw$DS_RESUMO) < 10
  ) |> #n:
  dplyr::select(-c(NM_SUBTIPO_PRODUCAO, CD_AREA_CONHECIMENTO)) |>
  dplyr::mutate(DOC_ID = row_number())

# Salvar banco em .RDS -- n: 5.413
saveRDS(catalogo_raw, file = "01_dados/catalogo_limpo.RDS")
