# STM_NUTRIÇÃO -----------------------------------------
#  Dados Gerais -------------------------------------

# Pacotes ####
library(tidyverse)
library(here)
library(stringi)
library(textcat)

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
  "CodigoPrograma",
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
  "CD_PROGRAMA",
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
# Adição de UFPE(CAV), UNIFESP (RP), UNICAMP(LM), UFPB, UFCSPA
catalogo_raw <- catalogo_raw |>
  mutate(
    NM_ENTIDADE_ENSINO = case_when(
      CD_PROGRAMA == "25001019028P2" &
        str_detect(NM_ENTIDADE_ENSINO, "UNIVERSIDADE FEDERAL DE PERNAMBUCO") ~
        "UNIVERSIDADE FEDERAL DE PERNAMBUCO (CAV)",
      .default = NM_ENTIDADE_ENSINO
    )
  ) |>
  mutate(
    NM_ENTIDADE_ENSINO = NM_ENTIDADE_ENSINO |>
      dplyr::replace_values(
        c(
          "UNIVERSIDADE DE SÃO PAULO - CAMPUS RIBEIRÃO PRETO",
          "UNIVERSIDADE DE SÃO PAULO ( RIBEIRÃO PRETO )",
          "UNIVERSIDADE DE SÃO PAULO (RIBEIRÃO PRETO)"
        ) ~
          "UNIVERSIDADE DE SÃO PAULO (RP)",
        c(
          "UNIVERSIDADE ESTADUAL DE CAMPINAS (LIMEIRA)",
          "UNIVERSIDADE ESTADUAL DE CAMPINAS - CAMPUS LIMEIRA",
          "UNIVERSIDADE ESTADUAL DE CAMPINAS/LIMEIRA",
          "UNIVERSIDADE ESTADUAL DE CAMPINAS ( LIMEIRA )"
        ) ~
          "UNIVERSIDADE ESTADUAL DE CAMPINAS (LM)",
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

# Exclusão de Mestrado Profissional (n: 355)
# variáveis irrelevantes e inclusão da variável de identidade de docs (DOC_ID)
catalogo_raw <- catalogo_raw |>
  dplyr::filter_out(CD_PROGRAMA == "25001019075P0") |> # n: 30
  dplyr::filter_out(NM_GRAU_ACADEMICO == "MESTRADO PROFISSIONAL") # n: 325

# Excluir resumos insuficientes (n: 96)
catalogo_raw <- catalogo_raw |>
  dplyr::filter_out(
    stringi::stri_count_words(DS_RESUMO) < 50
  ) |>
  dplyr::filter_out(stri_detect_fixed(
    DS_RESUMO,
    "SILVA, DANIELA MARTINS DA"
  ))

# Excluir resumos em inglês (n: 8)
catalogo_raw <- catalogo_raw |>
  mutate(
    IDIOMA = textcat::textcat(DS_RESUMO)
  ) |>
  dplyr::filter_out(IDIOMA == "english") |> # 6
  dplyr::filter_out(
    # 2 resumos erroneamente rotulados com idioma alemão ('german')
    AN_BASE == 2017 &
      NM_ENTIDADE_ENSINO == "UNIVERSIDADE FEDERAL DA BAHIA" &
      NM_PRODUCAO ==
        "OBESIDADE SARCOPÊNICA EM IDOSAS DE UMA UNIVERSIDADE ABERTA À TERCEIRA IDADE"
  ) |>
  dplyr::filter_out(
    AN_BASE == 2024 &
      NM_ENTIDADE_ENSINO == "UNIVERSIDADE FEDERAL DO RIO DE JANEIRO" &
      NM_PRODUCAO ==
        "METABOLÔMICA COMO FERRAMENTA PARA CARACTERIZAÇÃO DAS ALTERAÇÕES METABÓLICAS CAUSADAS PELA COVID-19 SEVERA EM COORTES PROSPECTIVAS DE INDIVÍDUOS ADULTOS E GESTANTES"
  )

# Catálogo Limpo
catalogo_limpo <- catalogo_raw |>
  dplyr::select(
    -c(NM_SUBTIPO_PRODUCAO, CD_AREA_CONHECIMENTO, CD_PROGRAMA, IDIOMA)
  ) |>
  dplyr::mutate(DOC_ID = row_number())

# Salvar banco em .csv -- n: 5284
catalogo_limpo |>
  readr::write_csv("01_dados/catalogo_limpo.csv")

# Salvar banco em .RDS -- n: 5.284
saveRDS(catalogo_limpo, file = "01_dados/catalogo_limpo.RDS")
