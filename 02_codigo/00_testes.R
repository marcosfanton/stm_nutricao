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

# STM - PACOTE ####
dados_stm <- readRDS(file = "01_dados/dados_pre-stm.RDS")

processed <- textProcessor(dados$DS_RESUMO, metadata = dados_stm)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

stm_nutricao1 <- stm(
  documents = out$documents,
  vocab = out$vocab,
  K = 70,
  prevalence = ~AN_BASE,
  seed = 4016325, # RANDOM.ORG - Timestamp: 2026-05-07 16:45:08 UTC
  data = out$meta,
  init.type = "Spectral"
)

topics <- labelTopics(stm_nutricao2, n = 10)

df_topics <- data.frame(
  topic = topics$topicnums,
  frex = apply(topics$frex, 1, paste, collapse = ", "),
  highest_prob = apply(topics$prob, 1, paste, collapse = ", ")
)

df_topics |>
  readr::write_csv("01_dados/TESTE70_solo.csv")

storage <- searchK(
  out$documents,
  out$vocab,
  K = c(60, 70, 80, 90, 100),
  prevalence = ~AN_BASE,
  data = meta,
  init.type = "Spectral",
)


ggplot(storage$results, aes(x = K, y = heldout)) +
  geom_line() +
  geom_point()


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


# EFEITO ANO ####

# TABELA TOP10 TÓPICOS ####
top10_topicos <- gamma_tb |>
  mutate(categoria = str_squish(categoria)) |>
  filter_out(categoria == "Excluído") |>
  slice_max(GAMMA, n = 10, with_ties = FALSE) |>
  pull(topic)

rotulos_top10 <- tidy_ano |>
  distinct(topic, rotulo) |>
  mutate(rotulo = str_wrap(str_squish(rotulo), 30)) |>
  tibble::deframe()

tidy_ano |>
  filter(topic %in% top10_topicos) |>
  mutate(
    categoria = factor(
      str_squish(categoria),
      levels = names(paleta_categorias)
    ),
    topic = factor(topic, levels = top10_topicos)
  ) |>
  ggplot(
    aes(
      covariate.value,
      estimate,
      ymin = ci.lower,
      ymax = ci.upper
    )
  ) +
  geom_ribbon(aes(fill = categoria), alpha = .2) +
  geom_line(aes(color = categoria), linewidth = 1) +
  scale_color_manual(
    values = paleta_categorias,
    aesthetics = c("color", "fill")
  ) +
  facet_wrap(
    ~topic,
    ncol = 5,
    scales = "free_y",
    labeller = as_labeller(rotulos_top10)
  ) +
  guides(
    color = guide_legend(
      ncol = 3,
      override.aes = list(linewidth = 3, alpha = 1)
    ),
    fill = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    title = "Efeito do ano sobre os dez tópicos mais prevalentes",
    subtitle = paste0(
      anos[1],
      " a ",
      anos[2],
      " | escala vertical própria de cada painel"
    ),
    caption = "Fonte: Catálogo de Teses e Dissertações da CAPES"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8),
    strip.text = element_text(
      size = 8,
      hjust = 0,
      lineheight = 0.9,
      margin = margin(2, 0, 2, 0)
    ),
    panel.spacing = unit(8, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "grey40"),
    plot.caption = element_text(color = "grey40", hjust = 1)
  )

top10_tb <- gamma_tb |>
  mutate(categoria = str_squish(categoria)) |>
  filter_out(categoria == "Excluído") |>
  slice_max(GAMMA, n = 10, with_ties = FALSE) |>
  select(rotulo, categoria, GAMMA)

# Tabela

top10_tb |>
  gt(locale = "pt") |>
  fmt_number(columns = GAMMA, decimals = 2) |>
  cols_label(
    rotulo = "Tópico",
    categoria = "Categoria",
    GAMMA = "γ (%)"
  ) |>
  tab_header(title = "Dez tópicos mais prevalentes no período") |>
  tab_source_note("Fonte: Catálogo de Teses e Dissertações da CAPES") |>
  cols_align(align = "left", columns = c(rotulo, categoria)) |>
  cols_align(align = "right", columns = GAMMA) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_title(groups = "title"), cells_column_labels())
  ) |>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes()
  ) |>
  tab_style(
    style = cell_fill(color = "grey95"),
    locations = cells_body(rows = seq(1, nrow(top10_tb), by = 2))
  ) |>
  data_color(
    columns = categoria,
    target_columns = everything(),
    fn = \(x) unname(paleta_categorias[x]),
    alpha = 0.5,
    autocolor_text = FALSE
  ) |>
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) |>
  tab_options(
    data_row.padding = px(4),
    table.font.size = px(14),
    column_labels.border.bottom.color = "grey40",
    table_body.hlines.color = "grey90"
  )


# Gráfico com apenas os TOP10 ####

# Gráfico
fig4_catsano <-
  # Salvar Gráfico
  ggsave(
    filename = here("04_relatorio", "fig4_catsano.png"),
    plot = fig4_catsano,
    width = 10,
    height = 6,
    dpi = 300,
    bg = "white"
  )

# TABELA COM 10 TÓPICOS MAIS PREVALENTES POR ANO ####
top10 <- gamma_doc |>
  summarise(
    GAMMA = (mean(gamma) * 100),
    .by = c(AN_BASE, topic, rotulo, categoria)
  ) |>
  slice_max(GAMMA, n = 10, by = AN_BASE) |>
  mutate(posicao = row_number(), .by = AN_BASE) |>
  arrange(AN_BASE, posicao)


# Tabela Completa
top10_tab1 <- top10 |> gt()

# Números apenas
top10_tab2 <- top10 |>
  select(AN_BASE, posicao, topic) |>
  pivot_wider(names_from = AN_BASE, values_from = topic) |>
  gt() |>
  cols_label(posicao = "Posição") |>
  cols_align(align = "center") |>
  tab_header(title = "Dez tópicos mais prevalentes por ano")

# Salvar Tabela 5
# Completa
gtsave(top10_tab2, here("04_relatorio", "top10n.html"))

# Gráfico Top10
fig5_top10 <- top10 |>
  ggplot(aes(factor(AN_BASE), posicao, fill = categoria)) +
  geom_tile(color = "white", alpha = .6) +
  geom_text(aes(label = str_wrap(rotulo, 16)), size = 3, lineheight = 1) +
  scale_x_discrete(position = "top") +
  scale_y_reverse(breaks = 1:10, expand = c(0, 0)) +
  scale_fill_manual(values = unname(palette.colors(palette = "Tableau 10"))) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    title = "Top10 Tópicos por Ano"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.x.top = element_text(face = "bold", size = 16),
    margin = margin(b = 2)
  )

# Salvar gráfico
ggsave(
  here("04_relatorio", "fig5_top10.png"),
  plot = fig5_top10,
  width = 16,
  height = 9,
  dpi = 300,
  bg = "white"
)

# FUNÇÃO GRÁFICOS ####
top10_ano <- top10_ano |>
  mutate(
    categoria = str_squish(categoria),
    rotulo = str_squish(rotulo)
  )
graf_top10_destaque <- function(cat_sel) {
  top10_ano |>
    mutate(destaque = if_else(categoria == cat_sel, cat_sel, "Outras")) |>
    ggplot(aes(factor(AN_BASE), posicao, fill = destaque)) +
    geom_tile(color = "white", alpha = .6) +
    geom_text(
      aes(label = str_wrap(rotulo, 18), color = destaque == cat_sel),
      size = 2.5,
      lineheight = 0.9
    ) +
    scale_x_discrete(position = "top") +
    scale_y_reverse(breaks = 1:10, expand = c(0, 0)) +
    scale_fill_manual(
      values = c(paleta_categorias[cat_sel], Outras = "grey92"),
      breaks = cat_sel
    ) +
    scale_color_manual(
      values = c("TRUE" = "black", "FALSE" = "grey60"),
      guide = "none"
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = .6))) +
    labs(x = NULL, y = NULL, fill = NULL) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 9, face = "bold"),
      panel.grid = element_blank(),
      axis.text.x.top = element_text(
        face = "bold",
        size = 16,
        margin = margin(b = 2)
      ),
      axis.text.y = element_text(face = "bold")
    )
}

# Variação da prevalência de tópicos por ano inicial e final ####
variacao_topicos <- tidy_ano |>
  mutate(across(c(categoria, rotulo), str_squish)) |>
  filter_out(categoria == "Excluído") |>
  arrange(topic, covariate.value) |>
  summarise(
    inicial = first(estimate),
    final = last(estimate),
    .by = c(topic, rotulo, categoria)
  ) |>
  mutate(variacao = (final - inicial) * 100)

# 5 maiores aumentos + 5 maiores quedas
top_variacao <- bind_rows(
  slice_max(variacao_topicos, variacao, n = 5, with_ties = FALSE),
  slice_min(variacao_topicos, variacao, n = 5, with_ties = FALSE)
) |>
  mutate(
    rotulo_painel = str_glue(
      "{rotulo} ({scales::number(variacao, accuracy = 0.01, decimal.mark = ',', style_positive = 'plus')} p.p.)"
    )
  ) |>
  select(topic, variacao, rotulo_painel)

# Conferência
top_variacao

# Gráfico: tópicos com maior variação ####
fig_variacao <- tidy_ano |>
  inner_join(top_variacao, by = "topic") |>
  mutate(
    categoria = str_squish(categoria) |>
      factor(levels = names(paleta_categorias)),
    rotulo_painel = fct_reorder(rotulo_painel, variacao, .desc = TRUE)
  ) |>
  ggplot(
    aes(
      covariate.value,
      estimate,
      ymin = ci.lower,
      ymax = ci.upper
    )
  ) +
  geom_ribbon(aes(fill = categoria), alpha = .2) +
  geom_line(aes(color = categoria), linewidth = .8) +
  scale_color_manual(
    values = paleta_categorias,
    aesthetics = c("color", "fill")
  ) +
  scale_x_continuous(
    breaks = seq(
      min(tidy_ano$covariate.value),
      max(tidy_ano$covariate.value),
      by = 3
    )
  ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1, decimal.mark = ",")
  ) +
  facet_wrap(
    ~rotulo_painel,
    ncol = 5,
    scales = "free_y",
    labeller = label_wrap_gen(width = 25)
  ) +
  guides(
    color = guide_legend(
      nrow = 1,
      override.aes = list(linewidth = 3, alpha = 1)
    ),
    fill = "none"
  ) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 8, face = "bold"),
    strip.text = element_text(
      size = 9,
      face = "bold",
      hjust = 0,
      lineheight = 0.9,
      margin = margin(1, 0, 2, 0)
    ),
    panel.spacing = unit(8, "pt"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
    axis.text = element_text(size = 6, color = "grey40")
  )

fig_variacao
