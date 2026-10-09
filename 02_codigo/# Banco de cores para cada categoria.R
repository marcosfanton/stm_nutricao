# Banco de cores para cada categoria
# Paleta Redon -- https://github.com/BlakeRMills/MetBrewer
paleta_categorias <- c(
  "Nutrição Clínica" = "#1e395f",
  "Experimentos em Nutrição e Metabolismo" = "#5b859e",
  "Epidemiologia" = "#59385c",
  "Métodos de Pesquisa e Avaliação em Nutrição" = "#ab84a5",
  "Nutrição Esportiva e Exercício" = "#af4f2f",
  "Alimentação e Corpo" = "#df8d71",
  "Gestão e Políticas de Alimentação, Nutrição e Saúde" = "#b38711",
  "Ambientes e Insegurança Alimentar" = "#d8b847",
  "Ciência e Tecnologia de Alimentos" = "#75884b",
  "Alimentação e Nutrição nos Ciclos da Vida" = "#732f30"
  # , "Excluído" = "grey70"
)
# Salvar Paleta
saveRDS(paleta_categorias, (here::here("03_figs", "paleta_categorias.RDS")))


# Gráfico com rótulos dos tópicos
# Listagem de rótulos
lista_rotulos <- tidy_ano |>
  filter_out(categoria == "Excluído") |>
  distinct(topic, rotulo, categoria) |>
  arrange(categoria, topic) |>
  mutate(linha = row_number()) |>
  ggplot(aes(
    0,
    linha,
    label = paste0(topic, " – ", str_squish(rotulo)),
    color = categoria
  )) +
  geom_text(hjust = 0, size = 2.5, show.legend = FALSE) +
  scale_color_manual(values = unname(palette.colors(palette = "Tableau 10"))) +
  scale_y_reverse() +
  xlim(0, 1) +
  theme_void()

# Gráfico com patchwork
fig3_lista <- fig3_free + lista_rotulos + plot_layout(widths = c(3, 1))

# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "fig3_efeitoano_rotulos.png"),
  plot = fig3_lista,
  width = 20,
  height = 12,
  dpi = 300,
  bg = "white"
)


# Normalização de cada tópico
tidy_ano <- tidy_ano |>
  mutate(
    media = mean(estimate),
    indice = estimate / media,
    indice_lower = ci.lower / media,
    indice_upper = ci.upper / media,
    .by = topic
  ) |>
  left_join(rotulos, by = "topic")

# Gráfico
fig3 <- tidy_ano |>
  filter_out(categoria == "Excluído") |>
  mutate(
    topic = forcats::fct_reorder(factor(topic), as.integer(factor(categoria)))
  ) |>
  ggplot(
    aes(
      covariate.value,
      indice,
      ymin = indice_lower,
      ymax = indice_upper,
    )
  ) +
  geom_ribbon(alpha = .2) +
  geom_line(aes(color = categoria), linewidth = .8) +
  scale_color_manual(values = unname(palette.colors(palette = "Tableau 10"))) +
  facet_wrap(~topic) +
  coord_cartesian(ylim = c(-0.5, 2.5)) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  labs(x = "Ano", y = "") +
  theme(
    legend.position = "top",
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 6)
  )

# Salvar Gráfico
ggsave(
  filename = here("04_relatorio", "fig3_efeitoano.png"),
  plot = fig3,
  width = 14,
  height = 12,
  dpi = 300,
  bg = "white"
)

# TABELA COM CATEGORIAS POR ANO ####
topcats <- gamma_doc |>
  summarise(gamma_topic = mean(gamma), .by = c(AN_BASE, topic, categoria)) |>
  summarise(
    gamma_cat = (sum(gamma_topic) * 100),
    .by = c(AN_BASE, categoria)
  ) |>
  arrange(AN_BASE, desc(gamma_cat))
