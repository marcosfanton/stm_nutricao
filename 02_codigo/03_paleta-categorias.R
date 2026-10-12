# Banco de cores para cada categoria
# Paleta Redon -- https://github.com/BlakeRMills/MetBrewer
paleta_categorias <- c(
  "Epidemiologia" = "#59385c",
  "Experimentos em Nutrição e Metabolismo" = "#5b859e",
  "Ciência e Tecnologia de Alimentos" = "#75884b",
  "Nutrição Clínica" = "#1e395f",
  "Alimentação e Nutrição nos Ciclos da Vida" = "#732f30",
  "Métodos de Pesquisa e Avaliação em Nutrição" = "#ab84a5",
  "Gestão e Políticas de Alimentação, Nutrição e Saúde" = "#b38711",
  "Alimentação e Corpo" = "#df8d71",
  "Ambientes e Insegurança Alimentar" = "#d8b847",
  "Nutrição Esportiva e Exercício" = "#af4f2f"
  # "Excluído" = "grey70"
)
# Salvar Paleta
saveRDS(paleta_categorias, (here::here("03_figs", "paleta_categorias.RDS")))
