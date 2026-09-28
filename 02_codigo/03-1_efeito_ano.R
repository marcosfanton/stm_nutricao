library(tidyverse)
library(here)
library(tidystm)
library(gt)

# Bancos
stm_nutricao <- readRDS(here::here("01_dados", "stm65.RDS"))
rotulos <- read.csv(here::here("01_dados", "tabela_rotulos.csv"))
metadados <- readRDS("01_dados/dados_resumos.RDS")

# Efeito ano
efeito_ano <- stm::estimateEffect(
  1:65 ~ s(AN_BASE),
  stmobj = stm_nutricao,
  metadata = metadados
)

tidy_ano <- tidystm::extract.estimateEffect(
  x = efeito_ano,
  covariate = "AN_BASE",
  model = stm_nutricao,
  method = "continuous",
  labeltype = "frex",
  n = 2
)

ggplot(
  tidy_ano,
  aes(covariate.value, estimate, ymin = ci.lower, ymax = ci.upper)
) +
  facet_wrap(~label, scales = "free_y") +
  geom_ribbon(alpha = .5) +
  geom_line() +
  labs(x = "Ano", y = "Proporção esperada do tópico")


# TABELA COM 10 TÓPICOS MAIS PREVALENTES POR ANO ####
theta_ano <- as_tibble(
  stm_nutricao$theta,
  .name_repair = ~ paste0("Topic", seq_along(.))
) |>
  mutate(AN_BASE = metadados$AN_BASE) |>
  pivot_longer(-AN_BASE, names_to = "topic", values_to = "gamma") |>
  mutate(topic = as.integer(parse_number(topic)))

top10_ano <- theta_ano |>
  summarise(gamma_medio = mean(gamma), .by = c(AN_BASE, topic)) |>
  slice_max(gamma_medio, n = 10, by = AN_BASE, with_ties = FALSE) |>
  arrange(AN_BASE, desc(gamma_medio)) |>
  left_join(rotulos, by = "topic") |>
  select(AN_BASE, topic, rotulo, categoria, gamma_medio)

tabela_top10_ano |> gt()
