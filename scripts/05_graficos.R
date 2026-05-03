# 05_graficos.R
# Constrói os plots a partir de data/processed/resumos.rds e salva PNGs
# em outputs/.

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(fs)

# Fonte plot
sysfonts::font_add_google("Roboto", "Roboto", bold.wt = 900, regular.wt = 300)
sysfonts::font_add_google("Roboto", "Roboto Bold", regular.wt = 900)
sysfonts::font_add_google("Roboto", "Roboto Bold2", regular.wt = 400)
showtext::showtext_auto()


PROC_DIR <- here::here("data/processed")
OUT_DIR  <- here::here("outputs")
dir_create(OUT_DIR)

resumos       <- readRDS(file.path(PROC_DIR, "resumos.rds"))
ordem_mandato <- resumos$ordem_mandato

# --- Tema comum dos plots --------------------------------------------------

tema <- theme_minimal(base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", family = "Roboto", size = 22, hjust = 0.5),
    plot.subtitle = element_text(family = "Roboto", size = 18, hjust = 0.5),
    legend.position = "top",
    strip.text = element_text(face = "bold", family = "Roboto", size = 15),
    axis.text.y = element_text(family = "Roboto", size = 13, color = "black"),
    axis.text.x = element_text(family = "Roboto", size = 14, color = "black"),
    axis.title = element_text(family = "Roboto", size = 15),
    plot.caption = element_text(family = "Roboto", size = 14),
    legend.text = element_text(family = "Roboto", size = 15),
    panel.grid.minor = element_blank()
  )

# Paletas
cor_decisao <- c(
  "Sanção"         = "forestgreen",
  "Veto parcial"   = "#E1A84A",
  "Veto integral"  = "firebrick3"
)

cor_congresso <- c(
  "Mantido"                 = "forestgreen",
  "Parcialmente derrubado"  = "#E1A84A",
  "Derrubado integralmente" = "firebrick3",
  "Em tramitação"           = "grey70"
)

cor_dispositivo <- c(
  "Mantido"     = "forestgreen",
  "Derrubado"   = "firebrick3",
  "Sem decisão" = "grey70"
)

# --- Gráfico 1: decisão presidencial ---------------------------------------

p1 <- resumos$decisoes_long |>
  # Tirando FHC pois dados não são confiáveis pré 2001
  filter(presidente != "FHC") %>% 
  ggplot(aes(x = mandato, y = n, fill = decisao)) +
  geom_col() +
  scale_fill_manual(values = cor_decisao, name = NULL) +
  labs(
    title = "Decisão presidencial sobre projetos de lei",
    subtitle = "Sanções e vetos por mandato (2003 até hoje)",
    x = NULL, y = "Número de projetos\n",
    caption = "\nFonte: Dados Abertos do Senado Federal e Congresso Nacional\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema

ggsave(file.path(OUT_DIR, "01_decisao_presidencial.png"),
       p1, width = 9, height = 5, dpi = 150)

# Versão proporcional (% de cada decisão)
p1_pct <- resumos$decisoes_long |>
  # Tirando FHC pois dados não são confiáveis pré 2001
  filter(presidente != "FHC") %>% 
  group_by(mandato) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  ggplot(aes(x = mandato, y = pct, fill = decisao)) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = cor_decisao, name = NULL) +
  labs(
    title = "Composição da decisão presidencial",
    subtitle = "Proporção de sanções, vetos parciais e vetos integrais por mandato",
    x = NULL, y = "Composição (%)\n",
    caption = "\nFonte: Dados Abertos do Senado Federal e Congresso Nacional\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema

ggsave(file.path(OUT_DIR, "02_decisao_presidencial_pct.png"),
       p1_pct, width = 9, height = 5, dpi = 150)

# --- Gráfico 2: decisão do Congresso sobre o veto --------------------------

p2 <- resumos$congresso_por_veto |>
  # Tirando FHC pois dados não são confiáveis pré 2001
  filter(presidente != "FHC") %>% 
  ggplot(aes(x = mandato, y = n, fill = categoria_final)) +
  geom_col() +
  scale_fill_manual(values = cor_congresso, name = NULL) +
  scale_y_continuous(breaks = seq(0, 1000, 50)) +
  labs(
    title = "Resposta do Congresso aos vetos presidenciais",
    subtitle = "Decisão final do Congresso em relação aos vetos",
    x = NULL, y = "Número de vetos\n",
    caption = "\nFonte: Dados Abertos do Senado Federal e Congresso Nacional\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema 

ggsave(file.path(OUT_DIR, "03_congresso_decisao.png"),
       p2, width = 9, height = 5, dpi = 150)

# Variação: separando vetos integrais e parciais em facetas
p2_facet <- resumos$congresso_por_veto_tipo |>
  # Tirando FHC pois dados não são confiáveis pré 2001
  filter(presidente != "FHC") %>% 
  mutate(
    tipo_veto = factor(tipo_veto, levels = c("integral", "parcial"),
                       labels = c("Vetos integrais\n", "Vetos parciais\n")),
    categoria_final = factor(
      categoria_final,
      levels = c("mantido", "parcialmente_derrubado", "derrubado",
                 "em_tramitacao"),
      labels = c("Mantido", "Parcialmente derrubado",
                 "Derrubado integralmente", "Em tramitação")
    )
  ) |>
  ggplot(aes(x = mandato, y = n, fill = categoria_final)) +
  geom_col() +
  facet_wrap(~ tipo_veto, scales = "fixed") +
  scale_fill_manual(values = cor_congresso, name = NULL) +
  labs(
    title = "Decisão do Congresso por tipo de veto",
    x = NULL, y = "Número de vetos",
    caption = "\nFonte: Dados Abertos do Senado Federal e Congresso Nacional\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUT_DIR, "04_congresso_por_tipo.png"),
       p2_facet, width = 11, height = 5, dpi = 150)

# --- Gráfico 3: dispositivos --------------------------------------------------

p3 <- resumos$dispositivos_por_mandato |>
  # Tirando FHC pois dados não são confiáveis pré 2001
  filter(presidente != "FHC") %>% 
  ggplot(aes(x = mandato, y = n, fill = situacao_norm)) +
  geom_col() +
  scale_fill_manual(values = cor_dispositivo, name = NULL) +
  labs(
    title = "Dispositivos vetados pelo presidente",
    subtitle = "Granularidade dispositivo-a-dispositivo",
    x = NULL, y = "Número de dispositivos",
    caption = "\nFonte: Dados Abertos do Senado Federal e Congresso Nacional\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema

ggsave(file.path(OUT_DIR, "05_dispositivos.png"),
       p3, width = 9, height = 5, dpi = 150)

# --- Gráfico 4: taxa de derrota ----------------------------------------------

p4 <- resumos$taxa_derrota |>
  # Tirando FHC pois dados não são confiáveis pré 2001
  filter(presidente != "FHC") %>% 
  ggplot(aes(x = mandato, y = pct_qualquer_derrota)) +
  geom_col(fill = "#B43F3F") +
  geom_text(aes(label = percent(pct_qualquer_derrota, accuracy = 0.1)),
            vjust = -0.4, size = 5.5, family = "Roboto Bold2", color = "black") +
  scale_y_continuous(labels = percent_format(),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Taxa de derrota dos vetos no Congresso",
    subtitle = "% de vetos presidenciais derrubados ou parcialmente derrubados pelo Congresso",
    x = NULL, y = NULL,
    caption = "\nFonte: Dados Abertos do Senado Federal e Congresso Nacional\nFeito por: Artur Vidaurre de Almeida"
  ) +
  tema

ggsave(file.path(OUT_DIR, "06_taxa_derrota.png"),
       p4, width = 9, height = 5, dpi = 150)

cat("==> Gráficos gerados em ", OUT_DIR, "\n")
print(dir_ls(OUT_DIR, glob = "*.png"))
