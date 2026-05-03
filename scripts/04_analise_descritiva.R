# 04_analise_descritiva.R
# Calcula as agregações que alimentam os gráficos do script 05.
# Lê data/processed/{vetos,dispositivos,leis,mandatos}.rds e grava resumos
# em data/processed/resumos.rds, para facilitar análise

library(dplyr)
library(tidyr)
library(forcats)

PROC_DIR <- "data/processed"

vetos        <- readRDS(file.path(PROC_DIR, "vetos.rds"))
dispositivos <- readRDS(file.path(PROC_DIR, "dispositivos.rds"))
leis         <- readRDS(file.path(PROC_DIR, "leis.rds"))
mandatos     <- readRDS(file.path(PROC_DIR, "mandatos.rds"))

# Ordem cronológica usada nos gráficos
ordem_mandato <- levels(mandatos$mandato)

vetos        <- vetos        |> filter(!is.na(mandato))
dispositivos <- dispositivos |> filter(!is.na(mandato))
leis         <- leis         |> filter(!is.na(mandato))

# --- 1) Decisões do presidente: sanção vs veto integral vs veto parcial ----
#
# - nº vetos integrais e parciais vêm de `vetos` (atribuição pela data do veto)
# - nº sanções = leis no mandato - vetos parciais no mandato
#   (vetos parciais já viram lei na parte sancionada e estão em `leis`).
#   Vetos integrais derrubados também viram lei

vetos_por_mandato <- vetos |>
  count(mandato, tipo_veto, name = "n") |>
  pivot_wider(names_from = tipo_veto, values_from = n, values_fill = 0) |>
  rename(veto_integral = integral, veto_parcial = parcial)

leis_por_mandato <- leis |>
  count(mandato, name = "n_leis")

decisoes_presidente <- mandatos |>
  select(mandato, presidente) |>
  left_join(vetos_por_mandato, by = "mandato") |>
  left_join(leis_por_mandato,  by = "mandato") |>
  mutate(across(c(veto_integral, veto_parcial, n_leis), ~ tidyr::replace_na(.x, 0L))) |>
  mutate(
    sancao_pura      = pmax(n_leis - veto_parcial, 0L),
    total_decisoes   = sancao_pura + veto_integral + veto_parcial,
    pct_vetada       = (veto_integral + veto_parcial) / total_decisoes,
    pct_veto_integral= veto_integral / total_decisoes,
    pct_veto_parcial = veto_parcial  / total_decisoes
  )

# Pivotando para facilitar gráfico empilhado
decisoes_long <- decisoes_presidente |>
  select(mandato, presidente, sancao_pura, veto_integral, veto_parcial) |>
  pivot_longer(cols = c(sancao_pura, veto_integral, veto_parcial),
               names_to = "decisao", values_to = "n") |>
  mutate(
    decisao = factor(decisao,
                     levels = c("veto_integral", "veto_parcial", "sancao_pura"),
                     labels = c("Veto integral", "Veto parcial", "Sanção"))
  )

# --- 2) Decisão do Congresso por veto --------------------------------------

congresso_por_veto <- vetos |>
  count(mandato, presidente, categoria_final, name = "n") |>
  mutate(
    categoria_final = factor(
      categoria_final,
      levels = c("mantido", "parcialmente_derrubado", "derrubado",
                 "em_tramitacao"),
      labels = c("Mantido", "Parcialmente derrubado",
                 "Derrubado integralmente", "Em tramitação")
    )
  )

# Mesma decomposição, separando integral vs parcial
congresso_por_veto_tipo <- vetos |>
  count(mandato, presidente, tipo_veto, categoria_final, name = "n")

# --- 3) Dispositivos vetados -----------------------------------------------

dispositivos_por_mandato <- dispositivos |>
  mutate(
    situacao_norm = case_when(
      situacao == "Rejeitado"                               ~ "Derrubado",
      situacao %in% c("Mantido", "Prejudicado",
                      "Não Apreciado")                      ~ "Mantido",
      TRUE                                                  ~ "Sem decisão"
    ) |> factor(levels = c("Mantido", "Derrubado", "Sem decisão"))
  ) |>
  count(mandato, presidente, situacao_norm, name = "n")

# --- 4) Taxa de derrota ----------------------------------------------------
# % de vetos derrubados (integral ou parcialmente) por presidente

taxa_derrota <- vetos |>
  filter(categoria_final %in% c("mantido", "derrubado",
                                "parcialmente_derrubado")) |>
  group_by(mandato, presidente) |>
  summarise(
    n_decididos = n(),
    n_derrubado_total   = sum(categoria_final == "derrubado"),
    n_derrubado_parcial = sum(categoria_final == "parcialmente_derrubado"),
    n_qualquer_derrota  = n_derrubado_total + n_derrubado_parcial,
    pct_qualquer_derrota = n_qualquer_derrota / n_decididos,
    pct_derrubado_total  = n_derrubado_total  / n_decididos,
    .groups = "drop"
  )

# --- 5) Salvar -------------------------------------------------------------

resumos <- list(
  decisoes_presidente      = decisoes_presidente,
  decisoes_long            = decisoes_long,
  congresso_por_veto       = congresso_por_veto,
  congresso_por_veto_tipo  = congresso_por_veto_tipo,
  dispositivos_por_mandato = dispositivos_por_mandato,
  taxa_derrota             = taxa_derrota,
  ordem_mandato            = ordem_mandato
)

saveRDS(resumos, file.path(PROC_DIR, "resumos.rds"))

cat("Resumos calculados:\n")
print(decisoes_presidente)
cat("\nTaxa de derrota:\n")
print(taxa_derrota)
