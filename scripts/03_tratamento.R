# 03_tratamento.R
# Lê os JSONs brutos baixados em 01 e 02, atribui cada veto e cada lei ao
# presidente correspondente (pela data) e gera 4 tabelas tidy em
# data/processed/:
#   - mandatos.rds          : tabela de presidentes (referência)
#   - vetos.rds             : 1 linha por veto, com categoria final
#   - dispositivos.rds      : 1 linha por dispositivo vetado
#   - leis.rds              : 1 linha por lei (LEI-n / LCP) assinada
#
# Regra de atribuição: o veto é sempre creditado ao presidente que o
# publicou (data_publicacao do veto), independentemente de quando o
# Congresso decidiu sobre ele.

library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(fs)
library(glue)

RAW_DIR  <- "data/raw"
PROC_DIR <- "data/processed"
dir_create(PROC_DIR)

# --- 1) Tabela de mandatos --------------------------------------------------
# Datas de transição:
# - FHC, Lula (1 e 2), Dilma1: posses em 1º de janeiro do ano seguinte ao da eleição
# - Dilma 2 -> Temer: Dilma foi afastada em 2016-05-12 (impeachment provisório).
#   Temer assumiu interinamente nessa data e definitivamente em 2016-08-31.
#   Como o que importa para nós é quem assinava vetos, usamos 2016-05-12 como
#   fim de Dilma2 e início de Temer.

mandatos <- tibble::tribble(
  ~mandato_id, ~presidente,    ~mandato,   ~inicio,        ~fim,
  "FHC1",      "FHC",          "FHC 1",    "1995-01-01",   "1998-12-31",
  "FHC2",      "FHC",          "FHC 2",    "1999-01-01",   "2002-12-31",
  "LULA1",     "Lula",         "Lula 1",   "2003-01-01",   "2006-12-31",
  "LULA2",     "Lula",         "Lula 2",   "2007-01-01",   "2010-12-31",
  "DILMA1",    "Dilma",        "Dilma 1",  "2011-01-01",   "2014-12-31",
  "DILMA2",    "Dilma",        "Dilma 2",  "2015-01-01",   "2016-05-11",
  "TEMER",     "Temer",        "Temer",    "2016-05-12",   "2018-12-31",
  "BOLSONARO", "Bolsonaro",    "Bolsonaro","2019-01-01",   "2022-12-31",
  "LULA3",     "Lula",         "Lula 3",   "2023-01-01",   as.character(Sys.Date())
) |>
  mutate(
    inicio = as.Date(inicio),
    fim    = as.Date(fim),
    mandato = factor(mandato, levels = mandato)  # ordem cronológica
  )

# Atribui um vetor de datas ao mandato_id correspondente
atribuir_mandato <- function(datas) {
  datas <- as.Date(datas)
  out <- rep(NA_character_, length(datas))
  for (i in seq_len(nrow(mandatos))) {
    dentro <- !is.na(datas) & datas >= mandatos$inicio[i] & datas <= mandatos$fim[i]
    out[dentro] <- mandatos$mandato_id[i]
  }
  out
}

saveRDS(mandatos, file.path(PROC_DIR, "mandatos.rds"))

# --- 2) Vetos: índice + resultados ------------------------------------------

message("Lendo resultados por veto")

resultado_files <- dir_ls(file.path(RAW_DIR, "resultado_veto"), glob = "*.json")

# extrai (1) metadados do veto e (2) dispositivos, de um arquivo de resultado
parse_resultado <- function(arquivo) {
  raw <- read_json(arquivo)
  v   <- raw$ResultadoVetoItemCN$Veto
  if (is.null(v)) return(list(veto = tibble(), disp = tibble()))

  # dispositivos: pode vir como objeto único (lista de 1) ou lista
  disp_node <- v$Dispositivos$Dispositivo
  if (is.null(disp_node)) {
    disp <- tibble()
  } else {
    if (!is.null(disp_node$Codigo)) disp_node <- list(disp_node)
    disp <- tibble(
      codigo_veto        = as.character(v$Codigo),
      codigo_dispositivo = map_chr(disp_node, ~ as.character(.x$Codigo %||% NA)),
      identificador      = map_chr(disp_node, ~ as.character(.x$Identificador %||% NA)),
      descricao          = map_chr(disp_node, ~ as.character(.x$Descricao %||% NA)),
      situacao           = map_chr(disp_node, ~ as.character(.x$Situacao %||% NA)),
      possui_votos       = map_chr(disp_node, ~ as.character(.x$PossuiVotos %||% NA)),
      data_sessao        = map_chr(disp_node, ~ as.character(.x$DataSessao %||% NA))
    )
  }

  veto <- tibble(
    codigo_veto      = as.character(v$Codigo),
    materia_codigo   = as.character(v$Materia$Codigo %||% NA),
    materia_sigla    = as.character(v$Materia$Sigla %||% NA),
    materia_numero   = as.character(v$Materia$Numero %||% NA),
    materia_ano      = as.character(v$Materia$Ano %||% NA),
    em_tramitacao    = as.character(v$Materia$EmTramitacao %||% NA),
    materia_vetada_sigla  = as.character(v$MateriaVetada$Sigla %||% NA),
    materia_vetada_numero = as.character(v$MateriaVetada$Numero %||% NA),
    materia_vetada_ano    = as.character(v$MateriaVetada$Ano %||% NA),
    mensagem_sigla   = as.character(v$Mensagem$Sigla %||% NA),
    mensagem_numero  = as.character(v$Mensagem$Numero %||% NA),
    mensagem_ano     = as.character(v$Mensagem$Ano %||% NA),
    total            = as.character(v$Total %||% NA),
    assunto          = as.character(v$Assunto %||% NA),
    data_publicacao  = as.character(v$DataPublicacao %||% NA),
    data_recebimento = as.character(v$DataRecebimentoCongresso %||% NA),
    n_dispositivos   = as.integer(nrow(disp))
  )

  list(veto = veto, disp = disp)
}

parsed <- map(resultado_files, parse_resultado)
vetos_raw       <- map_dfr(parsed, "veto")
dispositivos    <- map_dfr(parsed, "disp")

message(glue("  vetos lidos:        {nrow(vetos_raw)}"))
message(glue("  dispositivos lidos: {nrow(dispositivos)}"))

# --- 2b) Enriquecimento com DataApresentacao da matéria --------------------
# Para vetos antigos a API não expõe DataPublicacao na lista nem no
# resultado; o detalhe da matéria, contudo, traz DataApresentacao, que
# representa a apresentação ao Congresso (proxy da publicação do veto, com
# diferença típica de poucos dias).

message("Enriquecendo vetos com DataApresentacao da matéria")

parse_materia_data <- function(arquivo) {
  raw <- read_json(arquivo)
  m <- raw$DetalheMateria$Materia
  if (is.null(m)) return(tibble())
  tibble(
    materia_codigo    = as.character(m$IdentificacaoMateria$CodigoMateria %||% NA),
    data_apresentacao = as.character(m$DadosBasicosMateria$DataApresentacao %||% NA)
  )
}

materia_files <- dir_ls(file.path(RAW_DIR, "materia_detalhe"), glob = "*.json")
materias_df <- map_dfr(materia_files, parse_materia_data) |>
  filter(!is.na(materia_codigo)) |>
  distinct(materia_codigo, .keep_all = TRUE)

vetos_raw <- vetos_raw |>
  left_join(materias_df, by = "materia_codigo")

# --- 3) Categoriza cada veto ------------------------------------------------
# Situações possíveis do dispositivo:
#   - "Mantido"        : Congresso manteve o veto (dispositivo NÃO entra na lei)
#   - "Rejeitado"      : Congresso derrubou o veto (dispositivo entra na lei)
#   - "Prejudicado"    : decisão prejudicada por questão regimental;
#                        para fins do efeito final, equivale a manutenção
#                        (o dispositivo permanece fora da lei)
#   - "Não Apreciado"  : veto não chegou a ser apreciado e foi arquivado;
#                        resultado prático = manutenção
#
# Categoria_final do veto:
#   - "derrubado"               : todos os dispositivos rejeitados
#   - "parcialmente_derrubado"  : pelo menos um rejeitado e um efetivamente mantido
#   - "mantido"                 : sem dispositivos rejeitados (mantido / prejudicado /
#                                 não apreciado)
#   - "em_tramitacao"           : matéria ainda em tramitação no Congresso

efetivamente_mantido <- c("Mantido", "Prejudicado", "Não Apreciado")

resumo_por_veto <- dispositivos |>
  mutate(situacao = na_if(situacao, "")) |>
  group_by(codigo_veto) |>
  summarise(
    n_disp_total      = n(),
    n_disp_mantidos   = sum(situacao %in% efetivamente_mantido, na.rm = TRUE),
    n_disp_derrubados = sum(situacao == "Rejeitado", na.rm = TRUE),
    .groups = "drop"
  )

vetos <- vetos_raw |>
  mutate(
    data_publicacao   = as.Date(data_publicacao),
    data_recebimento  = as.Date(data_recebimento),
    data_apresentacao = as.Date(data_apresentacao),
    # Data efetiva: prioriza DataPublicacao; o resto usa DataApresentacao da matéria
    # (usada para vetos antigos onde a API não traz DataPublicacao).
    data_veto = coalesce(data_publicacao, data_apresentacao, data_recebimento),
    tipo_veto = case_when(
      str_to_lower(total) %in% c("sim", "s", "true") ~ "integral",
      TRUE                                            ~ "parcial"
    )
  ) |>
  left_join(resumo_por_veto, by = "codigo_veto") |>
  mutate(
    em_tramitacao_flag = str_to_lower(em_tramitacao) %in% c("sim", "s", "true"),
    # Confiamos primeiro nos dispositivos: se todos os dispositivos já têm uma
    # situação registrada, o veto é considerado decidido — ainda que a API
    # marque a matéria como "em tramitação" (situação aparentemente comum em
    # vetos antigos cujo encerramento formal nunca foi registrado).
    todos_dispositivos_decididos = n_disp_total > 0 &
      (n_disp_mantidos + n_disp_derrubados) == n_disp_total,
    categoria_final = case_when(
      n_disp_total == 0 & em_tramitacao_flag              ~ "em_tramitacao",
      n_disp_total == 0                                   ~ "em_tramitacao",
      !todos_dispositivos_decididos & em_tramitacao_flag  ~ "em_tramitacao",
      n_disp_derrubados == 0                              ~ "mantido",
      n_disp_derrubados == n_disp_total                   ~ "derrubado",
      TRUE                                                ~ "parcialmente_derrubado"
    ),
    mandato_id = atribuir_mandato(data_veto)
  ) |>
  left_join(mandatos |> select(mandato_id, presidente, mandato),
            by = "mandato_id")

# Dispositivos: também recebem o mandato (do veto que originou)
dispositivos <- dispositivos |>
  left_join(vetos |> select(codigo_veto, mandato_id, presidente, mandato,
                            data_veto, tipo_veto),
            by = "codigo_veto") |>
  mutate(data_sessao = as.Date(data_sessao))

# --- 4) Leis ----------------------------------------------------------------

message("Lendo índice de leis")

leis_idx <- read_json(file.path(RAW_DIR, "leis_index.json")) |>
  map_dfr(~ tibble(
    id_norma        = as.character(.x$id_norma %||% NA),
    tipo            = as.character(.x$tipo %||% NA),
    numero          = as.character(.x$numero %||% NA),
    norma_nome      = as.character(.x$norma_nome %||% NA),
    ementa          = as.character(.x$ementa %||% NA),
    data_assinatura = as.character(.x$data_assinatura %||% NA),
    ano_assinatura  = as.character(.x$ano_assinatura %||% NA)
  ))

# Datas vêm em "DD/MM/YYYY"
leis <- leis_idx |>
  mutate(
    data_assinatura = dmy(data_assinatura),
    ano_assinatura  = as.integer(ano_assinatura)
  ) |>
  filter(!is.na(data_assinatura)) |>
  mutate(mandato_id = atribuir_mandato(data_assinatura)) |>
  left_join(mandatos |> select(mandato_id, presidente, mandato),
            by = "mandato_id") |>
  filter(!is.na(mandato_id))     # ignora normas anteriores a 1995

# --- 5) Salvar bancos -------------------------------------------------------

saveRDS(vetos,        file.path(PROC_DIR, "vetos.rds"))
saveRDS(dispositivos, file.path(PROC_DIR, "dispositivos.rds"))
saveRDS(leis,         file.path(PROC_DIR, "leis.rds"))

message("Tratamento concluído")
message(glue("  vetos:        {nrow(vetos)} linhas -> {file.path(PROC_DIR,'vetos.rds')}"))
message(glue("  dispositivos: {nrow(dispositivos)} linhas -> {file.path(PROC_DIR,'dispositivos.rds')}"))
message(glue("  leis:         {nrow(leis)} linhas -> {file.path(PROC_DIR,'leis.rds')}"))

