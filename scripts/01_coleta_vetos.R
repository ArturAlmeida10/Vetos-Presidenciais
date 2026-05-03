# 01_coleta_vetos.R
# Baixa, da API de Dados Abertos do Senado:
#   (a) a lista anual de vetos presidenciais (1995 -> ano corrente);
#   (b) o resultado dispositivo-a-dispositivo de cada veto;
#   (c) o detalhe da matéria (mensagem do veto), que traz a data de
#       apresentação, necessária para vetos antigos cujo DataPublicacao
#       não é exposto pela API.
# Salva em data/raw/ em JSON bruto, com cache por arquivo.

library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)
library(fs)
library(glue)

# --- Config -----------------------------------------------------------------

API_BASE     <- "https://legis.senado.leg.br/dadosabertos"
ANOS         <- 1995:as.integer(format(Sys.Date(), "%Y"))
RAW_DIR      <- "data/raw"
DIR_VETOS    <- file.path(RAW_DIR, "vetos_por_ano")
DIR_RESULT   <- file.path(RAW_DIR, "resultado_veto")
DIR_MATERIA  <- file.path(RAW_DIR, "materia_detalhe")

dir_create(DIR_VETOS)
dir_create(DIR_RESULT)
dir_create(DIR_MATERIA)

# --- Função pros JSON -------------------------------------------------------

fetch_json <- function(url, dest, overwrite = FALSE) {
  if (file_exists(dest) && !overwrite) return(invisible(dest))

  resp <- request(url) |>
    req_headers(Accept = "application/json") |>
    req_retry(max_tries = 4, backoff = \(i) 2 ^ i) |>
    req_timeout(60) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  status <- resp_status(resp)
  if (status == 200 && resp_body_string(resp) != "") {
    writeBin(resp_body_raw(resp), dest)
    invisible(dest)
  } else {
    message(glue("  ! status {status} em {url}"))
    invisible(NULL)
  }
}

# Lista de vetos de um ano -> tibble com Codigo e DataPublicacao
parse_vetos_ano <- function(arquivo) {
  if (!file_exists(arquivo)) return(tibble())
  raw <- read_json(arquivo)
  veto_node <- raw$ListaVetosAnoCN$Vetos$Veto
  if (is.null(veto_node)) return(tibble())
  # API às vezes retorna objeto único, às vezes lista
  if (!is.null(veto_node$Codigo)) veto_node <- list(veto_node)

  tibble(
    codigo_veto       = map_chr(veto_node, ~ as.character(.x$Codigo %||% NA)),
    data_publicacao   = map_chr(veto_node, ~ as.character(.x$DataPublicacao %||% NA)),
    total             = map_chr(veto_node, ~ as.character(.x$Total %||% NA)),
    materia_codigo    = map_chr(veto_node, ~ as.character(.x$Materia$Codigo %||% NA)),
    qtd_dispositivos  = map_chr(veto_node, ~ as.character(.x$QuantidadeDispositivos %||% NA))
  )
}

# --- 1) Lista anual de vetos ------------------------------------------------

message("Baixando lista anual de vetos")
walk(ANOS, function(ano) {
  dest <- file.path(DIR_VETOS, glue("vetos_{ano}.json"))
  url  <- glue("{API_BASE}/materia/vetos/{ano}")
  message(glue("  - {ano}"))
  fetch_json(url, dest)
})

# --- 2) Consolidação dos códigos de veto ------------------------------------

message("Consolidando códigos de veto")
arquivos_anuais <- dir_ls(DIR_VETOS, glob = "*.json")
vetos_idx <- map_dfr(arquivos_anuais, parse_vetos_ano) |>
  filter(!is.na(codigo_veto))

message(glue("  total de vetos catalogados: {nrow(vetos_idx)}"))
write_json(vetos_idx, file.path(RAW_DIR, "vetos_index.json"),
           auto_unbox = TRUE, pretty = TRUE)

# --- 3) Resultado de cada veto (dispositivos + situação) --------------------

message("Baixando resultado por veto (cache: pula se já existe)")
walk(vetos_idx$codigo_veto, function(cod) {
  dest <- file.path(DIR_RESULT, glue("veto_{cod}.json"))
  url  <- glue("{API_BASE}/plenario/resultado/veto/{cod}")
  fetch_json(url, dest)
})

# --- 4) Detalhe da matéria (para resgatar DataApresentacao em vetos antigos)-

message("Baixando detalhe da matéria (mensagem do veto)")
materias_codigos <- unique(na.omit(vetos_idx$materia_codigo))
walk(materias_codigos, function(cod) {
  dest <- file.path(DIR_MATERIA, glue("materia_{cod}.json"))
  url  <- glue("{API_BASE}/materia/{cod}")
  fetch_json(url, dest)
})

message("Coleta concluída")
message(glue("  índice:    {file.path(RAW_DIR, 'vetos_index.json')}"))
message(glue("  por ano:   {DIR_VETOS}"))
message(glue("  resultado: {DIR_RESULT}"))
message(glue("  matérias:  {DIR_MATERIA}"))
