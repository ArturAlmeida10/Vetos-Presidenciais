# 02_coleta_sancoes.R
# Baixa, da API de Dados Abertos do Senado, todas as Leis Ordinárias
# (LEI-n) e Leis Complementares (LCP) publicadas entre 1995 e o ano corrente.
# A "data de assinatura" da norma é o que usaremos para atribuir cada lei ao
# presidente em exercício no script 03.
#
# Observação: este endpoint retorna leis independentemente da origem
# (PL, PLP, PLV de conversão de MP, etc). Vamos refinar essa origem em 03,
# combinando com a base de vetos para classificar cada lei como
# sanção pura, veto integral derrubado ou veto parcial.

library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)
library(fs)
library(glue)

API_BASE  <- "https://legis.senado.leg.br/dadosabertos"
ANOS      <- 1995:as.integer(format(Sys.Date(), "%Y"))
TIPOS     <- c("LEI-n", "LCP")  # Lei Ordinária e Lei Complementar
RAW_DIR   <- "data/raw"
DIR_LEIS  <- file.path(RAW_DIR, "leis_por_ano")

dir_create(DIR_LEIS)

fetch_json <- function(url, dest, overwrite = FALSE) {
  if (file_exists(dest) && !overwrite) return(invisible(dest))

  resp <- request(url) |>
    req_headers(Accept = "application/json") |>
    req_retry(max_tries = 4, backoff = \(i) 2 ^ i) |>
    req_timeout(60) |>
    req_error(is_error = \(r) FALSE) |>
    req_perform()

  if (resp_status(resp) == 200 && resp_body_string(resp) != "") {
    writeBin(resp_body_raw(resp), dest)
    invisible(dest)
  } else {
    message(glue("  ! status {resp_status(resp)} em {url}"))
    invisible(NULL)
  }
}

parse_leis <- function(arquivo) {
  if (!file_exists(arquivo)) return(tibble())
  raw <- read_json(arquivo)
  docs <- raw$ListaDocumento$documentos$documento
  if (is.null(docs)) return(tibble())
  if (!is.null(docs$id)) docs <- list(docs)  # caso retorne objeto único

  tibble(
    id_norma        = map_chr(docs, ~ as.character(.x$id %||% NA)),
    tipo            = map_chr(docs, ~ as.character(.x$tipo %||% NA)),
    numero          = map_chr(docs, ~ as.character(.x$numero %||% NA)),
    norma_nome      = map_chr(docs, ~ as.character(.x$normaNome %||% NA)),
    ementa          = map_chr(docs, ~ as.character(.x$ementa %||% NA)),
    data_assinatura = map_chr(docs, ~ as.character(.x$dataassinatura %||% NA)),
    ano_assinatura  = map_chr(docs, ~ as.character(.x$anoassinatura %||% NA))
  )
}

# --- 1) Baixar listas por (tipo, ano) ---------------------------------------

message("Baixando leis (LEI-n e LCP) por ano")
combos <- expand.grid(ano = ANOS, tipo = TIPOS, stringsAsFactors = FALSE)
pwalk(combos, function(ano, tipo) {
  tipo_safe <- gsub("-", "", tipo)
  dest <- file.path(DIR_LEIS, glue("{tipo_safe}_{ano}.json"))
  url  <- glue("{API_BASE}/legislacao/lista?tipo={tipo}&ano={ano}")
  message(glue("  - {tipo} {ano}"))
  fetch_json(url, dest)
})

# --- 2) Consolidar índice ---------------------------------------------------

message("Consolidando índice de leis")
arquivos <- dir_ls(DIR_LEIS, glob = "*.json")
leis_idx <- map_dfr(arquivos, parse_leis) |>
  filter(!is.na(id_norma))

message(glue("  total de leis catalogadas: {nrow(leis_idx)}"))
write_json(leis_idx, file.path(RAW_DIR, "leis_index.json"),
           auto_unbox = TRUE, pretty = TRUE)

message("Coleta concluída")
message(glue("  índice:  {file.path(RAW_DIR, 'leis_index.json')}"))
message(glue("  por ano: {DIR_LEIS}"))
