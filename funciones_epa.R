# =============================================================================
# FUNCIONES COMPARTIDAS - EPA HORAS EXTRA
# -----------------------------------------------------------------------------
# Copiadas tal cual de tu app.R original. Las usan tanto app.R (la app Shiny)
# como agregar_datos_epa.R (el script que corre en GitHub Actions).
# NO se ejecuta nada al cargar este fichero, solo define funciones.
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(httr)

# -----------------------------------------------------------------------------
# CONFIG
# -----------------------------------------------------------------------------

ANIO_MIN <- 2005
CACHE_DIR <- "epa_cache_horas_extra"
CACHE_VERSION <- "v3"
ANEXOS_LOCAL_DIR <- "anexos_locales"

dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(ANEXOS_LOCAL_DIR, showWarnings = FALSE, recursive = TRUE)

ruta_anexo_local <- function(anio, trim) {
  candidato <- file.path(ANEXOS_LOCAL_DIR, sprintf("EPAAnexo_%dT%d.tab", anio, trim))
  if (file.exists(candidato)) candidato else NA_character_
}

# Columna de grupo de actividad esperada según el periodo (ver nota en tu
# app.R original: ACT1 es la columna estable; cambia el esquema de
# codificación CNAE-2009 -> CNAE-2025 a partir de 2026).
seleccionar_col_rama <- function(anio) {
  if (anio <= 2025) {
    list(col = "ACT1", clasif = "CNAE2009")
  } else {
    list(col = "ACT1", clasif = "CNAE2025")
  }
}

# -----------------------------------------------------------------------------
# HELPERS DE TEXTO / PARSEO
# -----------------------------------------------------------------------------

parse_factor_mixto <- function(x) {
  s <- trimws(as.character(x))
  dplyr::case_when(
    s == "" ~ NA_real_,
    stringr::str_detect(s, ",") ~ readr::parse_number(
      s, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    ),
    stringr::str_detect(s, "\\.") ~ readr::parse_number(
      s, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    ),
    TRUE ~ suppressWarnings(as.numeric(s))
  )
}

normalizar_nombres <- function(nms) {
  nms <- trimws(nms)
  nms <- toupper(nms)
  nms <- gsub('"', "", nms, fixed = TRUE)
  nms <- gsub("\\s+", "", nms, perl = TRUE)
  nms
}

leer_tab_generico <- function(path) {
  df <- read_delim(
    file = path,
    delim = "\t",
    locale = locale(encoding = "Latin1", decimal_mark = ",", grouping_mark = "."),
    show_col_types = FALSE,
    progress = FALSE,
    quote = "\"",
    escape_double = TRUE,
    trim_ws = TRUE,
    col_types = cols(.default = col_character())
  )
  names(df) <- normalizar_nombres(names(df))
  df
}

# HHMM -> horas decimales
hhmm_to_hours <- function(x, allow_99 = FALSE) {
  s <- str_pad(as.character(x), width = 4, side = "left", pad = "0")
  hh <- suppressWarnings(as.integer(substr(s, 1, 2)))
  mm <- suppressWarnings(as.integer(substr(s, 3, 4)))
  ok_basic <- !is.na(hh) & !is.na(mm) & hh >= 0 & hh <= 98 & mm >= 0 & mm <= 59
  has_99   <- allow_99 & ((hh == 99) | (mm == 99))
  is_special <- s %in% c("0000", "9999") | has_99
  dplyr::if_else(!is_special & ok_basic, hh + (mm / 60), NA_real_)
}

# -----------------------------------------------------------------------------
# DESCARGA + CACHE
# -----------------------------------------------------------------------------

url_micro <- function(anio, trim) {
  yy <- sprintf("%02d", anio %% 100)
  sprintf("https://www.ine.es/ftp/microdatos/epa/datos_%dt%s.zip", trim, yy)
}

url_anexo_candidatos <- function(anio, trim) {
  yy <- sprintf("%02d", anio %% 100)
  c(
    sprintf("https://www.ine.es/ftp/microdatos/epa/anexob21_%dt%s.zip", trim, yy),
    sprintf("https://www.ine.es/ftp/microdatos/epa/anexo_%dt%s.zip", trim, yy),
    sprintf("https://www.ine.es/ftp/microdatos/epa/anexoB21_%dt%s.zip", trim, yy),
    sprintf("https://www.ine.es/ftp/microdatos/epa/AnexoB21_%dt%s.zip", trim, yy)
  )
}

descargar_zip <- function(url, timeout_s = 60) {
  destino <- tempfile(fileext = ".zip")
  ok <- tryCatch({
    resp <- httr::GET(
      url,
      httr::write_disk(destino, overwrite = TRUE),
      httr::timeout(timeout_s)
    )
    httr::status_code(resp) == 200
  }, error = function(e) FALSE)

  if (!ok || !file.exists(destino) || file.info(destino)$size < 1000) {
    unlink(destino)
    return(NULL)
  }
  destino
}

extraer_fichero_datos <- function(zip_path, es_anexo = FALSE) {
  dir_extract <- tempfile("epa_extract_")
  dir.create(dir_extract)
  unzip(zip_path, exdir = dir_extract)

  todos <- list.files(dir_extract, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  candidatos <- todos[str_detect(toupper(todos), "\\.(TAB|CSV)$")]

  en_csv <- candidatos[str_detect(toupper(candidatos), "[/\\\\]CSV[/\\\\]")]
  if (length(en_csv) > 0) candidatos <- en_csv

  if (es_anexo) {
    elegido <- candidatos[str_detect(toupper(candidatos), "ANEXO")]
  } else {
    elegido <- candidatos[!str_detect(toupper(candidatos), "ANEXO")]
  }

  if (length(elegido) == 0) return(NULL)
  elegido[1]
}

leer_anexo_df <- function(path_anexo) {
  df <- leer_tab_generico(path_anexo)
  req_cols <- c("NVIVI", "FACB2021")
  faltan <- setdiff(req_cols, names(df))
  if (length(faltan) > 0) {
    stop(
      "ANEXO inválido: ", basename(path_anexo),
      " -> faltan columnas: ", paste(faltan, collapse = ", ")
    )
  }
  fac_raw <- trimws(as.character(df$FACB2021))
  fac_num <- dplyr::case_when(
    fac_raw == "" ~ NA_real_,
    stringr::str_detect(fac_raw, ",") ~ readr::parse_number(
      fac_raw, locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    ),
    stringr::str_detect(fac_raw, "\\.") ~ readr::parse_number(
      fac_raw, locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
    ),
    TRUE ~ suppressWarnings(as.numeric(fac_raw))
  )
  tibble(NVIVI = as.character(df$NVIVI), FACB2021 = fac_num) |>
    filter(!is.na(NVIVI), NVIVI != "") |>
    group_by(NVIVI) |>
    summarise(FACB2021 = dplyr::first(FACB2021), .groups = "drop")
}

cols_keep_base <- c("CICLO", "CCAA", "PROV", "NVIVI", "NIVEL", "SITU", "EXTPAG", "EXTNPG", "FACTOREL")
cols_keep <- c(cols_keep_base, "ACT1", "ACT09")

# Descarga (o recupera de cache) y procesa UN trimestre.
cargar_trimestre_he <- function(anio, trim) {
  etiqueta <- sprintf("%dT%d", anio, trim)
  cache_file <- file.path(CACHE_DIR, sprintf("epa_he_%s_%s.rds", CACHE_VERSION, etiqueta))

  if (file.exists(cache_file)) {
    cacheado <- readRDS(cache_file)
    if (is.data.frame(cacheado) && all(c("RAMA_COD", "CLASIF_RAMA") %in% names(cacheado))) {
      return(cacheado)
    }
  }

  zip_micro <- descargar_zip(url_micro(anio, trim))
  if (is.null(zip_micro)) {
    return(list(error = sprintf(
      "No se pudo descargar el trimestre %s (%s). ¿Aún no está publicado?",
      etiqueta, url_micro(anio, trim)
    )))
  }

  path_micro <- extraer_fichero_datos(zip_micro, es_anexo = FALSE)
  if (is.null(path_micro)) {
    return(list(error = sprintf("No se encontró el fichero de microdatos dentro del zip de %s", etiqueta)))
  }

  df_raw <- leer_tab_generico(path_micro)

  requeridas <- c("NIVEL", "CICLO", "CCAA", "PROV", "NVIVI", "SITU", "EXTPAG", "EXTNPG", "FACTOREL")
  faltan <- setdiff(requeridas, names(df_raw))
  if (length(faltan) > 0) {
    return(list(error = sprintf(
      "Trimestre %s: faltan columnas %s", etiqueta, paste(faltan, collapse = ", ")
    )))
  }

  rama_sel <- seleccionar_col_rama(anio)
  col_rama <- if (rama_sel$col %in% names(df_raw)) rama_sel$col else NA_character_

  df <- df_raw |>
    select(any_of(cols_keep)) |>
    filter(NIVEL == "1") |>
    mutate(
      CICLO    = as.character(CICLO),
      CCAA     = str_pad(as.character(CCAA), 2, pad = "0"),
      PROV     = str_pad(as.character(PROV), 2, pad = "0"),
      NVIVI    = as.character(NVIVI),
      SITU     = as.character(SITU),
      EXTPAG   = as.character(EXTPAG),
      EXTNPG   = as.character(EXTNPG),
      FACTOREL = parse_factor_mixto(FACTOREL)
    )

  if (!is.na(col_rama)) {
    df$RAMA_COD <- df_raw |>
      filter(NIVEL == "1") |>
      pull(!!col_rama) |>
      as.character() |>
      str_pad(2, pad = "0")
  } else {
    df$RAMA_COD <- NA_character_
  }
  df$CLASIF_RAMA <- rama_sel$clasif

  if (anio >= 2021 && anio <= 2023) {
    path_anexo <- ruta_anexo_local(anio, trim)

    if (is.na(path_anexo)) {
      zip_anexo <- NULL
      for (u in url_anexo_candidatos(anio, trim)) {
        zip_anexo <- descargar_zip(u)
        if (!is.null(zip_anexo)) break
      }
      if (is.null(zip_anexo)) {
        return(list(error = sprintf(
          paste0(
            "Trimestre %s requiere anexo (factor base 2021) y no se pudo descargar ni ",
            "encontrar localmente en '%s'. Coloca el fichero EPAAnexo_%dT%d.tab en esa carpeta."
          ), etiqueta, ANEXOS_LOCAL_DIR, anio, trim
        )))
      }
      path_anexo <- extraer_fichero_datos(zip_anexo, es_anexo = TRUE)
      if (is.null(path_anexo)) {
        return(list(error = sprintf("No se encontró el fichero de anexo dentro del zip de %s", etiqueta)))
      }
    }

    anexo_df <- leer_anexo_df(path_anexo)
    df <- df |>
      left_join(anexo_df, by = "NVIVI", relationship = "many-to-one") |>
      mutate(FACTOR_USAR = FACB2021)

    n_na <- sum(is.na(df$FACTOR_USAR))
    if (n_na > 0) {
      return(list(error = sprintf(
        "Trimestre %s: %d registros sin FACB2021 tras el cruce con el anexo", etiqueta, n_na
      )))
    }
  } else {
    df <- df |> mutate(FACTOR_USAR = FACTOREL)
  }

  df$TRIMESTRE_ANIO  <- anio
  df$TRIMESTRE_NUM   <- trim
  df$TRIMESTRE_LABEL <- etiqueta
  df$COL_RAMA_USADA  <- col_rama

  saveRDS(df, cache_file)
  df
}

# Detecta el último trimestre publicado
detectar_ultimo_trimestre <- function(max_intentos = 6) {
  hoy <- Sys.Date()
  anio <- as.integer(format(hoy, "%Y"))
  mes  <- as.integer(format(hoy, "%m"))
  trim_natural <- ceiling(mes / 3) - 1
  if (trim_natural < 1) { trim_natural <- 4; anio <- anio - 1 }

  candidato_anio <- anio
  candidato_trim <- trim_natural

  for (i in seq_len(max_intentos)) {
    url <- url_micro(candidato_anio, candidato_trim)
    disponible <- tryCatch({
      resp <- httr::HEAD(url, httr::timeout(15))
      httr::status_code(resp) == 200
    }, error = function(e) FALSE)

    if (disponible) return(list(anio = candidato_anio, trim = candidato_trim))

    candidato_trim <- candidato_trim - 1
    if (candidato_trim < 1) { candidato_trim <- 4; candidato_anio <- candidato_anio - 1 }
  }
  list(anio = anio, trim = trim_natural)
}

ventana_n_trimestres <- function(anio_fin, trim_fin, n) {
  pares <- vector("list", n)
  a <- anio_fin; t <- trim_fin
  for (i in n:1) {
    pares[[i]] <- list(anio = a, trim = t)
    t <- t - 1
    if (t < 1) { t <- 4; a <- a - 1 }
  }
  pares
}

n_trimestres_hasta <- function(anio_fin, trim_fin, anio_ini, trim_ini) {
  (anio_fin * 4 + trim_fin) - (anio_ini * 4 + trim_ini) + 1
}

parse_trimestre_label <- function(label) {
  m <- str_match(label, "^(\\d{4})T(\\d)$")
  list(anio = as.integer(m[, 2]), trim = as.integer(m[, 3]))
}

# -----------------------------------------------------------------------------
# CÁLCULO DE HORAS EXTRA
# -----------------------------------------------------------------------------

preparar_horas_extra <- function(df) {
  df |>
    mutate(
      SITU2   = str_pad(as.character(SITU), 2, pad = "0"),
      EXTPAG4 = str_pad(as.character(EXTPAG), 4, pad = "0"),
      EXTNPG4 = str_pad(as.character(EXTNPG), 4, pad = "0"),

      hizo_pag = !is.na(EXTPAG4) & EXTPAG4 != "0000",
      hizo_npg = !is.na(EXTNPG4) & EXTNPG4 != "0000",
      hizo_tot = hizo_pag | hizo_npg,

      h_pag = hhmm_to_hours(EXTPAG4, allow_99 = FALSE),
      h_npg = hhmm_to_hours(EXTNPG4, allow_99 = TRUE),
      h_tot = coalesce(h_pag, 0) + coalesce(h_npg, 0),

      es_asalariado = SITU2 %in% c("07", "08")
    )
}
