# =============================================================================
# AGREGADOR DE DATOS EPA - HORAS EXTRA
# -----------------------------------------------------------------------------
# Este script NO es la app Shiny. Es el proceso que se ejecuta por separado
# (vía GitHub Actions, ver .github/workflows/actualizar_datos.yml) para:
#
#   1. Detectar el último trimestre publicado por el INE.
#   2. Descargar SOLO los trimestres que aún no están en el resumen guardado.
#   3. Agregar esos trimestres a nivel de grano fino: por
#      (TRIMESTRE, PROVINCIA, GRUPO DE ACTIVIDAD).
#   4. Guardar el resultado en un único fichero pequeño: resumen_global.rds
#
# La app Shiny NUNCA vuelve a tocar microdatos crudos: solo descarga este
# .rds (unas pocas MB) y hace sumas/filtros sobre él, que son instantáneos.
#
# Por qué grano PROVINCIA x RAMA y no algo ya colapsado:
#   - España   = sumar todas las provincias
#   - CCAA     = sumar las provincias de esa CCAA (mapeo PROV->CCAA estable)
#   - Con rama = agrupar por RAMA_COD dentro del territorio elegido
#   - Sin rama = sumar sobre todas las ramas
#   Todo esto son sumas sobre columnas ya agregadas -> válido estadísticamente
#   porque personas/horas son sumas ponderadas por FACTOR_USAR, no medias.
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(httr)

# -----------------------------------------------------------------------------
# CONFIG (debe coincidir con las funciones de tu app.R)
# -----------------------------------------------------------------------------

ANIO_MIN <- 2005
CACHE_DIR_MICRO <- "epa_cache_horas_extra"   # caché temporal de microdatos crudos (efímera, solo dura la ejecución de la Action)
ANEXOS_LOCAL_DIR <- "anexos_locales"          # si guardas los .tab de anexo 2021-2023 en el repo, ponlos aquí
RESUMEN_PATH <- "datos/resumen_global.rds"    # fichero final que se sube al repo
CACHE_VERSION <- "v3"

dir.create(CACHE_DIR_MICRO, showWarnings = FALSE, recursive = TRUE)
dir.create(ANEXOS_LOCAL_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(RESUMEN_PATH), showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# FUNCIONES REUTILIZADAS DE TU APP (idénticas o casi a app.R)
# -----------------------------------------------------------------------------
# Pega aquí, sin modificar, las funciones de tu app.R:
#   parse_factor_mixto, normalizar_nombres, leer_tab_generico, hhmm_to_hours,
#   url_micro, url_anexo_candidatos, descargar_zip, extraer_fichero_datos,
#   leer_anexo_df, seleccionar_col_rama, ruta_anexo_local,
#   cols_keep_base, cols_keep, cargar_trimestre_he, detectar_ultimo_trimestre,
#   ventana_n_trimestres, n_trimestres_hasta, preparar_horas_extra
#
# Para no duplicar +200 líneas aquí, la forma más limpia es moverlas a un
# fichero R/funciones_epa.R que sea compartido (source()) tanto por app.R
# como por este script. Así solo mantienes una copia. Ejemplo:

if (file.exists("R/funciones_epa.R")) {
  source("R/funciones_epa.R")
} else {
  stop(
    "Falta R/funciones_epa.R con las funciones de descarga/parseo.\n",
    "Mueve a ese fichero las funciones indicadas arriba (copiadas de tu app.R\n",
    "actual, sin cambios) y haz que app.R también las cargue con source()."
  )
}

# -----------------------------------------------------------------------------
# PASO 1: ¿qué trimestre es el último publicado?
# -----------------------------------------------------------------------------

ultimo <- detectar_ultimo_trimestre()
etiqueta_ultimo <- sprintf("%dT%d", ultimo$anio, ultimo$trim)
message(sprintf("Último trimestre detectado en el INE: %s", etiqueta_ultimo))

# -----------------------------------------------------------------------------
# PASO 2: cargar el resumen existente (si lo hay) y ver qué falta
# -----------------------------------------------------------------------------

resumen_existente <- if (file.exists(RESUMEN_PATH)) readRDS(RESUMEN_PATH) else NULL

trimestres_ya_procesados <- if (!is.null(resumen_existente)) {
  unique(resumen_existente$grano$TRIMESTRE_LABEL)
} else {
  character(0)
}

todos_los_trimestres <- ventana_n_trimestres(
  ultimo$anio, ultimo$trim,
  n_trimestres_hasta(ultimo$anio, ultimo$trim, ANIO_MIN, 1)
)
etiquetas_todas <- map_chr(todos_los_trimestres, ~ sprintf("%dT%d", .x$anio, .x$trim))

pendientes <- todos_los_trimestres[!(etiquetas_todas %in% trimestres_ya_procesados)]

if (length(pendientes) == 0) {
  message("Nada que actualizar: el resumen ya incluye el último trimestre. Fin.")
  quit(save = "no", status = 0)
}

message(sprintf("Trimestres a descargar/agregar: %s",
                paste(map_chr(pendientes, ~ sprintf("%dT%d", .x$anio, .x$trim)), collapse = ", ")))

# -----------------------------------------------------------------------------
# PASO 3: descargar y agregar SOLO los trimestres pendientes, a grano fino
# -----------------------------------------------------------------------------

agregar_trimestre_grano_fino <- function(anio, trim) {
  res <- cargar_trimestre_he(anio, trim)   # reutiliza tu función tal cual
  if (!is.data.frame(res)) {
    warning(sprintf("%dT%d: %s", anio, trim, res$error))
    return(NULL)
  }

  dfx <- preparar_horas_extra(res)         # tu función tal cual (añade hizo_pag, h_pag, etc.)

  dfx |>
    filter(!is.na(RAMA_COD), RAMA_COD != "", RAMA_COD != "NA") |>
    group_by(TRIMESTRE_LABEL, TRIMESTRE_ANIO, TRIMESTRE_NUM, PROV, CCAA, RAMA_COD, CLASIF_RAMA) |>
    summarise(
      personas_pag = sum(FACTOR_USAR[hizo_pag], na.rm = TRUE),
      personas_npg = sum(FACTOR_USAR[hizo_npg], na.rm = TRUE),
      personas_tot = sum(FACTOR_USAR[hizo_tot], na.rm = TRUE),
      horas_pag    = sum(FACTOR_USAR * h_pag, na.rm = TRUE),
      horas_npg    = sum(FACTOR_USAR * h_npg, na.rm = TRUE),
      horas_tot    = sum(FACTOR_USAR * h_tot, na.rm = TRUE),
      asalariados  = sum(FACTOR_USAR[es_asalariado], na.rm = TRUE),
      .groups = "drop"
    )
}

nuevos <- map(pendientes, ~ agregar_trimestre_grano_fino(.x$anio, .x$trim))
nuevos <- compact(nuevos)   # descarta NULL (trimestres con error de descarga)

if (length(nuevos) == 0) {
  message("Ningún trimestre pendiente se pudo procesar (revisa avisos arriba). No se actualiza el resumen.")
  quit(save = "no", status = 0)
}

grano_nuevo <- bind_rows(nuevos)

# -----------------------------------------------------------------------------
# PASO 4: fusionar con lo que ya había y guardar
# -----------------------------------------------------------------------------

grano_final <- if (!is.null(resumen_existente)) {
  bind_rows(resumen_existente$grano, grano_nuevo) |>
    distinct(TRIMESTRE_LABEL, PROV, RAMA_COD, .keep_all = TRUE)  # por si se reprocesa algo
} else {
  grano_nuevo
}

resultado <- list(
  grano = grano_final,
  ultimo_trimestre = etiqueta_ultimo,
  generado_el = Sys.time()
)

saveRDS(resultado, RESUMEN_PATH, compress = "xz")

message(sprintf(
  "Guardado %s (%s filas, %s trimestres, %.2f MB).",
  RESUMEN_PATH, nrow(grano_final), n_distinct(grano_final$TRIMESTRE_LABEL),
  file.info(RESUMEN_PATH)$size / 1e6
))

# Limpieza: borra la caché de microdatos crudos descargados en esta ejecución
# (no queremos que el runner de GitHub Actions acumule GB de microdatos;
# el .rds de resumen es lo único que se commitea).
unlink(CACHE_DIR_MICRO, recursive = TRUE)
