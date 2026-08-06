# =============================================================================
# DASHBOARD EPA - HORAS EXTRA (PAGADAS / NO PAGADAS)
# VERSIÓN "LIGERA": lee un resumen ya agregado desde GitHub en vez de
# descargar y procesar microdatos del INE en cada sesión de usuario.
# La descarga/agregación de microdatos ahora vive en agregar_datos_epa.R,
# ejecutado por GitHub Actions (.github/workflows/actualizar_datos.yml).
# =============================================================================

# ⚠️ CAMBIA ESTO por la URL raw de tu repo, rama y ruta reales.
#    Ejemplo: https://raw.githubusercontent.com/tuusuario/turepo/main/datos/resumen_global.rds
URL_RESUMEN <- "https://raw.githubusercontent.com/TU_USUARIO/TU_REPO/main/datos/resumen_global.rds"

# Si el repo es PRIVADO, define esta variable de entorno en posit.cloud con
# un token de acceso personal (fine-grained, solo lectura sobre ese repo).
# Si el repo es público, deja GITHUB_TOKEN_RESUMEN vacío y no pasa nada.
GITHUB_TOKEN_RESUMEN <- Sys.getenv("GITHUB_TOKEN_RESUMEN", unset = "")

#PARA ACTUALIZAR EN https://shernandez-cleon-ccoo.shinyapps.io/horasextra/, PONER EN LA CONSOLA:
#rsconnect::deployApp("X:/SERGIO/VISUALIZACION VALLADOLID/ONLINE/HORASEXTRA")

rsconnect::setAccountInfo(name='shernandez-cleon-ccoo',
                          token='5C0EE8461C63CAFC2C633FAC36375719',
                          secret='XBfUaSP6ImvVlRZrYee5iEynEgIi4EGsXL+4stsT')

#PARA ACTUALIZAR EN https://shernandez-cleon-ccoo-horasextra.share.connect.posit.cloud/, PONER EN LA CONSOLA:
#rsconnect::connectCloudUser()

library(shiny)
library(bslib)
library(shinycssloaders)
library(DT)
library(plotly)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(httr)
library(zoo)
library(scales)
library(ggplot2)
library(gridExtra)
library(grid)

# Funciones compartidas con el script de agregación (agregar_datos_epa.R):
# parseo de ficheros, descarga de zips del INE, cálculo de horas extra por
# persona, detección del último trimestre, etc. La app en sí YA NO las
# ejecuta contra el INE — solo las usa detectar_ultimo_trimestre()/
# ventana_n_trimestres()/n_trimestres_hasta()/parse_trimestre_label(), que
# son puro cálculo de fechas, sin red.
source("R/funciones_epa.R")

MAPAS_DISPONIBLES <- requireNamespace("mapSpain", quietly = TRUE) &&
  requireNamespace("sf", quietly = TRUE)
if (MAPAS_DISPONIBLES) {
  library(sf)
}

JPEG_DISPONIBLE <- requireNamespace("jpeg", quietly = TRUE)

informe_disponible <- function() {
  requireNamespace("ggplot2", quietly = TRUE) &&
    requireNamespace("gridExtra", quietly = TRUE)
}

# -----------------------------------------------------------------------------
# CONFIGURACION
# -----------------------------------------------------------------------------

# Geometrías de CCAA y provincias (mapSpain), con caché local en disco.
# Nota: esta caché SÍ es efímera en posit.cloud (se pierde al reiniciar el
# contenedor), pero no importa: son pocos MB y se regeneran solas en unos
# segundos la próxima vez que arranque la app, sin depender del INE.
CACHE_DIR_GEO <- "geo_cache"
dir.create(CACHE_DIR_GEO, showWarnings = FALSE, recursive = TRUE)

obtener_geo_ccaa <- function() {
  cache_geo <- file.path(CACHE_DIR_GEO, "geo_ccaa.rds")
  if (file.exists(cache_geo)) return(readRDS(cache_geo))
  geo <- mapSpain::esp_get_ccaa(epsg = "4326", moveCAN = TRUE)
  geo$cod <- str_pad(as.character(geo$codauto), 2, pad = "0")
  saveRDS(geo, cache_geo)
  geo
}

obtener_geo_prov <- function() {
  cache_geo <- file.path(CACHE_DIR_GEO, "geo_prov.rds")
  if (file.exists(cache_geo)) return(readRDS(cache_geo))
  geo <- mapSpain::esp_get_prov(epsg = "4326", moveCAN = TRUE)
  geo$cod <- str_pad(as.character(geo$cpro), 2, pad = "0")
  saveRDS(geo, cache_geo)
  geo
}

# Horas semanales de una jornada completa "de referencia".
JORNADA_SEMANAL_HORAS <- 40

CCOO_ROJO    <- "#E2231C"
CCOO_NEGRO   <- "#1a1a1a"
CCOO_GRANATE <- "#7A1F2B"

options(spinner.color = CCOO_ROJO, spinner.type = 4)

rama_labels_simplificadas <- c(
  "00" = "Agricultura y pesca",
  "01" = "Alimentación, textil y papel",
  "02" = "Ind. extractiva, energía y química",
  "03" = "Maquinaria y equipo",
  "04" = "Construcción",
  "05" = "Comercio y hostelería",
  "06" = "Transporte y comunicaciones",
  "07" = "Finanzas, inmobiliarias y serv. profesionales",
  "08" = "AAPP, educación y sanidad",
  "09" = "Otros servicios"
)

etiqueta_rama <- function(cod, clasif = "CNAE2009") {
  cod <- str_pad(as.character(cod), 2, pad = "0")
  ifelse(cod %in% names(rama_labels_simplificadas),
         rama_labels_simplificadas[cod],
         paste("Grupo", cod, sprintf("(%s)", clasif)))
}

prov_labels <- c(
  "01"="Araba/Álava","02"="Albacete","03"="Alicante/Alacant","04"="Almería","05"="Ávila",
  "06"="Badajoz","07"="Balears, Illes","08"="Barcelona","09"="Burgos","10"="Cáceres",
  "11"="Cádiz","12"="Castellón/Castelló","13"="Ciudad Real","14"="Córdoba","15"="Coruña, A",
  "16"="Cuenca","17"="Girona","18"="Granada","19"="Guadalajara","20"="Gipuzkoa",
  "21"="Huelva","22"="Huesca","23"="Jaén","24"="León","25"="Lleida",
  "26"="Rioja, La","27"="Lugo","28"="Madrid","29"="Málaga","30"="Murcia",
  "31"="Navarra","32"="Ourense","33"="Asturias","34"="Palencia","35"="Palmas, Las",
  "36"="Pontevedra","37"="Salamanca","38"="Santa Cruz de Tenerife","39"="Cantabria","40"="Segovia",
  "41"="Sevilla","42"="Soria","43"="Tarragona","44"="Teruel","45"="Toledo",
  "46"="Valencia/València","47"="Valladolid","48"="Bizkaia","49"="Zamora","50"="Zaragoza",
  "51"="Ceuta","52"="Melilla"
)

ccaa_labels <- c(
  "01"="Andalucía","02"="Aragón","03"="Asturias","04"="Illes Balears",
  "05"="Canarias","06"="Cantabria","07"="Castilla y León","08"="Castilla-La Mancha",
  "09"="Cataluña","10"="Comunitat Valenciana","11"="Extremadura","12"="Galicia",
  "13"="Comunidad de Madrid","14"="Región de Murcia","15"="Navarra","16"="País Vasco","17"="La Rioja",
  "51"="Ceuta","52"="Melilla"
)

generar_opciones_trimestre <- function(ultimo) {
  anios <- ANIO_MIN:ultimo$anio
  opciones <- c()
  for (a in rev(anios)) {
    trims <- if (a == ultimo$anio) ultimo$trim:1 else 4:1
    for (t in trims) {
      if (a == ultimo$anio && t > ultimo$trim) next
      opciones <- c(opciones, sprintf("%dT%d", a, t))
    }
  }
  opciones
}

swap_choices <- function(x) {
  v <- setNames(names(x), unname(x))
  v[order(names(v))]
}

# -----------------------------------------------------------------------------
# AGREGACIÓN "LIGERA": estas 3 funciones sustituyen a las que antes calculaban
# sobre microdatos por persona. Ahora reciben `grano` (o un subconjunto de
# `grano`) ya agregado por (TRIMESTRE, PROVINCIA, RAMA) y solo hacen sumas
# adicionales — nada de leer ficheros ni descargar nada. Mantienen el MISMO
# nombre y la MISMA forma de resultado que las funciones originales, así que
# todo el resto de la app (KPIs, gráficos, informe PDF) no necesita tocarse.
# -----------------------------------------------------------------------------

# Serie trimestral agregada (personas, horas, asalariados) por periodo, a
# partir de filas de `grano` ya filtradas por territorio (o sin filtrar,
# para "España").
resumen_trimestral <- function(dfx) {
  dfx |>
    group_by(TRIMESTRE_LABEL, TRIMESTRE_ANIO, TRIMESTRE_NUM) |>
    summarise(
      personas_pag = sum(personas_pag, na.rm = TRUE),
      personas_npg = sum(personas_npg, na.rm = TRUE),
      personas_tot = sum(personas_tot, na.rm = TRUE),
      horas_pag = sum(horas_pag, na.rm = TRUE),
      horas_npg = sum(horas_npg, na.rm = TRUE),
      horas_tot = sum(horas_tot, na.rm = TRUE),
      asalariados = sum(asalariados, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(TRIMESTRE_ANIO, TRIMESTRE_NUM) |>
    mutate(idx = TRIMESTRE_ANIO * 4 + TRIMESTRE_NUM) |>
    mutate(
      pct_npg_sobre_tot   = if_else(horas_tot > 0, 100 * horas_npg / horas_tot, NA_real_),
      pct_asalariados_pag = if_else(asalariados > 0, 100 * personas_pag / asalariados, NA_real_),
      pct_asalariados_npg = if_else(asalariados > 0, 100 * personas_npg / asalariados, NA_real_),
      pct_asalariados_tot = if_else(asalariados > 0, 100 * personas_tot / asalariados, NA_real_),
      intensidad_pag = if_else(personas_pag > 0, horas_pag / personas_pag, NA_real_),
      intensidad_npg = if_else(personas_npg > 0, horas_npg / personas_npg, NA_real_),
      intensidad_tot = if_else(personas_tot > 0, horas_tot / personas_tot, NA_real_),
      empleos_eq_pag = horas_pag / JORNADA_SEMANAL_HORAS,
      empleos_eq_npg = horas_npg / JORNADA_SEMANAL_HORAS,
      empleos_eq_tot = horas_tot / JORNADA_SEMANAL_HORAS
    )
}

# Media móvil de 4 trimestres (idéntica a la versión original: opera sobre
# la tabla YA agregada por periodo, no cambia nada aquí).
anadir_ma4 <- function(tabla) {
  roll4 <- function(x) zoo::rollapplyr(x, width = 4, FUN = mean, fill = NA_real_, partial = FALSE)
  tabla |>
    arrange(idx) |>
    mutate(
      personas_pag_ma4 = roll4(personas_pag),
      personas_npg_ma4 = roll4(personas_npg),
      personas_tot_ma4 = roll4(personas_tot),
      horas_pag_ma4 = roll4(horas_pag),
      horas_npg_ma4 = roll4(horas_npg),
      horas_tot_ma4 = roll4(horas_tot),
      asalariados_ma4 = roll4(asalariados)
    ) |>
    mutate(
      pct_npg_sobre_tot_ma4   = if_else(horas_tot_ma4 > 0, 100 * horas_npg_ma4 / horas_tot_ma4, NA_real_),
      pct_asalariados_pag_ma4 = if_else(asalariados_ma4 > 0, 100 * personas_pag_ma4 / asalariados_ma4, NA_real_),
      pct_asalariados_npg_ma4 = if_else(asalariados_ma4 > 0, 100 * personas_npg_ma4 / asalariados_ma4, NA_real_),
      pct_asalariados_tot_ma4 = if_else(asalariados_ma4 > 0, 100 * personas_tot_ma4 / asalariados_ma4, NA_real_),
      intensidad_pag_ma4 = if_else(personas_pag_ma4 > 0, horas_pag_ma4 / personas_pag_ma4, NA_real_),
      intensidad_npg_ma4 = if_else(personas_npg_ma4 > 0, horas_npg_ma4 / personas_npg_ma4, NA_real_),
      intensidad_tot_ma4 = if_else(personas_tot_ma4 > 0, horas_tot_ma4 / personas_tot_ma4, NA_real_),
      empleos_eq_pag_ma4 = horas_pag_ma4 / JORNADA_SEMANAL_HORAS,
      empleos_eq_npg_ma4 = horas_npg_ma4 / JORNADA_SEMANAL_HORAS,
      empleos_eq_tot_ma4 = horas_tot_ma4 / JORNADA_SEMANAL_HORAS
    )
}

# Snapshot por grupo de actividad para los últimos `ultimos_n` periodos,
# a partir de filas de `grano` (ya filtradas por territorio). Misma lógica
# de "no mezclar CNAE-2009 con CNAE-2025" que la versión original.
resumen_por_rama <- function(dfx, ultimos_n = 1, tipo = "npg") {
  if (nrow(dfx) == 0 || all(is.na(dfx$RAMA_COD))) return(tibble())

  personas_col <- paste0("personas_", tipo)
  horas_col    <- paste0("horas_", tipo)

  periodos <- dfx |>
    distinct(TRIMESTRE_ANIO, TRIMESTRE_NUM, CLASIF_RAMA) |>
    mutate(idx = TRIMESTRE_ANIO * 4 + TRIMESTRE_NUM) |>
    arrange(desc(idx)) |>
    slice_head(n = ultimos_n)

  clasif_reciente <- periodos$CLASIF_RAMA[1]
  n_descartados <- sum(periodos$CLASIF_RAMA != clasif_reciente)
  periodos <- periodos |> filter(CLASIF_RAMA == clasif_reciente)
  n_usados <- nrow(periodos)

  resultado <- dfx |>
    semi_join(periodos, by = c("TRIMESTRE_ANIO", "TRIMESTRE_NUM")) |>
    filter(!is.na(RAMA_COD), RAMA_COD != "", RAMA_COD != "NA") |>
    group_by(RAMA_COD) |>
    summarise(
      personas    = sum(.data[[personas_col]], na.rm = TRUE) / n_usados,
      horas       = sum(.data[[horas_col]], na.rm = TRUE) / n_usados,
      asalariados = sum(asalariados, na.rm = TRUE) / n_usados,
      .groups = "drop"
    ) |>
    mutate(
      pct_asalariados = if_else(asalariados > 0, 100 * personas / asalariados, NA_real_),
      intensidad      = if_else(personas > 0, horas / personas, NA_real_),
      Rama = etiqueta_rama(RAMA_COD, clasif_reciente)
    ) |>
    arrange(desc(horas))

  attr(resultado, "clasif_usada") <- clasif_reciente
  attr(resultado, "trimestres_descartados") <- n_descartados
  resultado
}

# Agregación por territorio (CCAA o provincia) del ÚLTIMO periodo disponible
# (trimestral o MA4), para TODA España a la vez -> alimenta la pestaña "Mapas".
# `df` aquí es un subconjunto de `grano` (sin filtrar por territorio, para
# que salgan todas las CCAA/provincias), ya recortado a la ventana de
# trimestres visible.
resumen_mapa <- function(df, nivel_col, criterio) {
  if (nrow(df) == 0) return(tibble())

  base <- df |>
    group_by(.data[[nivel_col]], TRIMESTRE_ANIO, TRIMESTRE_NUM) |>
    summarise(
      personas_pag = sum(personas_pag, na.rm = TRUE),
      personas_npg = sum(personas_npg, na.rm = TRUE),
      personas_tot = sum(personas_tot, na.rm = TRUE),
      horas_pag = sum(horas_pag, na.rm = TRUE),
      horas_npg = sum(horas_npg, na.rm = TRUE),
      horas_tot = sum(horas_tot, na.rm = TRUE),
      asalariados = sum(asalariados, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(idx = TRIMESTRE_ANIO * 4 + TRIMESTRE_NUM) |>
    arrange(.data[[nivel_col]], idx)

  if (criterio == "ma4") {
    roll4 <- function(x) zoo::rollapplyr(x, width = 4, FUN = mean, fill = NA_real_, partial = FALSE)
    base <- base |>
      group_by(.data[[nivel_col]]) |>
      mutate(
        personas_pag = roll4(personas_pag), personas_npg = roll4(personas_npg), personas_tot = roll4(personas_tot),
        horas_pag = roll4(horas_pag), horas_npg = roll4(horas_npg), horas_tot = roll4(horas_tot),
        asalariados = roll4(asalariados)
      ) |>
      ungroup()
  }

  base |>
    filter(!is.na(horas_tot)) |>
    group_by(.data[[nivel_col]]) |>
    slice_max(order_by = idx, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(
      pct_asalariados_pag = if_else(asalariados > 0, 100 * personas_pag / asalariados, NA_real_),
      pct_asalariados_npg = if_else(asalariados > 0, 100 * personas_npg / asalariados, NA_real_),
      pct_asalariados_tot = if_else(asalariados > 0, 100 * personas_tot / asalariados, NA_real_),
      intensidad_pag = if_else(personas_pag > 0, horas_pag / personas_pag, NA_real_),
      intensidad_npg = if_else(personas_npg > 0, horas_npg / personas_npg, NA_real_),
      intensidad_tot = if_else(personas_tot > 0, horas_tot / personas_tot, NA_real_),
      empleos_eq_pag = horas_pag / JORNADA_SEMANAL_HORAS,
      empleos_eq_npg = horas_npg / JORNADA_SEMANAL_HORAS,
      empleos_eq_tot = horas_tot / JORNADA_SEMANAL_HORAS,
      pct_npg_sobre_tot = if_else(horas_tot > 0, 100 * horas_npg / horas_tot, NA_real_)
    ) |>
    rename(cod = all_of(nivel_col))
}

# -----------------------------------------------------------------------------
# FORMATO
# -----------------------------------------------------------------------------

fmt_es_int <- function(x) format(round(x, 0), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
fmt_es_dec <- function(x, n = 1) format(round(x, n), big.mark = ".", decimal.mark = ",", nsmall = n, scientific = FALSE, trim = TRUE)
fmt_es_pct <- function(x, n = 1) paste0(fmt_es_dec(x, n), "%")

territorio_label_str <- function(ambito, zona) {
  if (ambito == "ccaa" && !is.null(zona)) sprintf("Comunidad Autónoma: %s", ccaa_labels[[zona]])
  else if (ambito == "prov" && !is.null(zona)) sprintf("Provincia: %s", prov_labels[[zona]])
  else "España"
}

tipo_label <- function(tipo) switch(tipo, pag = "pagadas", npg = "no pagadas", tot = "totales", tipo)
tipo_label_cap <- function(tipo) switch(tipo, pag = "Pagadas", npg = "No pagadas", tot = "Totales", tipo)
tipo_color <- function(tipo) switch(tipo, pag = CCOO_ROJO, npg = "#222222", tot = "#7a7a7a", CCOO_ROJO)

col_tipo <- function(tabla, base, tipo) tabla[[paste0(base, "_", tipo)]]

info_metrica_mapa <- function(metrica, nivel_txt) {
  switch(metrica,
         pct_asalariados   = list(etiqueta = "% asalariados", fmt = fmt_es_pct,
                                  titulo = sprintf("%% de asalariados con horas extra no pagadas, por %s", nivel_txt)),
         intensidad        = list(etiqueta = "Horas / persona", fmt = fmt_es_dec,
                                  titulo = sprintf("Horas extra no pagadas por persona, por %s", nivel_txt)),
         pct_npg_sobre_tot = list(etiqueta = "% s/ total horas extra", fmt = fmt_es_pct,
                                  titulo = sprintf("%% de horas extra no pagadas sobre el total, por %s", nivel_txt))
  )
}

variacion_subindicador <- function(valor_actual, valor_comparado, etiqueta) {
  if (is.null(valor_comparado) || length(valor_comparado) == 0 || is.na(valor_comparado)) {
    return(tags$div(class = "he-subvar",
                    tags$div(class = "he-subvar-titulo", etiqueta),
                    tags$div(class = "he-subvar-valor", "\u2014")))
  }
  delta_abs <- valor_actual - valor_comparado
  sube <- delta_abs >= 0
  signo <- if (sube) "+" else ""
  texto_pct <- if (valor_comparado != 0) {
    sprintf(" (%s%s)", signo, fmt_es_pct(100 * (valor_actual / valor_comparado - 1)))
  } else ""
  tags$div(class = "he-subvar",
           tags$div(class = "he-subvar-titulo", etiqueta),
           tags$div(class = "he-subvar-valor",
                    icon(if (sube) "arrow-up" else "arrow-down", class = if (sube) "he-subvar-sube" else "he-subvar-baja"),
                    sprintf("%s%s%s", signo, fmt_es_int(delta_abs), texto_pct))
  )
}

variacion_absoluta <- function(valor_actual, valor_comparado, etiqueta, sufijo) {
  if (is.null(valor_comparado) || length(valor_comparado) == 0 || is.na(valor_comparado)) return(NULL)
  delta <- valor_actual - valor_comparado
  signo <- if (delta >= 0) "+" else ""
  tags$div(class = "he-kpi-note", sprintf("%s: %s%s%s", etiqueta, signo, fmt_es_dec(delta), sufijo))
}

# =============================================================================
# GENERADOR DE INFORME PDF (sin cambios respecto al original: opera sobre
# tablas ya resumidas -serie/rama/ranking-, que siguen teniendo la misma
# forma que antes)
# =============================================================================

cargar_logo_pdf <- function() {
  ruta_logo <- file.path("www", "logo_ccoo.jpg")
  if (JPEG_DISPONIBLE && file.exists(ruta_logo)) {
    tryCatch(jpeg::readJPEG(ruta_logo), error = function(e) NULL)
  } else {
    NULL
  }
}

dibujar_cabecera_pdf <- function(logo, titulo_pagina) {
  if (!is.null(logo)) {
    grid.raster(logo, x = unit(0.5, "cm"), y = unit(1, "npc") - unit(0.3, "cm"),
                height = unit(1.4, "cm"), just = c("left", "top"))
  }
  grid.text(titulo_pagina, x = unit(1, "npc") - unit(0.5, "cm"), y = unit(1, "npc") - unit(0.95, "cm"),
            just = "right", gp = gpar(fontsize = 12, fontface = "bold", col = CCOO_NEGRO))
  grid.lines(x = unit(c(0.5, 1), c("cm", "npc")), y = unit(1, "npc") - unit(2, "cm"),
             gp = gpar(col = "#dddddd", lwd = 1))
}

pagina_con_cabecera <- function(logo, titulo_pagina, dibujar_contenido) {
  grid.newpage()
  dibujar_cabecera_pdf(logo, titulo_pagina)
  pushViewport(viewport(y = 0, height = unit(1, "npc") - unit(2.3, "cm"), just = "bottom"))
  dibujar_contenido()
  popViewport()
}

tema_informe_pdf <- theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 12, margin = margin(b = 8)),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.margin = margin(t = 10, r = 24, b = 8, l = 8)
  )

tema_rama_pdf <- theme_minimal(base_size = 9) +
  theme(
    plot.title = element_text(face = "bold", size = 11, margin = margin(b = 6)),
    plot.margin = margin(t = 8, r = 30, b = 8, l = 8)
  )

pagina_titulo_kpis <- function(logo, territorio, periodo, criterio, ventana, kpis, fecha_generacion) {
  pagina_con_cabecera(logo, "Informe de resultados", function() {
    grid.text("Observatorio de horas extra según la EPA", y = unit(0.9, "npc"),
              gp = gpar(fontsize = 22, fontface = "bold", col = CCOO_NEGRO))

    subt <- sprintf("Ámbito: %s   ·   Periodo: %s   ·   Criterio: %s   ·   Ventana: %s",
                    territorio, periodo, criterio, ventana)
    grid.text(subt, y = unit(0.81, "npc"), gp = gpar(fontsize = 12, col = "#333333"))

    grid.text(sprintf("Generado el %s. Fuente: microdatos EPA (INE).",
                      format(fecha_generacion, "%d/%m/%Y a las %H:%M")),
              y = unit(0.75, "npc"), gp = gpar(fontsize = 9.5, col = "grey45"))

    grid.text("Indicadores clave (horas extra no pagadas)",
              x = unit(0.06, "npc"), y = unit(0.62, "npc"), just = "left",
              gp = gpar(fontsize = 15, fontface = "bold"))

    lineas <- c(
      sprintf("Personas asalariadas con horas extra no pagadas / semana:   %s", fmt_es_int(kpis$personas_npg)),
      sprintf("Horas extra no pagadas / semana:   %s", fmt_es_int(kpis$horas_npg)),
      sprintf("Empleos equivalentes a jornada completa:   %s  (asumiendo %dh/semana)",
              fmt_es_int(kpis$empleos_eq_npg), kpis$jornada),
      sprintf("%% de horas extra no pagadas sobre el total:   %s", fmt_es_pct(kpis$pct_npg_sobre_tot)),
      sprintf("%% población asalariada que hace horas extra no pagadas:   %s", fmt_es_pct(kpis$pct_asalariados_npg)),
      sprintf("Horas extra no pagadas por persona afectada:   %s h/semana", fmt_es_dec(kpis$intensidad_npg))
    )

    grid.text(paste(paste0("\u2022  ", lineas), collapse = "\n"),
              x = unit(0.08, "npc"), y = unit(0.46, "npc"), just = c("left", "top"),
              gp = gpar(fontsize = 13, lineheight = 2.2, col = "#1a1a1a"))
  })
}

pagina_aviso <- function(logo, titulo_cabecera, titulo, texto) {
  pagina_con_cabecera(logo, titulo_cabecera, function() {
    grid.text(titulo, y = unit(0.72, "npc"), gp = gpar(fontsize = 16, fontface = "bold"))
    grid.text(texto, y = unit(0.5, "npc"), x = unit(0.5, "npc"),
              gp = gpar(fontsize = 12, col = "grey30"))
  })
}

grafico_evol_horas_pdf <- function(serie) {
  niveles <- unique(serie$TRIMESTRE_LABEL)
  d <- serie |>
    dplyr::select(TRIMESTRE_LABEL, horas_pag, horas_npg) |>
    tidyr::pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "Horas") |>
    dplyr::mutate(
      Tipo = dplyr::recode(Tipo, horas_pag = "Pagadas", horas_npg = "No pagadas"),
      Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
      TRIMESTRE_LABEL = factor(TRIMESTRE_LABEL, levels = niveles)
    )
  d_lab <- d |> dplyr::filter(TRIMESTRE_LABEL == tail(niveles, 1))

  ggplot(d, aes(TRIMESTRE_LABEL, Horas, color = Tipo, group = Tipo)) +
    geom_line(linewidth = 1) + geom_point(size = 1.6) +
    geom_text(data = d_lab, aes(label = fmt_es_int(Horas)), hjust = -0.2, size = 3.3,
              fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO)) +
    scale_x_discrete(expand = expansion(add = c(0.6, 2.4))) +
    scale_y_continuous(labels = fmt_es_int) +
    labs(x = NULL, y = "Horas / semana", title = str_wrap("Horas extra trabajadas semanalmente", 45)) +
    tema_informe_pdf
}

grafico_evol_personas_pdf <- function(serie) {
  niveles <- unique(serie$TRIMESTRE_LABEL)
  d <- serie |>
    dplyr::select(TRIMESTRE_LABEL, personas_pag, personas_npg) |>
    tidyr::pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "Personas") |>
    dplyr::mutate(
      Tipo = dplyr::recode(Tipo, personas_pag = "Pagadas", personas_npg = "No pagadas"),
      Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
      TRIMESTRE_LABEL = factor(TRIMESTRE_LABEL, levels = niveles)
    )
  d_lab <- d |> dplyr::filter(TRIMESTRE_LABEL == tail(niveles, 1))

  ggplot(d, aes(TRIMESTRE_LABEL, Personas, color = Tipo, group = Tipo)) +
    geom_line(linewidth = 1) + geom_point(size = 1.6) +
    geom_text(data = d_lab, aes(label = fmt_es_int(Personas)), hjust = -0.2, size = 3.3,
              fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO)) +
    scale_x_discrete(expand = expansion(add = c(0.6, 2.4))) +
    scale_y_continuous(labels = fmt_es_int) +
    labs(x = NULL, y = "Personas / semana", title = str_wrap("Personas asalariadas que hicieron horas extra", 45)) +
    tema_informe_pdf
}

pagina_evolucion <- function(logo, serie) {
  pagina_con_cabecera(logo, "Evolución temporal", function() {
    grid.arrange(grafico_evol_horas_pdf(serie), grafico_evol_personas_pdf(serie), ncol = 1, newpage = FALSE)
  })
}

grafico_pct_npg_pdf <- function(serie) {
  niveles <- unique(serie$TRIMESTRE_LABEL)
  d <- serie |> dplyr::mutate(TRIMESTRE_LABEL = factor(TRIMESTRE_LABEL, levels = niveles))
  d_lab <- d |> dplyr::filter(TRIMESTRE_LABEL == tail(niveles, 1))

  ggplot(d, aes(TRIMESTRE_LABEL, pct_npg_sobre_tot, group = 1)) +
    geom_line(linewidth = 1, color = CCOO_ROJO) + geom_point(size = 1.8, color = CCOO_ROJO) +
    geom_text(data = d_lab, aes(label = fmt_es_pct(pct_npg_sobre_tot)), hjust = -0.2, size = 3.3,
              fontface = "bold", color = CCOO_ROJO) +
    scale_x_discrete(expand = expansion(add = c(0.6, 2.4))) +
    scale_y_continuous(labels = fmt_es_dec) +
    labs(x = NULL, y = "%", title = str_wrap("% horas extra NO pagadas s/ total", 45)) +
    tema_informe_pdf
}

grafico_pct_asalariados_pdf <- function(serie) {
  niveles <- unique(serie$TRIMESTRE_LABEL)
  d <- serie |>
    dplyr::select(TRIMESTRE_LABEL, pct_asalariados_pag, pct_asalariados_npg) |>
    tidyr::pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "pct") |>
    dplyr::mutate(
      Tipo = dplyr::recode(Tipo, pct_asalariados_pag = "Pagadas", pct_asalariados_npg = "No pagadas"),
      Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
      TRIMESTRE_LABEL = factor(TRIMESTRE_LABEL, levels = niveles)
    )
  d_lab <- d |> dplyr::filter(TRIMESTRE_LABEL == tail(niveles, 1))

  ggplot(d, aes(TRIMESTRE_LABEL, pct, color = Tipo, group = Tipo)) +
    geom_line(linewidth = 1) + geom_point(size = 1.6) +
    geom_text(data = d_lab, aes(label = fmt_es_pct(pct)), hjust = -0.2, size = 3.3,
              fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO)) +
    scale_x_discrete(expand = expansion(add = c(0.6, 2.4))) +
    scale_y_continuous(labels = fmt_es_dec) +
    labs(x = NULL, y = "%", title = str_wrap("% asalariados que hacen horas extra", 45)) +
    tema_informe_pdf
}

grafico_intensidad_pdf <- function(serie) {
  niveles <- unique(serie$TRIMESTRE_LABEL)
  d <- serie |>
    dplyr::select(TRIMESTRE_LABEL, intensidad_pag, intensidad_npg) |>
    tidyr::pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "Horas") |>
    dplyr::mutate(
      Tipo = dplyr::recode(Tipo, intensidad_pag = "Pagadas", intensidad_npg = "No pagadas"),
      Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
      TRIMESTRE_LABEL = factor(TRIMESTRE_LABEL, levels = niveles)
    )
  d_lab <- d |> dplyr::filter(TRIMESTRE_LABEL == tail(niveles, 1))

  ggplot(d, aes(TRIMESTRE_LABEL, Horas, color = Tipo, group = Tipo)) +
    geom_line(linewidth = 1) + geom_point(size = 1.6) +
    geom_text(data = d_lab, aes(label = fmt_es_dec(Horas)), hjust = -0.2, size = 3.3,
              fontface = "bold", show.legend = FALSE) +
    scale_color_manual(values = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO)) +
    scale_x_discrete(expand = expansion(add = c(0.6, 2.4))) +
    scale_y_continuous(labels = fmt_es_dec) +
    labs(x = NULL, y = "Horas/persona", title = str_wrap("Intensidad media por persona afectada", 45)) +
    tema_informe_pdf
}

grafico_empleos_eq_pdf <- function(serie) {
  niveles <- unique(serie$TRIMESTRE_LABEL)
  d <- serie |>
    dplyr::select(TRIMESTRE_LABEL, empleos_eq_pag, empleos_eq_npg) |>
    tidyr::pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "Empleos") |>
    dplyr::mutate(
      Tipo = dplyr::recode(Tipo, empleos_eq_pag = "Pagadas", empleos_eq_npg = "No pagadas"),
      Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
      TRIMESTRE_LABEL = factor(TRIMESTRE_LABEL, levels = niveles)
    )
  d_lab <- d |> dplyr::filter(TRIMESTRE_LABEL == tail(niveles, 1))

  ggplot(d, aes(TRIMESTRE_LABEL, Empleos, fill = Tipo)) +
    geom_col() +
    geom_text(data = d_lab, aes(label = fmt_es_int(Empleos)), position = position_stack(vjust = 0.5),
              size = 3, color = "white", fontface = "bold") +
    scale_fill_manual(values = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO)) +
    scale_x_discrete(expand = expansion(add = c(0.6, 1.4))) +
    scale_y_continuous(labels = fmt_es_int) +
    labs(x = NULL, y = "Empleos equivalentes", title = str_wrap("Empleos equivalentes a jornada completa", 45)) +
    tema_informe_pdf
}

pagina_indicadores_1 <- function(logo, serie) {
  pagina_con_cabecera(logo, "Indicadores (1/2)", function() {
    grid.arrange(grafico_pct_npg_pdf(serie), grafico_pct_asalariados_pdf(serie), ncol = 1, newpage = FALSE)
  })
}

pagina_indicadores_2 <- function(logo, serie) {
  pagina_con_cabecera(logo, "Indicadores (2/2)", function() {
    grid.arrange(grafico_intensidad_pdf(serie), grafico_empleos_eq_pdf(serie), ncol = 1, newpage = FALSE)
  })
}

grafico_rama_pdf <- function(rama, columna, titulo, xlab, color, fmt_fun) {
  d <- rama |>
    dplyr::arrange(.data[[columna]]) |>
    dplyr::mutate(Rama = factor(Rama, levels = Rama))
  ggplot(d, aes(Rama, .data[[columna]])) +
    geom_col(fill = color) +
    geom_text(aes(label = fmt_fun(.data[[columna]])), hjust = -0.15, size = 3, color = "#333333") +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = fmt_fun) +
    labs(x = NULL, y = xlab, title = str_wrap(titulo, 45)) +
    tema_rama_pdf
}

pagina_rama <- function(logo, rama) {
  if (is.null(rama) || !is.data.frame(rama) || nrow(rama) == 0) {
    pagina_aviso(logo, "Sectores de actividad", "Sectores de actividad",
                 "No hay desglose por grupo de actividad disponible para esta selección.")
    return(invisible(NULL))
  }
  pagina_con_cabecera(logo, "Sectores de actividad (horas extra no pagadas)", function() {
    grid.arrange(
      grafico_rama_pdf(rama, "horas", "Horas extra NO pagadas por grupo de actividad", "Horas/semana", CCOO_ROJO, fmt_es_int),
      grafico_rama_pdf(rama, "personas", "Personas asalariadas con horas extra NO pagadas", "Personas/semana", CCOO_ROJO, fmt_es_int),
      grafico_rama_pdf(rama, "pct_asalariados", "% personas asalariadas del sector con horas extra no pagadas", "%", CCOO_NEGRO, fmt_es_pct),
      grafico_rama_pdf(rama, "intensidad", "Horas extra no pagadas por persona asalariada", "Horas/persona", CCOO_NEGRO, fmt_es_dec),
      ncol = 2, newpage = FALSE
    )
  })
}

grafico_ranking_pdf <- function(datos, columna, titulo, xlab, color, fmt_fun) {
  d <- datos |>
    dplyr::arrange(.data[[columna]]) |>
    dplyr::mutate(nombre = factor(nombre, levels = nombre))
  ggplot(d, aes(nombre, .data[[columna]])) +
    geom_col(fill = color) +
    geom_text(aes(label = fmt_fun(.data[[columna]])), hjust = -0.15, size = 3, color = "#333333") +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2)), labels = fmt_fun) +
    labs(x = NULL, y = xlab, title = str_wrap(titulo, 60)) +
    tema_rama_pdf
}

pagina_ranking_metrica <- function(logo, datos, columna, titulo_pagina, titulo_grafico, xlab, color, fmt_fun) {
  if (is.null(datos) || nrow(datos) == 0) {
    pagina_aviso(logo, titulo_pagina, titulo_pagina, "No hay datos de ranking disponibles.")
    return(invisible(NULL))
  }
  pagina_con_cabecera(logo, titulo_pagina, function() {
    print(grafico_ranking_pdf(datos, columna, titulo_grafico, xlab, color, fmt_fun), newpage = FALSE)
  })
}

pagina_mapa_metrica <- function(logo, mapa_geo, mapa_disponible, mapa_titulo, mapa_etiqueta, fmt_fun) {
  if (!isTRUE(mapa_disponible) || is.null(mapa_geo)) {
    pagina_aviso(logo, "Comparación territorial", "Mapa no disponible",
                 "Mapa no disponible en esta generación del informe.")
    return(invisible(NULL))
  }
  pagina_con_cabecera(logo, "Comparación territorial", function() {
    pts <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(mapa_geo)))
    coords <- sf::st_coordinates(pts)
    mapa_geo$lbl_x <- coords[, 1]
    mapa_geo$lbl_y <- coords[, 2]
    mapa_geo$lbl_txt <- ifelse(is.na(mapa_geo$valor), "", fmt_fun(mapa_geo$valor))

    p <- ggplot(mapa_geo) +
      geom_sf(aes(fill = valor), color = "#555555", linewidth = 0.2) +
      geom_text(aes(x = lbl_x, y = lbl_y, label = lbl_txt), size = 2.6,
                color = "#1a1a1a", fontface = "bold") +
      scale_fill_gradient(low = "#fde5e3", high = CCOO_ROJO, na.value = "#e0e0e0", name = mapa_etiqueta) +
      labs(title = str_wrap(mapa_titulo, 75)) +
      theme_minimal(base_size = 12) +
      theme(axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank(),
            plot.title = element_text(face = "bold", size = 14),
            plot.margin = margin(t = 10, r = 20, b = 10, l = 20))
    print(p, newpage = FALSE)
  })
}

generar_informe_pdf <- function(file, territorio, periodo, criterio, ventana,
                                kpis, serie, rama, ranking_personas, ranking_horas,
                                nivel_mapa_label, mapas_lista, fecha_generacion) {

  logo <- cargar_logo_pdf()

  resultado <- tryCatch({
    grDevices::pdf(file, width = 11.69, height = 8.27, onefile = TRUE,
                   title = "Observatorio de horas extra según la EPA")
    on.exit(grDevices::dev.off(), add = TRUE)

    pagina_titulo_kpis(logo, territorio, periodo, criterio, ventana, kpis, fecha_generacion)

    if (!is.null(serie) && nrow(serie) > 0) {
      pagina_evolucion(logo, serie)
      pagina_indicadores_1(logo, serie)
      pagina_indicadores_2(logo, serie)
    } else {
      pagina_aviso(logo, "Evolución temporal", "Evolución temporal",
                   "No hay datos para el ámbito/ventana seleccionados.")
    }

    pagina_rama(logo, rama)

    pagina_ranking_metrica(logo, ranking_personas, "personas_npg",
                           sprintf("Comparación territorial (%s)", nivel_mapa_label),
                           sprintf("Personas con horas NO pagadas, por %s", nivel_mapa_label),
                           "Personas/semana", CCOO_ROJO, fmt_es_int)
    pagina_ranking_metrica(logo, ranking_horas, "horas_npg",
                           sprintf("Comparación territorial (%s)", nivel_mapa_label),
                           sprintf("Horas NO pagadas / semana, por %s", nivel_mapa_label),
                           "Horas/semana", CCOO_NEGRO, fmt_es_int)

    for (m in mapas_lista) {
      pagina_mapa_metrica(logo, m$geo, m$disponible, m$titulo, m$etiqueta, m$fmt)
    }

    TRUE
  }, error = function(e) {
    if (!is.null(grDevices::dev.list())) grDevices::dev.off()
    grDevices::pdf(file, width = 11.69, height = 8.27)
    plot.new()
    text(0.5, 0.5, paste("No se pudo generar el informe:", conditionMessage(e)), cex = 1)
    grDevices::dev.off()
    FALSE
  })

  invisible(resultado)
}

# -----------------------------------------------------------------------------
# CSS
# -----------------------------------------------------------------------------

he_css <- "
.he-kpi-note { color: inherit; opacity: 0.82; font-size: 12.5px; margin-top: -6px; }
.he-aviso { background-color: #fff3cd; border: 1px solid #ffe08a; border-radius: 6px; padding: 8px 12px; margin-bottom: 10px; font-size: 13px; }
.sidebar hr { border-top: 1px solid rgba(255,255,255,0.25); }
.sidebar .form-group label, .sidebar label { color: #f2f2f2; font-weight: 500; }
.he-titulo-app { display:flex; align-items:center; gap:16px; padding: 4px 0; }
.he-titulo-app img { background:#ffffff; padding:6px 10px; border-radius:4px; }
.he-titulo-app span { color:#ffffff; font-weight:600; font-size:22px; }
.he-fuente { color: rgba(255,255,255,0.6); font-size: 11.5px; margin-top: 14px; }

.he-subvar-row { display: flex; gap: 14px; margin-top: 4px; }
.he-subvar { flex: 1; }
.he-subvar-titulo { font-size: 11px; opacity: 0.82; }
.he-subvar-valor { font-size: 13.5px; font-weight: 600; }
.he-subvar-valor svg { margin-right: 3px; }

.nav-tabs .nav-link { color: #1a1a1a; font-weight: 600; border-color: transparent; }
.nav-tabs .nav-link:hover { color: #E2231C; border-color: transparent; }
.nav-tabs .nav-link.active, .nav-tabs .nav-link.active:hover {
  color: #ffffff !important;
  background-color: #E2231C !important;
  border-color: #E2231C #E2231C #E2231C !important;
}

.bslib-sidebar-layout .collapse-toggle {
  width: 52px !important;
  height: 52px !important;
  border-radius: 50% !important;
  background: #E2231C !important;
  color: white !important;
  box-shadow: 0 3px 10px rgba(0,0,0,.35);
}

.bslib-sidebar-layout .collapse-toggle svg {
  width: 24px !important;
  height: 24px !important;
}

@media (max-width: 768px) {

  .bslib-sidebar-layout .collapse-toggle {
    width: auto !important;
    padding: 0 16px !important;
  }

  .bslib-sidebar-layout .collapse-toggle::after {
    content: \" Menú\";
    font-size: 15px;
    font-weight: 600;
  }
}

"

# -----------------------------------------------------------------------------
# CARGA DEL RESUMEN PRECALCULADO (una sola vez, al arrancar la app - NO en
# cada sesión de usuario). Esto es lo que reemplaza a la descarga de
# microdatos: aquí solo se trae un fichero .rds pequeño desde GitHub.
# -----------------------------------------------------------------------------

cargar_resumen_remoto <- function() {
  tmp <- tempfile(fileext = ".rds")
  headers <- if (nzchar(GITHUB_TOKEN_RESUMEN)) {
    httr::add_headers(Authorization = paste("token", GITHUB_TOKEN_RESUMEN))
  } else {
    httr::add_headers()
  }
  resp <- tryCatch(
    httr::GET(URL_RESUMEN, headers, httr::write_disk(tmp, overwrite = TRUE), httr::timeout(30)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200) {
    stop(sprintf(
      "No se pudo descargar el resumen desde %s (¿URL correcta? ¿repo privado sin token?).",
      URL_RESUMEN
    ))
  }
  readRDS(tmp)
}

resumen_global_inicial <- cargar_resumen_remoto()

ultimo_trimestre   <- parse_trimestre_label(resumen_global_inicial$ultimo_trimestre)
etiqueta_ultimo     <- resumen_global_inicial$ultimo_trimestre
opciones_trimestre  <- generar_opciones_trimestre(ultimo_trimestre)

opciones_ventana <- c(
  "Últimos 3 años"  = "3",
  "Últimos 5 años"  = "5",
  "Últimos 10 años" = "10"
)
opciones_ventana <- c(
  opciones_ventana,
  setNames("completa", sprintf("Serie completa (desde %d)", ANIO_MIN))
)

titulo_dashboard <- tags$div(
  class = "he-titulo-app",
  tags$img(src = "logo_ccoo.jpg", height = "64px"),
  tags$span("Observatorio de horas extra según la EPA")
)

# -----------------------------------------------------------------------------
# UI (sin cambios respecto al original)
# -----------------------------------------------------------------------------

ui <- page_sidebar(
  title = titulo_dashboard,
  theme = bs_theme(bootswatch = "flatly", primary = CCOO_ROJO, "navbar-bg" = CCOO_ROJO),
  tags$head(tags$style(HTML(he_css))),
  sidebar = sidebar(
    width = 300, bg = CCOO_NEGRO, fg = "white",
    open = list(desktop = "open", mobile = "closed"), 
    selectInput("trimestre_fin", "Periodo final", choices = opciones_trimestre, selected = etiqueta_ultimo),
    selectInput("ventana_he", "Ventana temporal", choices = opciones_ventana, selected = "3"),
    radioButtons("criterio_temporal", "Criterio",
                 choices = c("Trimestral" = "trim", "Media móvil 4 trimestres" = "ma4"),
                 selected = "trim"),
    actionButton("recargar_he", "Recargar datos", icon = icon("rotate"), class = "btn-primary btn-sm"),
    hr(),
    selectInput("ambito_he", "Ámbito territorial",
                choices = c("España" = "esp", "Comunidad Autónoma" = "ccaa", "Provincia" = "prov")),
    uiOutput("selector_zona_he"),
    hr(),
    downloadButton("descargar_informe", "Informe PDF", icon = icon("file-pdf"), class = "btn-primary btn-sm"),
    uiOutput("aviso_informe"),
    hr(),
    uiOutput("estado_carga_he"),
    tags$div(class = "he-fuente", "Fuente: Microdatos de la EPA (INE).")
  ),
  navset_tab(
    nav_panel(
      "Resumen",
      br(),
      uiOutput("kpis_he"),
      br(),
      withSpinner(plotlyOutput("plot_horas_he", height = 380)),
      br(),
      withSpinner(plotlyOutput("plot_personas_he", height = 380))
    ),
    nav_panel(
      "Indicadores",
      br(),
      fluidRow(
        column(6, withSpinner(plotlyOutput("plot_pct_npg", height = 340))),
        column(6, withSpinner(plotlyOutput("plot_pct_asalariados", height = 340)))
      ),
      br(),
      fluidRow(
        column(6, withSpinner(plotlyOutput("plot_intensidad", height = 340))),
        column(6, withSpinner(plotlyOutput("plot_empleos_eq", height = 340)))
      )
    ),
    nav_panel(
      "Sectores de actividad",
      br(),
      uiOutput("aviso_rama"),
      fluidRow(
        column(6, withSpinner(plotlyOutput("plot_rama_horas", height = 560))),
        column(6, withSpinner(plotlyOutput("plot_rama_personas", height = 560)))
      ),
      br(),
      fluidRow(
        column(6, withSpinner(plotlyOutput("plot_rama_pct", height = 560))),
        column(6, withSpinner(plotlyOutput("plot_rama_intensidad", height = 560)))
      )
    ),
    nav_panel(
      "Mapas",
      br(),
      uiOutput("aviso_mapa"),
      fluidRow(
        column(6, withSpinner(plotlyOutput("plot_ranking_personas", height = 560))),
        column(6, withSpinner(plotlyOutput("plot_ranking_horas", height = 560)))
      ),
      br(), hr(),
      fluidRow(
        column(5,
               selectInput("mapa_metrica", "Métrica del mapa",
                           choices = c(
                             "% de asalariados afectados" = "pct_asalariados",
                             "Horas extra no pagadas por persona" = "intensidad",
                             "% horas extra no pagadas s/ total" = "pct_npg_sobre_tot"
                           ))
        )
      ),
      fluidRow(
        column(10, withSpinner(plotlyOutput("plot_mapa", height = 600))),
        column(2, uiOutput("mapa_leyenda"))
      )
    )
  )
)

# -----------------------------------------------------------------------------
# SERVER
# -----------------------------------------------------------------------------

server <- function(input, output, session) {

  # ---- Estado del resumen en memoria (cargado una vez al arrancar la app;
  # "Recargar datos" ya NO descarga microdatos, solo vuelve a pedir el
  # resumen a GitHub por si hay una versión más reciente) ----
  grano_reactivo             <- reactiveVal(resumen_global_inicial$grano)
  ultimo_trimestre_reactivo  <- reactiveVal(resumen_global_inicial$ultimo_trimestre)
  generado_el_reactivo       <- reactiveVal(resumen_global_inicial$generado_el)
  error_recarga_reactivo     <- reactiveVal(NULL)

  observeEvent(input$recargar_he, {
    nuevo <- tryCatch(cargar_resumen_remoto(), error = function(e) e)
    if (inherits(nuevo, "error")) {
      error_recarga_reactivo(conditionMessage(nuevo))
    } else {
      error_recarga_reactivo(NULL)
      grano_reactivo(nuevo$grano)
      ultimo_trimestre_reactivo(nuevo$ultimo_trimestre)
      generado_el_reactivo(nuevo$generado_el)
    }
  })

  output$estado_carga_he <- renderUI({
    partes <- list(
      tags$div(
        style = "color: rgba(255,255,255,0.75); font-size: 11.5px;",
        sprintf("Datos disponibles hasta %s.", ultimo_trimestre_reactivo()),
        tags$br(),
        sprintf("Última sincronización: %s",
                format(generado_el_reactivo(), "%d/%m/%Y %H:%M"))
      )
    )
    if (!is.null(error_recarga_reactivo())) {
      partes <- c(partes, list(tags$div(
        class = "he-aviso",
        tags$b("No se pudo recargar: "), error_recarga_reactivo()
      )))
    }
    tagList(partes)
  })

  output$selector_zona_he <- renderUI({
    if (input$ambito_he == "ccaa") {
      selectInput("zona_he", "Comunidad Autónoma", choices = swap_choices(ccaa_labels))
    } else if (input$ambito_he == "prov") {
      selectInput("zona_he", "Provincia", choices = swap_choices(prov_labels))
    } else {
      NULL
    }
  })

  # ---- Ventana de trimestres visible (filtra `grano`, ya en memoria; nada
  # de red aquí -> instantáneo incluso con la serie completa desde 2005) ----
  n_mostrar_react <- reactive({
    req(input$trimestre_fin, input$ventana_he)
    fin <- parse_trimestre_label(input$trimestre_fin)
    if (input$ventana_he == "completa") {
      n_trimestres_hasta(fin$anio, fin$trim, ANIO_MIN, 1)
    } else {
      as.integer(input$ventana_he) * 4
    }
  })

  grano_ventana <- reactive({
    req(input$trimestre_fin)
    fin <- parse_trimestre_label(input$trimestre_fin)
    n_total <- n_mostrar_react() + 3   # margen para poder calcular MA4 desde el primer trimestre mostrado
    max_disponible <- n_trimestres_hasta(fin$anio, fin$trim, ANIO_MIN, 1)
    n_total <- min(n_total, max_disponible)

    pares <- ventana_n_trimestres(fin$anio, fin$trim, n_total)
    etiquetas <- map_chr(pares, ~ sprintf("%dT%d", .x$anio, .x$trim))

    grano_reactivo() |> filter(TRIMESTRE_LABEL %in% etiquetas)
  })

  # ---- Filtro territorial (sobre `grano`, ya agregado por PROV/CCAA) ----
  df_zona_he <- reactive({
    df <- grano_ventana()
    if (nrow(df) == 0) return(df)
    if (input$ambito_he == "ccaa") {
      req(input$zona_he)
      df <- df |> filter(CCAA == input$zona_he)
    } else if (input$ambito_he == "prov") {
      req(input$zona_he)
      df <- df |> filter(PROV == input$zona_he)
    }
    df
  })

  # ---- Serie trimestral + MA4 (idéntico al original, ya no descarga nada) ----
  serie_he <- reactive({
    df <- df_zona_he()
    if (nrow(df) == 0) return(tibble())

    tabla <- resumen_trimestral(df)
    if (nrow(tabla) == 0) return(tabla)

    tabla <- anadir_ma4(tabla)

    tabla |>
      arrange(idx) |>
      slice_tail(n = n_mostrar_react())
  })

  serie_activa <- reactive({
    tabla <- serie_he()
    if (nrow(tabla) == 0) return(tabla)

    if (input$criterio_temporal == "ma4") {
      tabla |>
        transmute(
          TRIMESTRE_LABEL, TRIMESTRE_ANIO, TRIMESTRE_NUM, idx,
          personas_pag = personas_pag_ma4, personas_npg = personas_npg_ma4, personas_tot = personas_tot_ma4,
          horas_pag = horas_pag_ma4, horas_npg = horas_npg_ma4, horas_tot = horas_tot_ma4,
          asalariados = asalariados_ma4,
          pct_npg_sobre_tot = pct_npg_sobre_tot_ma4,
          pct_asalariados_pag = pct_asalariados_pag_ma4,
          pct_asalariados_npg = pct_asalariados_npg_ma4,
          pct_asalariados_tot = pct_asalariados_tot_ma4,
          intensidad_pag = intensidad_pag_ma4, intensidad_npg = intensidad_npg_ma4, intensidad_tot = intensidad_tot_ma4,
          empleos_eq_pag = empleos_eq_pag_ma4, empleos_eq_npg = empleos_eq_npg_ma4, empleos_eq_tot = empleos_eq_tot_ma4
        )
    } else {
      tabla |>
        select(
          TRIMESTRE_LABEL, TRIMESTRE_ANIO, TRIMESTRE_NUM, idx,
          personas_pag, personas_npg, personas_tot,
          horas_pag, horas_npg, horas_tot,
          asalariados,
          pct_npg_sobre_tot, pct_asalariados_pag, pct_asalariados_npg, pct_asalariados_tot,
          intensidad_pag, intensidad_npg, intensidad_tot,
          empleos_eq_pag, empleos_eq_npg, empleos_eq_tot
        )
    }
  })

  serie_plot <- reactive({
    tabla <- serie_activa()
    if (nrow(tabla) == 0) return(tabla)
    tabla |> filter(!is.na(horas_tot))
  })

  # ---- KPIs (sin cambios) ----
  output$kpis_he <- renderUI({
    tabla <- serie_plot()
    if (nrow(tabla) == 0) {
      return(tags$div(class = "he-aviso", "No hay datos para el ámbito/ventana seleccionados."))
    }
    ult <- tabla |> slice_tail(n = 1)
    anterior <- if (nrow(tabla) >= 2) tabla |> slice_tail(n = 2) |> slice_head(n = 1) else NULL
    hace_un_anio_df <- tabla |> dplyr::filter(idx == ult$idx - 4)
    hace_un_anio <- if (nrow(hace_un_anio_df) == 1) hace_un_anio_df else NULL

    layout_column_wrap(
      width = 1/3, heights_equal = "row",
      value_box(
        title = "Personas asalariadas con horas extra NO pagadas / semana",
        value = fmt_es_int(ult$personas_npg),
        showcase = icon("user-clock"), theme = "danger",
        tags$div(class = "he-subvar-row",
                 variacion_subindicador(ult$personas_npg, if (!is.null(anterior)) anterior$personas_npg else NA, "vs. trim. anterior"),
                 variacion_subindicador(ult$personas_npg, if (!is.null(hace_un_anio)) hace_un_anio$personas_npg else NA, "vs. año anterior")
        )
      ),
      value_box(
        title = "Horas extra NO pagadas / semana",
        value = fmt_es_int(ult$horas_npg),
        showcase = icon("clock"), theme = "danger",
        tags$div(class = "he-subvar-row",
                 variacion_subindicador(ult$horas_npg, if (!is.null(anterior)) anterior$horas_npg else NA, "vs. trim. anterior"),
                 variacion_subindicador(ult$horas_npg, if (!is.null(hace_un_anio)) hace_un_anio$horas_npg else NA, "vs. año anterior")
        )
      ),
      value_box(
        title = "Empleos equivalentes (no pagadas)",
        value = fmt_es_int(ult$empleos_eq_npg),
        showcase = icon("briefcase"), theme = bslib::value_box_theme(bg = CCOO_GRANATE, fg = "white"),
        p(class = "he-kpi-note", sprintf("asumiendo jornada de %dh/semana", JORNADA_SEMANAL_HORAS))
      ),
      value_box(
        title = "% horas extra no pagadas s/ total",
        value = fmt_es_pct(ult$pct_npg_sobre_tot),
        showcase = icon("percent"), theme = "secondary",
        variacion_absoluta(ult$pct_npg_sobre_tot, if (!is.null(anterior)) anterior$pct_npg_sobre_tot else NA, "vs. trim. anterior", " p.p."),
        variacion_absoluta(ult$pct_npg_sobre_tot, if (!is.null(hace_un_anio)) hace_un_anio$pct_npg_sobre_tot else NA, "vs. año anterior", " p.p.")
      ),
      value_box(
        title = "% población asalariada que hace horas extra no pagadas",
        value = fmt_es_pct(ult$pct_asalariados_npg),
        showcase = icon("people-group"), theme = "secondary",
        variacion_absoluta(ult$pct_asalariados_npg, if (!is.null(anterior)) anterior$pct_asalariados_npg else NA, "vs. trim. anterior", " p.p."),
        variacion_absoluta(ult$pct_asalariados_npg, if (!is.null(hace_un_anio)) hace_un_anio$pct_asalariados_npg else NA, "vs. año anterior", " p.p.")
      ),
      value_box(
        title = "Horas extra no pagadas por persona",
        value = fmt_es_dec(ult$intensidad_npg),
        showcase = icon("gauge-high"), theme = "secondary",
        variacion_absoluta(ult$intensidad_npg, if (!is.null(anterior)) anterior$intensidad_npg else NA, "vs. trim. anterior", " h"),
        variacion_absoluta(ult$intensidad_npg, if (!is.null(hace_un_anio)) hace_un_anio$intensidad_npg else NA, "vs. año anterior", " h")
      )
    )
  })

  # ---- Gráficos "Resumen" (sin cambios) ----
  output$plot_horas_he <- renderPlotly({
    tabla <- serie_plot()
    if (nrow(tabla) == 0) return(NULL)
    d_long <- tabla |>
      select(TRIMESTRE_LABEL, horas_pag, horas_npg) |>
      pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "Horas") |>
      mutate(
        Tipo = recode(Tipo, horas_pag = "Pagadas", horas_npg = "No pagadas"),
        Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
        hover = paste0(TRIMESTRE_LABEL, "<br>", Tipo, ": ", fmt_es_int(Horas), " h/semana")
      )

    plot_ly(d_long, x = ~TRIMESTRE_LABEL, y = ~Horas, color = ~Tipo,
            colors = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO),
            type = "scatter", mode = "lines+markers",
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = "Horas extra trabajadas semanalmente",
        xaxis = list(title = ""), yaxis = list(title = "Horas / semana"),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  output$plot_personas_he <- renderPlotly({
    tabla <- serie_plot()
    if (nrow(tabla) == 0) return(NULL)
    d_long <- tabla |>
      select(TRIMESTRE_LABEL, personas_pag, personas_npg) |>
      pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "Personas") |>
      mutate(
        Tipo = recode(Tipo, personas_pag = "Pagadas", personas_npg = "No pagadas"),
        Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
        hover = paste0(TRIMESTRE_LABEL, "<br>", Tipo, ": ", fmt_es_int(Personas), " personas")
      )

    plot_ly(d_long, x = ~TRIMESTRE_LABEL, y = ~Personas, color = ~Tipo,
            colors = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO),
            type = "scatter", mode = "lines+markers",
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = "Personas asalariadas que hicieron horas extra",
        xaxis = list(title = ""), yaxis = list(title = "Personas / semana"),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  # ---- Gráficos "Indicadores" (sin cambios) ----
  output$plot_pct_npg <- renderPlotly({
    tabla <- serie_plot()
    if (nrow(tabla) == 0) return(NULL)
    tabla <- tabla |> mutate(hover = paste0(TRIMESTRE_LABEL, "<br>", fmt_es_pct(pct_npg_sobre_tot)))
    plot_ly(tabla, x = ~TRIMESTRE_LABEL, y = ~pct_npg_sobre_tot,
            type = "scatter", mode = "lines+markers",
            line = list(color = CCOO_ROJO), marker = list(color = CCOO_ROJO, size = 7),
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = "% de horas extra NO pagadas sobre el total de horas extra",
        xaxis = list(title = "", tickangle = -90), yaxis = list(title = "%", ticksuffix = "%"),
        hoverlabel = list(bgcolor = CCOO_ROJO, font = list(color = "white"))
      )
  })

  output$plot_pct_asalariados <- renderPlotly({
    tabla <- serie_plot()
    if (nrow(tabla) == 0) return(NULL)
    d_long <- tabla |>
      select(TRIMESTRE_LABEL, pct_asalariados_pag, pct_asalariados_npg) |>
      pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "pct") |>
      mutate(
        Tipo = recode(Tipo, pct_asalariados_pag = "Pagadas", pct_asalariados_npg = "No pagadas"),
        Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
        hover = paste0(TRIMESTRE_LABEL, "<br>", Tipo, ": ", fmt_es_pct(pct))
      )

    plot_ly(d_long, x = ~TRIMESTRE_LABEL, y = ~pct, color = ~Tipo,
            colors = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO),
            type = "scatter", mode = "lines+markers",
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = "% de la población asalariada que hace horas extra",
        xaxis = list(title = "", tickangle = -90), yaxis = list(title = "%", ticksuffix = "%"),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  output$plot_intensidad <- renderPlotly({
    tabla <- serie_plot()
    if (nrow(tabla) == 0) return(NULL)
    d_long <- tabla |>
      select(TRIMESTRE_LABEL, intensidad_pag, intensidad_npg) |>
      pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "Horas") |>
      mutate(
        Tipo = recode(Tipo, intensidad_pag = "Pagadas", intensidad_npg = "No pagadas"),
        Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
        hover = paste0(TRIMESTRE_LABEL, "<br>", Tipo, ": ", fmt_es_dec(Horas), " h/persona")
      )

    plot_ly(d_long, x = ~TRIMESTRE_LABEL, y = ~Horas, color = ~Tipo,
            colors = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO),
            type = "scatter", mode = "lines+markers",
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = "Intensidad media: horas extra por persona afectada / semana",
        xaxis = list(title = "", tickangle = -90), yaxis = list(title = "Horas / persona"),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  output$plot_empleos_eq <- renderPlotly({
    tabla <- serie_plot()
    if (nrow(tabla) == 0) return(NULL)
    d_long <- tabla |>
      select(TRIMESTRE_LABEL, empleos_eq_pag, empleos_eq_npg) |>
      pivot_longer(-TRIMESTRE_LABEL, names_to = "Tipo", values_to = "Empleos") |>
      mutate(
        Tipo = recode(Tipo, empleos_eq_pag = "Pagadas", empleos_eq_npg = "No pagadas"),
        Tipo = factor(Tipo, levels = c("Pagadas", "No pagadas")),
        hover = paste0(TRIMESTRE_LABEL, "<br>", Tipo, ": ", fmt_es_int(Empleos), " empleos")
      )

    plot_ly(d_long, x = ~TRIMESTRE_LABEL, y = ~Empleos, color = ~Tipo,
            colors = c("Pagadas" = CCOO_NEGRO, "No pagadas" = CCOO_ROJO),
            type = "bar", textposition = "none",
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        barmode = "stack",
        title = sprintf("Empleos equivalentes a jornada completa (%dh/semana)", JORNADA_SEMANAL_HORAS),
        xaxis = list(title = "", tickangle = -90), yaxis = list(title = "Empleos equivalentes"),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  # ---- Pestaña "Sectores de actividad" ----
  rama_disponible <- reactive({
    df <- df_zona_he()
    nrow(df) > 0 && !all(is.na(df$RAMA_COD))
  })

  output$aviso_rama <- renderUI({
    if (rama_disponible()) {
      tabla <- resumen_rama_react()
      n_desc <- attr(tabla, "trimestres_descartados")
      clasif <- attr(tabla, "clasif_usada")

      if (!is.null(n_desc) && n_desc > 0) {
        n_utilizados <- 4 - n_desc
        return(tags$div(
          class = "he-aviso",
          sprintf(
            paste0(
              "Aviso: En 2026, el INE ha comenzado a aplicar la nueva clasificación de actividades ",
              "económicas CNAE-2025 en sus estadísticas. ",
              "Para que este desglose no mezcle dos codificaciones distintas, las medias móviles se han calculado únicamente ",
              "con los trimestres disponibles de 2026 hasta el periodo seleccionado."
            ),
            n_utilizados
          )
        ))
      }
      return(NULL)
    }
    tags$div(
      class = "he-aviso",
      tags$b("Desglose por grupo de actividad no disponible para este periodo. "),
      "No se ha encontrado la columna esperada (ACT1) en el resumen agregado. ",
      "Revisa el aviso del último trimestre en la Action de GitHub."
    )
  })

  rama_n_periodos <- reactive({
    if (input$criterio_temporal == "ma4") 4 else 1
  })

  resumen_rama_react <- reactive({
    df <- df_zona_he()
    if (nrow(df) == 0 || !rama_disponible()) return(tibble())

    fin <- parse_trimestre_label(input$trimestre_fin)
    df <- df |> filter(TRIMESTRE_ANIO * 4 + TRIMESTRE_NUM <= fin$anio * 4 + fin$trim)

    resumen_por_rama(df, ultimos_n = rama_n_periodos(), tipo = "npg")
  })

  output$plot_rama_horas <- renderPlotly({
    tabla <- resumen_rama_react()
    if (nrow(tabla) == 0) return(NULL)
    top <- tabla |>
      arrange(horas) |>
      mutate(hover = paste0(Rama, "<br>", fmt_es_int(horas), " h/semana"))

    plot_ly(top, x = ~horas, y = ~factor(Rama, levels = Rama), type = "bar", textposition = "none",
            orientation = "h", marker = list(color = CCOO_ROJO),
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = "Horas extra NO pagadas por grupo de actividad",
        xaxis = list(title = "Horas / semana"), yaxis = list(title = "")
      )
  })

  output$plot_rama_personas <- renderPlotly({
    tabla <- resumen_rama_react()
    if (nrow(tabla) == 0) return(NULL)
    top <- tabla |>
      arrange(personas) |>
      mutate(hover = paste0(Rama, "<br>", fmt_es_int(personas), " personas"))

    plot_ly(top, x = ~personas, y = ~factor(Rama, levels = Rama), type = "bar", textposition = "none",
            orientation = "h", marker = list(color = CCOO_ROJO),
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = "Personas asalariadas con horas extra NO pagadas",
        xaxis = list(title = "Personas / semana"), yaxis = list(title = "")
      )
  })

  output$plot_rama_pct <- renderPlotly({
    tabla <- resumen_rama_react()
    if (nrow(tabla) == 0) return(NULL)
    top <- tabla |>
      arrange(pct_asalariados) |>
      mutate(hover = paste0(Rama, "<br>", fmt_es_pct(pct_asalariados)))

    plot_ly(top, x = ~pct_asalariados, y = ~factor(Rama, levels = Rama), type = "bar", textposition = "none",
            orientation = "h", marker = list(color = "#222222"),
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = "% personas asalariadas del sector con horas extra no pagadas",
        xaxis = list(title = "%", ticksuffix = "%"), yaxis = list(title = "")
      )
  })

  output$plot_rama_intensidad <- renderPlotly({
    tabla <- resumen_rama_react()
    if (nrow(tabla) == 0) return(NULL)
    top <- tabla |>
      arrange(intensidad) |>
      mutate(hover = paste0(Rama, "<br>", fmt_es_dec(intensidad), " h/persona"))

    plot_ly(top, x = ~intensidad, y = ~factor(Rama, levels = Rama), type = "bar", textposition = "none",
            orientation = "h", marker = list(color = "#222222"),
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = "Horas extra no pagadas por persona asalariada",
        xaxis = list(title = "Horas / persona"), yaxis = list(title = "")
      )
  })

  # ---- Pestaña "Mapas" ----
  nivel_mapa <- reactive({
    if (input$ambito_he == "esp") "CCAA" else "PROV"
  })

  prov_ccaa_map_react <- reactive({
    grano_reactivo() |> distinct(PROV, CCAA)
  })

  ccaa_objetivo_react <- reactive({
    if (input$ambito_he == "esp") return(NULL)
    req(input[["zona_he"]])
    if (input$ambito_he == "ccaa") return(input$zona_he)
    fila <- prov_ccaa_map_react() |> filter(PROV == input$zona_he)
    if (nrow(fila) == 0) return(NULL)
    fila$CCAA[1]
  })

  provincias_permitidas_react <- reactive({
    ccaa_obj <- ccaa_objetivo_react()
    if (is.null(ccaa_obj)) return(NULL)
    prov_ccaa_map_react() |> filter(CCAA == ccaa_obj) |> pull(PROV)
  })

  resumen_mapa_react <- reactive({
    df <- grano_ventana()
    datos <- resumen_mapa(df, nivel_mapa(), input$criterio_temporal)
    permitidas <- provincias_permitidas_react()
    if (!is.null(permitidas)) datos <- datos |> filter(cod %in% permitidas)
    datos
  })

  geo_mapa_react <- reactive({
    if (!MAPAS_DISPONIBLES) return(NULL)
    geo <- tryCatch({
      if (nivel_mapa() == "CCAA") obtener_geo_ccaa() else obtener_geo_prov()
    }, error = function(e) NULL)
    if (is.null(geo)) return(NULL)
    permitidas <- provincias_permitidas_react()
    if (!is.null(permitidas)) geo <- geo |> filter(cod %in% permitidas)
    geo
  })

  output$aviso_mapa <- renderUI({
    if (!MAPAS_DISPONIBLES) {
      return(tags$div(
        class = "he-aviso",
        tags$b("Pestaña de mapas no disponible. "),
        "Instala los paquetes ", tags$code("mapSpain"), " y ", tags$code("sf"), " para activarla: ",
        tags$code('install.packages(c("mapSpain", "sf"))')
      ))
    }
    if (is.null(geo_mapa_react())) {
      return(tags$div(
        class = "he-aviso",
        "No se pudo obtener la geometría del mapa (se descarga de GISCO la primera vez que se usa ",
        "y se cachea en disco; revisa la conexión a internet)."
      ))
    }
    NULL
  })

  datos_mapa_nombrado <- reactive({
    datos <- resumen_mapa_react()
    if (nrow(datos) == 0) return(datos)
    etiquetas <- if (nivel_mapa() == "CCAA") ccaa_labels else prov_labels
    datos$nombre <- unname(etiquetas[datos$cod])
    datos
  })

  output$plot_mapa <- renderPlotly({
    req(MAPAS_DISPONIBLES)
    geo <- geo_mapa_react()
    req(geo)
    datos <- datos_mapa_nombrado()
    if (nrow(datos) == 0) return(NULL)

    metrica <- input$mapa_metrica
    nivel_txt <- if (nivel_mapa() == "CCAA") "Comunidad Autónoma" else "Provincia"

    datos$valor <- if (metrica == "pct_npg_sobre_tot") datos$pct_npg_sobre_tot else col_tipo(datos, metrica, "npg")

    geo_join <- geo |>
      left_join(datos |> select(cod, valor, nombre), by = "cod")

    info_metrica <- info_metrica_mapa(metrica, nivel_txt)

    geo_join$hover_txt <- paste0(geo_join$nombre, "<br>", info_metrica$etiqueta, ": ",
                                 ifelse(is.na(geo_join$valor), "sin datos", info_metrica$fmt(geo_join$valor)))

    rango <- range(geo_join$valor, na.rm = TRUE)
    paleta <- scales::col_numeric(palette = "Reds", domain = rango, na.color = "#e0e0e0")
    geo_join$color_hex <- paleta(geo_join$valor)

    tryCatch({
      plot_ly(geo_join) |>
        add_sf(
          split = ~cod, color = ~I(color_hex),
          stroke = I("#555555"), span = I(0.6),
          text = ~hover_txt, hoverinfo = "text", hoveron = "fills",
          showlegend = FALSE
        ) |>
        layout(
          separators = ",.",
          title = sprintf("%s — %s", info_metrica$titulo, input$trimestre_fin),
          xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)
        )
    }, error = function(e) {
      plotly_empty(type = "scatter", mode = "markers") |>
        layout(title = paste("No se pudo dibujar el mapa:", conditionMessage(e)),
               xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    })
  })

  output$mapa_leyenda <- renderUI({
    req(MAPAS_DISPONIBLES)
    req(geo_mapa_react())
    datos <- datos_mapa_nombrado()
    if (nrow(datos) == 0) return(NULL)

    metrica <- input$mapa_metrica
    datos$valor <- if (metrica == "pct_npg_sobre_tot") datos$pct_npg_sobre_tot else col_tipo(datos, metrica, "npg")
    rango <- range(datos$valor, na.rm = TRUE)
    if (!all(is.finite(rango))) return(NULL)

    fmt_valor <- if (metrica == "intensidad") fmt_es_dec else fmt_es_pct
    paleta <- scales::col_numeric(palette = "Reds", domain = rango)
    tramos <- paleta(seq(rango[1], rango[2], length.out = 6))

    tags$div(
      style = "display:flex; flex-direction:column; align-items:center; height:520px; padding-top:10px;",
      tags$span(fmt_valor(rango[2]), style = "font-size:12px; color:#6c757d; margin-bottom:4px;"),
      tags$div(style = sprintf(
        "flex:1; width:18px; border-radius:3px; background: linear-gradient(to top, %s);",
        paste(tramos, collapse = ", ")
      )),
      tags$span(fmt_valor(rango[1]), style = "font-size:12px; color:#6c757d; margin-top:4px;")
    )
  })

  output$plot_ranking_personas <- renderPlotly({
    datos <- datos_mapa_nombrado()
    if (nrow(datos) == 0) return(NULL)
    nivel_txt <- if (nivel_mapa() == "CCAA") "Comunidad Autónoma" else "Provincia"
    top <- datos |>
      arrange(personas_npg) |>
      mutate(hover = paste0(nombre, "<br>", fmt_es_int(personas_npg), " personas"))

    plot_ly(top, x = ~personas_npg, y = ~factor(nombre, levels = nombre), type = "bar", textposition = "none",
            orientation = "h", marker = list(color = CCOO_ROJO),
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = sprintf("Personas con horas extra NO pagadas / semana, por %s", nivel_txt),
        xaxis = list(title = "Personas / semana"), yaxis = list(title = "")
      )
  })

  output$plot_ranking_horas <- renderPlotly({
    datos <- datos_mapa_nombrado()
    if (nrow(datos) == 0) return(NULL)
    nivel_txt <- if (nivel_mapa() == "CCAA") "Comunidad Autónoma" else "Provincia"
    top <- datos |>
      arrange(horas_npg) |>
      mutate(hover = paste0(nombre, "<br>", fmt_es_int(horas_npg), " h/semana"))

    plot_ly(top, x = ~horas_npg, y = ~factor(nombre, levels = nombre), type = "bar", textposition = "none",
            orientation = "h", marker = list(color = "#222222"),
            text = ~hover, hoverinfo = "text") |>
      layout(
        separators = ",.",
        title = sprintf("Horas extra NO pagadas / semana, por %s", nivel_txt),
        xaxis = list(title = "Horas / semana"), yaxis = list(title = "")
      )
  })

  # ---- Informe PDF (sin cambios: opera sobre las tablas ya resumidas) ----
  output$aviso_informe <- renderUI({
    if (informe_disponible()) return(NULL)
    tags$div(
      style = "color: rgba(255,255,255,0.75); font-size: 11px; margin-top: 4px;",
      "PDF no disponible: faltan paquetes ggplot2/gridExtra."
    )
  })

  output$descargar_informe <- downloadHandler(
    filename = function() {
      sprintf("observatorio_horas_extra_%s_%s.pdf", input$ambito_he, input$trimestre_fin)
    },
    content = function(file) {
      serie <- serie_plot()
      if (nrow(serie) == 0) {
        grDevices::pdf(file, width = 11.69, height = 8.27)
        plot.new()
        text(0.5, 0.5, "No hay datos cargados para generar el informe con los filtros actuales.", cex = 1)
        grDevices::dev.off()
        return(invisible(NULL))
      }
      ult <- serie |> slice_tail(n = 1)

      kpis <- list(
        personas_npg = ult$personas_npg, horas_npg = ult$horas_npg,
        empleos_eq_npg = ult$empleos_eq_npg, pct_npg_sobre_tot = ult$pct_npg_sobre_tot,
        pct_asalariados_npg = ult$pct_asalariados_npg, intensidad_npg = ult$intensidad_npg,
        jornada = JORNADA_SEMANAL_HORAS
      )

      etiqueta_ventana <- names(opciones_ventana)[opciones_ventana == input$ventana_he]
      datos_terr <- datos_mapa_nombrado()
      nivel_txt_pdf <- if (nivel_mapa() == "CCAA") "Comunidades Autónomas" else "Provincias"

      metricas_mapa <- c("pct_asalariados", "intensidad", "pct_npg_sobre_tot")
      mapas_lista <- list()
      geo <- tryCatch(geo_mapa_react(), error = function(e) NULL)
      for (metrica in metricas_mapa) {
        entrada <- list(geo = NULL, disponible = FALSE, titulo = "", etiqueta = "", fmt = fmt_es_dec)
        if (!is.null(geo) && nrow(datos_terr) > 0) {
          datos_terr_m <- datos_terr
          datos_terr_m$valor <- if (metrica == "pct_npg_sobre_tot") datos_terr_m$pct_npg_sobre_tot else col_tipo(datos_terr_m, metrica, "npg")
          info_m <- info_metrica_mapa(metrica, if (nivel_mapa() == "CCAA") "Comunidad Autónoma" else "Provincia")
          entrada$geo <- geo |> dplyr::left_join(datos_terr_m |> dplyr::select(cod, valor, nombre), by = "cod")
          entrada$titulo <- info_m$titulo
          entrada$etiqueta <- info_m$etiqueta
          entrada$fmt <- info_m$fmt
          entrada$disponible <- TRUE
        }
        mapas_lista[[metrica]] <- entrada
      }

      generar_informe_pdf(
        file = file,
        territorio = territorio_label_str(input$ambito_he, input[["zona_he"]]),
        periodo = input$trimestre_fin,
        criterio = if (input$criterio_temporal == "ma4") "Media móvil 4 trimestres" else "Trimestral",
        ventana = etiqueta_ventana,
        kpis = kpis,
        serie = serie,
        rama = resumen_rama_react(),
        ranking_personas = datos_terr,
        ranking_horas = datos_terr,
        nivel_mapa_label = nivel_txt_pdf,
        mapas_lista = mapas_lista,
        fecha_generacion = Sys.time()
      )
    }
  )

}

shinyApp(ui, server)
