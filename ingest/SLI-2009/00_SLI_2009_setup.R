# =============================================================================
# Shared setup for SLI 2009 ingest scripts
#
# This expedition lives in the legacy database rather than in an expedition repo
# of its own, so both its inputs and its outputs sit under `legacy-db`: the SIO
# workbooks in `data/raw`, the curated tables in `data/output/uvs` alongside
# every other legacy expedition.
# =============================================================================
#
# -- Expedition info ----------------------------------------------------------
#
## Single source of truth for this expedition.

exp_id       <- "SLI_2009"
eez_id       <- 8441
date_start   <- "2009-03-01"
date_end     <- "2009-04-30"
science_lead <- "Alan Friedlander"
regions      <- c("Southern Line Islands")
# Malden and Starbuck were surveyed in 2009 and in no later year; Flint,
# Millennium and Vostok recur through 2013, 2017, 2021 and 2023.
subregions   <- c("Flint", "Malden", "Millennium", "Starbuck", "Vostok")
exp_year     <- as.integer(stringr::str_extract(exp_id, "\\d{4}$"))
exp_country  <- countrycode::countrycode("KIR", origin = "iso3c", destination = "country.name.en")

# -- Palettes ------------------------------------------------------------------
# region/subregion are expedition-specific, so their colors are defined here.
# Flint, Millennium and Vostok keep the colours used for SLI 2021 and 2023, so
# the three recurring islands read the same across every year of the series.

region_palette <- c("Southern Line Islands" = "#A23B72")

subregion_palette <- c("Flint"      = "#CA054D",
                       "Malden"     = "#3B7080",
                       "Millennium" = "#628395",
                       "Starbuck"   = "#8E6C88",
                       "Vostok"     = "#DBAD6A")

# -- Depth strata ---------------------------------------------------------------
# The Pristine Seas standard cuts `shallow` at <= 14 m and `deep` at >= 15 m.
# Every Southern Line Islands survey works a nominal 15 m contour, which puts the
# main reef stratum one metre on the wrong side of that cut. For this archipelago
# the boundary therefore moves up a metre: 15 m is shallow, and deep starts above
# it. Every other cut is untouched.
#
# 2009 recorded a single 10 m stratum, so the two rules agree here — it is
# defined anyway so that every SLI year bins depths identically.

stratify_sli <- function(depth_m) {
  PristineSeasR2::stratify(dplyr::if_else(depth_m > 14 & depth_m <= 15, 14, depth_m))
}

# -- Libraries ----------------------------------------------------------------

library(sf)
library(terra)
library(mregions2)
library(tidyverse)
library(janitor)
library(lubridate)
library(hms)
library(readxl)
library(pointblank)
library(gt)
library(ggtext)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(PristineSeasR2)
library(bigrquery)
library(htmltools)
library(htmlwidgets)

# -- Options ------------------------------------------------------------------

## Suppress scientific notation in printed numbers
options(scipen = 999)

## Inline numbers (e.g. `r nrow(x)`) get comma separators
knitr::knit_hooks$set(inline = function(x) {
  if (!is.numeric(x)) return(as.character(x))
  format(x, big.mark = ",", scientific = FALSE)
})

## set_theme() is ggplot2's alias for theme_set()
set_theme(PristineSeasR2::theme_ps())

# -- Figure export --------------------------------------------------------------
# Stamp the expedition onto every figure we write to disk. ggplot2's `tag` is the
# label meant for identifying a plot, and nothing in these notebooks uses it, so
# it sits in the corner without disturbing any title, subtitle or caption. Saved
# figures from different expeditions can then be read side by side.

ggsave <- function(filename, plot = ggplot2::last_plot(), ...) {
  if (inherits(plot, "gg")) {
    plot <- plot +
      ggplot2::labs(tag = exp_id) +
      ggplot2::theme(plot.tag          = ggplot2::element_text(size = 9, colour = "grey45",
                                                               hjust = 1),
                     plot.tag.position = c(0.995, 0.985))
  }
  ggplot2::ggsave(filename = filename, plot = plot, ...)
}

# -- Paths ---------------------------------------------------------------------

ps_paths <- PristineSeasR2::get_drive_paths()

prj_path <- file.path(ps_paths$projects, "legacy-db")

# The SIO workbooks for the Southern Line Islands series — metadata and fish for
# 2009, 2013 and 2017 — copied into the database project so the ingest depends on
# nothing outside it.
data_in     <- file.path(prj_path, "data/raw/SLI from SIO")
data_out    <- file.path(prj_path, "data/output/uvs")
figures_out <- file.path(prj_path, "figures/uvs")

stopifnot("legacy-db project folder not found" = dir.exists(prj_path),
          "SLI raw data folder not found"      = dir.exists(data_in))

purrr::walk(c(data_out, figures_out), \(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE))

# -- Auth & connection --------------------------------------------------------

bigrquery::bq_auth(email = "marine.data.science@ngs.org")

bq_connection <- DBI::dbConnect(bigrquery::bigquery(),
                                project = "pristine-seas")

# -- Expedition config --------------------------------------------------------

eez <- mregions2::gaz_search(eez_id) |>
  mregions2::gaz_geometry() |>
  terra::vect() |>
  terra::fillHoles()

stopifnot("EEZ geometry lookup returned no result" = nrow(eez) > 0)

# single source of truth for expedition-level constants used throughout

exp_config <- list(exp_id       = exp_id,
                   eez_id       = eez_id,
                   science_lead = science_lead,
                   date_bounds  = as.Date(c(date_start, date_end)),
                   lat_min      = eez$minLatitude,
                   lat_max      = eez$maxLatitude,
                   lon_min      = eez$minLongitude,
                   lon_max      = eez$maxLongitude)
