## Author : Simon Moulds
## Date   : Apr 2022

library(tidyverse)
library(yaml)

config = read_yaml("iri.yml")
supported_models = "GFDL-SPEAR"

format_x <- function(x) {
  xmn = min(x) %% 360
  xmx = max(x) %% 360
  return(paste0("X/(", xmn, ")", "(", xmx, ")RANGEEDGES"))
}

format_y <- function(y) {
  .fmt_y <- function(y) {
    if (y < 0) {
      return(paste0(y, "S"))
    } else {
      return(paste0(y, "N"))
    }
  }
  ymn = .fmt_y(min(y))
  ymx = .fmt_y(max(y))
  return(paste0("Y/(", ymn, ")", "(", ymx, ")RANGEEDGES"))
}

format_members <- function(members) {
  m1 = min(members) %>% format(nsmall = 1)
  mn = max(members) %>% format(nsmall = 1)
  return(paste0("M/(", m1, ")", "(", mn, ")RANGEEDGES"))
}

## format_lead_times <- function() {}

format_times <- function(start_year, start_month, end_year, end_month) {
  .fmt_time <- function(year, month) {
    if (!year %in% 1991:2020) stop("`start_year` and `end_year` must be in the range 1991-2020")
    if (!month %in% 1:12) stop("`start_month` and `end_month` must be in the range 1-12")
    return(paste0("(0000 1 ", month.abb[month], " ", year, ")"))
  }
  start_time = .fmt_time(start_year, start_month)
  end_time = .fmt_time(end_year, end_month)
  return(paste0("S/", start_time, end_time, "RANGEEDGES"))
}

format_lead_times <- function(lead_times) {
  valid_lead_times = seq(0.5, 11.5, 1)
  if (!all(lead_times %in% valid_lead_times)) stop("`lead_times` contains invalid values")
  return("")
}

nmme_url <- function(model = "GFDL-SPEAR",
                     simulation_type = "hindcast",
                     dataset = "monthly",
                     variable = "prec",
                     x_lim = NA,
                     y_lim = NA,
                     members = NA,
                     lead_times = seq(0.5, 11.5, 1),
                     start_year = 1991,
                     start_month = 1,
                     end_year = 2020,
                     end_month = 12) {

  if (!model %in% supported_models) {
    stop(sprintf("Model %s not yet supported!", model))
  }
  if (!simulation_type %in% c("hindcast", "forecast")) {
    stop("`simulation_type` must be one of hindcast, forecast")
  }
  ## Spatial query
  if (!is.na(x_lim)) {
    if (length(x_lim) != 2) stop("`x_lim` should be a numeric vector with two elements")
    X = format_x(x_lim)
  } else {
    X = ""
  }
  if (!is.na(y_lim)) {
    if (length(y_lim) != 2) stop()
    Y = format_y(y_lim)
  } else {
    Y = ""
  }
  ## lead-time query
  L = format_lead_times(lead_times)
  ## Temporal query
  if (end_year < start_year) {
    stop("`end_year` should be greater than or equal to `start_year`")
  }
  if (end_month < start_month) {
    stop("`end_month` should be greater than or equal to `start_month`")
  }
  ## Only download one year at a time
  years = seq(start_year, end_year, 1)
  urls = rep(NA, length(years))
  for (i in 1:length(years)) {
    year = years[i]
    S = format_times(year, start_month, year, end_month)
    ## URL extension
    url_ext = paste(
      "SOURCES/.Models/.NMME",
      paste0(".", model),
      paste0(".", toupper(simulation_type)),
      paste0(".", toupper(dataset)),
      paste0(".", variable),
      Y, X, S,
      "data.nc",
      sep = "/"
    )
    url_ext = gsub("(/)\\1+", "\\1", url_ext)
    url_ext = gsub(" ", "%20", url_ext)
    url = paste(config$base_url, url_ext, sep="/")
    urls[i] = url
  }
  urls
}

download_nmme <- function(urls = nmme_url(),
                          destdir = ".",
                          overwrite = FALSE, ...) {

  for (i in 1:length(urls)) {
    download.file(urls[i], destfile="test.nc", ...)
  }
  NULL
}
