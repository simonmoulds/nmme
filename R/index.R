#' Index URLs
#'
#' Construct URLs from `nmme` object.
#'
#' @param x nmme object.
#' @param location character.
#' @param ... Additional arguments.
#'
nmme_index <- function(x, location, ...) {
  base_url = paste(
    iri_spec$base_url,
    x$dataset$source$ext,
    x$dataset$model$ext,
    x$dataset$simulation_type$ext,
    x$dataset$dataset$ext,
    x$dataset$variable$ext,
    sep = "/"
  )
  S = x$dataset$S
  M = x$dataset$M
  L = x$dataset$L
  Y = x$dataset$Y
  X = x$dataset$X
  n_init_times = length(S$subset)
  n_members = length(M$subset)
  n_lead_times = length(L$subset)
  include_X = !isTRUE(is.null(X$ext))
  include_Y = !isTRUE(is.null(Y$ext))
  ## URL order: Y/S/X/L/M [I don't know whether this makes a difference]
  url_cols <- c("Y", "S", "X", "L", "M")
  if (!include_X) url_cols = url_cols[!url_cols %in% "X"]
  if (!include_Y) url_cols = url_cols[!url_cols %in% "Y"]
  url <-
    expand_grid(Y = Y$ext, S = S$ext, X = X$ext, L = L$ext, M = M$ext) %>%
    unite("URL", url_cols, sep = "/") %>%
    mutate(URL = paste(base_url, URL, sep="/")) %>%
    mutate(URL = gsub("(/)\\1+", "\\1", URL)) %>%
    mutate(URL = gsub(" ", "%20", URL)) %>%
    mutate(URL = gsub(",", "%2C", URL)) %>%
    mutate(URL = paste0(URL, "/data.nc"))
  idx <-
    expand_grid(
      variable = gsub("^\\.", "", x$dataset$variable$ext),
      model = gsub("^\\.", "", x$dataset$model$ext),
      S = S$subset, L = as.numeric(L$subset), M = M$subset
    ) %>%
    mutate(S = as.yearmon(S)) %>%
    mutate(L0 = S + ((ceiling(L) - 1) / 12)) %>%
    mutate(S = format(S, "%Y%m")) %>%
    mutate(L0 = format(L0, "%Y%m")) %>%
    mutate(L0 = paste0(L0, "-", L0)) %>%
    unite("destfile", variable, model, S, M, L0, remove = FALSE) %>%
    mutate(destfile = file.path(location, paste0(destfile, ".nc"))) %>%
    mutate(exists = file.exists(destfile)) %>%
    dplyr::select(-L0, -model)
  idx <- idx %>% mutate(URL = url$URL, .before = "destfile")
  idx
}

nmme_reindex <- function(x, ...) {
  x$index <- nmme_index(x, x$location)
  x
}

get_forecast_start <- function(init_time, lead_time) {
  tm = as.yearmon(init_time, format = "%Y%m")
  ## tm <- init_time
  n_month <- ceiling(min(lead_time)) - 1
  fcst_start <- tm + (n_month / 12)
  fcst_start
}

get_forecast_end <- function(init_time, lead_time) {
  tm = as.yearmon(init_time, format = "%Y%m")
  ## tm <- init_time
  n_month <- ceiling(max(lead_time)) - 1
  fcst_end <- tm + (n_month / 12)
  fcst_end
}
