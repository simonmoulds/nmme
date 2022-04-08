#' Index URLs
#'
#' Construct URLs from `nmme` object.
#'
#' @param index nmme object.
#' @param ... Additional arguments.
#'
index <- function(x, ...) {
  UseMethod("index")
}

#' @export
index.nmme <- function(x) {
  base_url = paste(
    iri_spec$base_url,
    x$dataset$source$ext,
    x$dataset$model$ext,
    x$dataset$simulation_type$ext,
    x$dataset$dataset$ext,
    x$dataset$variable$ext,
    sep = "/"
  )
  init_times = x$dataset$S
  members = x$dataset$M
  n_init_times = length(init_times$subset)
  n_members = length(members$subset)
  ## Pull out the other datasets
  Y = x$dataset$Y
  X = x$dataset$X
  L = x$dataset$L
  min_lead_time = min(L$subset)
  max_lead_time = max(L$subset)
  include_X = !isTRUE(is.na(X))
  include_Y = !isTRUE(is.na(Y))
  include_L = !(isTRUE(is.na(L)) | (length(L$subset) == length(x$config$lead_times)))
  ## Construct URLs
  n_urls = n_init_times * n_members
  urls = rep(NA, n_urls)
  destfiles = rep(NA, n_urls)
  init_time_index = rep(NA, n_urls)
  member_index = rep(NA, n_urls)
  indx = 1
  for (i in 1:n_init_times) {
    S = init_times$ext[i]
    init_time = init_times$subset[i]
    forecast_start = get_forecast_start(init_time, min_lead_time)
    forecast_end = get_forecast_end(init_time, max_lead_time)
    for (j in 1:n_members) {
      M = members$ext[j]
      member = members$subset[j]
      ## URL order: Y/S/X/L/M
      url = base_url
      url = ifelse(include_Y, paste0(url, "/", Y$ext), url)
      url = paste0(url, "/", S)
      url = ifelse(include_X, paste0(url, "/", X$ext), url)
      url = ifelse(include_L, paste0(url, "/", L$ext), url)
      url = paste0(url, "/", M)
      url = paste0(url, "/data.nc")
      url = gsub("(/)\\1+", "\\1", url)
      url = gsub(" ", "%20", url)
      url = gsub(",", "%2C", url)
      ## Filename
      destfile = paste0(
        x$dataset$variable$subset, "_",
        x$dataset$model$subset, "_",
        init_time, "_",
        member, "_",
        forecast_start, "-",
        forecast_end, ".nc"
      )
      urls[indx] = url
      destfiles[indx] = destfile
      init_time_index[indx] = i
      member_index[indx] = j
      indx = indx + 1
    }
  }
  urls = tibble(url = urls, destfile = destfiles, init_time_index = init_time_index, member_index = member_index)
  urls
}

reindex <- function(x, ...) {
  x$index <- index(x)
  x
}

get_forecast_start <- function(init_time, lead_time) {
  tm = as.yearmon(init_time, format = "%Y%m")
  n_month = ceiling(min(lead_time)) - 1
  fcst_start = tm + (n_month / 12)
  fcst_start %>% format("%Y%m")
}

get_forecast_end <- function(init_time, lead_time) {
  tm = as.yearmon(init_time, format = "%Y%m")
  n_month = ceiling(max(lead_time)) - 1
  fcst_end = tm + (n_month / 12)
  fcst_end %>% format("%Y%m")
}
