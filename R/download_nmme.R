#' NMME URLs
#'
#' This function returns a character vector of NMME URLs.
#'
#' @param urls List of character pairs. The first value of each pair
#'   should be the URL, the second value should be the destination
#'   filename.
#' @param destdir Character. Destination directory, which is created
#'   if it doesn't already exist.
#' @param overwrite Logical. Whether or not files should be downloaded
#'   again if they already exist.
#' @param ... Additional arguments to download.file.
#'
#' @return NULL.
#'
#' @examples
#' \dontrun{
#' urls = name_urls()
#' download_nmme(urls[1])
#' }
download_nmme <- function(x,
                          destdir = ".",
                          overwrite = FALSE, ...) {

  urls = x %>% construct_urls()
  n_urls = nrow(urls)
  for (i in 1:n_urls) {
    url = urls$url[i]
    destfile = file.path(destdir, urls$destfile[i])
    if (overwrite | !file.exists(destfile)) {
      download.file(url, destfile, ...)
    }
  }
  NULL
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

construct_urls <- function(x) {
  base_url = paste(
    iri_spec$base_url,
    x$dataset$source$filter,
    x$dataset$model$filter,
    x$dataset$simulation_type$filter,
    x$dataset$dataset$filter,
    x$dataset$variable$filter,
    sep = "/"
  )
  init_times = x$filter$S
  members = x$filter$M
  n_init_times = nrow(init_times)
  n_members = nrow(members)

  ## Pull out the other filters
  Y = x$filter$Y
  X = x$filter$X
  L = x$filter$L
  if (all(is.na(L))) {
    ## We need to know the min/max lead times
    min_lead_time = min(x$config$lead_times)
    max_lead_time = max(x$config$lead_times)
  } else {
    min_lead_time = as.numeric(L$min_lead_time)
    max_lead_time = as.numeric(L$max_lead_time)
  }

  n_urls = n_init_times * n_members
  urls = rep(NA, n_urls)
  destfiles = rep(NA, n_urls)
  indx = 1
  for (i in 1:n_init_times) {
    S = init_times$filter[i]
    init_time = init_times$name[i]
    forecast_start = get_forecast_start(init_time, min_lead_time)
    forecast_end = get_forecast_end(init_time, max_lead_time)
    for (j in 1:n_members) {
      M = members$filter[i]
      member = members$name[i]
      ## URL order: Y/S/X/L/M
      url = base_url
      url = ifelse(isTRUE(is.na(Y)), url, paste0(url, "/", Y$filter))
      url = paste0(url, "/", S)
      url = ifelse(isTRUE(is.na(X)), url, paste0(url, "/", X$filter))
      url = ifelse(isTRUE(is.na(L)), url, paste0(url, "/", L$filter))
      url = paste0(url, "/", M)
      url = paste0(url, "/data.nc")
      url = gsub("(/)\\1+", "\\1", url)
      url = gsub(" ", "%20", url)
      url = gsub(",", "%2C", url)

      ## Filename
      destfile = paste0(
        x$dataset$variable$name, "_",
        x$dataset$model$name, "_",
        init_time, "_",
        member, "_",
        forecast_start, "-",
        forecast_end, ".nc"
      )
      urls[indx] = url
      destfiles[indx] = destfile
      indx = indx + 1
    }
  }
  urls = tibble(url = urls, destfile = destfiles)
  urls
  ## x$urls = urls
  ## x
}
