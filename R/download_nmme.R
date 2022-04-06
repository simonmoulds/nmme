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
#' @param ... Additional arguments.
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

  base_url = iri_spec$base_url
  source = x$url$source
  variable = x$url$variable
  init_times = x$url$init_times
  members = x$url$members
  for (i in 1:length(init_times)) {
    for (j in 1:length(members)) {
      destfile = file.path(destdir, urls[[i]][2])
      download.file(url, destfile=destfile, ...)
    }
  }
  NULL
}

get_forecast_start <- function(init_time, lead_time) {
  tm = paste(init_time, "01", sep = "") %>% as.Date(format = "%Y%m%d")
  lead_time = as.numeric(lead_time)
  fcst_start = tm + month(ceiling(min(lead_time))) - month(1)
  ## fcst_end = tm + month(ceiling(max(lead_time))) - month(1)
  fcst_start
}

get_forecast_end <- function(init_time, lead_time) {
  tm = paste(init_time, "01", sep = "") %>% as.Date(format = "%Y%m%d")
  lead_time = as.numeric(lead_time)
  fcst_end = tm + month(ceiling(max(lead_time))) - month(1)
  fcst_end
}

construct_urls <- function(x) {

  ## TODO this needs a lot more work!

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
  n_init_times = length(init_times)
  n_members = length(members)
  Y = x$filter$Y
  X = x$filter$X
  L = x$filter$L
  if (all(is.na(L)))
    L = x$config$lead_times

  urls = list()
  for (i in 1:n_init_times) {
    S = init_times$filter[i]
    init_time = init_times$name[i]
    for (j in 1:n_members) {
      M = members$filter[i]
      member = members$name[i]
      ## URL order: Y/S/X/L/M
      url = base_url
      url = ifelse(is.na(Y), url, paste0(url, "/", Y))
      url = paste0(url, "/", S)
      url = ifelse(is.na(X), url, paste0(url, "/", X))
      url = ifelse(is.na(L), url, paste0(url, "/", L))
      url = paste0(url, "/", M)

      ## Filename
      destfile = paste0(
        x$dataset$variable$name, "_",
        x$dataset$model$name, "_",
        init_time, "_",
        member, "_")
    }
  }
  NULL
}

construct_filename <- function(...) {
  NULL
}
