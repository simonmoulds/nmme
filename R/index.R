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
  ## L = x$dataset$L
  ## min_lead_time = min(as.numeric(L$subset))
  ## max_lead_time = max(as.numeric(L$subset))
  include_X = !isTRUE(is.na(X))
  include_Y = !isTRUE(is.na(Y))
  ## include_L = !(isTRUE(is.na(L)) | (length(L$subset) == length(x$config$lead_times)))
  ## Construct URLs
  n_urls = n_init_times * n_members
  urls = rep(NA, n_urls)
  destfiles = rep(NA, n_urls)
  exists = rep(FALSE, n_urls)
  init_time_index = rep(NA, n_urls)
  member_index = rep(NA, n_urls)
  indx = 1
  ## TODO speed up this loop, which is currently very slow
  for (i in 1:n_init_times) {
    init_time_ext = S$ext[i]
    init_time = S$subset[i]
    ## forecast_start = get_forecast_start(init_time, min_lead_time)
    ## forecast_end = get_forecast_end(init_time, max_lead_time)
    for (j in 1:n_members) {
      member_ext = M$ext[j]
      member = M$subset[j]
      for (k in 1:n_lead_times) {
        ## URL order: Y/S/X/L/M
        lead_time_ext <- L$ext[k]
        lead_time <- as.numeric(L$subset[k])
        forecast_start = get_forecast_start(init_time, lead_time)
        forecast_end = get_forecast_end(init_time, lead_time)
        url = base_url
        url = ifelse(include_Y, paste0(url, "/", Y$ext), url)
        url = paste0(url, "/", init_time_ext)
        url = ifelse(include_X, paste0(url, "/", X$ext), url)
        ## url = ifelse(include_L, paste0(url, "/", L$ext), url)
        url = paste0(url, "/", lead_time_ext)
        url = paste0(url, "/", member_ext)
        url = paste0(url, "/data.nc")
        url = gsub("(/)\\1+", "\\1", url)
        url = gsub(" ", "%20", url)
        url = gsub(",", "%2C", url)
        ## Filename
        destfile = paste0(
          x$dataset$variable$subset, "_",
          x$dataset$model$subset, "_",
          format(init_time, "%Y%m"), "_",
          member, "_",
          format(forecast_start, "%Y%m"), "-",
          format(forecast_end, "%Y%m"), ".nc"
        )
        urls[indx] = url
        destfiles[indx] = file.path(location, destfile)
        exists[indx] = isTRUE(file.exists(destfiles[indx]))
        init_time_index[indx] = i
        member_index[indx] = j
        indx = indx + 1
      }
    }
  }
  urls = tibble(
    url = urls,
    destfile = destfiles,
    exists = exists,
    init_time_index = init_time_index,
    member_index = member_index
  )
  urls
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
