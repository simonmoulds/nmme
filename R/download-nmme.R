#' NMME URLs
#'
#' This function returns a character vector of NMME URLs.
#'
#' @param model Character.
#' @param simulation_type Character.
#' @param dataset Character.
#' @param variable Character.
#' @param x_lim Numeric.
#' @param y_lim Numeric.
#' @param members Integer.
#' @param lead_times Numeric.
#' @param start_year Integer.
#' @param start_month Integer.
#' @param end_year Integer.
#' @param end_month Integer.
#' @param ... Additional arguments.
#'
#' @return Character.
#'
#' @examples
#' \dontrun{
#' sum(1:10)
#' }
nmme_url <- function(model = "GFDL-SPEAR",
                     simulation_type = "hindcast",
                     dataset = "monthly",
                     variable = "prec",
                     xmin = -180,
                     xmax = 180,
                     ymin = -90,
                     ymax = 90,
                     members = NA,
                     start_lead_time = 0.5,
                     end_lead_time = 11.5,
                     start_year = 1991,
                     end_year = 2020,
                     start_month = 1,
                     end_month = 12,
                     config = iri_config(),
                     ...) {

  ## TODO make this a data file
  model_members = list("GFDL-SPEAR" = 1:15)
  if (is.na(members)) { members = model_members[[model]] }

  supported_models = names(config$sources$NMME)
  if (!model %in% supported_models) {
    stop(sprintf("Model %s not yet supported!", model))
  }
  if (!simulation_type %in% c("hindcast", "forecast")) {
    stop("`simulation_type` must be one of hindcast, forecast")
  }
  ## Check times
  if (end_year < start_year) {
    stop("`end_year` should be greater than or equal to `start_year`")
  }
  if (end_month < start_month) {
    stop("`end_month` should be greater than or equal to `start_month`")
  }
  ## Spatial query
  X = format_x(xmin, xmax)
  Y = format_y(ymin, ymax)
  ## lead-time query
  L = format_lead_times(start_lead_time, end_lead_time)
  ## Only download one year at a time
  years = seq(start_year, end_year, 1)
  ## TODO better to specify month range or month values?
  months = seq(start_month, end_month, 1)
  urls = vector(mode = "list", length=length(years))
  indx = 1
  for (i in 1:length(years)) {
    year = years[i]
    for (j in 1:length(months)) {
      month = months[j]
      for (k in 1:length(members)) {
        member = members[k]
        M = format_members(member)
        S = format_times(year, month, year, month)
        ## URL extension
        url_ext = paste(
          "SOURCES/.Models/.NMME",
          paste0(".", model),
          paste0(".", toupper(simulation_type)),
          paste0(".", toupper(dataset)),
          paste0(".", variable),
          Y, S, X, L, M,
          "data.nc",
          sep = "/"
        )
        url_ext = gsub("(/)\\1+", "\\1", url_ext)
        url_ext = gsub(" ", "%20", url_ext)
        url_ext = gsub(",", "%2C", url_ext)
        url = paste(config$base_url, url_ext, sep="/")
        ## Filename
        init_yearmon = paste0(year, formatC(month, width = 2, flag = 0))
        fcst_month_start = floor(month + min(lead_times))
        fcst_month_end = floor(month + max(lead_times))
        fcst_year_start = year + floor(fcst_month_start / 12.5)
        fcst_year_end = year + floor(fcst_month_end / 12.5)
        fcst_start_yearmon = paste0(fcst_year_start, formatC(fcst_month_start %% 12, width = 2, flag = 0))
        fcst_end_yearmon = paste0(fcst_year_end, formatC(fcst_month_end %% 12, width = 2, flag = 0))
        fname = paste0(
          variable, "_", model, "_", init_yearmon, "_",
          trimws(format(as.numeric(member), nsmall = 1)), "_",
          fcst_start_yearmon, "-", fcst_end_yearmon, ".nc"
        )
        ## Add to list
        urls[[indx]] = c(url, fname)
        indx = indx + 1
      }
    }
  }
  urls
}

download_nmme <- function(urls = nmme_url(),
                          destdir = ".",
                          overwrite = FALSE, ...) {
  for (i in 1:length(urls)) {
    url = urls[[i]][1]
    destfile = file.path(destdir, urls[[i]][2])
    download.file(url, destfile=destfile, ...)
  }
  NULL
}

format_lead_times <- function(start_lead_time, end_lead_time, ...) {
  lead_times = sort(c(start_lead_time, end_lead_time))
  valid_lead_times = seq(0.5, 11.5, 1)
  if (!all(lead_times %in% valid_lead_times)) {
    stop("`lead_times` contains invalid values")
  }
  lead_times = format(sort(lead_times), nsmall = 1)
  return(paste0("/L/(", lead_times[1], ")", "(", lead_times[2], ")RANGEEDGES"))
}

format_members <- function(members, ...) {
  if (is.na(members[1]))
    return("")
  members = format(round(members), nsmall = 1)
  members = trimws(paste(members, collapse = ", "))
  return(paste0("/M/(", members, ")VALUES"))
}

format_times <- function(start_year, start_month, end_year, end_month) {
  .fmt_time <- function(year, month) {
    if (!year %in% 1991:2020) stop("`start_year` and `end_year` must be in the range 1991-2020")
    if (!month %in% 1:12) stop("`start_month` and `end_month` must be in the range 1-12")
    return(paste0("(0000 1 ", month.abb[month], " ", year, ")"))
  }
  start_time = .fmt_time(start_year, start_month)
  end_time = .fmt_time(end_year, end_month)
  return(paste0("/S/", start_time, end_time, "RANGEEDGES"))
}

format_x <- function(xmin, xmax) {
  ## "longitude is best specified as west to east, two east values or two west values, otherwise you can end up with the wrong half of the world (e.g. 0.5E to 355.5E will work much better than 0.5E to 0.5W)"
  if (xmin > xmax)
    stop("`xmin` must be less than `xmax`")
  if (xmin == 0) {
    xmn = "0"
  } else if (xmin < 0) {
    xmn = paste0(abs(floor(xmin) + 0.5), "W")
  } else if (xmin > 0) {
    xmn = paste0(floor(xmin) + 0.5, "E")
  }
  if (xmax == 0) {
    xmx = "0"
  } else if (xmax < 0) {
    xmx = paste0(abs(ceiling(xmax)) - 0.5, "W")
  } else if (xmax > 0) {
    xmx = paste0(ceiling(xmax) - 0.5, "E")
  }
  return(paste0("/X/(", xmn, ")", "(", xmx, ")RANGEEDGES"))
}

format_y <- function(ymin, ymax) {
  myfun <- function(y) {
    if (y < 0) {
      return(paste0(abs(y), "S"))
    } else {
      return(paste0(y, "N"))
    }
  }
  if (ymin > ymax)
    stop("`ymin` must be less than `ymax`")
  ymn = myfun(ymin)
  ymx = myfun(ymax)
  return(paste0("/Y/(", ymn, ")", "(", ymx, ")RANGEEDGES"))
}
