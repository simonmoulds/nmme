#' IRI URLs
#'
#' This function returns a list of character pairs. The
#' first element of each character pair is an IRI Climate
#' Data Library URL for an NMME subset. The second element
#' is the default destination filename.
#'
#' @param model Character.
#' @param simulation_type Character.
#' @param dataset Character.
#' @param config List.
#' @param ... Additional arguments.
#'
#' @return List of character pairs.
#'
#' @examples
#' \dontrun{
#' urls = name_urls()
#' download_nmme(urls[1])
#' }
nmme_url <- function(model = "GFDL-SPEAR",
                     simulation_type = "hindcast",
                     dataset = "monthly",
                     ## variable = "prec",
                     ## start_init_time = c(1991, 1),
                     ## end_init_time = c(2020, 12),
                     ## variables = NA,
                     ## point = NA,
                     ## extent = NA,
                     ## members = NA,
                     ## lead_times = NA,
                     config = nmme_config(),
                     ...) {

  ## This will check the selection is valid at the same time
  model_config = get_model_config(model, simulation_type, dataset, config)
  url_ext = list(
    source = "SOURCES/.Models/.NMME",
    model = paste0(".", toupper(model)),
    simulation_type = paste0(".", toupper(simulation_type)),
    dataset = paste0(".", toupper(dataset))
  )
  ## url = paste(config$base_url, url_ext, sep="/")
  ## TODO make this an S3 object?
  list(url = url,
       spec = list(model = model),
       config = model_config)
}

#' @param x
#' @param start_init_time Integer.
#' @param end_init_time Integer.
filter_init_time <- function(x,
                             start_init_time = as.yearmon("2000-01"),
                             end_init_time = as.yearmon("2000-02")) {

  start_init_time = zoo::as.yearmon(start_init_time)
  end_init_time = zoo::as.yearmon(end_init_time)
  check_init_times(start_init_time, end_init_time, model_config)
  init_times = as.yearmon(seq(as.Date(start_init_time), as.Date(end_init_time), by = "month"))
  S = rep(NA, length(init_times))
  for (i in 1:length(init_times)) {
    tm = init_times[i]
    month = format(tm, "%b")
    year = format(tm, "%Y")
    S[i] = paste0("S/(0000 1 ", month, " ", year, ")VALUES")
  }
  x$url$init_times = S
  x$spec$init_times = init_times %>% format("%Y%m")
  x
}

#' @param x
#' @param members Integer.
filter_member <- function(x, members) {
  c1 = check_members(members, x$config)
  members = trimws(format(round(members), nsmall = 1))
  M = rep(NA, length(members))
  for (i in 1:length(members)) {
    M[i] = paste0("/M/(", members[i], ")VALUES")
  }
  x$url$members = M
  x$spec$members = members
  x
}

#' @param x
#' @param lead_times Numeric.
filter_lead_time <- function(x, lead_times) {
  lead_times = sort(lead_times)
  c1 = check_lead_times(lead_times, x$config)
  lead_times = trimws(format(lead_times, nsmall = 1))
  lead_times = paste0(lead_times, collapse = ", ")
  L = paste0("/L/(", lead_times, ")VALUES")
  x$url$lead_times = L
  x$spec$lead_times = lead_times
  x
}

filter_point <- function(x, xcoord, ycoord) {
  x
}

filter_x_range <- function(xmin, xmax) {
  ## "longitude is best specified as west to east, two east values or two west values, otherwise you can end up with the wrong half of the world (e.g. 0.5E to 355.5E will work much better than 0.5E to 0.5W)"
  if (xmin > xmax)
    stop("`xmin` must be less than `xmax`")
  if (xmin == 0) {
    xmn = "0"
  } else if (xmin < 0) {
    xmn = paste0(trimws(format(abs(floor(xmin)) + 0.5), nsmall = 1)), "W")
  } else if (xmin > 0) {
    xmn = paste0(trimws(format(floor(xmin) + 0.5, nsmall = 1)), "E")
  }
  if (xmax == 0) {
    xmx = "0"
  } else if (xmax < 0) {
    xmx = paste0(trimws(format(abs(ceiling(xmax)) - 0.5, nsmall = 1)), "W")
  } else if (xmax > 0) {
    xmx = paste0(trimws(format(ceiling(xmax) - 0.5, nsmall = 1)), "E")
  }
  return(paste0("/X/(", xmn, ")", "(", xmx, ")RANGEEDGES"))
}

filter_y_range <- function(ymin, ymax) {
  myfun <- function(y) {
    if (y < 0) {
      return(paste0(trimws(format(abs(y), nsmall = 1)), "S"))
    } else {
      return(paste0(trimws(format(y, nsmall = 1)), "N"))
    }
  }
  if (ymin > ymax)
    stop("`ymin` must be less than `ymax`")
  ymn = myfun(ymin)
  ymx = myfun(ymax)
  return(paste0("/Y/(", ymn, ")", "(", ymx, ")RANGEEDGES"))
}

#' @param x
#' @param xmin Numeric.
#' @param xmax Numeric.
#' @param ymin Numeric.
#' @param ymax Numeric.
filter_region <- function(x, xmin, xmax, ymin, ymax) {
  ## TODO check extent is valid - are all models global?
  X = filter_x_range(xmin, xmax)
  Y = filter_y_range(ymin, ymax)
  x$url$X = X
  x$url$Y = Y
  x$spec$X = c(xmin, xmax)
  x$spec$Y = c(ymin, ymax)
  x
}

filter_x_point <- function(x) {
  if (x == 0) {
    xv = "0"
  } else if (x < 0) {
    xv = paste0(trimws(format(abs(x), nsmall = 1)), "W")
  } else if (xmin > 0) {
    xv = paste0(trimws(format(c, nsmall = 1)), "E")
  }
  return(paste0("/X/(", xv, ")VALUES"))
}

filter_y_point <- function(y) {
  myfun <- function(y) {
    if (y < 0) {
      return(paste0(trimws(format(abs(y), nsmall = 1)), "S"))
    } else {
      return(paste0(trimws(format(y, nsmall = 1)), "N"))
    }
  }
  yv = myfun(y)
  return(paste0("/Y/(", yv, ")VALUES"))
}

#' @param x
#' @param xc Numeric.
#' @param yc Numeric.
filter_point <- function(x, xc, yc) {
  X = filter_x_point(xmin, xmax)
  Y = filter_y_point(ymin, ymax)
  x$url$x = X
  x$url$y = Y
  x$spec$X = c(xmin, xmax)
  x$spec$Y = c(ymin, ymax)
  x
}

## filter <- function(x, members, lead_times, xcoord, ycoord, xmin, xmax, ymin, ymax, ...) {
##   x
## }

check_model <- function(model, config) {
  if (!model %in% names(config)) {
    stop(sprintf("`model` %s not yet supported!", model))
  }
  TRUE
}

check_simulation_type <- function(simulation_type, model, config) {
  if (!simulation_type %in% names(config[[model]])) {
    stop(sprintf("`simulation_type` %s not yet supported!", simulation_type))
  }
  TRUE
}

check_dataset <- function(dataset, simulation_type, model, config) {
  if (!dataset %in% names(config[[model]][[simulation_type]])) {
    stop(sprintf("`dataset` %s not yet supported!", dataset))
  }
  TRUE
}

check_variable <- function(variable, model_config) {
  if (!is.na(variable)) {
    if (!all(variable %in% model_spec$variables)) {
      stop("Some `variables` are invalid!")
    }
  }
  TRUE
}

check_members <- function(members, model_config) {
  if (!is.na(members)) {
    if (!all(members %in% model_spec$members)) {
      stop("Some `members` are invalid!")
    }
  }
  TRUE
}

check_lead_times <- function(lead_times, model_config) {
  if (!is.na(lead_times)) {
    if (!lead_times %in% model_spec$lead_times) {
      stop("`start_lead_time` is invalid!")
    }
  }
  TRUE
}

check_init_times <- function(start_init_time, end_init_time, model_config) {
  if (!is.na(start_init_time)) {
    if (start_init_time < model_config$start_init_time) {
      stop(
        "`start_init_time` is invalid: earliest available time is %s.",
        as.character(model_config$start_init_time)
      )
    }
  }
  if (!is.na(end_init_time)) {
    if (end_init_time > model_config$end_init_time) {
      stop(
        "`end_init_time` is invalid: latest available time is %s.",
        as.character(model_config$end_init_time)
      )
    }
  }
  TRUE
}

get_model_config <- function(model, simulation_type, dataset, config, check = TRUE) {
  ## Apply consistent formatting
  model <- tolower(model)
  simulation_type <- tolower(simulation_type)
  dataset <- tolower(dataset)
  ## variable <- tolower(variable)
  if (check) {
    c1 = check_model(model, config)
    c2 = check_simulation_type(simulation_type, model, config)
    c3 = check_dataset(dataset, simulation_type, model, config)
    ## TODO logging?
  }
  model_config = config[[model]][[simulation_type]][[dataset]]
  model_config
}

## format_lead_times <- function(start_lead_time, end_lead_time, ...) {
##   lead_times = sort(c(start_lead_time, end_lead_time))
##   valid_lead_times = seq(0.5, 11.5, 1)
##   if (!all(lead_times %in% valid_lead_times)) {
##     stop("`lead_times` contains invalid values")
##   }
##   lead_times = format(sort(lead_times), nsmall = 1)
##   return(paste0("/L/(", lead_times[1], ")", "(", lead_times[2], ")RANGEEDGES"))
## }

## format_members <- function(members, ...) {
##   if (is.na(members[1]))
##     return("")
##   members = format(round(members), nsmall = 1)
##   members = trimws(paste(members, collapse = ", "))
##   return(paste0("/M/(", members, ")VALUES"))
## }


##   ## c4 = check_members(members, config[[model]][[simulation_type]][[dataset]])
##   ## c5 = check_lead_times(members, config[[model]][[simulation_type]][[dataset]])
##   ## model_config = get_model_config(model, simulation_type, dataset, variable)
##   start_init_time = zoo::as.yearmon(start_init_time)
##   end_init_time = zoo::as.yearmon(end_init_time)
##   check_init_times(start_init_time, end_init_time, model_config)
##   if (start_init_time > end_init_time) {
##     stop("`end_init_time` should be greater than or equal to `start_init_time`")
##   }
##   ## Spatial query
##   X = format_x(xmin, xmax)
##   Y = format_y(ymin, ymax)
##   ## lead-time query
##   L = format_lead_times(start_lead_time, end_lead_time)
##   ## Only download one year at a time
##   years = seq(start_year, end_year, 1)
##   ## TODO better to specify month range or month values?
##   months = seq(start_month, end_month, 1)
##   urls = vector(mode = "list", length=length(years))
##   indx = 1
##   for (i in 1:length(years)) {
##     year = years[i]
##     for (j in 1:length(months)) {
##       month = months[j]
##       for (k in 1:length(members)) {
##         member = members[k]
##         M = format_members(member)
##         S = format_times(year, month, year, month)
##         ## URL extension
##         url_ext = paste(
##           "SOURCES/.Models/.NMME",
##           paste0(".", model),
##           paste0(".", toupper(simulation_type)),
##           paste0(".", toupper(dataset)),
##           ## paste0(".", variable),
##           ## Y, S, X, L, M,
##           ## "data.nc",
##           sep = "/"
##         )
##         url_ext = gsub("(/)\\1+", "\\1", url_ext)
##         url_ext = gsub(" ", "%20", url_ext)
##         url_ext = gsub(",", "%2C", url_ext)
##         url = paste(config$base_url, url_ext, sep="/")
##         ## Filename
##         init_yearmon = paste0(year, formatC(month, width = 2, flag = 0))
##         fcst_month_start = floor(month + min(lead_times))
##         fcst_month_end = floor(month + max(lead_times))
##         fcst_year_start = year + floor(fcst_month_start / 12.5)
##         fcst_year_end = year + floor(fcst_month_end / 12.5)
##         fcst_start_yearmon = paste0(fcst_year_start, formatC(fcst_month_start %% 12, width = 2, flag = 0))
##         fcst_end_yearmon = paste0(fcst_year_end, formatC(fcst_month_end %% 12, width = 2, flag = 0))
##         fname = paste0(
##           variable, "_", model, "_", init_yearmon, "_",
##           trimws(format(as.numeric(member), nsmall = 1)), "_",
##           fcst_start_yearmon, "-", fcst_end_yearmon, ".nc"
##         )
##         ## Add to list
##         urls[[indx]] = c(url, fname)
##         indx = indx + 1
##       }
##     }
##   }
##   urls
## }
