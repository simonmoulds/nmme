#' Filter by initialization time
#'
#' @param x
#' @param start_init_time zoo::yearmon. Start initialization time.
#' @param end_init_time zoo::yearmon. End initialization time.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' print("Hello, world!")
#' }
filter_init_time <- function(x,
                             start_init_time = as.yearmon("2000-01"),
                             end_init_time = as.yearmon("2000-02")) {

  start_init_time = zoo::as.yearmon(start_init_time)
  end_init_time = zoo::as.yearmon(end_init_time)
  check_init_times(start_init_time, end_init_time, x$config)
  init_times = seq(as.Date.yearmon(start_init_time), as.Date.yearmon(end_init_time), by = "month")
  S = rep(NA, length(init_times))
  for (i in 1:length(init_times)) {
    tm = init_times[i]
    month = format(tm, "%b")
    year = format(tm, "%Y")
    S[i] = paste0("S/(0000 1 ", month, " ", year, ")VALUES")
  }
  init_time_filter = tibble(name = init_times %>% format("%Y%m"), filter = S)
  x$filter$S = init_time_filter
  x
}

#' Filter by members
#'
#' @param x
#' @param members Integer.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' print("Hello, world!")
#' }
filter_member <- function(x, members) {
  c1 = check_members(members, x$config)
  members = trimws(format(round(members), nsmall = 1))
  M = rep(NA, length(members))
  for (i in 1:length(members)) {
    M[i] = paste0("/M/(", members[i], ")VALUES")
  }
  member_filter = tibble(name = members, filter = M)
  x$filter$M = member_filter
  x
}

#' Filter by lead times
#'
#' @param x
#' @param lead_times Numeric.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' print("Hello, world!")
#' }
filter_lead_time <- function(x, lead_times) {
  lead_times = sort(lead_times)
  c1 = check_lead_times(lead_times, x$config)
  lead_times = trimws(format(lead_times, nsmall = 1))
  lead_times_joined = paste0(lead_times, collapse = ", ")
  L = paste0("/L/(", lead_times_joined, ")VALUES")
  ## x$url$lead_times = L
  ## x$spec$lead_times = lead_times
  lead_time_filter = tibble(name = lead_times, filter = L)
  x$filter$L = lead_time_filter
  x
}

format_y_coord <- function(ycoord) {
  if (xcoord == 0) {
    yv = "0"
  } else if (ycoord < 0) {
    yv = paste0(trimws(format(abs(ycoord), nsmall = 1)), "S")
  } else {
    yv = paste0(trimws(format(ycoord, nsmall = 1)), "N")
  }
  return(yv)
}

format_x_coord <- function(xcoord) {
  if (xcoord == 0) {
    xv = "0"
  } else if (xcoord < 0) {
    xv = paste0(trimws(format(abs(xcoord), nsmall = 1)), "W")
  } else if (xcoord > 0) {
    xv = paste0(trimws(format(xcoord, nsmall = 1)), "E")
  }
  return(xv)
}
format_xmin_coord <- function(xmin) {
  if (xmin == 0) {
    xmn = "0"
  } else if (xmin < 0) {
    xmn = paste0(trimws(format(abs(floor(xmin)) + 0.5, nsmall = 1)), "W")
  } else if (xmin > 0) {
    xmn = paste0(trimws(format(floor(xmin) + 0.5, nsmall = 1)), "E")
  }
  return(xmn)
}

format_xmax_coord <- function(xmax) {
  if (xmax == 0) {
    xmx = "0"
  } else if (xmax < 0) {
    xmx = paste0(trimws(format(abs(ceiling(xmax)) - 0.5, nsmall = 1)), "W")
  } else if (xmax > 0) {
    xmx = paste0(trimws(format(ceiling(xmax) - 0.5, nsmall = 1)), "E")
  }
  return(xmx)
}

filter_x_range <- function(xmin, xmax) {
  ## "longitude is best specified as west to east, two east values or two west values, otherwise you can end up with the wrong half of the world (e.g. 0.5E to 355.5E will work much better than 0.5E to 0.5W)"
  if (xmin > xmax)
    stop("`xmin` must be less than `xmax`")
  xmn = format_xmin_coord(xmin)
  xmx = format_xmax_coord(xmax)
  return(paste0("/X/(", xmn, ")", "(", xmx, ")RANGEEDGES"))
}

filter_y_range <- function(ymin, ymax) {
  if (ymin > ymax)
    stop("`ymin` must be less than `ymax`")
  ymn = format_y_coord(ymin)
  ymx = format_y_coord(ymax)
  return(paste0("/Y/(", ymn, ")", "(", ymx, ")RANGEEDGES"))
}

#' Filter by region.
#'
#' @param x
#' @param xmin Numeric.
#' @param xmax Numeric.
#' @param ymin Numeric.
#' @param ymax Numeric.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' print("Hello, world!")
#' }
filter_region <- function(x, xmin, xmax, ymin, ymax) {
  ## TODO check extent is valid - are all models global?
  X = filter_x_range(xmin, xmax)
  Y = filter_y_range(ymin, ymax)
  xnm = paste0(format_x_coord(xmin), "-", format_x_coord(xmax))
  ynm = paste0(format_y_coord(ymin), "-", format_y_coord(ymax))
  x_filter = tibble(name = xnm, filter = X)
  y_filter = tibble(name = ynm, filter = Y)
  x$filter$X = x_filter
  x$filter$Y = y_filter
  x
}

filter_x_point <- function(xcoord) {
  xv = format_x_coord(xcoord)
  return(paste0("/X/(", xv, ")VALUES"))
}

filter_y_point <- function(ycoord) {
  yv = format_y_coord(ycoord)
  return(paste0("/Y/(", yv, ")VALUES"))
}

#' Filter by point.
#'
#' @param x
#' @param xcoord Numeric.
#' @param ycoord Numeric.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' print("Hello, world!")
#' }
filter_point <- function(x, xcoord, ycoord) {
  X = filter_x_point(xcoord)
  Y = filter_y_point(ycoord)
  x_filter = tibble(name = format_x_coord(xcoord), filter = X)
  y_filter = tibble(name = format_y_coord(ycoord), filter = Y)
  x$filter$X = x_filter
  x$filter$Y = y_filter
  x
}

check_members <- function(members, model_config) {
  if (!all(members %in% model_config$members)) {
    stop("Some `members` are invalid!")
  }
  TRUE
}

check_lead_times <- function(lead_times, model_config) {
  if (!all(lead_times %in% model_config$lead_times)) {
    stop("`start_lead_time` is invalid!")
  }
  TRUE
}

check_init_times <- function(start_init_time, end_init_time, model_config) {
  if (start_init_time < model_config$start_init_time) {
    stop(
      "`start_init_time` is invalid: earliest available time is %s.",
      as.character(model_config$start_init_time)
    )
  }
  if (end_init_time > model_config$end_init_time) {
    stop(
      "`end_init_time` is invalid: latest available time is %s.",
      as.character(model_config$end_init_time)
    )
  }
  TRUE
}
