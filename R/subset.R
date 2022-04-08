#' Subset nmme object.
#'
#' @param x nmme object.
#' @inheritDotParams subset_init_times
#' @inheritDotParams subset_members
#' @inheritDotParams subset_lead_times
#' @inheritDotParams subset_point
#' @inheritDotParams subset_region
subset <- function(x, ...) {
  UseMethod("subset")
}

#' @export
subset.nmme <- function(x, ...) {
  dots = list(...)
  nms = names(dots)
  x <- x
  if ("start_init_time" %in% nms & "end_init_time" %in% nms)
    x$dataset$S <- subset_init_times(dots$start_init_time, dots$end_init_time, x$config)
  if ("members" %in% nms)
    x$dataset$M <- subset_members(dots$members, x$config)
  if ("lead_times" %in% nms)
    x$dataset$L <- subset_lead_times(dots$lead_times, x$config)
  use_subset_point = all(c("xcoord", "ycoord") %in% nms)
  use_subset_region = all(c("xmin", "xmax", "ymin", "ymax") %in% nms)
  if (use_subset_point & use_subset_region) {
    stop("Cannot specify spatial point (`xcoord` and `ycoord`) and region (`xmin`, `xmax`, `ymin`, `ymax`)")
  }
  if (use_subset_point) {
    point_selection <- subset_point(dots$xcoord, dots$ycoord)
  }
  if (use_subset_region) {
    region_selection <- subset_region(dots$xmin, dots$xmax, dots$ymin, dots$ymax)
    x$dataset$X <- region_selection$X
    x$dataset$Y <- region_selection$Y
  }
  x <- reindex(x)
  x
}

#' @export
`[.nmme` <- function(x, i) {
  indices <- x$index[i,]
  init_time_index <- indices$init_time_index
  member_index <- indices$member_index
  ## init_time_index <- x$index$init_time_index[i] %>% unique()
  ## member_index <- x$index$member_index[i] %>% unique()
  S = x$dataset$S
  M = x$dataset$M
  S$subset <- S$subset[init_time_index]
  S$ext <- S$ext[init_time_index]
  M$subset <- M$subset[member_index]
  M$ext <- M$ext[member_index]
  x$dataset$S <- S
  x$dataset$M <- M
  x <- reindex(x)
  x
}

#' Subset by initialization time
#'
#' @param start_init_time zoo::yearmon. Start initialization time.
#' @param end_init_time zoo::yearmon. End initialization time.
#' @param config TODO.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' print("Hello, world!")
#' }
subset_init_times <- function(start_init_time, end_init_time, config) {
  start_init_time = zoo::as.yearmon(start_init_time)
  end_init_time = zoo::as.yearmon(end_init_time)
  check_init_times(start_init_time, end_init_time, config)
  init_times = seq(as.Date.yearmon(start_init_time), as.Date.yearmon(end_init_time), by = "month")
  S = rep(NA, length(init_times))
  for (i in 1:length(init_times)) {
    tm = init_times[i]
    month = format(tm, "%b")
    year = format(tm, "%Y")
    S[i] = paste0("S/(0000 1 ", month, " ", year, ")VALUES")
  }
  nms <- init_times %>% format("%Y%m")
  return(list(subset = nms, ext = S))
}

#' Subset by members
#'
#' @param members Integer.
#' @param config TODO
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' print("Hello, world!")
#' }
subset_members <- function(members, config) {
  c1 = check_members(members, config)
  members = trimws(format(round(members), nsmall = 1))
  M = rep(NA, length(members))
  for (i in 1:length(members)) {
    M[i] = paste0("M/(", members[i], ")VALUES")
  }
  return(list(subset = members, ext = M))
}

#' Subset by lead times
#'
#' @param lead_times Numeric.
#' @param config TODO
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' print("Hello, world!")
#' }
subset_lead_times <- function(lead_times, config) {
  lead_times = sort(lead_times)
  c1 = check_lead_times(lead_times, config)
  ## lead_times = trimws(format(lead_times, nsmall = 1))
  lead_times_joined = paste0(trimws(format(lead_times, nsmall = 1)), collapse = ", ")
  L = paste0("L/(", lead_times_joined, ")VALUES")
  ## x$url$lead_times = L
  ## x$spec$lead_times = lead_times
  return(list(subset = lead_times, ext = L))
}

format_y_coord <- function(ycoord) {
  if (ycoord == 0) {
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

subset_x_range <- function(xmin, xmax) {
  ## "longitude is best specified as west to east, two east values or two west values, otherwise you can end up with the wrong half of the world (e.g. 0.5E to 355.5E will work much better than 0.5E to 0.5W)"
  if (xmin > xmax)
    stop("`xmin` must be less than `xmax`")
  xmn = format_xmin_coord(xmin)
  xmx = format_xmax_coord(xmax)
  return(paste0("X/(", xmn, ")", "(", xmx, ")RANGEEDGES"))
}

subset_y_range <- function(ymin, ymax) {
  if (ymin > ymax)
    stop("`ymin` must be less than `ymax`")
  ymn = format_y_coord(ymin)
  ymx = format_y_coord(ymax)
  return(paste0("Y/(", ymn, ")", "(", ymx, ")RANGEEDGES"))
}

#' Subset by region.
#'
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
subset_region <- function(xmin, xmax, ymin, ymax) {
  ## TODO check extent is valid - are all models global?
  X = subset_x_range(xmin, xmax)
  Y = subset_y_range(ymin, ymax)
  xnm = paste0(format_x_coord(xmin), "-", format_x_coord(xmax))
  ynm = paste0(format_y_coord(ymin), "-", format_y_coord(ymax))
  x_subset = list(subset = xnm, ext = X)
  y_subset = list(subset = ynm, ext = Y)
  return(list(X = x_subset, Y = y_subset))
}

subset_x_point <- function(xcoord) {
  xv = format_x_coord(xcoord)
  return(paste0("X/(", xv, ")VALUES"))
}

subset_y_point <- function(ycoord) {
  yv = format_y_coord(ycoord)
  return(paste0("Y/(", yv, ")VALUES"))
}

#' Subset by point.
#'
#' @param xcoord Numeric.
#' @param ycoord Numeric.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' print("Hello, world!")
#' }
subset_point <- function(xcoord, ycoord) {
  X = subset_x_point(xcoord)
  Y = subset_y_point(ycoord)
  x_subset = tibble(subset = format_x_coord(xcoord), ext = X)
  y_subset = tibble(subset = format_y_coord(ycoord), ext = Y)
  return(list(X = x_subset, Y = y_subset))
}
