#' Subset nmme object.
#'
#' @param x nmme object.
#' @inheritDotParams nmme_subset_init_times
#' @inheritDotParams nmme_subset_members
#' @inheritDotParams nmme_subset_lead_times
#' @inheritDotParams nmme_subset_point
#' @inheritDotParams nmme_subset_region
nmme_subset <- function(x, ...) {
  dots = list(...)
  nms = names(dots)
  x <- x
  if ("start_init_time" %in% nms & "end_init_time" %in% nms)
    x$dataset$S <- nmme_subset_init_times(dots$start_init_time, dots$end_init_time, x$config)
  if ("members" %in% nms)
    x$dataset$M <- nmme_subset_members(dots$members, x$config)
  if ("lead_times" %in% nms)
    x$dataset$L <- nmme_subset_lead_times(dots$lead_times, x$config)
  use_nmme_subset_point = all(c("xcoord", "ycoord") %in% nms)
  use_nmme_subset_region = all(c("xmin", "xmax", "ymin", "ymax") %in% nms)
  if (use_nmme_subset_point & use_nmme_subset_region) {
    stop("Cannot specify spatial point (`xcoord` and `ycoord`) and region (`xmin`, `xmax`, `ymin`, `ymax`)")
  }
  if (use_nmme_subset_point) {
    point_selection <- nmme_subset_point(dots$xcoord, dots$ycoord)
  }
  if (use_nmme_subset_region) {
    region_selection <- nmme_subset_region(dots$xmin, dots$xmax, dots$ymin, dots$ymax)
    x$dataset$X <- region_selection$X
    x$dataset$Y <- region_selection$Y
  }
  x <- nmme_reindex(x)
  x
}

#' @export
dim.nmme <- function(x) {
  n_init_times <- length(x$dataset$S$subset)
  n_members <- length(x$dataset$M$subset)
  n_lead_times <- length(x$dataset$L$subset)
  dims <- c(n_init_times, n_members, n_lead_times)
  names(dims) <- c("init_time", "member", "lead_time")
  return(dims)
}

init_times <- function(x) return(x$dataset$S$subset)
members <- function(x) return(x$dataset$M$subset)
lead_times <- function(x) return(x$dataset$L$subset)

#' @export
`[.nmme` <- function(x, i, j, k, m, n, drop = FALSE, ...) {
  ## See https://github.com/tidyverse/tibble/blob/main/R/subsetting.R
  if (missing(i)) {
    i <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }

  if (missing(j)) {
    j <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  if (missing(k)) {
    k <- NULL
  } else if (is.null(k)) {
    k <- integer()
  }

  if (missing(m)) {
    m <- NULL
  } else if (is.null(m)) {
    m <- integer()
  }

  if (missing(n)) {
    n <- NULL
  } else if (is.null(n)) {
    n <- integer()
  }

  ## arg_list = list()
  S <- x$dataset$S
  if (!is.null(i)) {
    ## start_init_time <- init_times(x)[min(i)]
    ## end_init_time <- init_times(x)[max(i)]
    S$subset <- S$subset[i]
    ## S$ext <- S$ext[i]
    S$ext <- nmme_init_times_ext(S$subset)
  }

  M <- x$dataset$M
  if (!is.null(j)) {
    M$subset <- M$subset[j]
    ## M$ext <- M$ext[j]
    M$ext <- nmme_members_ext(M$subset)
  }

  L <- x$dataset$L
  if (!is.null(k)) {
    L$subset <- L$subset[k]
    ## L$ext <- L$ext[k]
    L$ext <- nmme_lead_times_ext(L$subset)
  }

  X <- x$dataset$X
  if (!is.null(m)) {
    x_coords <- X$subset
    x_index <- m
    ## x_index <- nmme_x_index(x, xcoord = m)
    if (all(diff(x_index) == 1)) {
      xmin = min(x_coords[x_index])
      xmax = max(x_coords[x_index])
      X <- nmme_subset_x_range(xmin, xmax)
    } else {
      x_coords_new <- x_coords[x_index] %>% sort()
      X <- nmme_subset_x_point(xcoord = x_coords_new)
    }
  }

  Y <- x$dataset$Y
  if (!is.null(n)) {
    y_coords <- Y$subset
    y_index <- n
    ## y_index <- nmme_y_index(x, ycoord = n)
    if (all(diff(y_index) == 1)) {
      ymin = min(y_coords[y_index])
      ymax = max(y_coords[y_index])
      Y <- nmme_subset_y_range(ymin, ymax)
    } else {
      y_coords_new <- y_coords[y_index] %>% sort()
      Y <- nmme_subset_y_point(ycoord = y_coords_new)
    }
  }

  x$dataset$S <- S
  x$dataset$M <- M
  x$dataset$L <- L
  x$dataset$X <- X
  x$dataset$Y <- Y
  x <- nmme_reindex(x) # This will update URL request
  x
}

## These functions are used internally to construct the IRI URL
nmme_init_times_ext <- function(init_times, ...) {
  S <- rep(NA, length(init_times))
  for (i in 1:length(init_times)) {
    tm <- init_times[i]
    month <- format(tm, "%b")
    year <- format(tm, "%Y")
    S[i] <- paste0("S/(0000 1 ", month, " ", year, ")VALUES")
  }
  return(S)
}

nmme_members_ext <- function(members, ...) {
  M = rep(NA, length(members))
  for (i in 1:length(members)) {
    M[i] = paste0("M/(", members[i], ")VALUES")
  }
  return(M)
}

nmme_lead_times_ext <- function(lead_times, ...) {
  L <- rep(NA, length(lead_times))
  for (i in 1:length(lead_times)) {
    L[i] <- paste0("L/(", lead_times[i], ")VALUES")
  }
  return(L)
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
nmme_subset_init_times <- function(start_init_time, end_init_time, config) {
  start_init_time <- zoo::as.yearmon(start_init_time)
  end_init_time <- zoo::as.yearmon(end_init_time)
  check_init_times(start_init_time, end_init_time, config)
  init_times <- seq(as.Date.yearmon(start_init_time), as.Date.yearmon(end_init_time), by = "month")
  ext <- nmme_init_times_ext(init_times)
  nms <- init_times %>% format("%Y%m") # FIXME would having a time be better?
  return(list(subset = init_times, ext = ext))
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
nmme_subset_members <- function(members, config) {
  c1 = check_members(members, config)
  members = trimws(format(round(members), nsmall = 1))
  ext <- nmme_members_ext(members)
  return(list(subset = members, ext = ext))
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
nmme_subset_lead_times <- function(lead_times, config) {
  lead_times = sort(lead_times)
  c1 = check_lead_times(lead_times, config)
  lead_times = trimws(format(lead_times, nsmall = 1))
  ext <- nmme_lead_times_ext(lead_times)
  return(list(subset = lead_times, ext = ext))
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

nmme_subset_x_range <- function(xmin, xmax) {
  ## "longitude is best specified as west to east, two east values or two west values, otherwise you can end up with the wrong half of the world (e.g. 0.5E to 355.5E will work much better than 0.5E to 0.5W)"
  stopifnot(xmin < xmax)
  stopifnot(xmin >= -180)
  stopifnot(xmax <= 180)
  xmax <- min(xmax, 179)
  x_coords <- seq(xmin, xmax, 1)
  complete <- length(x_coords) == 360
  if (!complete) {
    xmn <- format_xmin_coord(xmin)
    xmx <- format_xmax_coord(xmax)
    ext <- paste0("X/(", xmn, ")", "(", xmx, ")RANGEEDGES")
  } else {
    ext <- NULL
  }
  return(list(subset = x_coords, ext = ext))
}

nmme_subset_y_range <- function(ymin, ymax) {
  stopifnot(ymin < ymax)
  stopifnot(ymin >= -90)
  stopifnot(ymax <= 90)
  y_coords <- seq(ymin, ymax, 1)
  complete <- length(y_coords) == 181
  if (!complete) {
    ymn <- format_y_coord(ymin)
    ymx <- format_y_coord(ymax)
    ext <- paste0("Y/(", ymn, ")", "(", ymx, ")RANGEEDGES")
  } else {
    ext <- NULL
  }
  return(list(subset = y_coords, ext = ext))
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
nmme_subset_region <- function(xmin, xmax, ymin, ymax) {
  ## TODO check extent is valid - are all models global?
  X = nmme_subset_x_range(xmin, xmax)
  Y = nmme_subset_y_range(ymin, ymax)
  return(list(X = X, Y = Y))
}

nmme_subset_x_point <- function(xcoord) {
  xcoord <- sort(xcoord)
  xv <- format_x_coord(xcoord)
  ext <- paste0("X/(", xv, ")VALUES")
  return(list(subset = xcoord, ext = ext))
}

nmme_subset_y_point <- function(ycoord) {
  ycoord <- sort(ycoord)
  yv <- format_y_coord(ycoord)
  ext <- paste0("Y/(", yv, ")VALUES")
  return(list(subset = ycoord, ext = ext))
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
nmme_subset_point <- function(xcoord, ycoord) {
  X = nmme_subset_x_point(xcoord)
  Y = nmme_subset_y_point(ycoord)
  x_subset = tibble(subset = format_x_coord(xcoord), ext = X)
  y_subset = tibble(subset = format_y_coord(ycoord), ext = Y)
  return(list(X = x_subset, Y = y_subset))
}

nmme_x_index <- function(x, ...) {
  dots <- list(...)
  nms <- names(dots)
  x_coords <- x$dataset$X$subset
  if ("xcoord" %in% nms) {
    x_coords_new <- round(as.numeric(dots[["xcoord"]]))
  } else if (all(c("xmin", "xmax") %in% nms)) {
    xmin <- floor(as.numeric(dots[["xmin"]]))
    xmax <- ceiling(as.numeric(dots[["xmax"]]))
    stopifnot(xmin < xmax)
    stopifnot(xmin >= -180)
    stopifnot(xmax <= 180)
    xmax <- min(xmax, 179)
    x_coords_new <- seq(xmin, xmax, 1)
  } else {
    warning("Either `xcoord` or `xmin` and `xmax` must be supplied.")
    return(x_coords)
  }
  if (!all(x_coords_new %in% x_coords)) {
    stop("Index out of range")
  }
  return(match(x_coords_new, x_coords))
}

nmme_y_index <- function(x, ...) {
  dots <- list(...)
  nms <- names(dots)
  y_coords <- x$dataset$Y$subset
  if ("ycoord" %in% nms) {
    y_coords_new <- round(as.numeric(dots[["ycoord"]]))
  } else if (all(c("ymin", "ymax") %in% nms)) {
    ymin <- floor(as.numeric(dots[["ymin"]]))
    ymax <- ceiling(as.numeric(dots[["ymax"]]))
    stopifnot(ymin < ymax)
    stopifnot(ymin >= -90)
    stopifnot(ymax <= 90)
    y_coords_new <- seq(ymin, ymax, 1)
  } else {
    warning("Either `ycoord` or `ymin` and `ymax` must be supplied.")
    return(y_coords)
  }
  if (!all(y_coords_new %in% y_coords)) {
    stop("Index out of range")
  }
  return(match(y_coords_new, y_coords))
}
