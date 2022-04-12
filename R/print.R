#' @export
print.nmme <- function(x, ...) {
  n_init_times <- length(x$dataset$S$subset)
  init_start <- x$dataset$S$subset[1] %>% format("%b %Y")
  init_end <- rev(x$dataset$S$subset)[1] %>% format("%b %Y")
  members <- format_members(x)
  n_members <- length(x$dataset$M$subset)
  lead_times <- format_lead_times(x)
  n_lead_times <- length(x$dataset$L$subset)
  if (isTRUE(is.null(x$dataset$X$ext) & is.null(x$dataset$Y$ext))) {
    spatial_selection <- "Global"
  } else {
    xmin <- min(x$dataset$X$subset)
    xmax <- max(x$dataset$X$subset)
    ymin <- min(x$dataset$Y$subset)
    ymax <- max(x$dataset$Y$subset)
    x_spatial_selection <- paste0(format_x_coord(xmin), "-", format_x_coord(xmax))
    y_spatial_selection <- paste0(format_y_coord(ymin), "-", format_y_coord(ymax))
    spatial_selection <- paste0(x_spatial_selection, ", ", y_spatial_selection)
  }
  n_files <- n_init_times * n_members * n_lead_times
  cat("Model             :", x$dataset$model$subset, "\n")
  cat("Variable          :", x$dataset$variable$subset, "\n")
  cat("Simulation type   :", x$dataset$simulation_type$subset, "\n")
  cat("Dataset           :", x$dataset$dataset$subset, "\n")
  cat("Init times        :", paste0(init_start, " -> ", init_end), "\n")
  cat("Members           :", members, "\n")
  cat("Lead times        :", lead_times, "\n")
  cat("Region            :", spatial_selection, "\n")
  cat("Files to download :", n_files, "\n")
  wdth = getOption("width")
  rows_to_print <- min(n_files, 10)
  if (n_files > 10) {
    for (i in 1:5) {
      url = x$index$URL[i]
      fn = x$index$destfile[i]
      fn_nchar = str_length(fn)
      cat(fn, " [", str_trunc(url, wdth - str_length(fn) - 3), "]\n", sep = "")
    }
    cat("...\n")
    for (i in 1:5) {
      url = tail(x$index$URL, n = 5)[i]
      fn = tail(x$index$destfile, n = 5)[i]
      fn_nchar = str_length(fn)
      cat(fn, " [", str_trunc(url, wdth - str_length(fn) - 3), "]\n", sep = "")
    }
  } else {
    for (i in 1:rows_to_print) {
      url = x$index$URL[i]
      fn = x$index$destfile[i]
      fn_nchar = str_length(fn)
      cat(fn, " [", str_trunc(url, wdth - str_length(fn) - 3), "]\n", sep = "")
    }
  }
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
format_members <- function(x) {
  members <- x$dataset$M$subset
  n_members <- length(members)
  total_members <- length(x$config$members)
  frac <- paste0("(", n_members, " / ", total_members, ")")
  if (all(diff(as.numeric(members)) == 1)) {
    fmt <- paste0(members[1], " -> ", rev(members)[1])
  } else {
    fmt <- paste(members, collapse = ", ")
  }
  return(paste(fmt, frac))
}

format_lead_times <- function(x) {
  lead_times <- x$dataset$L$subset
  n_lead_times <- length(lead_times)
  total_lead_times <- length(x$config$lead_times)
  frac <- paste0("(", n_lead_times, " / ", total_lead_times, ")")
  if (all(diff(as.numeric(lead_times)) == 1)) {
    fmt <- paste0(lead_times[1], " -> ", rev(lead_times)[1])
  } else {
    fmt <- paste(lead_times, collapse = ", ")
  }
  return(paste(fmt, frac))
}

## TODO more generics: length(), `[`, etc.
