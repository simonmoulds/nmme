#' Print nmme object
#'
#' @param x nmme object.
#' @param ... Additional arguments (ignored).
#' @export
print.nmme <- function(x, ...) {
  ## cat("An nmme object\n")
  cat("Model           :", x$dataset$model$name, "\n")
  cat("Variable        :", x$dataset$variable$name, "\n")
  cat("Simulation type :", x$dataset$simulation_type$name, "\n")
  cat("Dataset         :", x$dataset$dataset$name, "\n")
  cat("Init times      : X to Y", "\n")
  cat("Members         : M", "\n")
  cat("Total number of files to download: N", "\n")
}

## TODO more generics: length(), `[`, etc.
