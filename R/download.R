#' NMME URLs
#'
#' This function returns a character vector of NMME URLs.
#'
#' @param x nmme.
#' @param overwrite Logical. Whether or not files should be downloaded
#'   again if they already exist.
#' @param ... Additional arguments to download.file.
#'
#' @return NULL.
#'
#' @examples
#' \dontrun{
#' x <- nmme()
#' download_nmme(x[1,1,1])
#' }
download_nmme <- function(x,
                          overwrite = FALSE, ...) {
  n_urls = nrow(x$index)
  for (i in 1:n_urls) {
    url <- x$index$URL[i]
    destfile <- x$index$destfile[i]
    exists <- x$index$exists[i]
    if (overwrite | !exists) {
      download.file(url, destfile, ...)
    }
  }
  x <- nmme_reindex(x)
  return(x)
}
