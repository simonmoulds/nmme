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
