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
#' @param ... Additional arguments to download.file.
#'
#' @return NULL.
#'
#' @examples
#' \dontrun{
#' urls = name_urls()
#' download_nmme(urls[1])
#' }
download_nmme <- function(x,
                          destdir = ".",
                          overwrite = FALSE, ...) {

  urls = x %>% construct_urls()
  n_urls = nrow(urls)
  for (i in 1:n_urls) {
    url = urls$url[i]
    destfile = file.path(destdir, urls$destfile[i])
    if (overwrite | !file.exists(destfile)) {
      download.file(url, destfile, ...)
    }
  }
  NULL
}

