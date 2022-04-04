#' IRI configuration
#'
#' Return a list with IRI...
#'
#' @param ... Additional arguments (None implemented).
#'
#' @return A precipitation object.
#'
#' @examples
#' \dontrun{
#' sum(1:10)
#' }
iri_config <- function(...) {
  config = iri_spec$sources$models$nmme
  return(config)
}

