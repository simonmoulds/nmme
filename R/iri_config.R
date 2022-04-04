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
  base_url <- "https://iridl.ldeo.columbia.edu"
  models <- list(
    "NMME" = list(
      "GFDL-SPEAR" = list(
        HINDCAST = .hindcast_monthly_template()
      )
    )
  )
  config <- list(base_url = base_url, sources = models)
  return(config)
}

.hindcast_monthly_template = function(...) {
  template = list(
    Monthly = list(
      variables = c("h200", "prec", "sst", "T", "t_ref_max", "t_ref_min", "tref")
    ),
    mc9120 = list(
      variables = c("prec", "sst", "T", "tref")
    ),
    sc9120 = list(
      variables = c("prec", "sst", "T", "tref")
    )
  )
  return(template)
}
