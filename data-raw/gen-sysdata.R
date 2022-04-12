## Author : Simon Moulds
## Date   : Apr 2022

library(yaml)

gfdl_spear_spec <- function(...) {
  spec = list(
    name = "GFDL-SPEAR",
    hindcast = list(
      monthly = list(
        start_init_time = zoo::as.yearmon("199101", format = "%Y%m"),
        end_init_time = zoo::as.yearmon("202012", format = "%Y%m"),
        variables = c("h200", "prec", "sst", "T", "t_ref_max", "t_ref_min", "tref"),
        members = seq(1, 15, 1),
        lead_times = seq(0.5, 11.5, 1),
      ),
      mc9120 = list(
        start_init_time = zoo::as.yearmon("199101", format = "%Y%m"),
        end_init_time = zoo::as.yearmon("202012", format = "%Y%m"),
        variables = c("prec", "sst", "T", "tref"),
        members = seq(1, 15, 1),
        lead_times = seq(0.5, 11.5, 1)
      ),
      sc9120 = list(
        start_init_time = zoo::as.yearmon("199101", format = "%Y%m"),
        end_init_time = zoo::as.yearmon("202012", format = "%Y%m"),
        variables = c("prec", "sst", "T", "tref"),
        members = seq(1, 15, 1),
        lead_times = seq(0.5, 11.5, 1)
      )
    )
  )
  return(spec)
}

get_iri_spec <- function(...) {
  base_url <- "https://iridl.ldeo.columbia.edu"
  models <- list(
    models = list(
      "nmme" = list(
        "gfdl-spear" = gfdl_spear_spec()
      )
    )
  )
  config <- list(base_url = base_url, sources = models)
  return(config)
}

iri_spec = get_iri_spec()
usethis::use_data(iri_spec, internal = TRUE, overwrite = TRUE)
