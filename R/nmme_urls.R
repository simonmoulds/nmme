#' NMME
#'
#' This function returns an NMME object.
#'
#' @param model Character.
#' @param simulation_type Character.
#' @param dataset Character.
#' @param config List.
#' @param ... Additional arguments.
#'
#' @return List.
#'
#' @examples
#' \dontrun{
#' urls = nmme()
#' download_nmme(urls[1])
#' }
nmme <- function(model = "GFDL-SPEAR",
                 simulation_type = "hindcast",
                 dataset = "monthly",
                 variable = "prec",
                 config = nmme_config(),
                 ...) {

  validate_nmme_input(model, simulation_type, dataset, variable, config)
  model_config <- get_model_config(model, simulation_type, dataset, variable, config)
  dataset <- list(
    source = tibble(
      name = "NMME", filter = "SOURCES/.Models/.NMME"
    ),
    model = tibble(
      name = toupper(model), filter = paste0(".", toupper(model))
    ),
    simulation_type = tibble(
      name = tolower(simulation_type), filter = paste0(".", toupper(simulation_type))
    ),
    dataset = tibble(
      name = tolower(dataset), filter = paste0(".", toupper(dataset))
    ),
    variable = tibble(
      name = tolower(variable), filter = paste0(".", tolower(variable))
    )
  )
  obj <- list(dataset = dataset,
              filter = list(X = NA, Y = NA, L = NA, S = NA, M = NA),
              config = model_config)

  ## Initially assign all initialization times and model members
  obj <-
    obj %>%
    filter_init_time(model_config$start_init_time, model_config$end_init_time) %>%
    filter_member(model_config$members)
  nmme_obj <- structure(obj, class = "nmme")

  ## Pass objects to filter(...)
  nmme_obj <- nmme_obj %>% filter(...)
  nmme_obj
}

validate_nmme_input <- function(model, simulation_type, dataset, variable, config) {
  stopifnot(length(model) == 1)
  stopifnot(length(simulation_type) == 1)
  stopifnot(length(dataset) == 1)
  stopifnot(length(variable) == 1)
  model <- tolower(model)
  simulation_type <- tolower(simulation_type)
  dataset <- tolower(dataset)
  variable <- tolower(variable)
  c1 <- check_model(model, config)
  c2 <- check_simulation_type(simulation_type, config[[model]])
  c3 <- check_dataset(dataset, config[[model]][[simulation_type]])
  c4 <- check_variable(variable, config[[model]][[simulation_type]][[dataset]])
  TRUE
}

get_model_config <- function(model, simulation_type, dataset, variable, config) {
  model <- tolower(model)
  simulation_type <- tolower(simulation_type)
  dataset <- tolower(dataset)
  model_config = config[[model]][[simulation_type]][[dataset]]
  model_config
}

check_model <- function(model, config) {
  if (!tolower(model) %in% names(config)) {
    stop(sprintf("`model` %s not yet supported!", model))
  }
  TRUE
}

check_simulation_type <- function(simulation_type, config) {
  if (!tolower(simulation_type) %in% names(config)) {
    stop(sprintf("`simulation_type` %s not yet supported!", simulation_type))
  }
  TRUE
}

check_dataset <- function(dataset, config) {
  if (!tolower(dataset) %in% names(config)) {
    stop(sprintf("`dataset` %s not yet supported!", dataset))
  }
  TRUE
}

check_variable <- function(variable, config) {
  if (!all(variable %in% config$variables)) {
    stop("Some `variables` are invalid!")
  }
  TRUE
}
