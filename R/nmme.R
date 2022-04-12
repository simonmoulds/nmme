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
                 location = ".",
                 config = nmme_config(),
                 ...) {

  ## n_models <- length(model)
  ## for (i in 1:length(model)) {
  validate_nmme_input(model, simulation_type, dataset, variable, config)
  model_config <- get_model_config(model, simulation_type, dataset, variable, config)
  ## These options define the dataset root
  dataset <- list(
    source = list(subset = "NMME", ext = "SOURCES/.Models/.NMME"),
    model = list(subset = toupper(model), ext = paste0(".", toupper(model))),
    simulation_type = list(subset = tolower(simulation_type), ext = paste0(".", toupper(simulation_type))),
    dataset = list(subset = tolower(dataset), ext = paste0(".", toupper(dataset))),
    variable = list(subset = tolower(variable), ext = paste0(".", tolower(variable))),
    X = NA, Y = NA, L = NA, S = NA, M = NA
  )
  ## Apply default nmme_subsets
  dots <- list(...)
  start_init_time <- model_config$start_init_time
  end_init_time <- model_config$end_init_time
  members <- model_config$members
  lead_times <- model_config$lead_times
  x_coords <- seq(0, 359)
  y_coords <- seq(-90, 90)
  if (all(c("start_init_time", "end_init_time") %in% names(dots))) {
    start_init_time <- dots$start_init_time
    end_init_time <- dots$end_init_time
  }
  if ("members" %in% names(dots))
    members <- dots$members
  if ("lead_times" %in% names(dots))
    lead_times <- dots$lead_times
  dataset$S <- nmme_subset_init_times(start_init_time, end_init_time, model_config)
  dataset$M <- nmme_subset_members(members, model_config)
  dataset$L <- nmme_subset_lead_times(lead_times, model_config)
  dataset$X <- nmme_subset_x_range(-180, 179)
  dataset$Y <- nmme_subset_y_range(-90, 90)
  ## Create object
  obj <- list(location = location, dataset = dataset, config = model_config, index = NA)
  class(obj) <- c("nmme", class(obj))
  ## Create list of URLs
  obj$index <- nmme_index(obj, location)
  obj
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

