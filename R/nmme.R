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
  ## These options define the dataset root
  dataset <- list(
    source = list(subset = "NMME", ext = "SOURCES/.Models/.NMME"),
    model = list(subset = toupper(model), ext = paste0(".", toupper(model))),
    simulation_type = list(subset = tolower(simulation_type), ext = paste0(".", toupper(simulation_type))),
    dataset = list(subset = tolower(dataset), ext = paste0(".", toupper(dataset))),
    variable = list(subset = tolower(variable), ext = paste0(".", tolower(variable))),
    X = NA, Y = NA, L = NA, S = NA, M = NA
  )
  ## Apply default subsets
  dots <- list(...)
  start_init_time <- model_config$start_init_time
  end_init_time <- model_config$end_init_time
  members <- model_config$members
  lead_times <- model_config$lead_times
  if (all(c("start_init_time", "end_init_time") %in% names(dots))) {
    start_init_time <- dots$start_init_time
    end_init_time <- dots$end_init_time
  }
  if ("members" %in% names(dots))
    members <- dots$members
  if ("lead_times" %in% names(dots))
    lead_times <- dots$lead_times
  dataset$S <- subset_init_times(start_init_time, end_init_time, model_config)
  dataset$M <- subset_members(members, model_config)
  dataset$L <- subset_lead_times(lead_times, model_config)
  ## Create object
  obj <- list(dataset = dataset, config = model_config, index = NA)
  class(obj) <- c("nmme", class(obj))
  ## Create list of URLs
  obj$index <- index(obj)
  obj
}

## parse_subset_args <- function(..., config) {
##   dots <- list(...)
##   start_init_time <- model_config$start_init_time
##   end_init_time <- model_config$end_init_time
##   members <- model_config$members
##   lead_times <- model_config$lead_times
##   xcoord <- NA
##   yxoord <- NA
##   xmin <- NA
##   xmax <- NA
##   ymin <- NA
##   ymax <- NA
##   if (all(c("start_init_time", "end_init_time") %in% names(dots))) {
##     start_init_time <- dots$start_init_time
##     end_init_time <- dots$end_init_time
##   }
##   if ("members" %in% names(dots))
##     members <- dots$members
##   if ("lead_times" %in% names(dots))
##     lead_times <- dots$lead_times
##   use_subset_point <- all(c("xcoord", "ycoord") %in% names(dots))
##   use_subset_region <- all(c("xmin", "xmax", "ymin", "ymax") %in% names(dots))
##   if (use_subset_point & use_subset_region) {
##     stop("Cannot specify spatial point (`xcoord` and `ycoord`) and region (`xmin`, `xmax`, `ymin`, `ymax`)")
##   }
##   if (use_subset_point) {
##     xcoord <- dots$xcoord
##     ycoord <- dots$ycoord
##   } else if (use_subset_region) {
##     xmin <- dots$xmin
##     xmax <- dots$xmax
##     ymin <- dots$ymin
##     ymax <- dots$ymax
##   }
##   return(list(start_init_time = start_init_time,
##               end_init_time = end_init_time,
##               members = members, lead_times = lead_times,
##               xcoord = xcoord, ycoord = ycoord,
##               xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
## }

## construct_index <- function(..., config) {
##   subset_args <- parse_subset_args(..., config)
##   init_time_index <- construct_init_time_index(subset_args$start_init_time, subset_args$end_init_time, config)
##   member_index <- construct_member_index(subset_args$members, config)
##   lead_time_index <- construct_lead_time_index(subset_args$lead_times, config)

## }

## construct_init_time_index <- function(start_init_time, end_init_time, config) {
##   start_init_time = zoo::as.yearmon(start_init_time)
##   end_init_time = zoo::as.yearmon(end_init_time)
##   check_init_times(start_init_time, end_init_time, config)
##   init_times = seq(as.Date.yearmon(start_init_time), as.Date.yearmon(end_init_time), by = "month")
##   S = rep(NA, length(init_times))
##   for (i in 1:length(init_times)) {
##     tm = init_times[i]
##     month = format(tm, "%b")
##     year = format(tm, "%Y")
##     S[i] = paste0("S/(0000 1 ", month, " ", year, ")VALUES")
##   }
##   return(tibble(init_time = init_times %>% format("%Y%m"), init_time_ext = S))
## }

## construct_member_index <- function(members, config) {
##   c1 = check_members(members, config)
##   members = trimws(format(round(members), nsmall = 1))
##   M = rep(NA, length(members))
##   for (i in 1:length(members)) {
##     M[i] = paste0("M/(", members[i], ")VALUES")
##   }
##   return(tibble(member = members, member_ext = M))
## }

## ## construct_index <- function(start_init_time, end_init_time, members, ...) {
## ##   init_time_index <- construct_init_time_index(start_init_time, end_init_time, config)
## ##   member_index <- construct_member_index(members, config)
## ##   index = merge(init_time_index, member_index, by = NULL) %>% as_tibble()
## ## }

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

