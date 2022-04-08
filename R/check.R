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
check_members <- function(members, model_config) {
  if (!all(members %in% model_config$members)) {
    stop("Some `members` are invalid!")
  }
  TRUE
}

check_lead_times <- function(lead_times, model_config) {
  if (!all(lead_times %in% model_config$lead_times)) {
    stop("`start_lead_time` is invalid!")
  }
  TRUE
}

check_init_times <- function(start_init_time, end_init_time, model_config) {
  if (start_init_time < model_config$start_init_time) {
    stop(
      "`start_init_time` is invalid: earliest available time is %s.",
      as.character(model_config$start_init_time)
    )
  }
  if (end_init_time > model_config$end_init_time) {
    stop(
      "`end_init_time` is invalid: latest available time is %s.",
      as.character(model_config$end_init_time)
    )
  }
  TRUE
}
