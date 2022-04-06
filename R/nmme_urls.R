#' IRI URLs
#'
#' This function returns a list of character pairs. The
#' first element of each character pair is an IRI Climate
#' Data Library URL for an NMME subset. The second element
#' is the default destination filename.
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
#' urls = name_urls()
#' download_nmme(urls[1])
#' }
nmme_url <- function(model = "GFDL-SPEAR",
                     simulation_type = "hindcast",
                     dataset = "monthly",
                     variable = "prec",
                     config = nmme_config(),
                     ...) {

  ## This will check the selection is valid at the same time
  model_config = get_model_config(model, simulation_type, dataset, variable, config)
  dataset = list(
    source = tibble(name = "NMME", filter = "SOURCES/.Models/.NMME"),
    model = tibble(name = toupper(model), filter = paste0(".", toupper(model))),
    simulation_type = tibble(name = tolower(simulation_type), filter = paste0(".", toupper(simulation_type))),
    dataset = tibble(name = tolower(dataset), filter = paste0(".", toupper(dataset))),
    variable = tibble(name = tolower(variable), filter = paste0(".", tolower(variable)))
  )
  ## url = paste(config$base_url, url_ext, sep="/")
  ## TODO make this an S3 object?
  obj = list(dataset = dataset,
             filter = list(X = NA, Y = NA, L = NA, S = NA, M = NA),
             config = model_config)
  obj =
    obj %>%
    filter_init_time(model_config$start_init_time, model_config$end_init_time) %>%
    filter_member(model_config$members)
  obj
}

get_model_config <- function(model, simulation_type, dataset, variable, config, check = TRUE) {
  ## Apply consistent formatting
  model <- tolower(model)
  simulation_type <- tolower(simulation_type)
  dataset <- tolower(dataset)
  ## variable <- tolower(variable)
  if (check) {
    c1 = check_model(model, config)
    c2 = check_simulation_type(simulation_type, config[[model]])
    c3 = check_dataset(dataset, config[[model]][[simulation_type]])
    c4 = check_variable(variable, config[[model]][[simulation_type]][[dataset]])
    ## TODO logging?
  }
  model_config = config[[model]][[simulation_type]][[dataset]]
  model_config
}

check_model <- function(model, config) {
  if (!model %in% names(config)) {
    stop(sprintf("`model` %s not yet supported!", model))
  }
  TRUE
}

check_simulation_type <- function(simulation_type, config) {
  if (!simulation_type %in% names(config)) {
    stop(sprintf("`simulation_type` %s not yet supported!", simulation_type))
  }
  TRUE
}

check_dataset <- function(dataset, config) {
  if (!dataset %in% names(config)) {
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

## format_lead_times <- function(start_lead_time, end_lead_time, ...) {
##   lead_times = sort(c(start_lead_time, end_lead_time))
##   valid_lead_times = seq(0.5, 11.5, 1)
##   if (!all(lead_times %in% valid_lead_times)) {
##     stop("`lead_times` contains invalid values")
##   }
##   lead_times = format(sort(lead_times), nsmall = 1)
##   return(paste0("/L/(", lead_times[1], ")", "(", lead_times[2], ")RANGEEDGES"))
## }

## format_members <- function(members, ...) {
##   if (is.na(members[1]))
##     return("")
##   members = format(round(members), nsmall = 1)
##   members = trimws(paste(members, collapse = ", "))
##   return(paste0("/M/(", members, ")VALUES"))
## }


##   ## c4 = check_members(members, config[[model]][[simulation_type]][[dataset]])
##   ## c5 = check_lead_times(members, config[[model]][[simulation_type]][[dataset]])
##   ## model_config = get_model_config(model, simulation_type, dataset, variable)
##   start_init_time = zoo::as.yearmon(start_init_time)
##   end_init_time = zoo::as.yearmon(end_init_time)
##   check_init_times(start_init_time, end_init_time, model_config)
##   if (start_init_time > end_init_time) {
##     stop("`end_init_time` should be greater than or equal to `start_init_time`")
##   }
##   ## Spatial query
##   X = format_x(xmin, xmax)
##   Y = format_y(ymin, ymax)
##   ## lead-time query
##   L = format_lead_times(start_lead_time, end_lead_time)
##   ## Only download one year at a time
##   years = seq(start_year, end_year, 1)
##   ## TODO better to specify month range or month values?
##   months = seq(start_month, end_month, 1)
##   urls = vector(mode = "list", length=length(years))
##   indx = 1
##   for (i in 1:length(years)) {
##     year = years[i]
##     for (j in 1:length(months)) {
##       month = months[j]
##       for (k in 1:length(members)) {
##         member = members[k]
##         M = format_members(member)
##         S = format_times(year, month, year, month)
##         ## URL extension
##         url_ext = paste(
##           "SOURCES/.Models/.NMME",
##           paste0(".", model),
##           paste0(".", toupper(simulation_type)),
##           paste0(".", toupper(dataset)),
##           ## paste0(".", variable),
##           ## Y, S, X, L, M,
##           ## "data.nc",
##           sep = "/"
##         )
##         url_ext = gsub("(/)\\1+", "\\1", url_ext)
##         url_ext = gsub(" ", "%20", url_ext)
##         url_ext = gsub(",", "%2C", url_ext)
##         url = paste(config$base_url, url_ext, sep="/")
##         ## Filename
##         init_yearmon = paste0(year, formatC(month, width = 2, flag = 0))
##         fcst_month_start = floor(month + min(lead_times))
##         fcst_month_end = floor(month + max(lead_times))
##         fcst_year_start = year + floor(fcst_month_start / 12.5)
##         fcst_year_end = year + floor(fcst_month_end / 12.5)
##         fcst_start_yearmon = paste0(fcst_year_start, formatC(fcst_month_start %% 12, width = 2, flag = 0))
##         fcst_end_yearmon = paste0(fcst_year_end, formatC(fcst_month_end %% 12, width = 2, flag = 0))
##         fname = paste0(
##           variable, "_", model, "_", init_yearmon, "_",
##           trimws(format(as.numeric(member), nsmall = 1)), "_",
##           fcst_start_yearmon, "-", fcst_end_yearmon, ".nc"
##         )
##         ## Add to list
##         urls[[indx]] = c(url, fname)
##         indx = indx + 1
##       }
##     }
##   }
##   urls
## }
