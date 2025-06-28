#' Summarize individual level data into summary level data
#'
#' Transform an individual level dataset that contains DOOR outcome variable and
#' treatment/intervention variable to summary level. By default, the levels of
#' the DOOR outcome is ordered from 1 to K.
#'
#' @param data Data frame that includes DOOR outcome variable and
#' treatment variable at individual level
#' @param doorVar Variable name of DOOR outcome; the doorVar should be numeric
#' @param trtVar Variable name of treatments
#' @param trtCodes A numeric vector contains the codes for interventions in `trtVar`, ordered by `c(trt, ctr)`
#' @param trtLabels An optional vector contains the intervention labels for `trtCodes`, ordered by `c(trt, ctr)`
#' @param decreasing A logical value indicating the order of desirability of the DOOR levels. By default,
#' smaller value represents better outcomes
#' @param compVars An optional character vector of variable names of DOOR components
#'
#' @return An object of DOOR outcome distribution summary
#' @export
#'
#' @import dplyr tidyr
#'
#' @examples
#' data(mock_raw_data)
#' door_summary(data = mock_raw_data,
#'              trtVar = "ARM",
#'              doorVar = "DOOR",
#'              trtCodes = c(1,2),
#'              trtLabels = c("Test, Control"),
#'              compVars = c("infectious complications", "clinical failure", "death"))
#'
door_summary <- function(data, trtVar, doorVar, trtCodes, trtLabels = NULL, compVars = NULL,
                         decreasing = FALSE){
  if(missing(trtCodes)) stop("Missing intervention codes")
  if(is.vector(trtCodes) & length(trtCodes) != 2) stop("The length of intervention codes should be 2")


  ### DOOR variables
  # Select door data
  d <- data %>% dplyr::select(all_of(c(trtVar, doorVar)))
  d <- na.omit(d) # Remove NAs

  # Get DOOR levels
  doorLevels <- unique(unlist(d[,doorVar]))
  tryCatch(as.numeric(doorLevels),
           warning = function(w){
             message("Non-numeric DOOR outcome variable supplied. NAs introduced by coercion")
           })
  doorLevels <- sort(doorLevels, decreasing = decreasing)

  # Intervention names
  if(is.vector(trtLabels) & length(trtLabels) == 2){
    arm.name <- trtLabels
  }else{
    arm.name <- trtCodes
  }

  # Transform the variables
  d <- d %>% dplyr::mutate(factor_door = factor(.data[[doorVar]], levels = doorLevels),
                           factor_trt = factor(.data[[trtVar]], levels = trtCodes))
  d <- d %>%
    dplyr::group_by(.data$factor_trt, .data$factor_door, .drop=FALSE) %>%
    dplyr::summarise(n = n()) %>%
    tidyr::pivot_wider(names_from = .data$factor_trt, values_from = "n")

  colnames(d) <- c("DOOR ranks", arm.name)

  ### Components
  comp_vec <- NULL
  # Select component variables
  if(!is.null(compVars)){
    comp_vec <- list()
    # Select data
    d2 <- data %>% dplyr::select(any_of(c(trtVar, compVars)))

    # Transform data
    for(i in 1:length(compVars)){
      comp_name <- compVars[i]
      comp_d <- d2 %>% dplyr::select(any_of(c(trtVar, comp_name)))
      if(ncol(comp_d) < 2){     # Skip iteration if the variable is not in data
        next
      }

      comp_d <- na.omit(comp_d) # Remove NAs

      level <- sort(unique(comp_d[[comp_name]]), decreasing = decreasing)
      comp_d <- comp_d %>%
        dplyr::mutate(factor_trt = factor(comp_d[[trtVar]], levels = trtCodes),
                      factor_comp = factor(comp_d[[comp_name]], levels = level))
      comp_d <- comp_d %>%
        dplyr::group_by(.data$factor_trt, .data$factor_comp, .drop=FALSE) %>%
        dplyr::summarise(n = n()) %>%
        tidyr::pivot_wider(names_from = .data$factor_trt, values_from = "n")
      colnames(comp_d) <- c(comp_name, arm.name)

      comp_vec[[i]] <- comp_d
    }
    names(comp_vec) <- compVars # Set name of the list
  }

  # Output
  result <- list(door_distribution = d,
                 door_level = doorLevels,
                 intervention_label = arm.name,
                 door_components = comp_vec)
  structure(result, class = "DOORSummary")
}

