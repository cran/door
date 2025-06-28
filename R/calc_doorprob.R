#' Calculate DOOR probability
#'
#' For summary level data, `y1` and `y2` should be given. For individual level data,
#' a `summary_obj` should be given.
#'
#' @param y1 A vector of proportion or frequency distribution for group 1
#' @param y2 A vector of proportion or frequency distribution for group 2
#' @param summary_obj A object returned by `door_summary()`; Alternative
#' input for y1 and y2
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#'
#' @include door_summary.R
#'
#' @return DOOR probability
#' @export
#'
#' @seealso [door_summary()]
#'
#' @examples
#' y1 = c(60, 30, 10)
#' y2 = c(50, 40, 10)
#' calc_doorprob(y1, y2)
#'
#' ## DOOR probability
#' ##            0.545
#'
#' p1 = c(.6, .3, .1)
#' p2 = c(.5, .4, .1)
#' calc_doorprob(p1, p2, data_type = "prop")
#'
#' ## DOOR probability
#' ##            0.545
#'
calc_doorprob <- function(y1 = NULL, y2 = NULL, data_type = c("freq", "prop"), summary_obj = NULL){
  data_type <- match.arg(data_type)

  # Get proportions
  if(!is.null(y1) & !is.null(y2)){   # If y1, y2 are supplied
    if(any(is.na(y1)) | any(is.na(y2))){
      y1[which(is.na(y1))] <- 0
      y2[which(is.na(y2))] <- 0
      message("NA detected. NA values will be replaced by 0.")
    }
    if(data_type == "freq"){
      p1 <- y1 / sum(y1)
      p2 <- y2 / sum(y2)
    }else if(data_type == "prop"){
      p1 <- y1
      p2 <- y2
      #if(sum(p1) != 1 | sum(p2) != 1)  warning("Sum of proportions not equal to 1")
    }
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data = summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    p1 <- y1 / sum(y1, na.rm = T)
    p2 <- y2 / sum(y2, na.rm = T)
  }else(
    stop("Missing required DOOR outcome information")
  )

  # Calculate DOOR probability
  c <- length(y1)
  zero <- c(rep(0,c))
  one <- c(rep(1,c))
  J <- one %*% t(one)
  DD <- diag(c)
  A <- J - (0.5) * DD
  for(i in 1:c) {
    for(j in 1:c) {
      if(j>i) A[i,j] <- 0
    }
  }
  prob <- (t(p2) %*% A %*% p1)[[1]]
  names(prob) <- "DOOR probability"
  prob
}
