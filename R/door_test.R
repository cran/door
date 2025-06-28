#' Hypothesis testing for the DOOR probability
#'
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param n1,n2 Sample sizes of group 1, group 2; must be specified if method = "prop"
#' @param summary_obj An object returned by `individual_to_summary()`; Alternative
#' input for y1 and y2
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#' @param null_value A number specifying the hypothesized value of the DOOR probability
#' @param alternative A character describing the alternative hypothesis
#'
#' @return A `htest` object containing information of hypothesis test of DOOR probability
#'
#' @export
#'
#' @seealso [door_summary()]
#'
#'
#' @examples
#' y1 <- c(60, 30, 10)
#' y2 <- c(50, 40, 10)
#' door_test(y1 = y1, y2 = y2)
#'
#' ## Hypothesis test for DOOR probability
#' ## data:  y1 and y2
#' ## WMW statistic = 1.2372, p-value = 0.216
#' ## alternative hypothesis: true  is not equal to 0.5
#' ## sample estimates:
#' ## DOOR probability
#' ##	           0.545
door_test <- function(y1 = NULL, y2 = NULL, n1 = NULL, n2 = NULL,
                      summary_obj = NULL, data_type = c("freq", "prop"), null_value = 0.5,
                      alternative = c("two.sided", "less", "greater")){

  # Error checking
  data_type <- match.arg(data_type)
  alternative <- match.arg(alternative)

  if(is.null(summary_obj) & data_type == "prop"){
    if(is.null(y1) | is.null(y2)) stop("Missing DOOR outcome proportion informaion")
    if(is.null(n1) | is.null(n2)) stop("Missing sample size(s) information for proportion data")
  }

  # Get proportions
  if(!is.null(y1) & !is.null(y2)){   # If y1, y2 are supplied
    if(any(is.na(y1)) | any(is.na(y2))){
      y1[which(is.na(y1))] <- 0
      y2[which(is.na(y2))] <- 0
      warning("NA detected. NA values will be replaced by 0.")
    }
    if(data_type == "freq"){
      n1 <- sum(y1)
      n2 <- sum(y2)
      p1 <- y1 / n1
      p2 <- y2 / n2
    }else if(data_type == "prop"){
      p1 <- y1
      p2 <- y2
    }
    dname <- paste(deparse(substitute(y1)), "and", deparse(substitute(y2))) # data name
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data <- summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    n1 <- sum(y1, na.rm = TRUE)
    n2 <- sum(y2, na.rm = TRUE)
    p1 <- y1 / n1
    p2 <- y2 / n2
    dname <- paste(colnames(summary_data)[2], "and", colnames(summary_data)[3])
  }else(
    stop("Missing required DOOR outcome information")
  )

  # Allocation ratio
  n <- n1 + n2
  a1 <- n1 / n
  a2 <- n2 / n

  # Variance, DOOR Prob
  v0 <- (1 / (12 * (n - 1) * a1 * a2)) * (1 - sum((a1 * p1 + a2 * p2)^3))
  pi <- calc_doorprob(y1,y2)[[1]]

  # Normal approximation test statistic
  Z <- (pi - null_value) / sqrt(v0)

  if(alternative == "two.sided"){
    pval <-  2 * pnorm(abs(Z), lower.tail = FALSE)
  }else if(alternative == "greater"){
    pval <- pnorm(Z, lower.tail = FALSE)
  }else if(alternative == "less"){
    pval <- pnorm(Z)
  }

  estimate <- pi
  names(estimate) <- "DOOR probability"
  statistic <- Z
  names(statistic) <- "WMW statistic"

  result <- list(estimate = estimate,
                 statistic = statistic,
                 p.value = pval,
                 alternative = alternative,
                 null.value = null_value,
                 data.name = dname,
                 method = "Hypothesis test for DOOR probability")
  structure(result, class = "htest")

}
