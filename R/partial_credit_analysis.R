#' t test with summary statistics
#'
#' @param m1,m2 means of each group
#' @param s1,s2 standard deviation of each group
#' @param n1 sample size of each group
#' @param m0 value of null hypothesis
#' @param equal.variance whether to assume equal variance between the groups
#' @param conf confidence level for confidence interval
#'
#' @return an "htest" object of the test result
#'
#' @keywords internal
t_test_summary <- function(m1, m2, s1, s2, n1, n2, m0 = 0, equal.variance = FALSE, conf=0.95){
  if(equal.variance == FALSE){
    se <- sqrt((s1^2/n1) + (s2^2/n2))
    # welch-satterthwaite df
    stderr = sqrt(s1^2 + s2^2)
    #df <- stderr^4/(s1^4/(n1-1) + s2^4/(n2-1))
    df <- ((s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
  }else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt((1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
    df <- n1+n2-2
  }
  # t test statistic
  t <- (m1-m2-m0)/se
  # confidence interval
  alpha <- 1 - conf
  cint <- qt(1 - alpha/2, df)
  cint <- t + c(-cint, cint)
  cint <- m0 + cint*se
  # p-value
  pval <- 2*pt(-abs(t),df)

  estimate <- m1 - m2 - m0
  names(estimate) <- "Difference in mean"
  statistic <- t
  names(statistic) <- "Welch's t test statistic"
  parameter <- df
  names(parameter) <- "df"
  conf.int <- cint
  names(conf.int) <- c("lower", "upper")
  attr(conf.int, "conf.level") = (1 - alpha)
  method <- "Welch's t test with summary level data"

  structure(list(statistic = statistic,
                 parameter = df,
                 estimate = estimate,
                 p.value = pval,
                 conf.int = cint,
                 method = method), class = "htest")

}


#' Partial credit analysis for DOOR
#'
#' @param grade_key A numeric vector of grade key or a dataframe contains columns of grade keys
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param n1,n2 Sample sizes of group 1, group 2; must be specified if method = "prop"
#' @param summary_obj An object returned by `door_summary()`; Alternative
#' input for y1 and y2
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#' @param ci_method Specify the type of CI for DOOR probability given a grade key.
#' The default is "halperin" for Halperin et al. (1989)'s
#' method. Other options include "ps_h" for pseudo-score approach for Halperin's method and
#' "tanh" for inverse hyperbolc tangent transformed method
#' @param conf_level Confidence level
#' @param ... Optional additional parameters if `ci_method = "ps_h"`
#'
#' @return An object containing information of partial credit analysis given grade keys
#' @export
#'
#' @examples
#' grade.key <- c(100, 80, 60, 40, 0)
#' y1 <- c(60, 30, 20, 10, 5)
#' y2 <- c(50, 40, 10, 20, 5)
#' partial_credit_analysis(grade_key = grade.key, y1 = y1, y2 = y2)
partial_credit_analysis <- function(grade_key, y1 = NULL, y2 = NULL, n1 = NULL, n2 = NULL,
                                    summary_obj = NULL, data_type = c("freq", "prop"),
                                    ci_method = "halperin", conf_level = 0.95, ...){
  # Error checking
  data_type <- match.arg(data_type)

  if(is.null(summary_obj) & data_type == "prop"){
    if(is.null(y1) | is.null(y2)) stop("Missing DOOR outcome proportion informaion")
    if(is.null(n1) | is.null(n2)) stop("Missing sample size(s) information for proportion data")
  }

  # Check grade keys
  grade_key <- as.data.frame(grade_key)
  for(i in 1:ncol(grade_key)){
    if(!all(diff(grade_key[,i]) <= 0)) stop("The grade key is not of descending order")
    if(grade_key[,i][1] != 100) warning("The first entry of the grade key is not 100")
    if(last(grade_key[,1]) != 0) warning("The last entry of the grade key is not 0")
  }

  # Get DOOR outcome data
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
    ARM.name <- c(deparse(substitute(y1)), deparse(substitute(y2)))
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data = summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    n1 <- sum(y1, na.rm = T)
    n2 <- sum(y2, na.rm = T)
    p1 <- y1 / n1
    p2 <- y2 / n2
    ARM.name <- summary_obj$label
  }else(
    stop("Missing required DOOR outcome information")
  )

  pc_data <- list()
  door.prob <- calc_doorprob(p1, p2, data_type = "prop")[[1]]
  # analysis
  for(i in 1:ncol(grade_key)){
    # Welch t test
    m1 <- sum(p1 * grade_key[,i])
    m2 <- sum(p2 * grade_key[,i])
    s1 <- sum(n1 * p1 * (grade_key[,i] - m1)^2) / (n1 - 1)
    s2 <- sum(n2 * p2 * (grade_key[,i] - m2)^2) / (n2 - 1)
    t <- t_test_summary(m1, m2, sqrt(s1), sqrt(s2), n1 = n1, n2 = n2, conf = conf_level)
    # Assign ranks based on partial credits
    pc_rank <- dplyr::dense_rank(dplyr::desc(grade_key[,i]))
    pc_door <- cbind.data.frame(pc_rank, y1, y2)
    pc_door <- pc_door %>% aggregate(. ~ pc_rank, sum)
    # WMW test
    u <- door_test(y1 = pc_door$y1, y2 = pc_door$y2,
                   n1 = n1, n2 = n2, data_type = data_type)
    # CI
    ci <- door_ci(y1 = pc_door$y1, y2 = pc_door$y2, n1 = n1, n2 = n2, data_type = data_type,
                  conf_level = conf_level, ci_method = ci_method, ...)[[3]][[1]]
    # Save to df
    gk_name <- paste0("Grade key ", i)
    gk_value <- paste(grade_key[,i], collapse = " ")
    df <- data.frame(
      grade_key = gk_name,
      grade_key_value = gk_value,
      diff_est = t$estimate,
      p_value_gradekey = t$p.value,
      diff_ci_l = t$conf.int[1],
      diff_ci_u = t$conf.int[2],
      door_prob = u$estimate,
      p_value_door = u$p.value,
      door_ci_l = ci[1],
      door_ci_u = ci[2]
    )
    rownames(df) <- NULL
    pc_data[[i]] <- df
  }

  pc_summary <- do.call(rbind, pc_data)

  output <- list(trt_labels = ARM.name,
                 overall.door.prob = door.prob,
                 pc_summary = pc_summary)

  structure(output, class = "partial_credit_summary")

}





