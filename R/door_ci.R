#' Restricted maximum likelihood estimate of DOOR outcome cell proportions
#'
#' @param y1,y2 vector of frequency distribution of DOOR outcome for group 1, 2
#' @param theta.null The DOOR probability of interest
#'
#' @return An object for REML estimates
#'
#' @keywords internal
reml = function(y1, y2, theta.null){
  c = length(y1)
  yy = matrix(c(y1,y2), 2*c,1)

  zero <- c(rep(0,c))
  one <- c(rep(1,c))
  J <- one%*%t(one)
  DD <- diag(c)
  A <- J-(0.5)*DD
  for(i in 1:c) {
    for(j in 1:c) {
      if(j>i) A[i,j] <- 0
    } }

  Z <- kronecker(diag(2), matrix(1,c,1))

  ### Define strata. Two groups in this case
  strata <- c(rep(1,c), rep(2,c))

  ### Define restriction function
  h.fct <- function(m) {
    p<-diag(c(1/(Z%*%t(Z)%*%m)))%*%m
    t(p[(c+1):(2*c)])%*%A%*%p[1:c]-theta.null
  }

  ### Use mph.fit function to estimate
  estimates = mph.fit(yy, h.fct = h.fct, h.mean = F, strata = strata, maxiter = 100,
                      m.initial = yy, norm.diff.conv = 10)

  return(estimates)
}

#' Calculate inverse of logit transformation
#'
#' @param x A numerical value
#' @return Back transformed value
#' @keywords internal
inv.logit = function(x){
  exp(x)/(1+exp(x))
}

#' Logit transformation
#'
#' @param x A numerical value
#' @return Logit transformed value
#' @keywords internal
logit = function(x){
  log(x/(1-x))
}

#' Calculate confidence interval of DOOR probability based on Halperin et al. (1989)'s method
#'
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param n1,n2 Sample sizes of group 1, group 2; must be specified if data_type = "prop"
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#' @param summary_obj A object returned by `door_summary()`; Alternative
#' input for y1 and y2
#' @param conf_level Confidence level
#'
#' @return Halperin et al. (1989)'s CI
#' @export
#'
#' @references reference
#'
#' @seealso [door_ci()]
#' @include var_pi.R
#' @include calc_doorprob.R
#'
#' @examples
#' y1 = c(60, 30, 10)
#' y2 = c(50, 40, 10)
#' halperin_ci(y1, y2)
#'
#' ## $halperin_ci
#' ## [1] 0.4734504 0.6147386
halperin_ci <- function(y1 = NULL, y2 = NULL, n1 = NULL, n2 = NULL,
                        data_type = c("freq", "prop"), summary_obj = NULL, conf_level = 0.95){
  # Error checking
  data_type <- match.arg(data_type)

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
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data = summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    n1 <- sum(y1, na.rm = T)
    n2 <- sum(y2, na.rm = T)
    p1 <- y1 / n1
    p2 <- y2 / n2
  }else(
    stop("Missing required DOOR outcome information")
  )

  # chi square statistic
  alpha <- 1 - conf_level
  chisq <- qchisq(1-alpha, 1)
  # Actual DOOR prob
  pi_hat <- calc_doorprob(y1 = p1, y2 = p2, data_type = "prop")[[1]]
  # Estimates
  vars <- var_pi(p1,p2,n1,n2)
  # CI
  gamma <- (n1+n2-1) - (n1+n2-2) * vars$theta
  C <- gamma * chisq / (n1*n2)
  inner <- sqrt(C^2 + 4 * C * pi_hat * (1-pi_hat))
  lower <- (C + 2*vars$pi_hat - inner) / (2*(C+1))
  upper <- (C + 2*vars$pi_hat + inner) / (2*(C+1))

  # Return
  list(halperin_ci = c(lower[[1]], upper[[1]]))
}


#' Calculate confidence interval of DOOR probability based on inverse hyperbolic tangent transformation of Wald-type CI
#'
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param n1,n2 Sample sizes of group 1, group 2; must be specified if method = "prop"
#' @param summary_obj A object returned by `individual_to_summary()`; Alternative
#' input for y1 and y2
#' @param conf_level Confidence level
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#'
#' @return Inverse hyperbolic tangent transformation CI
#' @export
#'
#' @seealso [door_ci()]
#' @include var_pi.R
#' @include calc_doorprob.R
#'
#' @examples
#' inv_tanh_ci(c(60,30,10), c(50,40,10))
inv_tanh_ci <- function(y1 = NULL, y2 = NULL, n1 = NULL, n2 = NULL,
                        data_type = c("freq", "prop"), summary_obj = NULL, conf_level = 0.95){
  # Error checking
  data_type <- match.arg(data_type)

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
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data = summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    n1 <- sum(y1, na.rm = T)
    n2 <- sum(y2, na.rm = T)
    p1 <- y1 / n1
    p2 <- y2 / n2
  }else(
    stop("Missing required DOOR outcome information")
  )

  ## Z statistic
  alpha <- 1 - conf_level
  z <- qnorm(1-alpha/2)
  ## Actual DOOR prob
  pi_hat <- calc_doorprob(y1 = p1, y2 = p2, data_type = "prop")[[1]]
  ## Estimates
  vars <- var_pi(p1,p2,n1,n2)
  logit_pi <- log(pi_hat/(1-pi_hat))
  logit_sdf <- sqrt(vars$first)/(pi_hat*(1-pi_hat))

  ## First order
  lower_f <- logit_pi - z*logit_sdf
  upper_f <- logit_pi + z*logit_sdf

  ## Inverse logit of CI
  lb = inv.logit(lower_f)
  ub = inv.logit(upper_f)

  list(ci = c(lb, ub))
}


#' Calculate pseudo score type confidence interval of DOOR probability
#'
#' Some code of this function is adpated from the now-archived CRAN package "cta",
#' originally authored by Joseph B. Lang. The original package was licensed under GPL-2,
#' and the adapted code complies with this license.
#'
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param n1,n2 Sample sizes of group 1, group 2; must be specified if method = "prop"
#' @param summary_obj A object returned by `individual_to_summary()`; Alternative
#' input for y1 and y2
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#' @param conf_level Confidence level
#' @param cil,ciu Initial guesses of lower and upper limit, respectively
#' @param epsilon Convergence tolerance. Default to 1e-4
#' @param maxiter Maximum iteration
#'
#' @return pseudo-score type CI and the number of iterations to calculate the lower bound and
#' upper bound
#'
#' @seealso [door_ci()]
#' @export
#'
#'
#' @examples
#' pseudo_score_ci(c(60,30,10), c(50,40,10))
pseudo_score_ci <- function(y1 = NULL, y2 = NULL, n1 = NULL, n2 = NULL, summary_obj = NULL,
                            data_type = c("freq", "prop"), cil = 0.4, ciu = 0.6,
                            conf_level = 0.95, epsilon = 1e-4, maxiter = 100){
  # Error checking
  data_type <- match.arg(data_type)

  if(is.null(summary_obj) & data_type == "prop"){
    if(is.null(y1) | is.null(y2)) stop("Missing DOOR outcome proportion informaion")
    if(is.null(n1) | is.null(n2)) stop("Missing sample size(s) information for proportion data")
  }

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
      y1 <- p1 * n1
      y2 <- p2 * n2
    }
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data = summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    n1 <- sum(y1, na.rm = T)
    n2 <- sum(y2, na.rm = T)
    p1 <- y1 / n1
    p2 <- y2 / n2
  }else(
    stop("Missing required DOOR outcome information")
  )

  ## chi square statistic
  alpha <- 1 - conf_level
  chisq <- qchisq(1 - alpha, 1)
  ## Actual DOOR prob
  pi_hat <- calc_doorprob(y1 = p1, y2 = p2, data_type = "prop")[[1]]
  ## Get initial values for upper and lower. cil is the Halperin lower bound
  pi0_l <- cil
  pi1_l <- cil + 0.01
  pi0_u <- ciu
  pi1_u <- ciu - 0.01
  ## Estimated DOOR prob
  L = length(p1)
  p_0_l <- reml(y1,y2,pi0_l)$p
  p_1_l <- reml(y1,y2,pi1_l)$p
  p_0_u <- reml(y1,y2,pi0_u)$p
  p_1_u <- reml(y1,y2,pi1_u)$p
  ## Start loop for lower bound
  tol_l <- 1
  iter_l <- 1

  ## Store the ps statistics
  ps_vec_l <- c()
  lower_vec <- c()
  while(tol_l > epsilon & iter_l <= maxiter){
    # Calculate next ps values
    ps0 <- (pi_hat - pi0_l)^2 / var_pi(p_0_l[1:L], p_0_l[(L+1):(2*L)], n1, n2)$halperin
    ps1 <- (pi_hat - pi1_l)^2 / var_pi(p_1_l[1:L], p_1_l[(L+1):(2*L)], n1, n2)$halperin
    pi_new <- (pi0_l * (ps1 - chisq) - pi1_l * (ps0 - chisq)) / (ps1-ps0)
    ps_vec_l <- c(ps_vec_l, ps1)
    lower_vec <- c(lower_vec, pi_new)
    ## Check if calculated value is in bound
    if(pi_new <= 0 | pi_new >= 1){
      tol_l <- 1
      pi_new <- abs(pi1_l) + 0.005
    }else if(pi_new > pi_hat){
      tol_l <- 1
      pi_new <- pi1_l - 0.005
    }
    # Update values
    pi0_l <- pi1_l
    pi1_l <- pi_new
    p_0_l <- reml(y1,y2,pi0_l)$p
    p_1_l <- reml(y1,y2,pi1_l)$p
    # Loop values
    tol_l <- abs(pi_new - pi0_l)
    iter_l <- iter_l + 1

    ## If maxiter is reached, use the value that is cloest to 3.841 (chi-squared with 1,alpha)
    if(iter_l == maxiter){
      pi_new <- lower_vec[which.min(abs(ps_vec_l - 3.841))]
    }

  }
  lower <- pi_new[[1]]

  ## Start loop for upper bound
  tol_u <- 1
  iter_u <- 1

  ## Store the ps statitics
  ps_vec_u <- c()
  upper_vec <- c()
  while(tol_u > epsilon & iter_u <= maxiter){
    ## Calculate next ps values
    ps0 <- (pi_hat - pi0_u)^2 / var_pi(p_0_u[1:L],p_0_u[(L+1):(2*L)],n1,n2)$halperin
    ps1 <- (pi_hat - pi1_u)^2 / var_pi(p_1_u[1:L],p_1_u[(L+1):(2*L)],n1,n2)$halperin
    pi_new <- (pi0_u*(ps1-chisq) - pi1_u*(ps0-chisq))/(ps1-ps0)
    ps_vec_u <- c(ps_vec_u, ps1)
    upper_vec <- c(upper_vec, pi_new)
    ## Check if calculated value is in bound
    if(pi_new <= 0 | pi_new >= 1){
      tol_u <- 1
      pi_new <- pi1_u + 0.005
    }else if(pi_new < pi_hat){
      tol_u <- 1
      pi_new <- pi1_u + 0.005
    }
    # Update values
    pi0_u <- pi1_u
    pi1_u <- pi_new
    p_0_u <- reml(y1,y2,pi0_u)$p
    p_1_u <- reml(y1,y2,pi1_u)$p
    # Loop values
    tol_u <- abs(pi_new - pi0_u)
    iter_u <- iter_u+1

    ## If maxiter is reached, use the value that is cloest to 3.841 (chi-squared with 1,alpha)
    if(iter_u == maxiter){
      pi_new <- upper_vec[which.min(abs(ps_vec_u - 3.841))]
    }

  }
  upper <- pi_new[[1]]

  return(
    list(ps_ci = c(lower, upper), iterations = c(iter_l, iter_u))
  )
}


#' Calculate confidence intervals for DOOR probability
#'
#' This is a wrapper function for all CI calculation functions
#'
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param n1,n2 Sample sizes of group 1, group 2; must be specified if method = "prop"
#' @param summary_obj A object returned by `individual_to_summary()`; Alternative
#' input for y1 and y2
#' @param conf_level Confidence level
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#' @param ci_method One of "all" for all available methods, "halperin" for Halperin et al. (1989)'s
#' method, "ps_h" for pseudo-score approach for Halperin's method, "tanh" for inverse
#' hyperbolc tangent transformed method
#' @param ... Additional parameters passed for calculating pseudo-score type confidence interval
#'
#' @seealso [halperin_ci()], [pseudo_score_ci()]
#'
#' @return List of CIs
#' @export
#'
#' @examples
#' door_ci(c(60,30,10), c(50,40,10), ci_method = "all")

door_ci <- function(y1 = NULL, y2 = NULL, n1 = NULL, n2 = NULL, summary_obj = NULL,
                    conf_level = 0.95, data_type = c("freq", "prop"),
                    ci_method = c("all", "halperin", "ps_h", "tanh"), ...){
  # Error checking
  data_type <- match.arg(data_type)
  ci_method <- match.arg(ci_method)

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
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data = summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    n1 <- sum(y1, na.rm = T)
    n2 <- sum(y2, na.rm = T)
    p1 <- y1 / n1
    p2 <- y2 / n2
  }else(
    stop("Missing required DOOR outcome information")
  )

  # DOOR Prob
  doorp <- calc_doorprob(y1 = p1, y2 = p2, data_type = "prop")[[1]]

  # Calculate CIs
  if(ci_method == "all"){
    halp <- halperin_ci(y1 = p1, y2 = p2, n1 = n1, n2 = n2,
                        conf_level = conf_level, data_type = "prop")$halperin_ci

    tanh <- inv_tanh_ci(y1 = p1, y2 = p2, n1 = n1, n2 = n2,
                        conf_level = conf_level, data_type = "prop")$ci

    ps_h <- tryCatch({pseudo_score_ci(y1 = p1, y2 = p2, n1 = n1, n2 = n2,
                                      conf_level = conf_level, data_type = "prop",
                                      cil = halp[1], ciu = halp[2], ...)$ps_ci},
                     error = function(e){c(NaN, NaN)})

    ci <- list("Halperin et al. (1989)'s CI" = halp,
               "Inverse hyperbolic tangent transformation-based CI" = tanh,
               "Pseudo-score approach for Halperin et al. (1989)'s CI" = ps_h)
  }else if(ci_method == "halperin"){
    halp <- halperin_ci(y1 = p1, y2 = p2, n1 = n1, n2 = n2,
                        conf_level = conf_level, data_type = "prop")$halperin_ci
    ci <- list("Halperin et al. (1989)'s CI" = halp)
  }else if(ci_method == "ps_h"){
    halp <- halperin_ci(y1 = p1, y2 = p2, n1 = n1, n2 = n2,
                        conf_level = conf_level, data_type = "prop")$halperin_ci
    ps_h <- pseudo_score_ci(y1 = p1, y2 = p2, n1 = n1, n2 = n2,
                            conf_level = conf_level, data_type = "prop",
                            cil = halp[1], ciu = halp[2], ...)$ps_ci
    ci <- list("Pseudo-score approach for Halperin et al. (1989)'s CI" = ps_h)
  }else if(ci_method == "tanh"){
    tanh_ci = inv_tanh_ci(y1 = p1, y2 = p2, n1 = n1, n2 = n2,
                          conf_level = conf_level, data_type = "prop")$ci
    ci <- list("Inverse hyperbolic tangent transformation-based CI" = tanh_ci)
  }

  list("DOOR probability" = doorp,
       "Confidence level" = conf_level,
       "Confidence interval" = ci)
}


