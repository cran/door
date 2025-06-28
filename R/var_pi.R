#' Variance of DOOR probability
#'
#' Calculate multiple types of variances of DOOR probability
#' @param p1 Vector of DOOR outcome proportion distribution for group 1
#' @param p2 Vector of DOOR outcome proportion distribution for group 2
#' @param n1 Sample size of group 1
#' @param n2 Sample size of group 2
#'
#' @return DOOR probability, Wald-type variance, exact variance,
#'     Halperin variance; theta for Halperin method
#'
#' @include calc_doorprob.R

var_pi = function(p1,p2,n1,n2){
  # Error checking
  if(length(p1) != length(p2)){
    stop("Number of DOOR ranks between groups not equal")
  }

  # Calculate DOOR probability
  pi = calc_doorprob(p1, p2, data_type = "prop")[[1]]

  L = length(p1)
  outer_1 = c()
  outer_2 = c()
  outer_hh1 = c()
  outer_hh2 = c()
  for(k in seq(1,L-1)){
    i = k+1
    inner_1 = sum(p2[i:L],na.rm = T)
    inner_2 = sum(p1[1:k],na.rm = T)
    sum_1 = p1[k]*(p2[k]/2+inner_1)^2
    sum_2 = p2[k+1]*(p1[k+1]/2+inner_2)^2
    outer_1[k] = sum_1
    outer_2[k] = sum_2
    ## Halperin variance
    sum_a = p1[k]*((1-p2[k])*inner_1 - inner_1^2)
    sum_b = p2[k+1]*((1-p1[k+1])*inner_2 - inner_2^2)
    outer_hh1[k] = sum_a
    outer_hh2[k] = sum_b
  }
  A = sum(outer_1)+p1[L]*(p2[L]^2)/4
  B = sum(outer_2)+(p1[1]^2)*p2[1]/4
  ## Halperin
  A_hht = A - (1/(n2-1))*sum(outer_hh1) - 1/(4*(n2-1))*sum(p1*p2*(1-p2))
  B_hht = B - (1/(n1-1))*sum(outer_hh2) - 1/(4*(n1-1))*sum(p2*p1*(1-p1))
  donom = ((n1*n2-n1-n2+2)*pi-n1*n2*pi^2)/
          ((n1-1)*(n2-1))+A_hht/(n1-1)+B_hht/(n2-1)
  theta = ((n1+n2-2)*pi - (n2-1)*A_hht - (n1-1)*B_hht)/((n1+n2-2)*donom)
  theta = max(theta,0)
  theta = min(theta,1)
  if(is.nan(theta)) theta = 0

  first_order = (1/n1)*(A-pi^2) + (1/n2)*(B-pi^2)
  second_order = 1/(n1*n2)*(pi-(n1+n2-1)*pi^2+(n2-1)*A+(n1-1)*B-sum(p1*p2)/4)
  halperin = 1/(n1*n2)*((n1+n2-1)-(n1+n2-2)*theta)*pi*(1-pi)
  logit = first_order / (pi^2 * (1-pi)^2)

  list(
    pi_hat = pi,
    first = first_order,
    second = second_order,
    halperin = halperin,
    theta = theta,
    logit = logit
  )
}
