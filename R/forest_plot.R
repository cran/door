#' Forest plot render
#'
#' @param data a data frame for numerical values of the forest plot
#' @param text a data frame for the text part of the forest plot
#' @param xlab a character string of x-axis label
#' @param issum a logical vector indicating the summary row
#' @param xticks a vector specifying the x-axis tick marks
#' @param line_height a numeric value specifying the space between the rows
#'
#' @import forestplot
#' @return a forest plot object
#' @keywords internal
generate_forestp = function(data, text, xlab, issum, xticks, line_height=1.3){
  forestp = data %>% forestplot::forestplot(labeltext =text,
                                xlog = FALSE,
                                xticks = xticks,
                                graph.pos = 4,
                                graphwidth = unit(15,"cm"),
                                is.summary = issum,
                                col = forestplot::fpColors(box = "royalblue",
                                               line = "darkblue",
                                               summary = "darkblue",
                                               zero = "red"),
                                xlab = xlab,
                                align = c("l", "c","c","c","c","c"),
                                zero = 0.5,
                                lwd.zero = 2,
                                lwd.xaxis = 2,
                                #line.margin = unit(1,"cm"),
                                lineheight = unit(line_height, "cm"),
                                txt_gp = forestplot::fpTxtGp(label = list(grid::gpar(col = "Black")),
                                                 cex = 1.4,
                                                 ticks = grid::gpar(cex = 1.3),
                                                 xlab = grid::gpar(cex=1.3)),
                                colgap = unit(6,"mm"),
                                mar = unit(c(5,0,5,5), "mm"),
                                fn.ci_sum=function(col, size, ...) {
                                  forestplot::fpDrawNormalCI(clr.line = col, clr.marker = col, size=.5, lwd = 2, ...)
                                },
                                fn.ci_norm = function(size, lwd, ...){
                                  forestplot::fpDrawCircleCI(size = 0.5, lwd = 2, ...)
                                },
                                # grid = structure(seq(lower,upper,0.05),
                                #                  gp = gpar(lty = 2, col = "#CCCCFF")),
                                vertices = F
  )

  return(forestp)
}

#' Calculate lower/upper bound of forest plot given upper, lower of data
#'
#' @param lower a numerical vector of lower limits of confidence intervals
#' @param upper a numerical vector of upper limits of confidene intervals
#' @param mean  a numerical vector of DOOR probability estiamtes
#'
#' @return calculated upper and lower range of axis ticks of forest plot
#' @keywords internal
calc_bound = function(lower, upper, mean){
  min_lower = tryCatch(min(lower, na.rm = T), warning = function(w) return(min(mean, na.rm = T)))
  max_upper = tryCatch(max(upper, na.rm = T), warning = function(w) return(max(mean, na.rm = T)))

  lower_r = round(min_lower, 1)
  upper_r = round(max_upper, 1)

  if(upper_r <= 0.5){
    upper_r = 0.55
  }else if(upper_r <= max_upper){
    upper_r = upper_r + 0.05
  }

  if(lower_r >= 0.5){
    lower_r = 0.45
  }else if(lower_r >= min_lower){
    lower_r = lower_r - 0.05
  }

  return(c(lower_r, upper_r))
}


#' Generate cumulative DOOR forest plot
#'
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param n1,n2 Sample sizes of group 1, group 2; must be specified if method = "prop"
#' @param summary_obj An object returned by `individual_to_summary()`; Alternative
#' input for y1 and y2
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#' @param conf_level confidence level
#' @param ci_method methods for calculating confidence interval
#'
#' @return a forestplot object
#' @export
#'
#' @examples
#' y1 = c(60, 30, 10)
#' y2 = c(50, 40, 10)
#' door_cumulative_forestplot(y1, y2)
door_cumulative_forestplot <- function(y1 = NULL, y2 = NULL, n1 = NULL, n2 = NULL,
                         data_type = c("freq", "prop"), summary_obj = NULL,
                         conf_level = 0.95, ci_method = c("halperin", "ps_h", "tanh")){
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
    ARM.name <- c(deparse(substitute(y1)), deparse(substitute(y2)))
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data = summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    n1 <- sum(y1, na.rm = T)
    n2 <- sum(y2, na.rm = T)
    p1 <- y1 / n1
    p2 <- y2 / n2
    ARM.name <- summary_obj$intervention_label
  }else(
    stop("Missing required DOOR outcome information")
  )

  level = length(y1)
  ARM.length = c(n1, n2)
  door = cbind.data.frame(y1, y2)

  # Get cumulative information
  text = data.frame(text = rep(NA, level - 1))
  door_cumul = list()
  for(i in 1 : (level - 1)){
    cumul = data.frame(Rank = c(1,2),
                       g1 = c(sum(door[1:i, 1]), n1 - sum(door[1:i, 1])),
                       g2 = c(sum(door[1:i, 2]), n2 - sum(door[1:i, 2])))
    text[,1][i] = paste0("DOOR <= ",i," - ", "DOOR > ",i)
    door_cumul[[i]] = cumul
  }
  comp_num = length(door_cumul)
  comp_name = text[,1]

  ##### Create DOOR & CI for each components
  est_door = door_ci(y1 = y1, y2 = y2, conf_level= conf_level, ci_method = ci_method)
  door_overall = est_door$`DOOR probability`
  CI_overall = est_door$`Confidence interval`[[1]]

  ## Mann-Whitney U test
  u = door_test(y1 = y1, y2 = y2)
  p_u = ifelse(u$p.value<0.0001, "< 0.0001",
               ifelse(u$p.value>0.9999, "> 0.9999", sprintf("%.4f", u$p.value)))

  doorprob_list <- vector(length=comp_num)
  doorCI_list <- list()
  for(i in 1:comp_num){
    y1 = door_cumul[[i]]$g1
    y2 = door_cumul[[i]]$g2
    est_comp = door_ci(y1 = y1, y2 = y2, conf_level= conf_level, ci_method = ci_method)
    doorprob_list[i] = est_comp$`DOOR probability`
    doorCI_list[[i]] = est_comp$`Confidence interval`[[1]]
  }
  lower_bound = matrix(unlist(doorCI_list),nrow=2)[1,]
  upper_bound = matrix(unlist(doorCI_list),nrow=2)[2,]
  row_length = length(doorprob_list) + 4

  ##### Forest data ####
  forest_data <- structure(list(
    mean  = c(NA,NA,door_overall, NA,doorprob_list),
    lower = c(NA,NA,CI_overall[1],NA, lower_bound),
    upper = c(NA,NA,CI_overall[2],NA, upper_bound)),
    .Names = c("mean", "lower", "upper"),
    row.names = c(NA, -row_length),
    class = "data.frame")

  ##### Text data ####
  ## Col1
  text_col1 = c(NA,NA,"DOOR","Sequential Dichotomization of DOOR" , paste0("    ", comp_name) )
  ## Col4
  forest_data2 =  forest_data[-1,]*100
  text_col4 <- c(paste0("DOOR probability\n","(", (conf_level)*100,"% CI)"),paste(sprintf("%.1f",round(as.vector(forest_data2[,1]),1)),"% (",
                                                                               sprintf("%.1f",round(as.vector(forest_data2[,2]),1)),"%, ",
                                                                               sprintf("%.1f",round(as.vector(forest_data2[,3]),1)),"%)",sep=""))
  ## Col2
  ## proportion of cumulative data
  cumul_prop = data.frame()
  for(i in 1:length(door_cumul)){
    cumul_prop[i,1] = door_cumul[[i]][1,2] / sum(door_cumul[[i]][,2]) * 100
    cumul_prop[i,2] = door_cumul[[i]][1,3] / sum(door_cumul[[i]][,3]) * 100
    cumul_prop[i,3] = door_cumul[[i]][1,2]
    cumul_prop[i,4] = door_cumul[[i]][1,3]
  }
  text_col2 = paste0(cumul_prop[,3], " (", sprintf("%.1f", cumul_prop[,1]),"%)")
  text_col2 = c(paste0(ARM.name[1],"\n (N=",ARM.length[1],")\n n(%)"),
                NA,NA,NA,
                text_col2)

  ## Col3
  text_col3 = paste0(cumul_prop[,4], " (",  sprintf("%.1f", cumul_prop[,2]),"%)")
  text_col3 = c(paste0(ARM.name[2],"\n (N=",ARM.length[2],")\n n(%)"),
                NA,NA,NA,
                text_col3)

  text_col4[grepl("NA",text_col4)] = NA
  ## Col5
  text_col5 = c("p-value",NA,p_u,rep(NA,comp_num+1))

  forest_text <- cbind(text_col1,
                       text_col2,
                       text_col3,
                       text_col4,
                       text_col5)

  # Forest plot parameters
  bounds = calc_bound(forest_data$lower, forest_data$upper, forest_data$mean)
  lower = bounds[1]
  upper = bounds[2]

  xticks = seq(from = lower, to = upper, by = 0.05)
  tick_label = paste0(xticks * 100, "%")
  attr(xticks, "labels") = tick_label
  xlab = paste0("Probability of a more desirable result in ", ARM.name[1], " vs. ", ARM.name[2], "\n",
                ARM.name[2] ," more desirable"," <--------------------------------> ", ARM.name[1], " more desirable")
  issum = c(T,T,T, rep(F, nrow(forest_text) - 3))
  height = 1

  ## Render plot
  forest_data$lower[which(is.nan(forest_data$lower))] = forest_data$mean[which(is.nan(forest_data$lower))] - 0.0001
  forest_data$upper[which(is.nan(forest_data$upper))] = forest_data$mean[which(is.nan(forest_data$upper))] + 0.0001

  forestp = generate_forestp(forest_data, forest_text, xlab, issum, xticks, line_height = height)
  forestp
}



#' Create DOOR component forest plot
#'
#' @param comp_table a data frame of DOOR components. See example.
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param n1,n2 Sample sizes of group 1, group 2; must be specified if method = "prop"
#' @param summary_obj An object returned by `individual_to_summary()`; Alternative
#' input for y1 and y2
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#' @param conf_level confidence level
#' @param ci_method method for confidence interval calculation; one of "halperin",
#' "ps_h", "ps_tanh"
#'
#' @return a forest plot object
#' @export
#'
#'
#' @examples
#' comp_table = data.frame(compname = c("A", "B"), trt = c(30, 20), ctr = c(40, 25))
#' y1 = c(60, 30, 10)
#' y2 = c(60, 30, 10)
#' door_component_forestplot(comp_table = comp_table,
#'                           y1 = y1,
#'                           y2 = y2)
door_component_forestplot <- function(comp_table = NULL, y1 = NULL, y2 = NULL, n1 = NULL, n2 = NULL,
                                       data_type = c("freq", "prop"), summary_obj = NULL,
                                       conf_level = 0.95, ci_method = c("halperin", "ps_h", "ps_tanh")){

  data_type <- match.arg(data_type)
  ci_method <- match.arg(ci_method)

  if(is.null(comp_table)){
    if(is.null(summary_obj)) stop("Missing DOOR component information")
    if(is.null(summary_obj$door_components)) stop("Missing DOOR component information")
  }

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
      comp_table[, 2:3] <- comp_table[, 2:3] * cbind(c(n1, n1), c(n2, n2))
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
    ARM.name <- summary_obj$intervention_label
  }

  if(!is.null(comp_table)){
    comp_num = nrow(comp_table)
    comp_name = comp_table[,1]
  }else if(!is.null(summary_obj)){
    comp_num = length(summary_obj$door_components)
    comp_name = sapply(summary_obj$door_components, function(x) colnames(x)[1])
  }

  alpha = 1 - conf_level
  ARM.length = c(n1,n2)
  door = cbind.data.frame(y1, y2)

  ###### Overall DOOR
  est_door = door_ci(y1 = y1, y2 = y2, conf_level= conf_level, ci_method = ci_method)
  door_overall = est_door$`DOOR probability`
  CI_overall = est_door$`Confidence interval`[[1]]

  ## Mann-Whitney U test
  u = door_test(y1 = y1, y2 = y2)
  p_u = ifelse(u$p.value < 0.0001, "< 0.0001",
               ifelse(u$p.value > 0.9999, "> 0.9999",
                      sprintf("%.4f", u$p.value)))

  ##### Create DOOR & CI for each components
  doorprob_list <- vector(length=comp_num)
  doorCI_list <- list()

  if(!is.null(comp_table)){
    for(i in 1:comp_num){
      y1 = c(n1 - comp_table[i,2], comp_table[i,2])
      y2 = c(n2 - comp_table[i,3], comp_table[i,3])
      est_comp = door_ci(y1, y2, conf_level = conf_level, ci_method = ci_method)
      doorprob_list[i] = est_comp$`DOOR probability`
      doorCI_list[[i]] = est_comp$`Confidence interval`[[1]]
    }
  }else if(!is.null(summary_obj)){
    comp_origin <- summary_obj$door_components
    for(i in 1:comp_num){
      y1 = comp_origin[[i]][,2]
      y2 = comp_origin[[i]][,3]
      est_comp = door_ci(y1, y2, conf_level = conf_level, ci_method = ci_method)
      doorprob_list[i] = est_comp$`DOOR probability`
      doorCI_list[[i]] = est_comp$`Confidence interval`[[1]]
    }
  }

  lower_bound = matrix(unlist(doorCI_list),nrow=2)[1,]
  upper_bound = matrix(unlist(doorCI_list),nrow=2)[2,]
  row_length = length(doorprob_list) + 4

  ##### Forest data ######
  forest_data <- structure(list(
    mean  = c(NA, NA, door_overall, NA, doorprob_list),
    lower = c(NA, NA, CI_overall[1], NA, lower_bound),
    upper = c(NA, NA, CI_overall[2], NA, upper_bound)),
    .Names = c("mean", "lower", "upper"),
    row.names = c(NA, -row_length),
    class = "data.frame")

  ##### Text data ######
  ## Col1
  text_col1 = c(NA,NA,"DOOR","DOOR Component                    ", paste0("    ", comp_name) )

  ## Col2
  if(is.null(summary_obj)){  # If y1, y2 are supplied
    if(data_type == "freq"){
      col2 = comp_table[,2] / n1
    }else{
      col2 = comp_table[,2]
    }
  }else{ # If summary object is supplied
    col2 = sapply(summary_obj$door_components, function(x) (x[,2]/n1)[[1]][1])
  }
  text_col2 = paste0(sprintf("%.1f", col2 * 100), "%")
  text_col2 = c(paste0(ARM.name[1], "\n (N=", ARM.length[1], ")\n %"),
                NA, NA, NA,
                text_col2)

  ## Col3
  if(is.null(summary_obj)){  # If y1, y2 are supplied
    if(data_type == "freq"){
      col3 = comp_table[,3] / n1
    }else{
      col3 = comp_table[,3]
    }
  }else{ # If summary object is supplied
    col3 = sapply(summary_obj$door_components, function(x) (x[,3]/n1)[[1]][1])
  }
  text_col3 = paste0(sprintf("%.1f", col3 * 100),"%")
  text_col3 = c(paste0(ARM.name[2],"\n (N=",ARM.length[2],")\n %"),
                NA,NA,NA,
                text_col3)

  ## Col4
  ## column4 is DOOR prob and CIs
  forest_data2 = forest_data[-1,] * 100
  text_col4 <- c(paste0("DOOR probability\n","(", (1-alpha)*100,"% CI)"),
                 paste(sprintf("%.1f",round(as.vector(forest_data2[,1]),1)),
                       "% (",
                       sprintf("%.1f",round(as.vector(forest_data2[,2]),1)),
                       "%, ",
                       sprintf("%.1f",round(as.vector(forest_data2[,3]),1)),
                       "%)", sep=""))
  text_col4[grepl("NA",text_col4)]=NA

  ## Col5
  text_col5 = c("p-value",NA,p_u,rep(NA,comp_num+1))

  forest_text <- cbind(text_col1,
                       text_col2,
                       text_col3,
                       text_col4,
                       text_col5)

  # Forest plot parameters
  bounds = calc_bound(forest_data$lower, forest_data$upper, forest_data$mean)
  lower = bounds[1]
  upper = bounds[2]

  xticks = seq(from = lower, to = upper, by = 0.05)
  tick_label = paste0(xticks * 100, "%")
  attr(xticks, "labels") = tick_label
  xlab = paste0("Probability of a more desirable result in ", ARM.name[1], " vs. ", ARM.name[2], "\n",
                ARM.name[2] ," more desirable"," <--------------------------------> ", ARM.name[1], " more desirable")
  issum = c(T,T,T, rep(F, nrow(forest_text) - 3))
  height = 1

  ## Render plot
  forest_data$lower[which(is.nan(forest_data$lower))] = forest_data$mean[which(is.nan(forest_data$lower))] - 0.0001
  forest_data$upper[which(is.nan(forest_data$upper))] = forest_data$mean[which(is.nan(forest_data$upper))] + 0.0001

  forestp = generate_forestp(forest_data, forest_text, xlab, issum, xticks, line_height = height)
  forestp
}



