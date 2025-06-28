#' Generate shapes for partial credit plot
#'
#' @param n number of shapes to generate
#' @return a vector of code shapes
#' @keywords internal
generate_shape = function(n){
  if(n==1){
    shape = c(16)
  }else if(n == 2){
    shape = c(16,15)
  }else if(n == 3){
    shape = c(16,15,17)
  }else if(n == 4){
    shape = c(16,15,17,18)
  }else if(n == 5){
    shape = c(16,15,17,18,7)
  }else if(n == 6){
    shape = c(16,15,17,18,7,0)
  }else if(n == 7){
    shape = c(16,15,17,18,7,0,1)
  }
}

#' Perform partial credit analysis given a grade key
#'
#' @param door a data frame of DOOR outcome distribution
#' @param pcvalues a numeric vector of grade key
#' @param n1,n2 samples size for group 1, 2
#'
#' @return results from a partial credit analysis
#' @keywords internal
assign_pc = function(door, pcvalues, n1, n2){
  g1 = door[,1]/n1
  g2 = door[,2]/n2
  m1 = sum(g1*pcvalues)
  m2 = sum(g2*pcvalues)
  s1 = sum(n1*g1*(pcvalues-m1)^2)/(n1-1)
  s2 = sum(n2*g2*(pcvalues-m2)^2)/(n2-1)
  t = t_test_summary(m1,m2,sqrt(s1),sqrt(s2),n1 = n1,n2 = n2)
  diff = m1 - m2

  return(list(diff = diff, p.value = t$p.value, ci = t$conf.int))
}

#' Calculates values for contour plots
#'
#' @param DOOR a data frame of DOOR outcome distribution
#' @param n_inc size of increments of partial credits
#'
#' @return calculated values for the contour plot
#' @keywords internal
pc.contour = function(DOOR, n_inc = 1){
  if(nrow(DOOR)>5 | nrow(DOOR)<3){
    stop("This function only supports DOOR ranks of 3 or 4")
  }

  sz = c(sum(DOOR[,1]), sum(DOOR[,2]))
  nlevel = nrow(DOOR)
  n = round(100 / n_inc)

  if(nlevel == 4){
    pc2 = seq(from=0, to=100, length.out = (n+1))
    pc3 = seq(from=0, to=100, length.out = (n+1))
    data.pc = expand.grid(pc2 = pc2, pc3 = pc3)
    data.pc$pc1 = 100; data.pc$pc4 = 0
    data.pc = data.pc[,c("pc1","pc2","pc3","pc4")]
  }else if(nlevel == 3){
    pc2 = seq(from=0,to=100,length.out = (n+1))
    pc1 = rep(100, n+1)
    pc3 = rep(0, n+1)
    data.pc = cbind.data.frame(pc1, pc2, pc3)
  }

  diff_pc = vector(length = nrow(data.pc))
  pvalues = vector(length = nrow(data.pc))
  lb = vector(length = nrow(data.pc))
  ub = vector(length = nrow(data.pc))
  for(i in 1:nrow(data.pc)){
    t = assign_pc(door = DOOR, pcvalues = data.pc[i,], n1 = sz[1], n2 = sz[2])
    diff_pc[i] = t$diff
    pvalues[i] = t$p.value
    lb[i] = t$ci[1]
    ub[i] = t$ci[2]
  }

  data.pc$pvalues = pvalues
  data.pc$diff_pc = diff_pc
  data.pc$lb = lb
  data.pc$ub = ub
  data.pc[data.pc$pc2 < data.pc$pc3, ] = NaN
  data.pc$nlevel = nlevel

  return(data.pc)
}

#' Generate contour plot for partial credit analysis
#'
#' The contour plot is for sensitivity analysis. Currently it supports given DOOR
#' outcome categories of three or four. The contour plot assigns every combinations
#' of grade keys given a DOOR outcome distribution
#'
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param n1,n2 Sample sizes of group 1, group 2; must be specified if method = "prop"
#' @param summary_obj An object returned by `individual_to_summary()`; Alternative
#' input for y1 and y2
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#' @param pc_inc Increment of partial credits
#' @param contour_inc Increment of contour lines
#'
#' @return A graph object
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' y1 <- c(60, 30, 10)
#' y2 <- c(50, 40, 10)
#' partial_credit_contour_plot(y1, y2)
partial_credit_contour_plot <- function(y1 = NULL, y2 = NULL, n1 = NULL, n2 = NULL,
                                        summary_obj = NULL, data_type = c("freq", "prop"),
                                        pc_inc = 10, contour_inc = 1){
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
    labels <- c(deparse(substitute(y1)), deparse(substitute(y2)))
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data <- summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    n1 <- sum(y1, na.rm = T)
    n2 <- sum(y2, na.rm = T)
    p1 <- y1 / n1
    p2 <- y2 / n2
    labels <- summary_obj$intervention_label
  }else(
    stop("Missing required DOOR outcome information")
  )

  # Calculate values
  DOOR <- cbind(y1, y2)
  contourp <- pc.contour(DOOR = DOOR, n_inc = pc_inc)
  nlevel <- length(y1)
  n <- round(100 / pc_inc)

  # Make plot
  ### Plot
  if(nlevel == 4){   # 4 level door
    diff_pc <- t(matrix(contourp$diff_pc, ncol=(n+1)))
    diff_pc <- rbind(diff_pc, rep(NaN, ncol(diff_pc)))
    diff_pc <- rbind(rep(NaN, ncol(diff_pc)), diff_pc)
    diff_pc <- cbind(diff_pc, rep(NaN, nrow(diff_pc)))
    diff_pc <- cbind(rep(NaN, nrow(diff_pc)), diff_pc)

    pvalue_pc <- t(matrix(contourp$pvalues,ncol=(n+1)))
    pvalue_pc <- rbind(pvalue_pc, rep(NaN, ncol(pvalue_pc)))
    pvalue_pc <- rbind(rep(NaN, ncol(pvalue_pc)), pvalue_pc)
    pvalue_pc <- cbind(pvalue_pc, rep(NaN, nrow(pvalue_pc)))
    pvalue_pc <- cbind(rep(NaN, nrow(pvalue_pc)), pvalue_pc)

    pc2 <- seq(from=0, to=100, length.out = (n+1))
    pc2 <- c(-3, pc2, 103)
    pc3 <- seq(from=0, to=100, length.out = (n+1))
    pc3 <- c(-3, pc3, 103)

    tgap <- max(diff_pc, na.rm = T) - min(diff_pc, na.rm = T)
    clevel <- round(tgap / contour_inc)

    # pc_emp = v$data3[,2:ncol(v$data3)]
    # pc_emp = as.matrix(pc_emp)
    # point_labels = pc_key_name()
    # point_shapes = generate_shape(length(point_labels))
    # point_colors = gg_color_hue(length(point_labels))
    #
    # pc_emp = matrix(c(100,100,100,0,100,0,0,0,100,80,60,0),ncol=3)
    # point_labels = c("A","B","C")

    filled.contour(x = pc3,y = pc2,z = pvalue_pc,levels=c(0,0.05,0.1,0.5,1),main="Difference in means of partial credit score",
                   key.title = title(main = expression(p-value), cex.main = 1.3),key.axes = axis(4, c(0,0.05,0.1,0.5,1), cex.axis = 1.3),
                   color.palette = terrain.colors,
                   plot.axes = {
                     axis(1,at = c(-3,0,20,40,60,80,100,103),labels = c("",0,20,40,60,80,100,""), cex.axis=1.5)
                     axis(2,at = c(-5,0,20,40,60,80,100,103),labels = c("",0,20,40,60,80,100,""), cex.axis=1.5)
                     contour(x=pc3,y=pc2,z=diff_pc,nlevel=clevel,labcex = 1.4, add=TRUE,method="simple")
                     # text(pc_emp[3,],pc_emp[2,],labels = point_labels,font=2,adj = c(-0.1,0.5),cex=1.8)
                     # text(0,0,labels = "B",font=2,adj = c(0,-0.1),cex=1.8)
                     # points(pc_emp[3,], pc_emp[2,], col=point_colors, pch=point_shapes, cex=1.7)
                     contour(x=pc3,y=pc2,z=diff_pc,level=0,drawlabels = FALSE,col = "blue",lwd=2, add=TRUE)
                   },
                   plot.title = {
                     title(xlab=labels[2],
                           #main="Difference in means of partial credit",
                           cex.lab=1.5)
                     mtext(labels[1],2,cex=1.5,line=3,las=0)
                   })

  }else if(nlevel == 3){  # 3 level door
    p <- ggplot2::ggplot(data = contourp, aes(x = pc2, y = diff_pc, colour = pvalues)) +
      geom_line(linewidth = 2) +
      geom_linerange(aes(ymin = lb, ymax = ub)) +
      geom_hline(yintercept = 0, color = "red") +
      theme_bw() +
      binned_scale(aesthetics = "color",
                   scale_name = "stepsn",
                   palette = function(x) c("green", "yellow", "orange", "grey"),
                   breaks = c(0.05, 0.1, 0.5),
                   limits = c(0,1),
                   show.limits = TRUE,
                   guide = "colorbar",
                   name = "p-value") +
      ylab("Difference in means in partial credit") +
      xlab(labels[1]) +
      theme(axis.title = element_text(size = 17, face = "bold"), axis.text = element_text(size = 16),
            legend.position = "none"
            #legend.key.height = unit(2,"cm"), legend.text = element_text(size = 16),
            #legend.title = element_text(size = 17, face = "bold")
      )
    p
  }

}

#' Partial credit plot
#'
#' @param pc_object an object returned by `partial_credit_analysis()`
#' @param ... additional arguments for other functions
#'
#' @return a plot object
#' @export
#'
#' @import scales
#' @examples
#' grade.key <- c(100, 60, 0)
#' y1 <- c(60, 30, 10)
#' y2 <- c(50, 40, 10)
#' pc_object <- partial_credit_analysis(grade_key = grade.key, y1 = y1, y2 = y2)
#' partial_credit_biplot(pc_object)
partial_credit_biplot <- function(pc_object, ...){
  if(!is(pc_object, "partial_credit_summary")) stop("Require an object from partial_credit_analysis()")

  pc_plot_data = pc_object$pc_summary
  ARM.name = pc_object$trt_labels
  overall_p = pc_object$overall.door.prob
  overall_p = round(overall_p, 3)

  legend_label = paste(pc_plot_data$grade_key, pc_plot_data$grade_key_value)

  pc_plot_data$label = paste0(pc_plot_data$grade_key)

  ylimit = max(max(abs(pc_plot_data$diff_ci_l), na.rm = T), max(abs(pc_plot_data$diff_ci_u), na.rm = T),na.rm = T)
  ylimit = ceiling(ylimit)
  xlimit = max(max(abs(pc_plot_data$door_ci_l - 0.5),na.rm = T), max(abs(pc_plot_data$door_ci_u - 0.5),na.rm = T), na.rm = T)
  xlimit = ceiling(xlimit * 100) / 100

  # Add door prob x axis tick
  def_breaks = labeling::extended(0.5 - xlimit, 0.5 + xlimit, m = 5)

  # axis titles
  fir_title = paste0("Probability of a more desirable result in ", ARM.name[1], "vs. ", ARM.name[2])
  sec_title = paste0(ARM.name[2], " more desirable <-------------> ", ARM.name[1], " more desirable")

  # adjust positions of the labels on plot
  xjust = 0.06 * xlimit
  yjust = 0.06 * ylimit

  shapes = generate_shape(7)

  p = ggplot(data = pc_plot_data, aes(x=door_prob, y = diff_est, color = grade_key)) +
    geom_point(aes(shape = grade_key), size = 6) +
    geom_text(aes(label = label), nudge_x = xjust, nudge_y = yjust, size = 6, key_glyph = draw_key_blank) +
    geom_linerange(aes(xmin = door_ci_l, xmax = door_ci_u), key_glyph = draw_key_blank) +
    geom_linerange(aes(ymin = diff_ci_l, ymax = diff_ci_u), key_glyph = draw_key_blank) +
    geom_vline(xintercept = 0.5, color = "#8B0000", linewidth = 1.2) +
    geom_vline(xintercept = overall_p, linetype = 2) +
    geom_hline(yintercept = 0, color = '#8B0000', linewidth = 1.2) +
    scale_x_continuous(limits = c(0.5 - xlimit, 0.5 + xlimit), expand = c(0.1, 0),
                       labels = scales::percent_format(accuracy = 0.1),
                       breaks = c(def_breaks, overall_p), minor_breaks = def_breaks) +
    scale_y_continuous(limits = c(-ylimit, ylimit), expand = c(0.1, 0)) +
    scale_shape_manual(name = "Grading Keys", labels = legend_label, values = shapes) +
    scale_color_discrete(l = 40, labels = legend_label, name = "Grading Keys") +
    theme_classic() +
    labs(x= paste0(fir_title, "\n", sec_title),
         y=paste0(sec_title, "\nDifference in Means in Partial Credits"))
  p
}



