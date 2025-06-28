#' Generate DOOR standard color scheme
#'
#' @param n Number of DOOR categories
#'
#' @return colors
#'
#' @keywords internal
generate_color <- function(n){
  if(n==2){
    color = c("#36A834","#E1151A")
  }else if(n==3){
    color = c("#36A834", "#F7EC00" ,"#E1151A")
  }else if(n==4){
    color = c("#36A834", "#CDD500", "#F7C000","#E1151A" )
  }else if(n==5){
    color = c("#36A834", "#90C221", "#F7EC00", "#F69E00", "#E1151A")
  }else if(n==6){
    color = c("#31b44b","#7bbf41","#b5d032","#fbcc12","#f8a122","#ee3023")
  }else if(n==7){
    color = c("#36A834", "#70B52A", "#CDD500","#F7E100", "#F7C000","#EA622D","#E1151A")
  }else if(n==8){
    color = c("#31b44b","#7bbf41","#b5d032","#d7dd28","#f9ec18","#fbcc12","#f8a122","#ee3023")
  }else if(n>8){
    ramp = colorRampPalette(c("#31b44b","#7bbf41","#b5d032","#d7dd28","#f9ec18","#fbcc12","#f8a122","#ee3023"))
    color = ramp(n)
  }
  return(color)
}

### And a vector containing names for treatments
#' Create DOOR summary barplot
#'
#' @param y1,y2 Numeric vectors of DOOR proportion or frequency distribution for
#' group 1, group 2.
#' The entries should be ordered from most desirable to least desirable
#' @param summary_obj An object returned by `door_summary()`; Alternative
#' input for y1 and y2
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' when using y1 and y2
#'
#' @return a ggplot object
#' @export
#'
#' @import ggplot2
#'
#' @examples
#' y1 = c(60, 30, 10)
#' y2 = c(50, 40, 10)
#' door_barplot(y1, y2)
door_barplot = function(y1 = NULL, y2 = NULL, summary_obj = NULL,
                        data_type = c("freq", "prop")){

  # Error checking
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
    }
    trt_names <- c(deparse(substitute(y1)), deparse(substitute(y2)))
  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    summary_data = summary_obj[[1]]
    y1 <- summary_data[[2]]
    y2 <- summary_data[[3]]
    p1 <- y1 / sum(y1, na.rm = T)
    p2 <- y2 / sum(y2, na.rm = T)
    trt_names <- summary_obj$intervention_label
  }else(
    stop("Missing required DOOR outcome information")
  )

  ## Color palettes and levels
  data = cbind.data.frame(p1, p2)
  colnames(data) = trt_names
  rank_num = nrow(data)

  door_level = paste0("Rank ", 1 : rank_num)
  color = generate_color(rank_num)
  names(color) = door_level
  data$rank = door_level

  ## transform data
  data <- data %>%
    pivot_longer(cols = c(1,2), names_to = "group") %>%
    group_by(group) %>%
    mutate(perc = value, perct = paste0(sprintf("%.1f", perc * 100),"%"))

  data$rank = factor(data$rank, levels = rev(door_level))
  data$group = factor(data$group, levels = c(trt_names[2], trt_names[1]))
  data$group_numeric = ifelse(data$group == trt_names[1],
                              as.numeric(data$group) - 0.35,
                              as.numeric(data$group) + 0.35)

  p <- data %>%
    ggplot(aes(x=group, y=perc, fill=rank)) +
    geom_bar(position = "fill",stat = 'identity', width = 0.7) +
    geom_line(aes(x = group_numeric), position = position_stack(), colour = "#444444", linetype = 2) +
    geom_text(aes(label=perct), position = position_stack(vjust = 0.5),size = 5.5, check_overlap = T) +
    theme_classic() +
    theme(axis.title = element_text(size = 18),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 15),
          legend.position = "bottom") +
    scale_y_continuous(labels = paste0(seq(0,100,10),"%"), breaks = seq(0,1,0.1), expand = c(0.01,0.01)) +
    scale_fill_manual(values = color, guide = guide_legend(reverse = TRUE)) +
    labs(fill="", y = "Proportion (%)", x = "") +
    coord_flip()

  return(p)
}


#' Create DOOR component barplot
#'
#' @param comp_table A DOOR component table
#' @param n1,n2 Sample sizes of group 1, group 2
#' @param summary_obj An object returned by `individual_to_summary()`; Alternative
#' input for comp_table.
#' @param data_type Either "freq" for frequency input or "prop" for proportion input
#' if "comp_table" is used
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' comp_table = data.frame(compname = c("A", "B"), trt = c(30, 20), ctr = c(40, 25))
#' door_component_barplot(comp_table = comp_table, n1 = 100, n2 = 100)
door_component_barplot = function(comp_table = NULL, n1 = NULL, n2 = NULL,
                                  summary_obj = NULL,
                                  data_type = c("freq", "prop")){
  data_type <- match.arg(data_type)

  if(is.null(comp_table)){
    if(is.null(summary_obj)) stop("Missing DOOR component information")
    if(is.null(summary_obj$door_components)) stop("Missing DOOR component information")
  }

  if(is.null(summary_obj)){
    if(is.null(n1) | is.null(n2)) stop("Missing sample size(s) information for proportion data")
  }

  # Get proportions
  if(!is.null(comp_table)){   # If component table is supplied
    if(any(is.na(comp_table))){
      comp_table[which(is.na(comp_table))] <- 0
      warning("NA detected. NA values will be replaced by 0.")
    }

    if(data_type == "prop"){
      comp_table[, 2:3] <- comp_table[, 2:3] * cbind(c(n1, n1), c(n2, n2))
    }

    comp_num = nrow(comp_table)
    comp_name = comp_table[,1]
    trt_name = colnames(comp_table)[2:3]
    colnames(comp_table)[1] = "comp"

    data <- comp_table %>%
      pivot_longer(cols = c(2,3), names_to = "group")
    data$comp_value = ifelse(data$group == trt_name[1], n1 - data$value, n2 - data$value)
    data = data %>% gather(type, number, ends_with("value"))
    data$perc = ifelse(data$group == trt_name[1], data$number / n1, data$number / n2)
    data$perct = ifelse(data$perc == 0, " ", paste0(sprintf("%.1f", data$perc * 100), "%"))
    data$comp = factor(data$comp, levels = comp_name)
    data$group = factor(data$group, levels = rev(trt_name))

  }else if(!is.null(summary_obj) & is(summary_obj, "DOORSummary")){  # Else check if individual level data are supplied
    comp_table = summary_obj[[4]]
    n1 = sum(summary_obj[[1]][, 2], na.rm = T)
    n2 = sum(summary_obj[[1]][, 3], na.rm = T)
    comp_num = length(summary_obj$door_components)
    comp_name = sapply(summary_obj$door_components, function(x) colnames(x)[1])
    trt_name = summary_obj$intervention_label
    col1 = sapply(summary_obj$door_components, function(x) x[,2][1])
    col2 = sapply(summary_obj$door_components, function(x) x[,3][1])

    data <- data.frame(comp = comp_name, trt = col1, ctr = col2)
    colnames(data)[2:3] = trt_name
    data <- data %>%
      pivot_longer(cols = c(2,3), names_to = "group")
    data$comp_value = ifelse(data$group == trt_name[1], n1 - data$value, n2 - data$value)
    data = data %>% gather(type, number, ends_with("value"))
    data$perc = ifelse(data$group == trt_name[1], data$number / n1, data$number / n2)
    data$perct = ifelse(data$perc==0, " ", paste0(sprintf("%.1f", data$perc * 100), "%"))
    data$comp = factor(data$comp, levels = comp_name)
    data$group = factor(data$group, levels = rev(trt_name))
  }

  # Plot
  p = data %>%
    ggplot(aes(x=perc, y=group, fill=type)) +
    geom_col(position = "fill") +
    geom_text(aes(label=perct), position = position_stack(vjust = 0.5),size = 5.5,check_overlap = T) +
    theme_classic() +
    facet_grid(comp ~ ., switch = "y", labeller = label_wrap_gen(width = 25)) +
    theme(axis.title.x = element_text(size = 18),
          strip.text.y.left = element_text(angle = 0, size = 18, color = "#444444"),
          strip.placement = "outside",
          strip.background = element_rect(color = "white", fill = "#ecf0f1"),
          axis.text = element_text(size = 18),
          legend.text = element_text(size = 15),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title=element_text(hjust = 0.5, size = 15, color = "#444444")) +
    scale_x_continuous(labels = paste0(seq(0,100,10),"%"), breaks = seq(0,1,0.1), expand = c(0.01,0.01)) +
    scale_fill_manual(values = generate_color(2), guide = guide_legend(reverse = TRUE), label = c("No","Yes")) +
    labs(x = "Proportion (%)", y = "")

  p
}






