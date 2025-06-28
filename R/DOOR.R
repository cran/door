#' @importFrom grDevices colorRampPalette terrain.colors
#' @importFrom graphics axis contour filled.contour mtext title
#' @importFrom methods is
#' @importFrom stats aggregate na.omit pnorm pt qchisq qnorm qt model.matrix pchisq runif

globalVariables(c("diff_ci_l", "diff_ci_u", "diff_est", "door_ci_l", "door_ci_u",
                "door_prob", "grade_key",
                "group", "group_numeric", "label", "lb", "perc", "perct",
                "pvalues", "type", "ub", "value"))
