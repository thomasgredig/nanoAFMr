###################################################
#'
#' Height Height Correlation 2 Quick Plot
#'
#' @description
#' Plot the data generated when using AFM.hhcf2 and having fitparameters = \code{TRUE}
#' Quick Note: userLimitsx and userLimitsy can both be ignored since ggplot will allow you to add additional properties to plots after first 
#' initialization of ggplot object. However, they are left in this function as arguments to allow for quick and clean plotting.
#
#'
#' @author Ryan Mizukami
#' @param dataset a dataset (list) that was generated using AFM.hhcf2 and having fitparameters = \code{TRUE}
#' @param userLimitsx user defined axis limits for the x axis on a log scale. Default value is \code{NULL}
#' @param userLimitsy user defined axis limits for the y axis on a log scale. Default value is \code{NULL}
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_path scale_x_log10 scale_y_log10 theme_bw geom_label theme 
#'
#' @examples
#' AFM.hhcf2QuickPlot{r}
#'
##################################################
#' @export
AFM.hhcf2QuickPlot = function(dataset, userLimitsx = NULL, userLimitsy = NULL){
  data.Points = data.frame(r = dataset[[1]]$r.nm,
                           g = dataset[[1]]$g,
                           num = dataset[[1]]$num
  )
  data.Fit = data.frame(r.fit = dataset[[2]]$r.nm,
                        g.fit = dataset[[2]]$g
  )
  data.Labels = dataset[[3]]
  
  g = ggplot() +
    geom_point(data = data.Points, aes(x = r, y = g), color = "blue") +
    theme(legend.position = 'none') +
    scale_x_log10(limits = userLimitsx) +
    scale_y_log10(limits = userLimitsy) +
    ylab(expression(paste('g(r)', ' [nm'^2,']'))) +
    xlab('r [nm]') +
    theme_bw() +
    geom_path(data=data.Fit, col='red', aes(x = r.fit, y=g.fit)) +
    geom_label(data = data.Labels,
               aes(x = r.nm, y = g, fill = 'white',label=myLabel),
               colour = "white",
               fontface = "bold", hjust=-0.1) +
    theme(legend.position = 'none')
  
  print(g)
}
