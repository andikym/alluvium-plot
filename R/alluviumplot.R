#' Alluvial Plot Function
#'
#' This function allows you to plot an alluvial/Sankey diagram
#' @docType data
#'
#' @usage data(melt_df)
#' 
#' @format An object of class \code{"dataframe"}
#' 
#' @param df A dataframe containing each variable as a column; must be in molted form (see reshape package for details)
#' @param x_axis the column in df containing the x axis information
#' @param grouping the column in df containing the group identity information
#' @param weights the column containing the y axis information
#' @param plot_title str; title of your plot
#' @param xlab str; x-axis label
#' @param ylab str; y-axis label
#' @param axis_breaks vector of how the x axis should be partitioned
#' @param has_lines if TRUE, includes vertical line breaks separating along the axis_breaks
#' @param line_breaks a vector that contains the x-values for where the lines should be manually placed. Only functional if has_lines = TRUE 
#' @param add_values if TRUE, includes values of the weights corresponding to the alluvium strata
#' @keywords alluvium plot Sankey
#' @export
#' @examples
#' yearaxes <- seq(1990, 2010, 5)
#' yearbreaks <- c(1991.3,1993.7,1996.3,1998.7,2001.3,2003.7,2006.3,2008.7)
#' plot_alluvium(df = melt_df, x_axis = melt_df$year, grouping = melt_df$factors, 
#' weights = melt_df$value, plot_title = "Risk Factors for Stroke in Blacks", 
#' xlab = "Year", ylab = "Cumulative Proportion", axis_breaks = yearaxes)

plot_alluvium <- function(df, x_axis, grouping, weights, plot_title, xlab, ylab,
                          axis_breaks, has_lines = FALSE, line_breaks = NULL,
                          add_values = FALSE){
  require(ggplot2)
  require(ggalluvial)
  
  g <- ggplot(data = df, aes(x = x_axis, weight = weights, alluvium = grouping)) +
    geom_alluvium(aes(fill = grouping, colour = grouping),
                  alpha = 0.5, decreasing = FALSE, width = 2.5, knot.pos = 0.05) + 
    scale_x_continuous(breaks = axis_breaks) + 
    labs(title = plot_title, 
         x = xlab,
         y = ylab)
  
  if(has_lines){
    if(add_values){
      return(g + geom_vline(xintercept = line_breaks, colour = "white") +
               geom_text_repel(aes(y = weights, x = x_axis, label = weights), 
                               size = 2, force = 6))
    }
    else{return(g + geom_vline(xintercept = line_breaks, colour = "white"))}
  }
  
  else {
    if(add_values){
      return(g + geom_text_repel(aes(y = weights, x = x_axis, label = weights), 
                                 size = 2, force = 6))
    }
    else{return(g)}
  }
}
