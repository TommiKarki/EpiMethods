#' Epicurver for aggregated data
#'
#' This function draws an epicurve for aggregated data. Draws a bar for 
#' each time unit (usually week, month, year, but also dates are possible). 
#' Works with data that has been pre-aggregated per time 
#' unit, and with a continuous x-axis (e.g. months or weeks as numbers). 
#' Allows using facet_grid() for quick and dirty month/week epicurve spanning over 
#' several years.
#' @param data Your data frame.
#' @param xvar Time variable, given without quotes.
#' @param yvar A variable with the number of cases for each time unit, given without quotes.
#' @param group If applicable, a grouping variable, given without quotes. Defaults to one group. 
#' In case of groupings, works up to 7 groups with ECDC qualitative colours. 
#' @param col_scale Colour scale for the bars, defaults to qualitative colour scale; select one of the following: "qualitative",
#' "green", "blue", "red".
#' @param border Bar border colour, defaults to no border (NA).
#' @param xtitle x-axis title.
#' @param Legend_title Legend title.
#' @param x_axis_ticks Tick marks for x-axis. Use ONLY if x-axis uses dates or months; select one of the following: "days", "2 days", 
#' "weeks", "2 weeks", "months", "years". Defaults to "days" with days and to numbers with numeric weeks or months. Selecting "weeks" prints only 
#' mondays for x-axis ticks. In case of "months" labelled automatically with abbreviated 
#' month names.
#' @param x_axis_limits x-axis limits, for dates use character dates as c("2017-01-01", "2017-12-31"), otherwise a numeric vector lenght of two.
#' @keywords epicurver, epicurve
#' @author Tommi Karki
#' @export
#' @examples
#' # Create dummy data
#' mydat <- data.frame(ID = c(seq(1,24,1)),
#' Case = c(sample(10:50, 24, replace = FALSE)),
#' Month = rep(c(seq(1,12,1)),2),
#' Gender = c(rep("F",12), rep("M", 12)))
#' 
#' # Plot one epicurve without any groupings
#' epicurver_aggr(data = mydat[1:12,], xvar = Month, yvar = Case, 
#' xtitle = "Month of onset")
#' 
#' # Plot by two level grouping (e.g. Gender), with tick marks as months
#' epicurver_aggr(data = mydat, xvar = Month, yvar = Case, 
#' xtitle = "Month of onset", group = Gender, x_axis_ticks = "months")
epicurver_aggr <- function(data, 
                      xvar,
                      yvar,
                      group = NULL,
                      col_scale = c("qualitative", "green", "blue", "red"),
                      border = NA,
                      xtitle = "Time",
                      legend_title = group,
                      x_axis_ticks = c(NULL, "days", "2 days", "weeks", "2 weeks", "months", "years"),
                      x_axis_limits = NULL) {
  
  xvar <- deparse(substitute(xvar))
  yvar <- deparse(substitute(yvar))
  group <- deparse(substitute(group))
  x_axis_ticks <- match.arg(x_axis_ticks)
  col_scale <- match.arg(col_scale)
  if(group == yvar){
  FIGBREAKS <- pretty(seq(0, max(data[[yvar]]),
                           by = max(data[[yvar]])/5))
  }else{
  FIGBREAKS <- pretty(seq(0, max(tapply(data[[yvar]], data[[xvar]], "sum")),
                          by = max(tapply(data[[yvar]], data[[xvar]], "sum"))/5))
  }
    
  # If x_axis_ticks is "months", the unit is months thus labelling 
  # could be abbreviated month names.
  
  if(x_axis_ticks == "months"){
    xlabs <- substring(month.name,1,3)
  }
  
  if(!is.null(x_axis_limits) & is(data[[xvar]], "Date")){
    x_axis_limits <- as.Date(x_axis_limits)}
  p1 <- ggplot(data, 
               aes_string(x = xvar, y = yvar, fill = group)) + 
    geom_bar(col=border, stat = "identity", width = 1) +
    theme_classic() +
    scale_y_continuous(expand = c(0,0), limits = c(0,max(FIGBREAKS)),
                       breaks = FIGBREAKS)
    if(is(data[[xvar]], "Date")){
    p1 <- p1 + scale_x_date(date_breaks = x_axis_ticks,
                 limits = x_axis_limits)
    }else if(x_axis_ticks == "months"){
    p1 <- p1 + scale_x_continuous(limits = x_axis_limits,
                                  breaks = seq(1,12,1),
                                  labels = xlabs)
    }else{
        p1 <- p1 + scale_x_continuous(breaks = seq(1, max(data[[xvar]], by = 1)))}
    p1 <- p1 + xlab(label = xtitle) +
    ylab("Number of cases") +
    theme(axis.text.x = element_text(size = 8, family = "Tahoma", colour = "black"),
          axis.text.y = element_text(size = 8, family = "Tahoma", colour = "black"),
          axis.title = element_text(family = "Tahoma", size = 9, colour = "black"),
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          legend.key.width = unit(0.8, "cm"), legend.key.size = unit(0.4, "cm"))
  if(group == "NULL") {
    p1 <- p1 + geom_bar(col=border, fill=SurvColors(col_scale), 
                        stat = "identity", width = 1) +
      guides(fill=FALSE)
  }else if(length(unique(data[[group]])) == 2){
    p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=2)) +
      guides(fill = guide_legend(title = legend_title))
  }else if(length(unique(data[[group]])) == 3){
    p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=3)) +
      guides(fill = guide_legend(title = legend_title))
  }else if(length(unique(data[[group]])) == 4){
    p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=4)) +
      guides(fill = guide_legend(title = legend_title))
  }else if(length(unique(data[[group]])) == 5){
    p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=5)) +
      guides(fill = guide_legend(title = legend_title))
  }else if(length(unique(data[[group]])) == 6){
    p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=6)) +
      guides(fill = guide_legend(title = legend_title))
  }else{
    p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=7)) +
      guides(fill = guide_legend(title = legend_title))
  }
  return(p1) 
}





