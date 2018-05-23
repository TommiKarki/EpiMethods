#' Simple bar chart with ECDC colours
#'
#' Somewhat pipe (dplyr) friendly bar chart. Allows a stacked bar chart with groupings.
#' @param data Your data frame
#' @param x vector in your dataframe, given without quotes
#' @param group vector in your dataframe, given without quotes
#' @param col_scale colour scale, defaults to "qualitative"
#' @param xtitle x axis title, defaults to x
#' @param ytitle y axis title, defaults to "count"
#' @param legend_title legend title, if applicable defaults to group 
#' @author Tommi Karki
#' @keywords bar chart
#' @seealso ggplot2
#' @export
#' @examples
#' Create dummy data
#' mydat <- data.frame(ID = c(seq(1,10,1)),
#' Gender = c(rep(c("F", "M"),5)),
#' AgeGroup = c(rep(c("0-18", "18-65", "65+"),3), "65+"),
#' Case = c(1,1,1,0,0,1,1,1,1,1),
#' DateOfOnset = as.Date(c("2017-06-11", "2017-06-11", 
#'                        "2017-06-11", NA, NA, "2017-06-10", 
#'                        "2017-06-14", "2017-06-14",
#'                        "2017-06-19", "2017-06-19")),
#' Month = c(sample(c(3:6),5, replace = TRUE), sample(c(1:12),5)),
#' Week = c(sample(c(10:12),5, replace = TRUE), sample(c(1:53),5)))
#' 
#' bar(mydat, Gender)
#' 
#' #Stacked bar by AgeGroup
#' bar(mydat, Gender, group = AgeGroup)
#' 
ggbar <- function(data, xvar, group=NULL, col_scale = c("qualitative", "green", "blue",  "red"),
                  xtitle=xvar, ytitle="count", legend_title = group){
  xvar <- deparse(substitute(xvar))
  group <- deparse(substitute(group))
  col_scale <- match.arg(col_scale)
  data[[xvar]] <- as.factor(data[[xvar]])
  FIGBREAKS <- pretty(seq(0, max(table(data[[xvar]])),
                           by = max(table(data[[xvar]]))/5))
  p1<-ggplot(data, aes_string(xvar, fill=group)) + 
    geom_bar() +
    scale_y_continuous(expand = c(0,0),
                       breaks = FIGBREAKS,
                       limits = c(0, max(FIGBREAKS))) +
    theme_classic() + 
    theme(axis.line.x = element_blank(),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 9),
          legend.key.width = unit(0.8, "cm"), 
          legend.key.size = unit(0.4, "cm")) +
    labs(x=xtitle, y=ytitle)
    if(group=="NULL") {
    p1 <- p1 + geom_bar(fill=SurvColors(col_scale)) +
    guides(fill=FALSE)
}else if(length(unique(data[[group]])) == 2){
  p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=2)) +
    guides(fill = guide_legend(title=legend_title))
}else if(length(unique(data[[group]])) == 3){
  p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=3)) +
    guides(fill = guide_legend(title=legend_title))
}else if(length(unique(data[[group]])) == 4){
  p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=4))+
    guides(fill = guide_legend(title=legend_title))
}else if(length(unique(data[[group]])) == 5){
  p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=5))+
    guides(fill = guide_legend(title=legend_title))
}else if(length(unique(data[[group]])) == 6){
  p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=6))+
    guides(fill = guide_legend(title=legend_title))
}else{
  p1 <- p1 + scale_fill_manual(values = SurvColors(col_scale, n=7))+
    guides(fill = guide_legend(title=legend_title))
}
  return(p1)
}


