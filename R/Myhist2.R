EcdcHist <- function(data, xvar, fill = EcdcColors("red"), fontsize = 12){
  ggplot(data, aes({{xvar}})) +
    geom_histogram(fill = fill) +
    scale_y_continuous(expand = c(0,0)) +
    theme(text = element_text(size = fontsize), 
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA))
}
