#' @export
myhist <- function(x, y) {
  x %>% ggplot(aes({{y}})) + geom_histogram(fill = "blue")
  }
