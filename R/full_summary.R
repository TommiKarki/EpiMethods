#' Full summarisation of a data frame
#'
#' A helper function to provide full summaries/tables of a data frame, summarises mean, median and 
#' quantiles for numerical/date vectors and tables for character/categorical vectors. To be prettified.
#' @param data Your data frame.
#' @param useNA whether to include NA values in the tables, defaults to "always". Use "no" to hide NA's and "ifany" to show only if there are any.
#' @param table_max_cat Defines how large tables (dimensions) are shown, defaults to max 5 categories. For vectors with more than given categories shows number of unique values and number of missing records. 
#' @keywords summary, table, codebook
#' @author Tommi Karki
#' @export
#' @examples
# Create dummy data
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
#' # Summaries/tables of all variables
#' full_summary(mydat)
#' @export

full_summary <- 
  function(data, useNA="always", table_max_cat=5) {
  z <- 1
  for(u in seq.int(1, ceiling(ncol(data)/5), by=1)){
    zx <- z+5
    if(zx > ncol(data)){
      zx <- ncol(data)
    }
    print(lapply(data[,c(seq.int(z,zx,by=1))], function(y) {
    if(is.factor(y) & length(unique(y))<=table_max_cat){
      table(as.factor(y), useNA = useNA)
    }else if(is.factor(y) & length(unique(y))>table_max_cat){
      paste0("Fctr vector with >", table_max_cat, " unique values;", 
             " N of unique values: ", length(unique(y)), 
             "; N of NA records: ", length(y[is.na(y)]))
    }else if(is.character(y) & length(unique(y))<=table_max_cat){
      table(as.factor(y), useNA = useNA)
    }else if(is.character(y) & length(unique(y))>table_max_cat){
      paste0("Chr vector with >", table_max_cat, " unique values;", 
             " N of unique values: ", length(unique(y)), 
             "; N of NA records: ", length(y[is.na(y)]))
    }else if(is.logical(y)){
      table(as.factor(y), useNA = useNA)
    }else{
      summary(y)
    }}))
    if(zx < ncol(data)){
    invisible(readline(prompt="Press [enter] to continue"))
    z <- z+5}
    }}