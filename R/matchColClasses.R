


#'Dataframe Column Classes Match
#'
#'Mutates second dataframe columns to match the column classes of first dataframe,
#'returns second dataframe
#'@param dataframe
#'@keywords match, mutate, classes, class, dataframe
#'@example matchColClasses(df1, df2)
#'@return second dataframe
#'@name matchColClasses
#'@export
#'
#'
matchColClasses <- function(df1, df2) {
  sharedColNames <- names(df1)[names(df1) %in% names(df2)]
  sharedColTypes <- sapply(df1[,sharedColNames], class)
  
  for (n in sharedColNames) {
    class(df2[, n]) <- sharedColTypes[n]
  }
  
  return(df2)
  
}

devtools::document()
