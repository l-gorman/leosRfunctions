#' Summarise for table
#'
#' Suitable for "column" in the format c("resp_a;resp_b", "resp_c;resp_a")
#'
#'
#' @param column The column you wish to summarise
#' @param seperator The string used to seperate the values in the string response
#' @param percentage_cut_off The cutoff you want to use. For example, the
#'
#' @return returns a string with the numbers summarised
#' @export
#'
#' @examples
presenting_info_for_table <- function(column, seperator, percentage_cut_off){
  # column <- data$data_type
  # seperator <- ";"
  # percentage_cut_off <- 10

  df <- List_to_True_False(column, seperator = seperator)
  counts <- colSums(df)
  percentage_occurence <- round(counts*100/nrow(df))
  percentage_occurence <- percentage_occurence[order(percentage_occurence, decreasing = T)]
  other_values <- percentage_occurence[percentage_occurence<percentage_cut_off]
  percentage_occurence <- percentage_occurence[percentage_occurence>=percentage_cut_off]
  percentage_occurence["other"] <- sum(other_values)

  string_to_return <- paste0(names(percentage_occurence)," (", percentage_occurence,"%)")
  string_to_return <- paste0(string_to_return, collapse=", ")


  return(string_to_return)
}
