

#---------------------------------------------------------------------------------------------------------------------#

#' Create a named nested list
#'
#' Will come back to this another time. I have completely forgotten what this function does, but I know it is necessary for the "list_to_true_false" function.
#'
#' @param longer_list
#' @param shorter_list
#'
#' @return
#' @export
#'
#' @examples
create_named_nested_list<- function(longer_list, shorter_list)
{
  temp_list<- longer_list %in% shorter_list
  names(temp_list)<- longer_list
  return(temp_list)
}

#' List_to_True_False
#'
#'This is a function to convert categorical data from a single-column format a more usefule dummy-variable format
#'For example, in the single column format data would appear as follows:
#'     Months
#' 1   jan mar ...
#' 2   feb mar ...
#' 3   mar ...
#'
#'Using this function the output would be:
#'     Jan           Feb         Mar         ...
#' 1   TRUE          FALSE       TRUE        ...
#' 2   FALSE         TRUE        TRUE        ...
#' 3   FALSE         FALSE       TRUE        ...
#'
#'
#' @param x a col
#' @param seperator
#'
#' @return
#' @export
#'
#' @examples
List_to_True_False<- function(x, seperator)
{

  # x <- c("Structured survey;Qualitative",
  # "Structured survey;Semi-structured survey",
  # "Structured survey;Aggregated statistics;Climate Model",
  # "Structured survey;Qualitative",
  # "Structured survey;Qualitative",
  # "Structured survey;Remote sensing",
  # "Structured survey",
  # "Structured survey;Aggregated statistics",
  # "Structured survey;Remote sensing;Aggregated statistics",
  # "Structured survey;Aggregated statistics")
  # seperator <- ";"

  x[is.na(x)]<-"NA"
  x <- tolower(x)
  #x <- gsub("[[:punct:]]", " ", x)



  split <- strsplit(x,seperator,  fixed=T )

  # Checking whether the unique values are in the sublists of the nested list
  all_potential_value <- unique(unlist(strsplit(x,seperator, fixed=T )))

  boolean_nested_list<- lapply(split, function (x) create_named_nested_list(longer_list=all_potential_value,
                                                                            shorter_list=x))
  df_to_return <- data.frame(do.call(rbind, boolean_nested_list), check.names = F)
  return(df_to_return)
}

#' Collapse dummy variables
#'
#' Reverse of List_to_True_False
#'
#' @param data_frame
#' @param columns_to_collapse
#'
#' @return
#' @export
#'
#' @examples
collapse_multi_column_dummy <- function(data_frame, columns_to_collapse){

  data <- data_frame[,columns_to_collapse]

  list_to_return <- c()
  iterator <- 1

  for (column in colnames(data)){

    data[data[,column]==T,column] <- colnames(data)[which(colnames(data)==column)]
    data[data[,column]==F,column] <- "unique_value_to_remove"
    if (iterator==1){
      list_to_return<-data[,column]
    }
    if (iterator>1) {
      list_to_return<- paste(list_to_return,data[,column], sep = ", ")
    }
    iterator <- iterator +1

  }
  list_to_return <- gsub("unique_value_to_remove, ","", list_to_return)
  list_to_return <- gsub(", unique_value_to_remove","", list_to_return)
  list_to_return <- gsub("unique_value_to_remove",NA, list_to_return)

  list_to_return<-trimws(list_to_return)

  return(list_to_return)
}



#' Co-occurence matrix
#'
#' A useful function for calculated the co-occurences of dummy variables
#'
#' @param TRUE_FALSE_DATAFRAME
#'
#' @return
#' @export
#'
#' @examples
co_occurence_matrix <- function(TRUE_FALSE_DATAFRAME) {
  co_occurence <- TRUE_FALSE_DATAFRAME %>%
    as.matrix() %>%
    crossprod()
  return (co_occurence)

}


