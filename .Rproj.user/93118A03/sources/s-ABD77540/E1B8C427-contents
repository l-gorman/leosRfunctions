
## Modified 5th July 2018, to remove NA column, by JH.

#This is a function to convert certain data from the format in the mother data set, to the format in the old data set.
#This means that old R scripts can be used to analyse these pieces of data
#For example, in th old format data appeared in the format

#     Jan           Feb         Mar         ...
# 1   TRUE          FALSE       TRUE        ...
# 2   FALSE         TRUE        TRUE        ...
# 3   FALSE         FALSE       TRUE        ...

#In the new format this would appear as

#     Months
# 1   jan mar ...
# 2   feb mar ...
# 3   mar ...

#To call this function enter  for example:

#wild_food_true_false<- List_to_True_False(dat_all$wildfood_collect_when)

#and this should return a TRUE FALSE data frame assigned to wild_food_true_false

#HOPE THIS WORKS

#Leo

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

