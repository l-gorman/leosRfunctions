#' List must be written in form:
#' temp <- c("a"=10,"c"=20)
#' Where "a" and "c" are the original values
#'
#-------------------------------------------------------

#' Replace multiple values
#'
#'A function for switching values in one list, using a named list
#'
#' @param original_list The list you would like to convert
#' @param conversion_list The named list you will use conversion
#'
#' @return Returns the converted list
#' @export
#'
#' @examples
#'
replace_multiple_values <-function(original_list,conversion_list) {

  for (old_name in names(conversion_list))
  {
    if (sum(grepl(old_name,original_list, fixed = T))>0)
    {
      original_list <- gsub(old_name,unname(conversion_list[[old_name]]), original_list, fixed=T)
    }
  }

  return (original_list)
}



#' Replace multiple values (exact)
#'
#' A function for switching values in one list, using a named list. Useful in the case where you need exact conversion.
#' For example, if your list includes any values which are likely to overlap (e.g. man and woman.)
#'
#' @param original_list The list you would like to convert
#' @param conversion_list The named list you will use conversion
#'
#' @return Returns the converted list
#' @export
#'
#' @examples
#'
replace_multiple_values_exact <-function(original_list,conversion_list) {

  for (old_name in names(conversion_list))
  {
    if (sum(grepl(old_name,original_list, fixed = T))>0)
    {
      original_list <- gsub(paste0("\\b",old_name,"\\b"),unname(conversion_list[[old_name]]), original_list)
    }
  }

  return (original_list)
}
