#' List must be written in form:
#' temp <- c("a"=10,"c"=20)
#' Where "a" and "c" are the original values
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
