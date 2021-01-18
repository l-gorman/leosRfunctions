library(dplyr)
library(tidyverse)
library(readr)



#' Outlier Detection
#'
#' @param column Column to detect outliers
#'
#' @return A vector of booleans. Indicating which values are outliers and which values are nto
#' @export
#'
#' @examples
outlier_detection <- function(column){
  column <- as.numeric(as.character(column))
  quantiles <- quantile(column, probs=c(0,0.25,0.5,0.75,1), na.rm = T)
  lower_quartile <- quantiles[2]
  upper_quartile <- quantiles[4]
  IQR <- upper_quartile - lower_quartile

  lower_bound <- lower_quartile - 1.5*IQR
  upper_bound <- upper_quartile + 1.5*IQR

  outlier <- column < lower_bound | column > upper_bound
  return(outlier)
}


#' Lower Quartile
#'
#'Findthe lower quartile of a column
#'
#' @param column
#'
#' @return
#' @export
#'
#' @examples
lower_quartile_function <- function(column){
  column <- as.numeric(as.character(column))
  return(quantile(column,probs=c(0.25), na.rm = T))
}

#' Upper quartile
#'
#'A function for retrieving the upper quartile of a column
#'
#' @param column
#'
#' @return
#' @export
#'
#' @examples
upper_quartile_function <- function(column){
  column <- as.numeric(as.character(column))
  return(quantile(column,probs=c(0.75), na.rm = T))
}

#' Summarise Indicator_sheet
#'
#' A function for summarising every single column of the indicator sheet
#'
#' @param indicator_sheet
#'
#' @return
#' @export
#'
#' @examples
summarise_indicator_sheet <- function(indicator_sheet){
  id <- colnames(indicator_sheet)


  class <- indicator_sheet %>%
    summarise_all(~class(.x)) %>%
    gather()%>%
    rename(column_class=value) %>%
    select(-key)

  na_values <- indicator_sheet %>%
    sapply(is.na) %>%
    as_tibble() %>%
    summarise_all(sum) %>%
    gather()%>%
    rename(na_values=value) %>%
    select(-key)

  zero_values <- indicator_sheet %>%
    sapply(function(x) as.numeric(x)==0) %>%
    as_tibble() %>%
    summarise_all(sum, na.rm = T) %>%
    gather()%>%
    rename(zero_values=value) %>%
    select(-key)

  outliers <-
    indicator_sheet %>%
    sapply(outlier_detection) %>%
    as_tibble() %>%
    summarise_all(sum, na.rm = T) %>%
    gather()%>%
    rename(outliers=value) %>%
    select(-key)

  means <- indicator_sheet %>%
    summarise_all(mean, na.rm = T) %>%
    gather() %>%
    rename(mean=value) %>%
    select(-key)

  standard_deviation <- indicator_sheet %>%
    summarise_all(sd, na.rm = T) %>%
    gather() %>%
    rename(sd=value)%>%
    select(-key)

  lower_quartile <- indicator_sheet %>%
    summarise_all(lower_quartile_function) %>%
    gather() %>%
    rename(lower_quartile=value)%>%
    select(-key)

  upper_quartile <- indicator_sheet %>%
    summarise_all(upper_quartile_function) %>%
    gather() %>%
    rename(upper_quartile=value)%>%
    select(-key)

  min <- indicator_sheet %>%
    summarise_all(min, na.rm = T) %>%
    gather() %>%
    rename(min=value) %>%
    select(-key)

  max <- indicator_sheet %>%
    summarise_all(max, na.rm = T) %>%
    gather() %>%
    rename(max=value) %>%
    select(-key)

  return(bind_cols(id=id,class, na_values,zero_values,outliers, means,standard_deviation,lower_quartile, upper_quartile, min, max))
}











