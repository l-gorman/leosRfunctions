#' A function for raising first letter to uppercase
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}
