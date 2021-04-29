
#' Wrapper for \code{View} - Invoke Data Viewer
#' @description see(x) = View(x). See \code{View} documentation with \code{?View} for more information.
#' @param x an R object which can be coerced to a data frame with non-zero numbers of rows and columns.
#' @return Invisible NULL. The functions puts up a window and returns immediately: the window can be closed via its controls or menus.
#' @export
see <- function(x){
  View(x, title = deparse(substitute(x)))
}
