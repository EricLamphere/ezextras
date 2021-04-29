#' Pattern Matching
#' @description Search for matches to an argument pattern within each element of a character vector. Uses perl style regex and ignores case. See \code{grepl} documentation for more information.
#' @param x Character vector
#' @param reg Character string containing a regular expression to be matched in the given character vector.
#' @return See \code{grepl} documentation for more information.
#' @export
`%~%` <- function(x, reg){
  grepl(reg, x, ignore.case = TRUE, perl = TRUE)
}

#' Anti-Pattern Matching
#' @description Search for anti-matches to an argument pattern within each element of a character vector. Uses perl style regex and ignores case. See \code{grepl} documentation for more information.
#' @param x Character vector
#' @param reg Character string containing a regular expression to be anti-matched in the given character vector.
#' @return See \code{grepl} documentation for more information.
#' @export
`%^~%` <- function(x, reg){
  !grepl(reg, x, ignore.case = TRUE, perl = TRUE)
}

#' Concatenate Strings
#' @description Concatenate vectors after converting to character.
#' @param x First R object to be converted to a character vector and concatenated to the left of \code{y}.
#' @param y Second R object to be converted to a character vector and concatenated to the right of \code{x}.
#' @return See \code{paste} documentation for more information.
#' @export
`%&%` <- function(x, y){
  paste0(
    ifelse(is.na(x),"",x),
    ifelse(is.na(y),"",y)
  )
}

#' Anti-Value Matching
#' @description Match returns a vector of the positions of (first) anti-matches of its first argument in its second.
#' @param x Vector or NULL: the values to be matched. Long vectors are supported.
#' @param y Vector or NULL: the values to be matched against. Long vectors are not supported.
#' @return See \code{%in%} documentation for more information.
#' @export
`%notin%` <- function(x, y){
  !(x %in% y)
}


#' Remove Patterns Within String
#' @description \code{gsub} with \code{ignore.case = TRUE, perl = TRUE, fixed = FALSE}.
#' @param x Character vector.
#' @param pattern Character string containing a regular expression to be matched and removed from \code{x}.
#' @return See \code{%in%} documentation for more information.
#' @export
`%remove%` <- function(x, pattern){
  gsub(pattern, "", x, ignore.case = TRUE, perl = TRUE, fixed = FALSE)
}


