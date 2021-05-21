#' Pattern Matching
#' @description Search for matches to an argument pattern within each element of a character vector. Uses perl style regex and ignores case. See \code{grepl} documentation for more information.
#' @param x Character vector
#' @param reg Character string containing a regular expression to be matched in the given character vector.
#' @return Logical, whether or not `x` matches `reg.` See \code{grepl} documentation for more information.
#' @export
`%~%` <- function(x, reg){
  res <- grepl(reg, x, ignore.case = TRUE, perl = TRUE)
  return(res)
}

#' Anti-Pattern Matching
#' @description Search for anti-matches to an argument pattern within each element of a character vector. Uses perl style regex and ignores case. See \code{grepl} documentation for more information.
#' @param x Character vector
#' @param reg Character string containing a regular expression to be anti-matched in the given character vector.
#' @return Logical, whether or not `x` doesn't match `reg.` See \code{grepl} documentation for more information.
#' @export
`%!~%` <- function(x, reg){
  res <- !grepl(reg, x, ignore.case = TRUE, perl = TRUE)
  return(res)
}

#' Concatenate Strings
#' @description Concatenate vectors after converting to character and ignoring NAs.
#' @param x First R object to be converted to a character vector and concatenated to the left of \code{y}.
#' @param y Second R object to be converted to a character vector and concatenated to the right of \code{x}.
#' @return Character, concatenated strings `x` and `y`, less NAs. See \code{paste} documentation for more information.
#' @export
`%&%` <- function(x, y){
  # only print messages if length(x) == 1, otherwise the message is too long
  if(length(x) == 1 && length(y) == 1) {
    nas <- c(x = is.na(x), y = is.na(y))

    if (all(nas)) {
      cli::cli_alert("Both x and y are NA, returning empty string")
    } else if (any(nas)) {
      cli::cli_alert("{names(nas[nas])} is NA, returning {names(nas[!nas])}")
    }
  }

  res <- paste0(
    ifelse(is.na(x),"",x),
    ifelse(is.na(y),"",y)
  )

  return(res)
}

#' Anti-Value Matching
#' @description Match returns a vector of the positions of (first) anti-matches of its first argument in its second.
#' @param x Vector or NULL: the values to be matched. Long vectors are supported.
#' @param y Vector or NULL: the values to be matched against. Long vectors are not supported.
#' @return Logical, whether or not `x` was in `y`. See \code{%in%} documentation for more information.
#' @export
`%notin%` <- function(x, y){
  res <- !(x %in% y)
  return(res)
}


#' Remove Patterns Within String
#' @description \code{gsub} with \code{ignore.case = TRUE, perl = TRUE, fixed = FALSE}.
#' @param x Character vector.
#' @param pattern Character string containing a regular expression to be matched and removed from \code{x}.
#' @return Character, `x` with `pattern` removed
#' @export
`%del%` <- function(x, pattern){
  res <- gsub(pattern, "", x, ignore.case = TRUE, perl = TRUE, fixed = FALSE)
  return(res)
}


