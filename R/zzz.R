.onAttach <- function(...) {
  packageStartupMessage(
    emo::ji("beers"),
    crayon::white(" ezextras "),
    crayon::cyan(packageVersion("ezextras"))
  )
}
