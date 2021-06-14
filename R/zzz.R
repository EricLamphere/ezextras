.onAttach <- function(...) {
  packageStartupMessage(
    emo::ji("beers"),
    crayon::white(" ezextras "),
    crayon::cyan(utils::packageVersion("ezextras"))
  )
}
