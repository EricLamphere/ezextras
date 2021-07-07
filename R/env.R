#' Snapshot of Global Environment
#' @param to Character, file path where the environment data will be saved
#' @return Invisible, location of the file containing saved environment data
#' @export
env_save <- function(to = ".envirs/", envir = .GlobalEnv){
  if (!file.exists(to)) {
    cli::cli_alert_info("Creating {crayon::blue(getwd() %//% to)} to store env")
    dir.create(to)
  }
  env_save_name <-
    gsub(
      "-| ",
      "_",
      "env_copy_" %&% as.character(Sys.time())
    )
  env_save_name <-
    gsub(
      ":",
      "",
      env_save_name
    )

  save(list = ls(all.names = TRUE, envir = envir), file = to %//% env_save_name)

  invisible(env_save_name)
}


#' Restore Saved Environment
#' @param from Character, directory where environment data is stored. Defaults
#' to `.envirs/`
#' @param file Character, which file in `from` contains the environment you wish
#'   to restore. Defaults to the most recently modified
#' @param rm_all Logical, whether or not to remove current environment data
#' before loading data from new environment. Defaults to `TRUE`
#' @param envir Environment, where data will be loaded. Defaults to `.GlobalEnv`
#'
#' @return Loads all variables to `envir`
#' @export
env_restore <- function(from = ".envirs/", file = get_latest_file(from), rm_all = TRUE, envir = .GlobalEnv){
  if (rm_all && length(ls(all.names = TRUE, envir = envir)) != 0) {
    cli::cli_alert_warning("Removing all vars from current environment")
    rm(list = ls(all.names = TRUE, envir = envir), envir = envir)
  }

  load(from %//% file, envir = envir)
}


#' Clear Environment Data
#'
#' @param envir Environment, which environment you wish to clear
#' @return Invisible NULL
#' @export
env_clear <- function(envir = .GlobalEnv) {
  rm(list = ls(all.names = TRUE, envir = envir), envir = envir)
  invisible(NULL)
}


#' Get Name of The Most Recently Saved File
#'
#' @param from Character, directory in which files are searched
#' @return Name of file most recently saved
#' @export
get_latest_file <- function(from = ".envirs/") {
  files_info <-
    from %//% list.files(from) %>%
    purrr::map(
      file.info
    ) %>%
    setNames(list.files(from))

  files_times <-
    files_info %>%
    purrr::map(
      ~ purrr::pluck(.x, "mtime") %>%
        difftime(Sys.time(), ., units = "secs")
    )

  file_max_time <-
    names(files_times)[as.numeric(files_times) == min(as.numeric(files_times))]

  return(file_max_time)
}

