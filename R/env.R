#' Snapshot of Global Environment
#' @param environment The environment to be saved
#' @return A copy of the global environment
#' @export
# save environment
env_save <- function(environment = .GlobalEnv){
  .GlobalCopy <<- as.environment(as.list(environment, all.names = TRUE))
}

# TODO: create function to restore the saved environment
# # restore saved environment
# env_restore <- function(){
#   # Remove difference between the saved copy and the current .GlobalEnv
#   #set_env(.GlobalEnv, .GlobalCopy)
#   rm(list = setdiff(ls(envir = .GlobalEnv, all.names = TRUE), '.GlobalCopy'))
#   # assign the values from the saved copy back to the .GlobalEnv
#   for(n in ls(.GlobalCopy, all.names = TRUE)) assign(n, get(n, .GlobalCopy), envir = .GlobalEnv)
# }
