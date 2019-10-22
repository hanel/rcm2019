.onLoad <- function(libname, pkgname) {

  message('\nRCM 2019 Rpackage')
  message('---')

  init_package()


  invisible()

}


#' Checks and sets environmental variables
#'
#' The package relies on several environmental variables. On load the package tests whether the variables are set, if not it prompts for setting them. The function `init_package` can be also called within the session to overwrite the current setting.
#'
#' The function simply writes the environmental variables into .Renviron file (typically in the home directory). If you encounter problems with calling the function or during package load, try to alter .Renviron file manually.
#'
#' The contents of the .Renviron can be e.g.
#'
#' R_DATA_PATH='/home/DATA'
#'
#' Currently implemented are:
#'
#' \describe{
#'   \item{`R_DATA_PATH`}{The path to store big data. Only local storage implemented at the moment.}
#' }
#'
#' @param R_DATA_PATH
#' @param renviron_path Path to .Renviron file
#'
#' @return
#' @export init_package
#'
#' @examples
init_package = function(R_DATA_PATH = NULL, renviron_path = Sys.getenv("R_ENVIRON_USER")){


  updateRenviron = function (renviron_path, R_DATA_PATH) {

      if (renviron_path == '') renviron_path = '~'

      wd = getwd()
      setwd(renviron_path)
      if (!file.exists('.Renviron')) file.create('.Renviron')

      renviron <- file(file.path(renviron_path, ".Renviron"))
      writeLines(sprintf("R_DATA_PATH='%s'", R_DATA_PATH), renviron)
      close(renviron)
      setwd(wd)
      invisible()
    }



  if (!is.null(R_DATA_PATH)){
    updateRenviron(renviron_path, R_DATA_PATH)
    message('R_DATA_PATH set to ', R_DATA_PATH)
    Sys.setenv(R_DATA_PATH = R_DATA_PATH)
    invisible()
  }


  R_DATA_PATH = Sys.getenv('R_DATA_PATH')


  if (R_DATA_PATH != '') {

     message('R_DATA_PATH:\t', R_DATA_PATH)

   } else {

     message('R_DATA_PATH not set. \nSet it now? y = yes, other = continue without setting (the package will not work properly).')
     ans = readline(prompt = 'Your choice: ')

     if (tolower(ans) != 'y') invisible()

     message('\nEnter the data directory:')

     de = FALSE

     while ( !de & tolower(ans)=='y' ){


      ans = readline(prompt = 'Data directory = ')
      de = dir.exists(ans)

      if (!de) {
        message('Directory does not exist.')
        message('Retry? y = yes, other = continue without setting (the package will not work properly).')
        ans = readline(prompt = 'Your choice: ')
        if (tolower(ans) != 'y') invisible()
        }
     }

     updateRenviron(renviron_path, ans)
     message('R_DATA_PATH set to ', ans)
     Sys.setenv(R_DATA_PATH = ans)

   }

}
