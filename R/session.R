
pkg.globals <- new.env()
pkg.globals$userSession$logged <-FALSE
pkg.globals$userSession$username <- NULL
pkg.globals$userSession$id <- NULL
pkg.globals$userSession$name <- NULL
pkg.globals$userSession$lastname <- NULL
#' get user session
#'
#'
#' @author Raul Arias
#' @export
#'
#'
getUserSession <- function(){
  return(pkg.globals$userSession)
}



#' set user session
#'
#' @param logged Bool user is logged
#' @param username user username
#' @param id user id
#' @param name user name
#' @param lastname user lastname
#' @author Raul Arias
#' @export
#'
#'

setUserSession <- function(logged , username , id, name= NULL, lastname=NULL){
  pkg.globals$userSession$logged <- logged
  pkg.globals$userSession$username <- username
  pkg.globals$userSession$id <- id
  pkg.globals$userSession$name <- name
  pkg.globals$userSession$lastname <- lastname
}

