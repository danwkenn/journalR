#' Use this function to memorise your password.
#' @param password Enter the correct password. If NULL, then a dialog opens to enter the password.
#' @export
memorise_password <- function(
  password = NULL){

  if(is.null(password)){
    password <- rstudioapi::askForPassword(
      prompt = "Enter the correct password:")
  }

  COMPLETE <- FALSE
  while(!COMPLETE){

    attempt <- rstudioapi::showPrompt(title = "Attempt", message = "Enter the password:")
    if(attempt == password){
      COMPLETE <- TRUE
      message("You got it!")
    }else{
      message("Nope...Try again:")
    }

  }

}
