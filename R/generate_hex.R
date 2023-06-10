#' Generates a string of randomly chosen characters.
#'
#' @param length Number of characters.
#' @param all_characters all chars or just alpha-numeric

generate_hex <- function(length = 30, all_characters = TRUE){
  if(all_characters == TRUE){
  characters <- c(LETTERS,letters,0:9,strsplit("!@#$%^&*()_+-=?/","")[[1]])
  }else{
  characters <- c(LETTERS,letters,0:9)
  }
  paste0(
    sample(
      characters,16,replace = TRUE),
    collapse = "")
}
