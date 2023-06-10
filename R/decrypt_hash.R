#' Decrypt the Hash
#' @param pword Journal password.
#'
decrypt_hash <- function(pword){
  encr_hash <- readRDS(file = "hash.RDS")
  hash <- safer::decrypt_string(encr_hash, pword)
  hash
}
