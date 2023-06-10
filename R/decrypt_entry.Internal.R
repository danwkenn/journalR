#' Decrypt a log entry with the journal password.
#' @param file Path to encrypted log entry file.
decrypt_entry.Internal <- function(file){

  metadata <- readRDS(file = "metadata.RDS")
  pword <- ""
  if(metadata$password){
    pword <- getPass::getPass(msg = 'Insert Password: ')
  }

  # Checks -------------
  ## Journal requires password
  if(pword == "" & metadata$password){
    stop("This journal requires a password")
  }

  ## Password is correct
  hash <- try(decrypt_hash(pword),silent = TRUE)
  if(class(hash) == "try-error"){
    stop("Password incorrect.")
  }

  # Create file-name
  temp <- tempfile()

  # Encrypt:
  safer::decrypt_file(infile = file, key = hash,outfile = temp)
  string <- readChar(temp,nchars = file.info(file)$size)
  unlink(temp)

  string
}
