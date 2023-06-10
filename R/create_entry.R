#' Create entry file.
#' @param type What type of entry. Options are currently md and rmd
#' @export
start_entry <- function(type = "md"){

  type.table <- data.frame(type = c("md","rmd"),
             fileext = c("md","Rmd"))
  fileext <- type.table$fileext[type.table$type == type]
  entry <- list()
  template_file <-  paste0("entry_template.",fileext)
  # Create temporary file from template -----
  tmp_file <- tempfile(fileext = fileext)
  template <- system.file("templates", template_file, package = "journalR")
  file.copy(template, tmp_file)
  tx  <- readLines(tmp_file)
  tx2  <- gsub(pattern = "{date}", replacement = Sys.Date(), x = tx,fixed = TRUE)
  tx3  <- gsub(pattern = "{time}", replacement = Sys.time(), x = tx2,fixed = TRUE)
  writeLines(tx3, con=tmp_file)

  entry[["tmp_file"]] <- tmp_file

  # Open temporary file from template -----
  rstudioapi::navigateToFile(tmp_file)
  entry
}

#' Save and encrypt entry.
#' @param entry journal entry object to be completed.
#' @export
complete_entry <- function(entry){

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
file_name <- glue::glue("logs/{generate_hex(all_characters = FALSE)}")

  file.copy(entry$tmp_file,to = file_name)
  # Delete temporary file
unlink(entry$tmp_file)
unlink(gsub(entry$tmp_file,pattern = "\\.md$",replacement = "\\.html"),force = TRUE)

  # Encrypt:
  safer::encrypt_file(infile = file_name, key = hash,outfile = paste0(file_name,"_enc"))
  unlink(file_name)

  TRUE
}
