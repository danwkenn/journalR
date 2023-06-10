#' Create a new journal.
#'
#' @param dir_path Directory of the new project.
#' @param password Character input. `"No"` if no password required,
#' @export
create_journal <- function(dir_path, password = "no"){

  if(!is.character(password) | (length(password)) != 1){
    stop("password must be a single string of characters.")
  }
  usethis::create_project(dir_path, open = TRUE)

  if(password == "no"){
    pword <- NULL
  }else{
    if(password == "yes"){
      pword <- readline("Set the password for this journal:")
    }else{
      pword <- password
    }
  }

  # Create metadata:
  meta_data <- list(
    password = !is.null(pword)
  )

  if(!is.null(pword)){
    hash <- generate_hex()
    encr_hash <- safer::encrypt_string(hash, key = pword)
    saveRDS(file = glue::glue("{dir_path}/hash.RDS"),object = encr_hash)
  }

  dir.create(glue::glue("{dir_path}/logs"))
  saveRDS(meta_data, file = glue::glue("{dir_path}/metadata.RDS"))
  TRUE
}
