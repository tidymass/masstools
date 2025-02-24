#' Retrieve Project Working Directory
#'
#' This function identifies and returns the working directory of an RStudio project 
#' (`*.Rproj`) that is located at or above the current working directory.
#' 
#' If there's a project directory within the current directory or its parent directories, 
#' it will return the path to that directory. If no such directory is found, it sends a message 
#' indicating that no `.Rproj` files were found and does not change the working directory.
#'
#' @return A character string representing the directory path to the RStudio project 
#' (`*.Rproj`) file. If no such file is found, the function returns `NULL` and a message 
#' indicating that there are no `.Rproj` files in your directory.
#'
#' @examples
#' \dontrun{
#' get_project_wd()
#' }
#'
#' @author Xiaotao Shen <xiaotao.shen@outlook.com>
#' @export

get_project_wd <- function() {
  current_wd <-
    getwd()
  candidate_wd <-
    current_wd %>%
    stringr::str_split("/") %>%
    unlist()
  
  if (length(candidate_wd) == 1) {
    candidate_wd <- current_wd
  } else {
    candidate_wd <-
      lapply(2:length(candidate_wd), function(i) {
        paste(candidate_wd[seq_len(i)], collapse = "/")
      })
  }
  
  candidate_wd <-
    rev(candidate_wd)
  
  for (i in seq_along(candidate_wd)) {
    wd <- candidate_wd[[i]]
    file_name <-
      list.files(wd,
                 recursive = ifelse(wd == current_wd, TRUE, FALSE),
                 full.names = TRUE)
    project_index <-
      grep(".Rproj", file_name)
    
    if (length(project_index) != 0) {
      project_wd <-
        file_name[project_index[1]] %>%
        stringr::str_split("/") %>%
        unlist() %>%
        head(-1) %>%
        paste(collapse = "/")
      message(
        "The project name is:",
        file_name[project_index[1]] %>%
          stringr::str_split("/") %>%
          unlist() %>%
          tail(1)
      )
      # message("The project wd is:",
      #         project_wd)
      
      return(project_wd)
      break()
    }
  }
  if (length(project_index) == 0) {
    message("There are no .Rproj in your file. No change for wd.")
  }
}
