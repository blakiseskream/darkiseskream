#' @title Import a bunch of xlsx files
#'
#' @description
#' This imports a set of xlsx files based on file extension and search string. It returns a list containing a dataframe for each file.
#' It is a simple extension of readr::read_xlsx
#'
#' @param search_string character. The string to detect using \code{stringr::str_detect} in the file name to pull in a subset of files. defaults to every file in the given folder path.
#' @param folderPath character. the relative path to the directory the files are in. defaults to the working directory.
#' @param add_filename_column boolean. Add the filename to the dataframe as a column?
#' @param ... parameters to pass into \code{readxl::read_xlsx}
#' @import dplyr
#'
#' @export
read_multiple_xlsx <- function(
  search_string = "."
  ,folderPath    = "./"
  ,add_filename_column = FALSE
  ,...
) {

  files <- list.files(folderPath)
  files_to_import <- files[
    stringr::str_detect(files, search_string)
    & stringr::str_detect(files, "xlsx")
    ]
  returnObject <- list()

  for(file in files_to_import) {
    tmp <- readxl::read_xlsx(paste0(folderPath,file), ...)
    item_name <- tools::file_path_sans_ext(file)

    if(add_filename_column){
      tmp$filename <- item_name
    }

    returnObject[[item_name]] <- tmp
    message(paste("file imported:",item_name))
    tmp <- NULL
    item_name <- NULL
  }

  return(returnObject)
}
