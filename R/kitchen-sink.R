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

#' @title Write a larger file to Google Sheets by writing csv then uploading that file
#'
#' @description
#' Extends \code{googlesheets::gs_upload()} by doing the write_csv and upload task for you.
#' If you want to modify \code{readr::wrte_csv()} do it in seperate calls.
#'
#' @param data the dataframe to upload.
#' @param sheet_title the title of the newly uploaded sheet
#' @param overwrite should it overwrite a sheet of the same name? defaults to \code{TRUE}.
#' @param ... parameters to pass into \code{googlesheets::gs_upload()}
#'
#' @export
gs_large_upload <- function(
    data
  , sheet_title
  , overwrite   = TRUE
  , ...
) {
  path <- tempfile(fileext = '.csv')
  readr::write_csv(data, path)
  googlesheets::gs_upload(path, sheet_title = sheet_title, overwrite = overwrite, ...)
}

#' @title use the Spark API to write a dataframe as a Hive table with optional file format and path params. Works with Spark 2.0 w/ Hive.
#'
#' @description
#' This function mimics a series of Spark API calls in the format of \code{df.write.format('format').mode('mode').option('path','path://').saveAsTable('table_name')}
#' This allows a user to save a spark dataframe to Hive by not only writing it to a specific path in a specific format but also saving it to Hive metastore.
#' For more details see the \href{https://spark.apache.org/docs/latest/sql-programming-guide.html#generic-loadsave-functions}{Spark docs} on loading and saving tables
#'
#' You can choose to chain further commands afer this function with the format sdf_
#'
#' @param tbl a spark dataframe or dplyr tbl, to be passed into \code{spark_dataframe()} call
#' @param table_name the name of the table for hive metastore. For database specific use the format \code{dbname.table_name}
#' @param format the file format to save as, defaults as orc. For more details see the \href{https://spark.apache.org/docs/latest/sql-programming-guide.html#generic-loadsave-functions}{Spark docs}
#' @param mode the write mode, defaults to 'overwrite'. Can also take, 'append', 'ignore', and 'error.' For more details see the \href{https://spark.apache.org/docs/latest/sql-programming-guide.html#generic-loadsave-functions}{Spark docs}
#' @param path optional character string. the path to save the file to, e.g. "s3://my_bucket/iris/"
#'
#' @import dplyr
#'
#' @export
sdf_write_and_save_table <- function(
    tbl
  , table_name
  , format = "orc"
  , mode   = "overwrite"
  , path   = NULL
) {
  sdf <- sparklyr::spark_dataframe(tbl)

  if(!is.null(path)) {
    writer <- sparklyr::invoke(sdf, "write") %>%
      sparklyr::invoke('format', format) %>%
      sparklyr::invoke('mode', mode) %>%
      sparklyr::invoke('option','path', path) %>%
      sparklyr::invoke('saveAsTable', table_name)

  } else {
    writer <- sparklyr::invoke(sdf, "write") %>%
      sparklyr::invoke('format', format) %>%
      sparklyr::invoke('mode', mode) %>%
      sparklyr::invoke('saveAsTable', table_name)
  }

  return(writer)
}

#' @title Download all files from Google Drive folder
#'
#' @description
#' Wrapper around \code{googledrive::download} to pull all files located in a Google Drive folder.
#' Optionally filter for files of a specific type or save to a temporary folder.
#' Returns a vector of file paths to the downloaded files.
#'
#' @param path folder path, id, or url to pass into \code{googledrive::drive_ls}. Use \code{googledrive::as_id()} if URL or id.
#' @param temp boolean. Should it save to a temp location? If FALSE the files are saved in the directory specified in saveToPath.
#' @param fileExt list of characters, optional. only download files with specified file extensions. Do not include a ".", just the extension.
#' @param saveToPath character, optional. If temp = FALSE then defaults to the current directory, otherwise pass in a folder path with trailing "/"
#' @param ... values to pass into \code{googledrive::drive_download()}
#'
#' @export
drive_download_all <- function(path, temp = TRUE, fileExt = NULL, saveToPath = "./", ...) {

  # get all the files
  files_folder <- googledrive::drive_ls(path = path)
  drive_resource <- files_folder$drive_resource

  paths <- purrr::map(drive_resource, function(resource) {

    # Verify file extension
    if(!is.null(fileExt)){
      if(! resource$fileExtension %in% fileExt) {
        return(NULL)
      }
    }

    # verify if temp or not
    if(temp) {
      path <- tempfile(resource$name, fileext = resource$fileExtension)
    } else {
      path <- paste0(saveToPath, resource$name)
    }

    # download the file
    returnPath <- googledrive::drive_download(googledrive::as_id(resource$id), path, ... )

  })

  return(paths)
}

#' @title produces a tidy output of the \code{acf} function from \code{rstats}
#'
#' @description
#' acf function output as a tibble!
#' credit goes to Matt Dancho at Business Science in the time series with Keras tutorial \href{https://www.business-science.io/timeseries-analysis/2018/04/18/keras-lstm-sunspots-time-series-prediction.html}{here}
#'
#'
#' @param data dataframe or tibble
#' @param value the value in the datframe to produce for
#' @param lags the lags to use, defaults to 0:20
#' @param ... values to pass into \code{acf()}
#'
#' @import dplyr
#'
#' @export
tidy_acf <- function(data, value, lags = 0:20, ...) {

  value_expr <- enquo(value)

  acf_values <- data %>%
    pull(value) %>%
    stats::acf(lag.max = utils::tail(lags, 1), plot = FALSE, ...) %>%
    .$acf %>%
    .[,,1]

  ret <- tibble::tibble(acf = acf_values) %>%
    tibble::rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)

  return(ret)
}

#' @title wrapper around dplyr \code{distinct()} and\code{count()}
#'
#' @description
#' Combine \code{count()} and \code{distinct()} !
#'
#' @param x sthe data frame to perform over
#' @param ... vars to perform over
#' @param wt wt parameter for \code{count()}
#' @param sort sort parameter for \code{count()}
#'
#' @import dplyr
#'
#' @export
count_distinct <- function(x, ..., wt = NULL, sort = FALSE) {

  df <- x %>%
    distinct(...) %>%
    count(
       ...
      ,wt = wt
      ,sort = sort
    )

  return(df)
}

