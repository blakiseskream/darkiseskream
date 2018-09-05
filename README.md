# darkiseskream
this is a catch all R package for things I do a lot that aren't proprietary

## Functions

### gs_large_upload

Upload a large dataframe to googlesheets. Extends the [googlsheets]("https://github.com/jennybc/googlesheets") package by speeding up the upload via saving a csv.

    spread_sheet <- gs_large_upload(iris, "iris", overwrite = TRUE)

### read_multiple_xlsx

Read in multiple xlsx files in a folder as a list of dataframes. Optionally only include files in a given folder path with a specific regex name. Simple extension of `readxl::read_xlsx`.

    my_dfs <- read_multiple_xlsx(search_string = "ugly", folderpath = "my_excel_files/", add_filename_column = FALSE)


### sdf_write_and_save_table

Write a Spark dataframe to the hive metastore to a specific path in a specific format in one function. Mimics the `df.write.format('format').mode('mode').option('path','path://').saveAsTable('table_name')` which is found in the [Spark API docs](https://spark.apache.org/docs/latest/sql-programming-guide.html#generic-loadsave-functions). Based on this SO article [here](https://stackoverflow.com/questions/51886236/sparklyr-can-i-pass-format-and-path-options-into-spark-write-table-or-use-savea)

    data("iris")
    iris_spark <- copy_to(sc, iris, name = "iris")
    spark_write <- sdf_write_and_save_table(
        iris_spark
      , table_name = "my_database.iris"
      , format     = 'orc'
      , path       = "s3://my_bucket/iris/
    )
