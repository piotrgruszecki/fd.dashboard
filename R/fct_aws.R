#' Basic function to establish connection with aws, reading parameters from the config.yml
#'
#' @importFrom RMySQL dbConnect
#'
get_aws_connection <-
    function() {

        if (!exists("config")) config <- config::get(file = app_sys("golem-config.yml"), use_parent = TRUE)

        con <- RMySQL::dbConnect(drv      = RMySQL::MySQL(),
                                 dbname   = config$dbname,
                                 username = config$username,
                                 password = config$password,
                                 host     = config$host,
                                 encoding = config$encoding
        )
        return(con)
    }

#' Reading whole table from aws
#'
#' @param table_name name of the table to read
#'
#' @importFrom RMySQL dbReadTable dbDisconnect
#' @importFrom data.table as.data.table
#'
#' @export
read_table_from_aws <-
    function(table_name) {

        con <- get_aws_connection()
        table_raw <- RMySQL::dbReadTable(con, table_name)
        RMySQL::dbDisconnect(con)

        table_dt <- table_raw %>% as.data.table()
        return(table_dt)
    }

#' Writing to a table on aws
#'
#' @param dt data.table, to be stored in aws database
#' @param table_name name of the table, where we will store data
#' @param overwrite passing a parameter for RMySQL::dbWriteTable; it is set TRUE as default
#' @param append passing a parameter for RMySQL::dbWriteTable; it is set FALSE as default
#'
#' @importFrom RMySQL dbWriteTable dbDisconnect
#'
#' @export
write_table_to_aws <-
    function(dt, table_name, overwrite = TRUE, append = FALSE) {

        con <- get_aws_connection()
        RMySQL::dbWriteTable(con, table_name, dt, overwrite = overwrite, append = append)
        RMySQL::dbDisconnect(con)

    }
