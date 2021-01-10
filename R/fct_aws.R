#' Basic function to establish connection with aws, reading parameters from the config.yml
#'
#' @importFrom RMySQL dbConnect
#'
get_aws_connection <-
    function() {

        #if (!exists("config")) config <- config::get(file = app_sys("golem-config.yml"), use_parent = TRUE)

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

#' Reading subset of columns from a leads database in aws
#'
#' @importFrom glue glue_sql
#' @importFrom RMySQL dbSendQuery dbFetch dbDisconnect
#' @importFrom data.table setDT
#'
#' @export
read_clean_leads <-
    function() {

       # if (!exists("config")) config <- config::get(file = "00_config/config.yml")

        col_subset_names <- c("date", "website_iso2c", "Date_y", "year", "month")

        table_name <- config$table_leads_clean

        con <- get_aws_connection()
        query_txt <- glue::glue_sql(
            "SELECT {`col_subset_names`*} FROM {`table_name`}",
           # cols = cols_subset,
            .con = con)

        query <- RMySQL::dbSendQuery(con, query_txt)
        res <- RMySQL::dbFetch(query, -1)
        RMySQL::dbDisconnect(con)

        setDT(res)
        return(res)
    }

#' Read data for report 9
#'
# @importFrom data.table setDT setnames .SD  :=
#' @import data.table
#' @importFrom lubridate month date
#'
#' @export
get_data_adapt_for_report_9 <-
    function() {

        dt <- read_clean_leads()
        setDT(dt)

        #-- adjust column names, types so the old script can work without changes
        date_cols <- c("date", "Date_y")
        dt[, (date_cols) := lapply(.SD, lubridate::date), .SDcols = date_cols]

        factor_cols <- c("website_iso2c", "year")
        dt[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

        dt[, `:=` (month = lubridate::month(date, label = T, abbr = T))]
       # dt[, month := lubridate::month(date, label = T, abbr = T)]

        setnames(x = dt, old = c("date"), new = c("Date"))

        return(dt)
    }
