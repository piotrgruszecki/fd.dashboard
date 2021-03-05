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

        col_subset_names <- c("date", "website_iso2c", "Date_y", "year", "month", "country", "state_city", "publish_date",
                              "lead_id", "client_id", "profile_id", "lead_status", "lead_source", "credited_lead",
                              "tech_date_start", "tech_prev_id", "tech_date_end", "tech_next_id",
                              "available_cash", "currency")

        table_name <- config$table_leads_clean

        con <- get_aws_connection()
        date_tmp <- "2020-10-01"
        query_txt <- glue::glue_sql(
            "SELECT {`col_subset_names`*} FROM {`table_name`} ",
            .con = con)

        query <- RMySQL::dbSendQuery(con, query_txt)
        res <- RMySQL::dbFetch(query, -1)
        RMySQL::dbDisconnect(con)

        setDT(res)
        return(res)
    }

#' Read leads data and then set proper column types
#'
# @importFrom data.table setDT setnames .SD  :=
#' @import data.table
#' @importFrom lubridate month date
#'
#' @export
get_clean_leads <-
    function() {

        dt <- read_clean_leads()
        setDT(dt)

        #-- set encoding, fixing special characters
        cols.character <- dt[ , .SD, .SDcols = is.character] %>% colnames()
        dt[, (cols.character) := lapply(.SD, `Encoding<-`, "latin1"), .SDcols = cols.character]

        #-- adjust column names, types so the old script can work without changes
        date_cols <- c("date", "Date_y")
        dt[, (date_cols) := lapply(.SD, lubridate::date), .SDcols = date_cols]

        date_time_cols <- c("publish_date", "tech_date_start", "tech_date_end")
        dt[, (date_time_cols) := lapply(.SD, lubridate::ymd_hms), .SDcols = date_time_cols]

        factor_cols <- c("website_iso2c", "year")
        dt[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

        dt[, `:=` (month = lubridate::month(date, label = T, abbr = T))]

        setnames(x = dt, old = c("date", "country"), new = c("Date", "Country"))

        dt[, `:=` (Country_original = Country)]


        return(dt)
    }

# Thu Jan 21 10:55:27 2021 ------------------------------
# profiles

#' Reading subset of columns from a profiles table in aws
#'
#' @importFrom glue glue_sql
#' @export
read_clean_profiles <-
    function() {

        #-- only those, which are needed for dashboard reports
        col_subset_names <- c("profile", "profile_id", "client","client_id",
                              "sales_rep", "website_iso2c", "contract_end_date", "status",
                              "ppl_price", "ppl_price_currency",
                              "min_investment", "min_investment_currency",
                              "publish_date",
                              "tech_date_start", "tech_date_end", "tech_prev_id", "tech_next_id")

        table_name <- config$table_profiles_clean

        con <- get_aws_connection()
        query_txt <- glue::glue_sql(
            "SELECT {`col_subset_names`*} FROM {`table_name`} ",
            .con = con)

        query <- RMySQL::dbSendQuery(con, query_txt)
        res <- RMySQL::dbFetch(query, -1)
        RMySQL::dbDisconnect(con)

        setDT(res)
        return(res)
    }

#' Return profiles for dashboard charting
#' @description Reading clean profiles using [read_clean_profiles()] function, and then adjusting column types
#' @import data.table
#' @importFrom lubridate date
#'
#' @export
get_clean_profiles <-
    function() {

        dt <- read_clean_profiles()
        setDT(dt)

        #-- set encoding, fixing special characters
        cols.character <- dt[ , .SD, .SDcols = is.character] %>% colnames()
        dt[, (cols.character) := lapply(.SD, `Encoding<-`, "latin1"), .SDcols = cols.character]

        #-- adjust column names, types so the old script can work without changes
        date_cols <- c("contract_end_date", "tech_date_start", "tech_date_end", "publish_date")
        dt[, (date_cols) := lapply(.SD, lubridate::date), .SDcols = date_cols]

        factor_cols <- c("website_iso2c", "status", "ppl_price_currency", "min_investment_currency")
        dt[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]

        numeric_cols <- c("ppl_price", "min_investment")
        dt[, (numeric_cols) := lapply(.SD, as.numeric), .SDcols = numeric_cols]

        return(dt)

    }

#' Generate default analysis period
#' @description Generate default period, which is full last month, and current month including today()
#' @importFrom lubridate today floor_date %--%
#' @export
get_default_analysis_period <-
    function(){
        end_date <- lubridate::today()
        start_date <- (end_date - months(1)) %>% lubridate::floor_date(unit = "month")
        analysis_period <- start_date %--% end_date

        return(analysis_period)
    }

#' Get active profiles, per each day, within the analysis period
#' @description Generate long table, with profile_id per each day within the period
#' @details There are following steps
#' - leave only changes related to status, while flattening all the others
#' - trim down profile_id to only these, which had at least one active day within the period; it allows to reduce no of profiles significantly
#' - create sequence of dates, for the analysis period
#' - roll join datasets, producing long table with dates, profile_id and status
#'
#' @import data.table
#' @importFrom lubridate %within% int_start int_end
#' @export
#' @param dt input data.table
#' @param analysis_period period to analyse
get_active_profiles_daily <-
    function(dt, analysis_period){

        #-- reduce changes to only those, having changed status, "flatten" all other changes, when status is not changed
        setkey(dt, tech_date_start, profile_id)
        dt[, `:=` (status_prev = shift(status, type = "lag", fill = NA)), .(profile_id)]
        x3_dt <- dt[, .(profile_id, status, status_prev, tech_date_start)][status != status_prev | is.na(status_prev),]

        #-- analysis window
        boundary_dates <- data.table(date = c(lubridate::int_start(analysis_period),
                                              lubridate::int_end(analysis_period)))
        boundary_dates[, `:=` (date = as.Date(date))]
        setkey(boundary_dates, date)

        #-- limit profile_id to only these, which had Active status, sometime within the analysis window
        setkey(x3_dt, tech_date_start)
        b1 <- boundary_dates[x3_dt, roll = -Inf]
        b2 <- x3_dt[, .SD[boundary_dates, roll = +Inf], .(profile_id)]
        b3 <- rbindlist(list(b1[, .(profile_id, status, date)],
                             b2[, .(profile_id, status, date = tech_date_start)]))
        setkey(b3, date)
        b4 <- b3[, .SD[date %within% analysis_period & status == "Active", ], .(profile_id) ]
        b5 <- b4[, .N, .(profile_id)]
        setkey(b5, profile_id)

        #-- select only these profile_id, which have at least one day active within the period (the purpose of b5)
        setkey(x3_dt, profile_id)
        b6 <- x3_dt[b5][, .(profile_id, status, date = tech_date_start)]

        #-- create dates sequence
        seq_dates <- seq.Date(from = as.Date(int_start(analysis_period)), to = as.Date(int_end(analysis_period)), by = "day")
        dates_dt <- data.table(date = seq_dates)
        setkey(dates_dt, date)

        setkey(b6, date)
        b7 <- b6[, .SD[dates_dt, roll = +Inf], .(profile_id)][, .(profile_id, status, date)][!is.na(status)]

        return(b7)
    }

#' Get active profiles, per each day, with a minimum_investment per each day
#' @description Function is very similar to [get_active_profiles_daily()], but it also takes into account changes of minimum investment levels
#' @import data.table
#' @importFrom lubridate %within% int_start int_end
#' @param dt input data.table
#' @param analysis_period period to analyse
#' @export
get_profiles_investment_daily <- function(dt, analysis_period){

    #-- reduce changes to only those, having changed status, "flatten" all other changes, when status is not changed
    setkey(dt, tech_date_start, profile_id)
    dt[, `:=` (status_prev = shift(status, type = "lag", fill = NA),
               min_investment_k_prev = shift(min_investment_k, type = "lag", fill = NA)), .(profile_id)]

    x3_dt <- dt[, .(profile_id, status, status_prev, tech_date_start, min_investment_k, min_investment_k_prev, min_investment_currency)
    ][status != status_prev | is.na(status_prev) | min_investment_k != min_investment_k_prev | (!is.na(min_investment_k) & is.na(min_investment_k_prev)),]

    #-- analysis window
    boundary_dates <- data.table(date = c(lubridate::int_start(analysis_period),
                                          lubridate::int_end(analysis_period)))
    boundary_dates[, `:=` (date = as.Date(date))]
    setkey(boundary_dates, date)

    #-- limit profile_id to only those, which had Active status, sometime within the analysis window
    setkey(x3_dt, tech_date_start)
    b1 <- boundary_dates[x3_dt, roll = -Inf]
    b2 <- x3_dt[, .SD[boundary_dates, roll = +Inf], .(profile_id)]
    b3 <- rbindlist(list(b1[, .(profile_id, status, min_investment_k, date)],
                         b2[, .(profile_id, status, min_investment_k, date = tech_date_start)]))

    setkey(b3, date)
    b4 <- b3[, .SD[date %within% analysis_period & status == "Active", ], .(profile_id) ]
    b5 <- b4[, .N, .(profile_id)]
    setkey(b5, profile_id)

    #-- select only these profile_id, which have at least one day active within the period (the pupose of b5)
    setkey(x3_dt, profile_id)
    b6 <- x3_dt[b5][, .(profile_id, status, min_investment_k, min_investment_currency, date = tech_date_start)]

    #-- create dates sequence
    seq_dates <- seq.Date(from = as.Date(int_start(analysis_period)), to = as.Date(int_end(analysis_period)), by = "day")
    dates_dt <- data.table(date = seq_dates)
    setkey(dates_dt, date)

    setkey(b6, date)
    b7 <- b6[, .SD[dates_dt, roll = +Inf], .(profile_id)][, .(profile_id, status, min_investment_k, min_investment_currency, date)][!is.na(status)]

    return(b7)
}

#' Identify credited leads
#' @description Function checks leads statuses, picking Sent and Credited
#' @details Function logic has following steps
#' - reduce changes to only these, having changed status, "flatten" all other changes, when status is not changed
#' - get lead_id, which were credited, sometimes a couple of days later, after initial send
#' - subset lead_id to these, which have been sent
#' - perform two joins of these datasets - leads_sent and credited
#' - as a result, we have a dataset with the earliest "sent" status, and also a flag credited = y/n (crediting occurs a few days after lead is sent)
#' @import data.table
#' @param dt input data.table
#' @export
mark_credited_leads <- function(dt){

    # Fri Feb  5 13:10:37 2021 ------------------------------
    #-- 1. reduce changes to only those, having changed status, "flatten" all other changes, when status is not changed
    setkey(dt, tech_date_start, lead_id)
    dt[, `:=` (status_prev = shift(lead_status, type = "lag", fill = NA)), .(lead_id)]
    x3_dt <- dt[lead_status != status_prev | is.na(status_prev), ]

    #-- 2. get lead_id which were credited
    factor_cols <- c("lead_status", "profile_id", "lead_id", "client_id")
    dt[, (factor_cols) := lapply(.SD, as.factor), .SDcols = factor_cols]
    leads_credited_dt <- dt[lead_status == "Credited Lead", .N, .(lead_id)][, .(lead_id)]

    #-- 3. get earliest "Sent" status per each lead_id
    x4_sent_dt <- x3_dt[lead_status == "Sent", ]

    #-- 4. mark, if the lead have been credited with a flag
    setkey(x4_sent_dt, lead_id)
    setkey(leads_credited_dt, lead_id)
    x5_dt <- rbindlist(list(x4_sent_dt[!leads_credited_dt][, `:=` (credited = FALSE)],
                            x4_sent_dt[ leads_credited_dt, nomatch = 0][, `:=` (credited = TRUE)]))

    return(x5_dt)
}

#' Return only credited leads
#' @description return only credited leads, the function is a next step after [mark_credited_leads()]
#' @import data.table
#' @import magrittr
# @importFrom forcats fct_reorder fct_rev
#' @param dt input data.table, after [mark_credited_leads()]
#'
#' @export
get_credited_leads <- function(dt){

    dt <- dt[credited == TRUE,][, `:=` (n = 1), ][!is.na(website_iso2c), ][, .(Date, Date_y, website_iso2c, Country, credited, n)]
    dt$website_iso2c %<>% as.character()
    dt[, `:=` (website_iso2c = as.factor(website_iso2c) %>% fct_reorder(.x = n, .fun = sum, na.rm = T) %>% fct_rev())]

    return(dt)
}


