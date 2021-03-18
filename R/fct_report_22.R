# Wed Mar 17 16:36:33 2021 ------------------------------
# functions for module 22 - daily profiles

#' Produce plot with sessions or transactions
#' @description Produce plot
#' @import data.table
#' @importFrom ggplot2 ggplot
#' @param dt data.table with input value, long form
#' @param metric whether we should plot sessions or transactions
#' @export
produce_daily_profile_plot <- function(dt, metric_selection, time_selection) {

    dt <- switch (time_selection,
                  user = dt[metric == metric_selection, .(value = sum(value)), .(country, deviceCategory, day_type = user_weekend, x_time_h = hour(user_time_h))],
                  serv = dt[metric == metric_selection, .(value = sum(value)), .(country, deviceCategory, day_type = serv_weekend, x_time_h = hour(serv_time_h))]
    )

    plot <- dt %>%
        ggplot(aes(x_time_h, value, color = day_type, fill = day_type)) +
        geom_area(position = "dodge", alpha = 0.5) +
        facet_grid(c("country", "deviceCategory"), scales = "free_y") +
        theme_bw() +
        scale_color_brewer(palette = "Set1") +
        scale_fill_brewer(palette = "Set1") +
        labs(title = glue::glue("{metric_selection}, {time_selection} time"), subtitle = glue::glue("Daily profiles by device, country."), y = "", x = "") +
        theme(legend.position = "bottom") +
        scale_x_continuous(breaks = c(0, 6, 12, 18, 24))

    return(plot)
}

#' Produce aggregate and long form, ready for plot
#' @description Function takes wide form, with all the countries, then
#' - aggregates countries
#' - produces long form
#' @import data.table
#' @importFrom stringr str_c
#' @param dt data.table, all countries, wide form
#' @param aggregation_level from user input
#' @export
aggregate_countries <- function(dt, aggregation_level) {

    #-- 1. refactor countries, according to aggregation level
    x0_dt <- dt[, `:=` (country = forcats::fct_lump(country_orig, n = aggregation_level, w = transactions, other_level = "Other Countries") %>%
                                  fct_reorder(.x = transactions, .fun = sum) %>%
                                  fct_rev())]

    #-- 2.2.2 perform aggregation with new countries
    aggregation_cols <- quote(list(country, deviceCategory, user_weekend, serv_weekend, user_time_h, serv_time_h))
    measure_vars <-  c("sessions", "transactions")
    x1_dt <- x0_dt[, lapply(.SD, sum), .SDcols = measure_vars, eval(aggregation_cols)]

    #-- 2.2.3 keep original value, just in case
    measure_vars_orig <- stringr::str_c(measure_vars, "orig", sep = "_")
    x1_dt[, (measure_vars_orig) := lapply(.SD, `[`), .SDcols = measure_vars]

    #-- 2.2.4 adjustment to get true daily numbers
    #-- 2.2.4.1 there are 5 working days + 2 weekend
    x1_dt[, (measure_vars) := lapply(.SD, function(x){
        fcase(user_weekend == "weekend",  x / 2, user_weekend == "working day", x / 5)
    }), .SDcols = measure_vars]
    #-- 2.2.4.2 to get daily values, divide by number of weeks in a period
    number_of_weeks <- 4 # for testing, when we have only Feb data
    x1_dt[, (measure_vars) := lapply(.SD, `/`, number_of_weeks), .SDcols = measure_vars]

    #-- 2.2.5 remove orig columns
    x1_dt[, (measure_vars_orig) := NULL]

    #-- 2.2.6 melt
    x2_dt <- x1_dt[, melt.data.table(.SD, measure.vars = c("sessions", "transactions"), variable.name = "metric")]

    return(x2_dt)
}
