# Tue Mar  9 16:37:10 2021 ------------------------------
# supplementary functions for report 21 - Industries/categories

#' Join tables for Industries report
#' @description Function which wraps components and produces table for the Industries report
#' @import data.table
#' @importFrom lubridate month
#' @importFrom stringr str_replace_all str_squish str_remove_all
#' @export
produce_join_table_for_industries <- function(){

    #-- 1.1 read leads_profiles dataset
    leads_profiles_dt <- fd.dashboard::get_clean_leads_profiles()

    #-- 1.2 read lookup table
    lookup_dt <- fd.dashboard::get_lookup_table()

    #-- 1.3 join these two
    setkey(leads_profiles_dt, client, profile, website_iso2c)
    setkey(lookup_dt, client, profile, website_iso2c)
    x1_dt <- leads_profiles_dt[lookup_dt, nomatch = 0]

    #-- 1.3.1 get records, which didn't join
    x1_neg_dt <- leads_profiles_dt[!lookup_dt]

    #-- 2. wrangle
    #-- 2.1 get month, as we will aggregate monthly
    x1_dt[, `:=` (month = lubridate::month(date_join, label = T, abbr = T))]

    #-- 2.2. aggregate
    x2_dt <- x1_dt[,  .(leads = .N), .(n, ppl_price, ppl_price_currency, month)]

    #-- 2.3 calculate rev, and number of leads
    x2_dt[, `:=` (rev = ppl_price * leads), ]

    #-- 3. read in PRimary Industry table
    primary_dt <- fd.dashboard::get_primary_industry_table()

    #-- 3.1 join
    setkey(x2_dt, n)
    setkey(primary_dt, n)
    z2_dt <- x2_dt[primary_dt, nomatch = 0]

    #-- 3.2 wrangle
    char_colnames <- z2_dt[, .SD, .SDcols = is.character] %>% colnames()
    #-- 3.2.1 remove spaces
    z2_dt[, (char_colnames) := lapply(.SD, stringr::str_replace_all, pattern = "- ", replacement = " "), .SDcols = char_colnames]
    z2_dt[, (char_colnames) := lapply(.SD, stringr::str_squish), .SDcols = char_colnames]

    #-- 3.2.2 remove dashes
    z2_dt[, (char_colnames) := lapply(.SD, stringr::str_replace_all, pattern = "-", replacement = "_"), .SDcols = char_colnames]

    #-- 3.2.3 produce sequences
    z2_dt[, `:=` (seq = str_c(primary_ind_v1, primary_ind_v2, primary_ind_v3, sep = "-"))]
    z2_dt[, `:=` (seq = stringr::str_remove_all(seq, pattern = "-$|--$"))]

    #-- 3.3 aggregate
    z3_dt <- z2_dt[, .(leads = sum(leads), rev = sum(rev)), .(seq, ppl_price_currency, month, website_iso2c)][order(-leads)]

    return(z3_dt)
}
