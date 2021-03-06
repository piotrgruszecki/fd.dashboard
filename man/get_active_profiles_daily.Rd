% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_aws.R
\name{get_active_profiles_daily}
\alias{get_active_profiles_daily}
\title{Get active profiles, per each day, within the analysis period}
\usage{
get_active_profiles_daily(dt, analysis_period)
}
\arguments{
\item{dt}{input data.table}

\item{analysis_period}{period to analyse}
}
\description{
Generate long table, with profile_id per each day within the period
}
\details{
There are following steps
- leave only changes related to status, while flattening all the others
- trim down profile_id to only these, which had at least one active day within the period; it allows to reduce no of profiles significantly
- create sequence of dates, for the analysis period
- roll join datasets, producing long table with dates, profile_id and status
}
