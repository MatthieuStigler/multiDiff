% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mdd_group_means.R
\name{mdd_group_means}
\alias{mdd_group_means}
\title{Compute group means by time period}
\usage{
mdd_group_means(
  mdd_dat,
  conf.int = FALSE,
  weights = NULL,
  by_treat_period = FALSE
)
}
\arguments{
\item{mdd_dat}{An object of class mdd_dat created by \code{\link{mdd_data_format}}}

\item{conf.int}{Weather to compute confidence intervals for the means}

\item{weights}{Optional column name of weights variable}

\item{by_treat_period}{Should the mean be computed by treated period, i.e. before/after?
Default to \code{FALSE}, so computes for every instance of \code{time.index}.}
}
\description{
Compute the means of each group for each time periods
}
\examples{
 library(multiDiff)
 dat_DiD_raw <- sim_dat_common()

 ## format data
 dat_DiD <- mdd_data_format(data=dat_DiD_raw)

 mdd_group_means(dat_DiD)
 mdd_group_means(dat_DiD, conf.int = TRUE)
}
