% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vcal_ultra.R
\name{vcal_ultra}
\alias{vcal_ultra}
\title{Ultrasound Calendar}
\usage{
vcal_ultra(dat)
}
\arguments{
\item{dat}{A data file created by \code{vcal_reprod()} (may be passed to
any other function starting with "vcal" first).}
}
\description{
Create all variables related to the Ultrasound calendar
(see details). If this calendar was not included in the sample (or if some
of the required information is not available), all variables will still be
 created, but all values will be NA.
}
\details{
The following variables will be created using case logic provided
to the function \code{dplyr::case_when()}. Please note that
\code{case_when()} returns NA through implicit logic: \emph{if a "case"
exists and is not explicitly handled here, the value NA will be returned!}
\itemize{
  \item{
    \strong{ultrasound} Logical: TRUE if \code{vcal_ultra} is "Y". FALSE if
    \code{vcal_ultra} is "N", and not available otherwise (NA).
  }
}
}
\author{
Matt Gunther
}
