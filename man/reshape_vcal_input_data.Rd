% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reshape_vcal_input_data.R
\name{reshape_vcal_input_data}
\alias{reshape_vcal_input_data}
\title{Reshape calendar data: one row per woman-month}
\usage{
reshape_vcal_input_data(dat, pad_months = NULL, vcal_availability = NULL)
}
\arguments{
\item{dat}{An IR data file created by \code{get_vcal_input_data()}}

\item{pad_months}{Optional Named List: if necessary, the user can specify a
list of calendar variables in the IR file that need to be padded with left
or right spaces to reach a desired length (usually 80). Note that this
function will generate an error if all calendars are \emph{not} the same
length (this will help you determine whether use of pad_months is needed).
For example: \code{
reshape_vcal_input_data(dat, list(vcal_1 = c(80, 'right')))}}

\item{vcal_availability}{Optional Character: the full path to a file called
vcal_availability.csv (this can usually be found automatically)}
}
\description{
Returns a data frame where the calendar strings have been
parsed into single characters, then pivoted "from wide to long" so that
each character gets stored in a separate row (the number of rows in the
data frame usually grows by a factor of 80). Several different tests are
conducted to ensure that all available calendars are identified and
reshaped correctly. If the user is aware that one or more calendar columns
contains the wrong number of months (e.g. 79, while all others have 80),
the user may pad the offending calendar(s) with spaces via the \code{
pad_months} argument.
}
\details{
This function will stop and issue a specific error message (with
instructions for next steps) if any of the following issues are found:

\itemize{
  \item{The file vcal_availability.csv cannot be found}
  \item{The file vcal_availability.csv does not contain the sample}
  \item{The file vcal_availability.csv does not include all of the
  available calendars found in the data dictionary for the sample}
  \item{The 'CMC start of calendar' variable (v017) contains multiple values
  (a non-fatal warning is issued, and the function will align the
  calendars - results should be manually verified)}
  \item{Individual persons have different calendar lengths (usually 80).
  This would indicate a problem with DHS processing.}
  \item{One or more of the calendars in a sample have different lengths
  (usually 80). Note: this may be resolved with the pad_months argument.}
  \item{The user has requested padding for a calendar named in pad_months,
  but the name of the requested calendar does not exist}
  \item{The user has requested padding for a calendar named in pad_months,
  but did not specify the desired length of the string for the requested
  calendar}
  \item{The user has requested padding for a calendar named in pad_months,
  but did not specify whether to pad the calendar with spaces on the left
  or right side of the existing string}
}
}
\note{
As an intermediate processing step, all existing calendar names
(e.g. vcal_1, vcal_2, ... vcal_9) will be replaced with human-readable
names (e.g. vcal_reprod, vcal_discont, ... vcal_ultra) based on the
availability of calendars shown in vcal_availability.csv. Any calendars
that are \emph{not} available for the sample will be attached as
placeholders, where all values are NA. All calendar variables will be
removed from the data in a later function.
}
\author{
Matt Gunther
}
