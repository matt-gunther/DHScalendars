#' @title master
#' @author Matt Gunther
#' @description description
#' @param samp description
#' @export master
master <- function(samp){
  # Load packages
  # library(tidyverse)
  # library(mpctools)
  # library(googlesheets4)
  # options(tibble.print_min = 20)

  # Functions for reading the data dictionary (as ddi) and input data
  # walk(list.files("r", full.names = T), ~source(.x))

  # Package functions
  # sample_tracking <- get_sample_tracking_info()

  # samp <- "bf2003ir"

  # for(samp in sample_tracking$SAMPLE){
  #   message(samp)
    samp %>%
      read_dhs_dict_as_ddi() %>%
      get_vcal_input_data() %>%
      reshape_vcal_input_data(sample_tracking) %>%
      vcal_reprod() %>%
      vcal_discont() %>%
      vcal_ppa() %>%
      vcal_ppabstain() %>%
      vcal_bfeed() %>%
      vcal_marstat() %>%
      vcal_mig() %>%
      vcal_work() %>%
      vcal_term() %>%
      vcal_ultra() %>%
      cleanup_months() %>%
      write_long_rds()
  # }
}


