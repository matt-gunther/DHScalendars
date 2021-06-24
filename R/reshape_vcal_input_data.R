#' @title reshape_vcal_input_data
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param dat
#' @param sample_tracking
#' @examples
#' \dontrun{}
#' @export reshape_vcal_input_data
reshape_vcal_input_data <- function(dat, sample_tracking, ...){
  # Create any vcal numbered 0-9 if it does not exist, then fill with NA
  dat <- dat %>%
    select(starts_with("vcal")) %>%
    names() %>%
    setdiff(paste0("vcal_", 0:9), .) %>%
    map_dfc(~tibble(!!.x := NA)) %>%
    bind_cols(dat, .)

  # create vcal_0 in case sample named one calendar unconventionally (not vcal)
  # right-pad vcal_2 with one space only for sample lr2013ir
  dat <- dat %>%
    mutate(
      vcal_0 = ifelse(v000 == "EG3", str_pad(scal, 80, "left"), vcal_0),
      vcal_2 = ifelse(v000 == "LB6", str_pad(vcal_2, 80, "right"), vcal_2),
      vcal_6 = ifelse(v000 == "EG4", str_pad(vcal_6, 80, "right"), vcal_6),
      vcal_7 = ifelse(v000 == "EG4", str_pad(vcal_7, 80, "right"), vcal_7)
    ) %>%
    select(-any_of("scal"))

  if(samp %in% sample_tracking$SAMPLE){
    calendar_locations <- sample_tracking %>%
      filter(SAMPLE == samp) %>%
      select(-c(SAMPLE, Comments)) %>%
      pivot_longer(
        everything(),
        values_drop_na = T,
        names_to = "cal_type",
        values_to = "vcal"
      ) %>%
      mutate(vcal = paste0("vcal_", vcal))
  } else {
    stop(
      samp, " was not found in the Google Sheet where we have been tracking \n ",
      "the availability of calendars for each sample. \n\n",
      "Without external information from the Google Sheet, I cannot guess \n ",
      "which `vcal` source variable contains which type of calendar!\n\n",
      "Suggested action: go to the Google Sheet and check that ", samp,
      "\n appears in the column SAMPLE (case-sensitive).\n"
    )
  }

  if(length(unique(dat$v017)) != 1){
    warning(
      "The 'CMC start of calendar' variable (v017) contains multiple values.\n\n",
      "I will attempt to align calendars with different start dates.\n\n",
      "Even so, you might want to check sample documentation to ensure that \n ",
      "multiple calendar start dates are expected!"
    )
  }

  vcal_widths <- dat %>%
    summarise(across(starts_with("vcal"), ~unique(nchar(.x))))

  if(nrow(vcal_widths) != 1){
    stop(
      "One or more `vcal` source variables is not the same width for \n ",
      "every person.\n\n",
      "Within any given variable, every person should have the same \n ",
      "number of values / months (even if some are blank).\n\n",
      "Please check the Data Dictionary or source reference materials."
    )
  }

  vcals_todo <- vcal_widths %>%
    pivot_longer(everything(), names_to = "vcal", values_to = "nchar") %>%
    filter(nchar != 1)

  if(all(vcals_todo$vcal %in% calendar_locations$vcal)){
    vcals_todo <- full_join(vcals_todo, calendar_locations, by = "vcal")
  } else {
    stop(
      "I found non-empty `vcal` source variables in the input data \n ",
      "that have not been identified in the Google Sheet where we have been \n ",
      "tracking the availability of calendars for each sample.\n\n",
      "Without external information from the Google Sheet, I cannot guess \n ",
      "which `vcal` source variable contains which type of calendar!\n\n",
      "Suggested action: go to the Google Sheet and identify all available \n ",
      "`vcal` source variable for ", samp, "."
    )
  }

  if(length(unique(vcals_todo$nchar)) != 1){
    stop(
      "One or more of the `vcal` source variables are different lengths. \n\n",
      "This excludes `vcal` source variables where the length is 1 \n ",
      "(e.g. 'B' values).\n\n",
      "Every `vcal` variable should have the same number of values / months \n ",
      "(even if some are blank).\n\n",
      "Please check the Data Dictionary or source reference materials."
    )
  }

  message(
    "\033[32m",
    "FYI: Input calendar data was checked with no formatting issues",
    "\033[0m"
  )

  # Make a new column `id` containing the original row index for each person
  dat <- dat %>%  rowid_to_column("id")

  # Parse all available calendar strings and pivot longer
  ## (For example, if a calendar has 80 characters, pivot values into 80 rows)
  ## If there are multiple calendar start-dates in a sample, handle separately
  dat <- map_df(unique(dat$v017), ~{
    timeline <- .x
    vcals_todo$vcal %>%
      map(~{
        width <- vcal_widths %>% pull(.x)
        dat %>%
          filter(v017 == timeline) %>%
          separate(
            col = .x,
            sep = 1:width,
            into = paste0(.x, "_", seq(
              to = timeline,
              by = -1,
              length = width
            ))
          ) %>%
          pivot_longer(
            starts_with(.x),
            names_pattern = paste0(.x,"_(.*)"),
            names_to = "cmc_month",
            values_to = .x
          )  %>%
          select(caseid, id, cmc_month, .x)
      }) %>%
      reduce(full_join, by = c("caseid", "id", "cmc_month")) %>%
      full_join(by = c("caseid", "id"), dat %>%
                  select(-vcals_todo$vcal) %>%
                  filter(v017 == timeline)
      ) %>%
      rename(sample = v000) %>%
      mutate(across(c(cmc_month, starts_with("v0")), ~as.integer(.x)))
  })

  # label the calendars included in this sample
  dat <- dat %>%
    rename_with(
      .cols = vcals_todo$vcal,
      .fn = ~vcals_todo %>% filter(vcal == .x) %>% pull(cal_type)
    )

  # add placeholders for all remaining calendars (all NA)
  dat <- sample_tracking %>%
    select(-c(SAMPLE, Comments)) %>%
    names() %>%
    setdiff(names(dat)) %>%
    map_dfc(~tibble(!!.x := NA)) %>%
    bind_cols(dat, .)

  return(dat)
}
