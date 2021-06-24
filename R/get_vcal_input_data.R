#' @title get_vcal_input_data
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param ddi
#' @examples
#' \dontrun{}
#' @export get_vcal_input_data
get_vcal_input_data <- function(ddi, ...){

  # get the path to the input data from the Samples control file
  dat_path <- paste0(ddi$file_path, "/", samp, ".dat")

  # rt_info is needed for the function used to read the data file
  rt_info <- ipumsr:::ddi_to_rtinfo(ddi)

  # IMPORTANT: col_spec specifies vars where white space should NOT be trimmed
  # White space is NOT trimmed for any variable name beginning with "vcal"
  col_spec <- ipumsr:::ddi_to_colspec(ddi, "long", verbose) %>%
    mutate(trim_ws = case_when(
      grepl("vcal", col_names) ~ FALSE,
      T ~ trim_ws
    ))

  # Read the input data into R as an object called dat
  # Hipread is maintained by Derek Burk; it is used inside ipumsr functions
  # The reason we can't just use ipumsr is because it trims white space
  output <- hipread::hipread_long(
    file = dat_path,
    var_info = col_spec,
    rt_info = rt_info,
    n_max = Inf,
    encoding = ddi$file_encoding
  )

  # Get any variable labels / descriptions (if they are in the data dictionary)
  output <- ipumsr:::set_ipums_var_attributes(
    output, ddi, var_attrs = "var_label"
  )

  return(output)
}
