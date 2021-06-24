#' @title get_sample_tracking_info
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @examples
#' \dontrun{}
#' @export get_sample_tracking_info

# determine vcals_to_do from Google Sheet
get_sample_tracking_info <- function(){
  gs4_deauth()
  sample_tracking <- suppressMessages(googlesheets4::read_sheet(
    "1AZnFIBu3w98nTbVKwyLTuxrfuZlodLMnxwgFY2eZl_g",
    sheet = 2
  ))
}
