#' @title write_long_rds
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param dat
#' @examples
#' \dontrun{}
#' @export write_long_rds
write_long_rds <- function(dat){
  dat %>%
    # remove source vars
    select(-c(
      starts_with("vcal"),
      starts_with("v0"),
      sample
    )) %>%
    rename_with(~paste0("cal", .x), !caseid) %>% # prepend names with "cal"
    write_rds(paste0("output/", samp, "_long.rds"),  compress = "gz") # save
}
