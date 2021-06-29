#' @title Get calendar data from an IR file
#' @author Matt Gunther
#' @description Returns a modified version of the IR file for a sample,
#' including \emph{only the handful of variables that are relevant for calendar
#' processing}. As a helpful side-effect, the path to the IPUMS DHS folder on
#' the user's system will be attached as an attribute of the returned data
#' frame.
#'
#' @details This function is basically a combination of
#' mpctools:::read_data_dict_as_ddi (modified to select a specific list of
#' variables and assume a few DHS data features, greatly improving speed) and
#' ipumsr::read_ipums_micro (modified to avoid trimming whitespace for each
#' of the calendar string variables).
#' @note All of the variables needed for calendar processing are stored as a
#' quosure list in the first line of this function. In the future, if a sample
#' includes a weird name for its calendar data, you may need to add variables
#' to this list (this happened with eg1995ir, where the termination
#' calendar was stored in a variable called "scal" for some reason). If a
#' sample does not include one or more of the variables listed, those variables
#' will be silently skipped: the resulting data frame will contain all of the
#' variables that were available for your sample.
#' @param sample Character: An IR sample that you want to process
#' (e.g. "af2015ir")
#' @param dhs_path Optional Character: If you're working in a local
#' version of RStudio, this function may have trouble finding the data
#' dictionary on its own. If so, you can just tell it where to look: specify
#' the file path here.
#' (e.g. "Z://dhs//country/afghanistan/2015/data/data_dict_af2015ir.xlsx")
#' @export get_vcal_input_data
get_vcal_input_data <- function(
  sample,
  dhs_path = NULL
){

  # Enter the variables needed for calendar processing:
  vars <- rlang::quos(
    caseid,
    v000,
    v006,
    v007,
    v008,
    v017,
    v019,
    vcal_1,
    vcal_2,
    vcal_3,
    vcal_4,
    vcal_5,
    vcal_6,
    vcal_7,
    vcal_8,
    vcal_9,
    scal
  )

  # Encourage user to try running this on the server
  if(file.path(mpc_root(), "dhs") %>% dir.exists()){
    dhs_path <- file.path(mpc_root(), "dhs")
  } else if(!is.null(dhs_path)){
    if(!dir.exists(dhs_path)){
      stop(
        "The path ", dhs_path, " does not exist on this system. \n\nIf you are ",
        "running R somewhere other than https://rstudio.pop.umn.edu/, \n check ",
        "to make sure that you are connected to VPN and have mapped the IPUMS ",
        "drive."
      )
    }
  } else {
    stop(
      "I'm having trouble finding the right path to the IPUMS DHS folder on ",
      "your system. \n\nPlease specify like this, but insert your own path: \n ",
      "get_vcal_input_data(", sample, ", dhs_path = '~/Z/ipums/dhs')"
    )
  }

  # Find data dictionary path
  samp <- sample
  if(!grepl("ir", samp)){
    stop(samp, " is not an IR file!")
  }
  data_dict <- dhs_path %>%
    file.path("metadata/control_files/samples.csv") %>%
    readr::read_csv(
      col_types = readr::cols(.default = readr::col_character())
    ) %>%
    filter(sample == samp) %>%
    pull(datadict) %>%
    file.path(dhs_path, "country", .)
  if(!file.exists(data_dict)){
    stop(
      "I looked for a data dictionary at ", data_dict,
      " but could not find one."
    )
  }

  # Find data file path
  data_file <- data_dict %>%
    str_replace("/data_dict_", "/") %>%
    str_replace("\\.xls(x)?$", "\\.dat")
  if(!file.exists(data_dict)){
    stop(
      "I looked for a data file at ", data_file,
      " but could not find one."
    )
  }

  # Read data dictionary as a data frame
  data_dict <- suppressMessages(
    suppressWarnings(
      read_data_dict(data_dict) # creates a useless warning AND message
    )
  )

  # Filter only `vars` specified above
  data_dict <- map_df(vars, ~data_dict %>% filter(Var %in% !!as_label(.x)))

  # Obtain variable metadata or specify as needed
  var_info <- tibble(
    var_name = data_dict$Var,
    var_label = data_dict$VarLabel,
    var_desc = data_dict$Notes,
    val_labels = NA,
    code_instr = NA_character_,
    start = as.numeric(data_dict$Col),
    end = as.numeric(data_dict$Col) + as.numeric(data_dict$Wid) - 1,
    imp_decim = ifelse(is.na(data_dict$Decim), 0, as.numeric(data_dict$Decim)),
    var_type = "character",
    rectypes = NA,
    var_type_svar = "character"
  )

  # make ddi from data dictionary and metdata in var_info
  ddi <- ipumsr::make_ddi_from_scratch(
    file_name = basename(data_file),
    file_path = dirname(data_file),
    file_type = "rectangular",
    ipums_project = "internal",
    rectypes = NULL,
    rectype_idvar = NA_character_,
    var_info = var_info,
    conditions = "Internal IPUMS Input Data"
  )

  # IMPORTANT: col_spec specifies vars where white space should NOT be trimmed
  # White space is NOT trimmed for any variable name beginning with "vcal"
  col_spec <- ipumsr:::ddi_to_colspec(ddi, "long", verbose) %>%
    mutate(trim_ws = case_when(
      grepl("vcal", col_names) ~ FALSE,
      T ~ trim_ws
    ))

  # Read the input data into R (hipread is used by ipumsr)
  output <- hipread::hipread_long(
    file = data_file,
    var_info = col_spec,
    rt_info =  ipumsr:::ddi_to_rtinfo(ddi),
    n_max = Inf,
    encoding = ddi$file_encoding
  )

  # Get any variable labels / descriptions (if they are in the data dictionary)
  output <- ipumsr:::set_ipums_var_attributes(
    output, ddi, var_attrs = "var_label"
  )

  # Assign dhs_path as an attribute of the output
  attr(output, "dhs_path") <- dhs_path

  # Assign sample name as an attribute of the output
  attr(output, "sample") <- samp

  return(output)
}
