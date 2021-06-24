#' @title read_dhs_dict_as_ddi
#' @author Matt Gunther
#' @description
#' @details
#' @note
#' @param sample
#' @examples
#' \dontrun{}
#' @export read_dhs_dict_as_ddi
read_dhs_dict_as_ddi <- function(sample, ...){
  # Matt inserts some variables to select, sets other arguments
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
  project <- "dhs"
  data_dict <- dd_files(proj_name = project, samples = sample)
  data_file = NULL
  use_ovar_names = TRUE
  trim_rectype_ws = TRUE
  # end

  if (is.null(data_file)) {
    if (!inherits(data_dict, "character")) {
      stop("data_file must be provided if dictionary file name wasn't given.")
    }
    data_file <- data_dict
    data_file <- stringr::str_replace(data_file, "/data_dict_", "/")
    data_file <- stringr::str_replace(data_file, "\\.xls(x)?$", "\\.dat")
  }

  if (inherits(data_dict, "character")) {
    data_dict <- suppressMessages(
      suppressWarnings(
        read_data_dict(data_dict) # creates a useless warning AND message
      )
    )
  }

  data_dict$used_name <- if (use_ovar_names){data_dict$Var}else{data_dict$Svar}

  if (trim_rectype_ws) {
    data_dict$RecordType <- stringr::str_trim(data_dict$RecordType)
  }

  all_rec_types <- dplyr::setdiff(
    unique(data_dict$RecordType),
    c("CR", "C")
  )

  if (length(all_rec_types) > 1) {
    file_type <- "hierarchical"
    rectypes <- all_rec_types
    rectype_idvar <- dplyr::filter_(
      data_dict,
      ~RecordType == "CR"
    )
    rectype_idvar <- rectype_idvar$used_name
    rec_types_by_var <- purrr::map(
      data_dict$RecordType,
      function(x) {
        if (x %in% c("C", "CR"))
          all_rec_types
        else (x)
      })
  } else {
    file_type <- "rectangular"
    rectypes <- NULL
    rectype_idvar <- NA_character_
    rec_types_by_var <- NA
  }

  # Matt inserts variable selection code
  data_dict <- map_df(vars, ~data_dict %>% filter(used_name %in% !!as_label(.x)))
  # end

  var_type_svar <- ifelse(
    is.na(data_dict$String) | data_dict$String == "",
    "numeric",
    "character"
  )

  val_labels <- purrr::map2(
    data_dict$values,
    var_type_svar,
    function(lbl_info, vtype) {
      if (is.null(lbl_info))
        return(dplyr::data_frame(val = numeric(0), lbl = character(0)))
      lbl_info <- dplyr::filter_(lbl_info, ~!is.na(Value))
      if (nrow(lbl_info) == 0)
        return(dplyr::data_frame(val = numeric(0), lbl = character(0)))
      lbl_info <- dplyr::transmute_(
        lbl_info,
        val = ~mpctools:::convert_numish_in_text(Value),
        lbl = ~mpctools:::convert_numish_in_text(ValueLabel),
        val_svar = ~ifelse(
          is.na(ValueSvar),
          val,
          mpctools:::convert_numish_in_text(ValueSvar)
        ),
        lbl_svar = ~ifelse(
          is.na(ValueLabelSvar),
          lbl,
          mpctools:::convert_numish_in_text(ValueLabelSvar)
        )
      )
      lbl_info <- dplyr::filter_(lbl_info, ~!val %in% c("#", "*"))
      if (vtype == "numeric") {
        lbl_info$val <- readr::parse_guess(lbl_info$val)
        lbl_info$val_svar <- readr::parse_number(lbl_info$val_svar)
      }
      lbl_info
    })
  var_type <- purrr::map2_chr(
    val_labels,
    var_type_svar,
    ~ifelse(nrow(.x) == 0, .y, class(.x$val))
  )
  var_info <- dplyr::data_frame(
    var_name = data_dict$used_name,
    var_label = data_dict$VarLabel, var_desc = data_dict$Notes,
    val_labels = val_labels, code_instr = NA_character_,
    start = as.numeric(data_dict$Col),
    end = as.numeric(data_dict$Col) + as.numeric(data_dict$Wid) - 1,
    imp_decim = ifelse(is.na(data_dict$Decim), 0, as.numeric(data_dict$Decim)),
    var_type = var_type,
    rectypes = rec_types_by_var,
    var_type_svar = var_type_svar
  )
  ipumsr::make_ddi_from_scratch(
    file_name = basename(data_file),
    file_path = dirname(data_file),
    file_type = file_type,
    ipums_project = "internal",
    rectypes = rectypes,
    rectype_idvar = rectype_idvar,
    var_info = var_info,
    conditions = "Internal IPUMS Input Data"
  )
}
