#' Configuration: Read
#'
#' @param config_dir directory with configuration files (default:
#' system.file("extdata/config/network_complete", package = "kwb.BerlinWaterModel")). It is mandatory
#' that there are three files within this folder: "flows_in_out.csv", "outflows_multiple.csv"
#' and "sections.csv"
#' @param file_encoding encoding for reading the files (default: "UTF-8")
#'
#' @return list with three sublists "flows_in_out", "outflows_multiple" and
#' "sections
#' @export
#'
#' @examples
#' \dontrun{config <- kwb.BerlinWaterModel.public::config_read()
#' config}
#' @importFrom stats setNames
#' @importFrom readr read_csv2 locale
#' @importFrom stringr str_remove
config_read <- function(config_dir = system.file("extdata/config/network_complete",
                                                 package = "kwb.BerlinWaterModel.public"),
                        file_encoding = "UTF-8") {

  files <- list.files(config_dir, full.names = TRUE)

  valid_config_files <- c("backflows_multiple",
                          "bfstypes_equations",
                          "bfstypes_wellgalleries",
                          "concentrations",
                          "cso",
                          "flows_in_out",
                          "k",
                          "outflows_multiple",
                          "qsimVis",
                          "sections",
                          "scenarios")

  filenames_without_extension <- basename(files) %>% stringr::str_remove("\\..*")

  bool_filenames_without_extension <- filenames_without_extension %in% valid_config_files

  filenames_without_extension <-  filenames_without_extension[bool_filenames_without_extension]

  files_idx <- which(bool_filenames_without_extension)

  stopifnot(all(valid_config_files %in% filenames_without_extension))

  config <- stats::setNames(lapply(files[files_idx], function(file) {

    readr::read_csv2(file, locale = readr::locale(encoding = file_encoding))
  }), nm = filenames_without_extension)


  if(!"section_out_function" %in% names(config$outflows_multiple)) {
    stop(sprintf("Column 'section_out_function' is missing in '%s/outflows_multiple.csv'. Please add the column specifiy a formula (maximum 1 per outflow_id) like y=a*x+b*x+c or similar where possible!",
                 config_dir))
  }

  # Extraction of multiple outflow formula after "y = ..."
  config$outflows_multiple <- config$outflows_multiple %>%
    dplyr::mutate(
      expr_clean = ifelse(
        grepl("\\by\\s*=\\s*", section_out_function),
        gsub("\\^", "**", sub(".*\\by\\s*=\\s*", "", section_out_function)),
        NA_character_
      )
    )

  # Conversation into R function (with error handling)
  config$outflows_multiple$section_out_function_parsed <- lapply(config$outflows_multiple$expr_clean, function(expr) {
    if (is.na(expr)) {
      return(NA_real_)
    } else {
      tryCatch(
        eval(parse(text = paste0("function(x) { ", expr, " }"))),
        error = function(e) NA_real_
      )
    }
    })

  config$outflows_multiple <- config$outflows_multiple %>%
    dplyr::select(- expr_clean)


  # check if implemented functions sum downstream flows to or close to section flow
  check_outflow_multiple_dynamic_functions(config = config,
                                           q = 1,
                                           allowed_relative_offset_percent = 0.001)

  # check if implementeds shares for all upstream sections for each backflow_id sum up to 100 percent
  check_backflows_multiple(config = config)

  config
}
