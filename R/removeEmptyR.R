#' removeEmptyR
#'
#' @param model_path Path to model
#' @param multiline_fix If multiple newlines should be cleaned. Default is FALSE
#' @return Modified GMS files with "optimized" parameter descriptions
#' @import stringr
#' @export
#' @examples
#' \dontrun{
#' removeEmptyR()
#' }
#' @author Abhijeet Mishra
#'
removeEmptyR <- function(model_path = NULL, multiline_fix = FALSE) {
    if (is.null(model_path)) stop("Invalid model folder path provided")
    path <- paste0(model_path, "/modules")
    target_files <- list.files(path = path, full.names = TRUE, recursive = TRUE)

    lapply(target_files, remove_empty_lines_from_end)
    if (multiline_fix) {
        message("Fixing multiple newline areas ...")
        lapply(target_files, replace_multiple_empty_lines)
    }
}
