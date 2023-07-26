#' addNewlineR
#'
#' @param model_path Path to model
#'
#' @return Modified GMS files with "optimized" parameter descriptions
#' @import stringr
#' @importFrom utils tail
#' @export
#' @examples
#' \dontrun{
#' addNewlineR()
#' }
#' @author Abhijeet Mishra
addNewlineR <- function(model_path = NULL) {
    if (is.null(model_path)) stop("Invalid model folder path provided")
    path <- paste0(model_path, "/modules")
    target_files <- list.files(path = path, full.names = TRUE, recursive = TRUE)
    # Read the contents of the file
    add_newline_to_file <- function(file_path) {
        # Read the contents of the file
        file_content <- suppressWarnings(readLines(file_path))

        # Check if the file ends with a newline
        if (length(file_content) == 0 || nchar(tail(file_content, 1)) > 0) {
            # Append a newline to the file
            cat("\n", file = file_path, append = TRUE)
        }
    }
    lapply(target_files, add_newline_to_file)
}
