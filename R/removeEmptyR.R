#' removeEmptyR
#'
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

    remove_empty_lines_from_end <- function(file_path) {
        # Read the contents of the file
        file_content <- suppressWarnings(readLines(file_path))

        # Find the last non-empty line
        last_non_empty_line <- length(file_content)
        while (last_non_empty_line > 0 &&
               nchar(file_content[last_non_empty_line]) == 0) {
            last_non_empty_line <- last_non_empty_line - 1
        }

        # If there are empty lines at the end, overwrite the file without them
        if (last_non_empty_line < length(file_content)) {
            writeLines(file_content[1:last_non_empty_line], file_path)
        }
    }

    replace_multiple_empty_lines <- function(file_path) {
        # Read the content of the file
        file_content <- readLines(file_path, warn = FALSE)

        # Find the indices of consecutive empty lines
        empty_line_indices <- which(file_content == "")
        consecutive_empty_indices <-
            split(empty_line_indices,
                  cumsum(c(1, diff(empty_line_indices) != 1)))

        # Remove consecutive empty lines except the first occurrence
        for (indices in consecutive_empty_indices) {
            if (length(indices) > 1) {
                file_content <- file_content[-(indices[-1])]
            }
        }

        # Overwrite the file with the updated content
        writeLines(file_content, file_path)
    }

    lapply(target_files, remove_empty_lines_from_end)
    if (multiline_fix) {
        message("Fixing multiple newline areas ...")
        lapply(target_files, replace_multiple_empty_lines)
    }
}
