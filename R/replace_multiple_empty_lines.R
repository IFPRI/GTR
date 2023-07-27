#' replace_multiple_empty_lines
#'
#' @param file_path File paths
#'
#' @return Removing empty lines
#' @export
#'
#' @examples
#' \dontrun{
#' replace_multiple_empty_lines()
#' }
#' @author Abhijeet Mishra
replace_multiple_empty_lines <- function(file_path) {
    for (i in file_path) {
        # Read the content of the file
        file_content <- readLines(i, warn = FALSE)

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
        writeLines(file_content, i)
    }
}
