#' remove_empty_lines_from_end
#'
#' @param file_path File paths
#'
#' @return Removing empty lines
#' @export
#'
#' @examples
#' \dontrun{
#' remove_empty_lines_from_end()
#' }
#' @author Abhijeet Mishra

remove_empty_lines_from_end <- function(file_path) {
    for (i in file_path) {
        # Read the contents of the file
        file_content <- suppressWarnings(readLines(i))

        # Find the last non-empty line
        last_non_empty_line <- length(file_content)
        while (last_non_empty_line > 0 &&
               nchar(file_content[last_non_empty_line]) == 0) {
            last_non_empty_line <- last_non_empty_line - 1
        }

        # If there are empty lines at the end, overwrite the file without them
        if (last_non_empty_line < length(file_content)) {
            writeLines(file_content[1:last_non_empty_line], i)
        }
    }
}
