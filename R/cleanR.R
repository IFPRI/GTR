#' cleanR
#'
#' @param return_text_only If only cleaned text be returned
#' @param file_path Path to file for cleanup of R block
#'
#' @return Cleanup for GMS files with "optimized" parameter descriptions
#' @export
#' @examples
#' \dontrun{
#' cleanR()
#' }
#' @author Abhijeet Mishra
cleanR <- function(file_path, return_text_only = FALSE) {

    content <- readLines(file_path)

    # Construct the regular expression pattern to match the text block
    pattern_start   <- "*########### Begin R"
    pattern_end     <- "*########### End R"

    # Remove the text block using the pattern
    start_clean <- grep(pattern = pattern_start, x = content)
    end_clean <- grep(pattern = pattern_end, x = content)


    if (length(start_clean) != 0 && length(end_clean) != 0) {
        # Split the cleaned content back into lines
        cleaned_content_lines <- content[-(start_clean:end_clean)]

        if (return_text_only) {
            return(cleaned_content_lines)
        } else {
            # Write the modified content back to the file
            writeLines(cleaned_content_lines, file_path)
        }
    }
}
