#' addOptimizedPars
#'
#' @param file_path Path of files to be modified
#' @param return_df If modded dataframe is returned with intermediate edits
#' @param post if only clean text is returned for postsolve files
#'
#' @return Modded text
#' @export
#'
#' @examples
#' \dontrun{
#' addOptimizedPars()
#' }
#' @author Abhijeet Mishra
addOptimizedPars <- function(file_path, return_df = FALSE, post = FALSE) {
    text_to_add <- NULL
    # Read the contents of the GMS file
    if (post) file_content <- cleanR(file_path, return_text_only = TRUE)
    if (!post) file_content <- readLines(file_path)

    if (!is.null(file_content)) {
        # Initialize a variable to indicate whether we
        # are currently capturing lines
        capture_lines <- FALSE
        lines_after_pattern <- c()

        # Loop through the lines of the file
        for (line in file_content) {
            # Check if the line contains the target string
            if (str_detect(line, "positive variables|variables|equations")) {
                capture_lines <- TRUE
                # Skip this line as we don't need to
                # store the target string itself
                next
            }

            # If we are currently capturing lines, store the current line
            if (capture_lines) {
                lines_after_pattern <- c(lines_after_pattern, line)

                # # Check if the current line contains a semicolon;
                # # if yes, stop capturing lines
                # if (str_detect(line, ";")) {
                #     break
                # }
            }
        }

        if (!is.null(lines_after_pattern)) {
            df <- parGen(lines_after_pattern)
            text_to_add <- paste0(df$par, df$descriptor)
        }
        if (return_df) {
            return(df)
        } else {
            return(text_to_add)
        }
    }
}
