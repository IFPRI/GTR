#' addHeadR
#'
#' @param model_path Path to file for cleanup of R block
#'
#' @return Adds header metadata
#' @export
#' @examples
#' \dontrun{
#' addHeadR()
#' }
#' @author Abhijeet Mishra
addHeadR <- function(model_path = NULL) {

    if (is.null(model_path)) stop("Invalid model folder path provided")
    target_files <-
        list.files(path = model_path, full.names = TRUE, recursive = TRUE)
    target_files <- target_files[grep(pattern = "gms", x = target_files)]

    # Remove existing header
    remove_header <- function(existing_content) {
        # Step 2A: Identify lines that start with "*** |"
        lines_to_remove <- which(grepl("^\\*\\*\\* \\|", existing_content))
        if (length(lines_to_remove) > 0) {
            # Step 2B: Remove the identified lines
            updated_content <- existing_content[-(lines_to_remove)]
            return(updated_content)
        } else {
            return(existing_content)
        }
    }

    for (i in target_files) {
        # Step 1: Read the content of the text file
        existing_content <- readLines(i)

        if (!is.null(length(existing_content))) {
            existing_content <- remove_header(existing_content)
        }

        # Step 3: Add the new text on top
        new_text <- c(
            "*** |  (C) 2008-2023 International Food Policy Research Institute (IFPRI)",
            "*** |  authors, and contributors. This file is part of the simple land use",
            "*** |  and licensed under AGPL-3.0-or-later. Under Section 7 of AGPL-3.0",
            "*** |  you are granted additional permissions described in the License",
            "*** |  Exception, version 1.0 (see LICENSE file).",
            "*** |  Contact: A.Mishra@cgiar.org"
        )

        updated_content <- c(new_text, existing_content)

        # Step 4: Write the updated content back to the GMS file
        writeLines(updated_content, con = i)
        lapply(model_path, removeEmptyR, multiline_fix = TRUE)
    }
    cat("New text has been added on top of the GMS file.")
}
