#' insertR
#'
#' @return Modified GMS files with "optimized" parameter descriptions
#' @import stringr
#' @export
#' @examples
#' \dontrun{
#' insertR()
#' }
#' @author Abhijeet Mishra
insertR <- function(model_path = NULL) {
    if (is.null(model_path)) stop("Invalid model folder path provided")
    path <- paste0(model_path, "/modules")
    target_files <- list.files(path = path, full.names = TRUE, recursive = TRUE)
    target_files <-
        grep(pattern = "declarations.gms", x = target_files, value = TRUE)

    # Clean empty lines
    message("Cleaning empty lines at the end of the files ...")
    lapply(model_path, removeEmptyR)

    # Clean section of R if It already exists
    message("Cleaning pre-existing R sections if they exist ...")
    lapply(target_files, cleanR)

    # Add newline if needed
    message("Adding newlines for file endings ...")
    lapply(model_path, addNewlineR)

    replace_v_with_o <- function(file_path) {
        text_to_add <- NULL
        # Read the contents of the GMS file
        file_content <- readLines(file_path)

        # Initialize a variable to indicate whether we
        # are currently capturing lines
        capture_lines <- FALSE
        lines_after_pattern <- c()

        # Loop through the lines of the file
        for (line in file_content) {
            # Check if the line contains the target string
            if (str_detect(line, "positive variables|variables|equations")) {
                capture_lines <- TRUE
            # Skip this line as we don't need to store the target string itself
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
            # Modify the text to add
            lines_to_remove  <-
                grep(pattern = ";|^\\s*$", x = lines_after_pattern)
            text_to_add <- lines_after_pattern[-lines_to_remove]


            # Remove leading/trailing whitespaces
            input_vector <- text_to_add

            # Split the vector into two columns based on the whitespace
            first_space_index <- regexpr("\\s\\s\\s", input_vector)
            par <- substring(input_vector, 1, first_space_index - 1)
            descriptor <- substring(input_vector, first_space_index + 1)

            # Create the dataframe
            df <- data.frame(par, descriptor)

            for (j in seq_len(nrow(df))) {
                if (!endsWith(df[j, 1], ")")) {
                    df[j, 1] <- paste0(df[j, 1], "(", ")")
                }
            }

            df$par <- paste0("o", df$par)
            df$par <-
                gsub(pattern = "ovm_", replacement = "ov_", x = df$par)
            df$par <- sub("\\(", "(t, ", df$par)
            df$par <- sub("\\)", ", type)", df$par)
            df$par <- gsub(", ,", ",", df$par)

            text_to_add <- paste0(df$par, df$descriptor)
        }
        return(text_to_add)
    }


    ##########################################################

    message("Adding R section ...")

    beginning   <- "*########### Begin R section ###########"
    end         <- "*########### End R section   ###########"
    for (i in target_files) {
        content     <- replace_v_with_o(file_path = i)
        if (!is.null(content)) {
            write(x = "\n", file = i, append = TRUE)
            write(x = beginning, file = i, append = TRUE)
            write(x = "parameters", file = i, append = TRUE)
            write(x = content, file = i, append = TRUE)
            write(x = ";", file = i, append = TRUE)
            write(x = end, file = i, append = TRUE)
        }
    }

    lapply(model_path, removeEmptyR, multiline_fix = TRUE)

    ##########################################################
}
