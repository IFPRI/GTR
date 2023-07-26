#' updateDataOutput
#'
#' @param model_path Path to model
#' @return Modified GMS files with "optimized" parameter descriptions
#' @import stringr
#' @export
#' @examples
#' \dontrun{
#' updateDataOutput()
#' }
#' @author Abhijeet Mishra
updateDataOutput <- function(model_path = NULL) {
    if (is.null(model_path)) stop("Invalid model folder path provided")
    path <- paste0(model_path, "/modules")
    target_files_raw <-
        list.files(path = path, full.names = TRUE, recursive = TRUE)
    target_type <- c("declarations", "postsolve")

    target_files <-
        grep(pattern = "declaration", x = target_files_raw, value = TRUE)

    # Clean empty lines
    message("Cleaning empty lines at the end of the files ...")
    lapply(model_path, removeEmptyR)

    # Clean section of R if It already exists
    message("Cleaning pre-existing R sections if they exist ...")
    lapply(target_files, cleanR)

    # Add newline if needed
    message("Adding newlines for file endings ...")
    lapply(model_path, addNewlineR)

    addDescriptionPar(target_files = target_files)

    lapply(model_path, removeEmptyR, multiline_fix = TRUE)

    addDescriptionPost(target_files = target_files)

    lapply(model_path, removeEmptyR, multiline_fix = TRUE)

}
