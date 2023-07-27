#' addRealizationPhase
#'
#' @param model_path Path of files to be modified
#'
#' @return Modded text
#' @export
#'
#' @examples
#' \dontrun{
#' addRealizationPhase()
#' }
#' @author Abhijeet Mishra
addRealizationPhase <- function(model_path = NULL) {

    if (is.null(model_path)) stop("Invalid model folder path provided")
    path <- paste0(model_path, "/modules")
    modules <- list.dirs(path, recursive = FALSE)
    for (module_top in modules) {
        realization_dirs <- list.dirs(module_top, recursive = FALSE)
        realization_dirs <- grep(pattern = "input", x = realization_dirs,
                                 invert =  TRUE, value = TRUE)
        for (realization in realization_dirs) {
            target_files_raw <-
                list.files(path = realization,
                           full.names = TRUE,
                           recursive = TRUE)

            phase_vector <- gsub(pattern = ".gms", replacement = "",
                                 x = basename(target_files_raw))

            target_files <-
                grep(pattern = "realization",
                     x = target_files_raw,
                     value = TRUE)

            # Clean section of R if It already exists
            message("Cleaning pre-existing R sections if they exist ...")
            lapply(target_files, cleanR)

            message("Adding R section for realization files...")
            beginning   <- "*########### Begin R section ###########"
            end         <- "*########### End R section   ###########"

            file_content <- readLines(target_files)

            correct_sequence <- c("sets", "declarations", "input", "solve",
                                  "equations", "scaling", "start", "preloop",
                                  "presolve", "postsolve")
            to_add <- intersect(correct_sequence, phase_vector)

            content <- c()
            for (phases in to_add) {
                suffix <- '$Ifi "%phase%" == '
                phase_text <- paste0(" ", '"', phases, '" ')
                incl <- "$include "
                file_name <- paste0('"./modules/',
                                    basename(dirname(realization_dirs)),
                                    "/",
                                    basename(realization),
                                    "/",
                                    phases,
                                    '.gms"')
                temp_content <- paste0(suffix, phase_text, incl, file_name)
                content <- c(content, temp_content)
            }


            if (!is.null(content)) {
                write(x = "\n", file = target_files, append = TRUE)
                write(x = beginning, file = target_files, append = TRUE)
                write(x = content, file = target_files, append = TRUE)
                write(x = end, file = target_files, append = TRUE)
            }
        }
    }
    lapply(model_path, removeEmptyR, multiline_fix = TRUE)
}
