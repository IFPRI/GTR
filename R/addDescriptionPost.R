#' addDescriptionPost
#'
#' @param target_files Files which should be edited
#'
#' @return Modded text
#' @export
#'
#' @examples
#' \dontrun{
#' addDescriptionPost()
#' }
#' @author Abhijeet Mishra
addDescriptionPost <- function(target_files) {

    message("Adding R section ...")

    types <- data.frame(type = c("marginal", "level", "upper", "lower"),
                        suffix = c(".m", ".l", ".up", ".lo"))

    beginning   <- "*########### Begin R section ###########"
    end         <- "*########### End R section   ###########"

    # Redo for post
    target_files <- gsub(pattern = "declarations",
                         replacement = "postsolve",
                         x = target_files)
    target_files <- target_files[file.exists(target_files)]

    # Clean section of R if It already exists
    message("Cleaning pre-existing R sections in ")
    invisible(lapply(target_files, cleanR))

    for (i in target_files) {
        dummy_path <-
            gsub(pattern = "postsolve", replacement = "declarations", i)
        df <- addOptimizedPars(file_path = dummy_path,
                               return_df = TRUE,
                               post = TRUE)

        if (!is.null(df)) {
            df_expanded <- NULL
            for (j in seq_len(nrow(types))) {
                temp <- df
                temp$par <- gsub(pattern = ", type",
                                 replacement = ', "type"', x = temp$par)

                temp$par <- gsub(pattern = 'type',
                                 replacement = types$type[j], x = temp$par)
                temp$par2 <- gsub(pattern = '\\(',
                                  replacement = paste0(types$suffix[j], '('),
                                  x = temp$par2)
                pattern <- paste0(types$suffix[j], "\\(")
                exists_suffix <- grep(pattern = pattern,
                                      x = temp$par2)
                temp$par2[-exists_suffix] <-
                    paste0(temp$par2[-exists_suffix], types$suffix[j])

                temp$par2 <- paste0(temp$par2, ";")
                df_expanded <- rbind(df_expanded, temp)
            }

            # Make it pretty
            max_chars <- max(nchar(df_expanded$par))
            for (j in seq_len(nrow(df_expanded))) {
                num_whitespaces <- (max_chars + 5) - nchar(df_expanded$par[j])
                df_expanded$spaces[j] <-
                    paste(rep(" ", num_whitespaces), collapse = "")
            }

            content <-
                paste0(df_expanded$par, df_expanded$spaces, " = ", df_expanded$par2)

            if (!is.null(content)) {
                write(x = "\n", file = i, append = TRUE)
                write(x = beginning, file = i, append = TRUE)
                write(x = content, file = i, append = TRUE)
                write(x = ";", file = i, append = TRUE)
                write(x = end, file = i, append = TRUE)
            }
        }
    }
}
