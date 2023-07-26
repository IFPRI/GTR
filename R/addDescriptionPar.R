#' addDescriptionPar
#'
#' @param target_files Files which should be edited
#'
#' @return Modded text
#' @export
#'
#' @examples
#' \dontrun{
#' addDescriptionPar()
#' }
#' @author Abhijeet Mishra

addDescriptionPar <- function(target_files) {

    message("Adding R section ...")

    beginning   <- "*########### Begin R section ###########"
    end         <- "*########### End R section   ###########"
    for (i in target_files) {
        content     <- addOptimizedPars(file_path = i)
        if (!is.null(content)) {
            write(x = "\n", file = i, append = TRUE)
            write(x = beginning, file = i, append = TRUE)
            write(x = "parameters", file = i, append = TRUE)
            write(x = content, file = i, append = TRUE)
            write(x = ";", file = i, append = TRUE)
            write(x = end, file = i, append = TRUE)
        }
    }
}
