#' parGen
#'
#' @param lines_after_pattern Lines for which parameters need to be generated
#'
#' @return Modded text
#' @export
#'
#' @examples
#' \dontrun{
#' parGen()
#' }
#' @author Abhijeet Mishra
parGen <- function(lines_after_pattern) {
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
    df$par2 <- df$par #save real vars

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
    return(df)
}
