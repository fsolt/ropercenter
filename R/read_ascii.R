#' Read ASCII datasets downloaded from the Roper Center
#'
#' \code{read_ascii} helps format ASCII data files downloaded from the Roper Center.
#'
#' @param file A path to an ASCII data file.
#' @param total_cards For multicard files, the number of cards in the file.
#' @param var_names A string vector of variable names.
#' @param var_cards For multicard files, a numeric vector of the cards on which \code{var_names} are recorded.
#' @param var_positions A numeric vector of the column positions in which \code{var_names} are recorded.
#' @param var_widths A numeric vector of the widths used to record \code{var_names}.
#'
#' @return A data frame containing any variables specified in the \code{var_names} argument, plus a numeric \code{respondent} identifier and as many string \code{card} variables (\code{card1}, \code{card2}, ...) as specified by the \code{total_cards} argument.
#'
#' @examples
#' \dontrun{
#' gallup9206 <- read_ascii(file = "roper_data/USAIPOGNS1992-222054/USAIPOGNS1992-222054.dat",
#'    total_cards = 4,
#'    var_names = c("q24", "weight"),
#'    var_cards = c(4, 1),
#'    var_positions = c(46, 13),
#'    var_widths = c(1, 3))
#' }
#' 
#' @importFrom readr read_lines parse_guess
#' @importFrom tibble as_tibble
#' @importFrom dplyr '%>%' mutate 
#' @importFrom tidyr spread
#' @importFrom stringr str_extract str_replace
#' 
#' @export
read_ascii <- function(file,
                       total_cards = 1,
                       var_names,
                       var_cards = 1,
                       var_positions,
                       var_widths) {

  . <- NULL   # satisfy R CMD check
     
  if (length(var_cards) == 1 & !missing(var_names)) {
    var_cards = rep(var_cards, length(var_names))
  }

  df <- read_lines(file) %>%
    as_tibble() %>%
    mutate(card = paste0("card", rep_len(seq_len(total_cards), nrow(.))),
           respondent = rep(seq(to = nrow(.)/total_cards), each = total_cards)) %>%
    spread(key = "card", value = "value")

  if (!missing(var_names)) {
    if (missing(var_positions) | missing(var_widths)) {
      stop("Variable positions and widths should also be given when variable names are specified", call. = FALSE)
    } else if (length(unique(sapply(list(var_names, var_positions, var_widths), length))) > 1) {
      stop("The lengths of the vectors of variable names, positions, and widths must be the same", call. = FALSE)
    } else if ((max(var_cards) > max(total_cards))) {
      stop("When reading a multi-card dataset, the numbers of the cards with variables to be read must not be greater than the total number of cards",
           call. = FALSE)
    }
    
    for (i in seq_along(var_names)) {
      card <- paste0("card", var_cards[i])
      df[[var_names[i]]] <- parse_guess(str_replace(df[[card]],
                                                    paste0("^.{", var_positions[i] - 1, "}(.{", var_widths[i], "}).*"),
                                                    "\\1"))
    }
  }
  
  return(df)
}
