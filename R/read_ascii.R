#' Read ASCII datasets from the Roper Center
#'
#' \code{read_ascii} does stuff
#'
#' @param file A path to an ASCII data file.
#' @param total_cards For multicard files, the number of cards in the file.
#' @param var_names A string vector of variable names.
#' @param var_cards For multicard files, a numeric vector of the cards on which \code{var_names} are recorded.
#' @param var_positions A numeric vector of the column positions in which \code{var_names} are recorded.
#' @param var_widths A numeric vector of the widths used to record \code{var_names}.
#' @param card_pattern For use when the file does not contain a line for every card for every respondent, a regular expression that matches the file's card identifier; e.g., if the card number is stored in the last digit of the line, "\\d$".
#'
#' @details 
#'
#' @return A data frame containing the variables specified in the \code{var_names} argument, plus a \code{respondent} identifier and as many \code{card} variables (\code{card1}, \code{card2}, ...) as specified by the \code{total_cards} argument.
#'
#' @examples
#' \dontrun{
#'  read_ascii(file = "roper_data/USAIPOGNS1992-222054/USAIPOGNS1992-222054.dat",
#'    total_cards = 4,
#'    var_names = c("q24", "weight"),
#'    var_cards = c(4, 1),
#'    var_positions = c(46, 13),
#'    var_widths = c(1, 3))
#' }
#' 
#' @import rvest
#' @importFrom readr read_lines
#' @importFrom tibble as_tibble
#' @importFrom dplyr '%>%' mutate spread
#' @importFrom stringr str_extract str_replace
#' 
#' @export
read_ascii <- function(file,
                       total_cards = 1,
                       var_names,
                       var_cards = 1,
                       var_positions,
                       var_widths,
                       card_pattern) {
  
  if (length(var_cards) == 1) {
    var_cards = rep(var_cards, length(var_names))
  }
  
  df0 <- read_lines(file) %>%
    as_tibble() 
  
  if (missing(card_pattern)) {
    df1 <- df0 %>%
      mutate(card = paste0("card", rep_len(seq_len(total_cards), nrow(.))),
             respondent = rep(seq(to = nrow(.)/total_cards), each = total_cards))
  } else {
    df1 <- df0 %>% 
      mutate(card = paste0("card", str_extract(value, card_pattern)),
             respondent = rep(seq(to = nrow(.)/total_cards), each = total_cards))
    
  }
  
  df2 <- df1 %>%
    spread(key = "card", value = "value")
  
  for (i in seq_along(var_names)) {
    card <- paste0("card", var_cards[i])
    df2[[var_names[i]]] <- as.numeric(gsub(df2[[card]], 
                                           paste0("^.{", var_positions[i] - 1, "}(.{", var_widths[i], "}).*"),
                                           "\\1"))
  }
  
  return(df2)
}
