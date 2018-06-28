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
#' @param card_pattern For use when the file does not contain a line for every card for every respondent (or contains extra lines that correspond to no respondent), a regular expression that matches the file's card identifier; e.g., if the card number is stored in the last digit of each line, "\\d$".
#' @param respondent_pattern For use when the file does not contain a line for every card for every respondent (or contains extra lines that correspond to no respondent), a regular expression that matches the file's respondent identifier; e.g., if the respondent number is stored in the first four digits of each line, preceded by a space, "(?<=^\\s)\\d{4}".
#'
#' @details Many older Roper Center datasets are available only in ASCII format, which is notoriously difficult to work with.  The `read_ascii` function facilitates the process of extracting selected variables from ASCII datasets. For single-card files, one can simply identify the names, positions, and widths of the needed variables from the codebook and pass them to \code{read_ascii}'s \code{var_names}, \code{var_positions}, and \code{var_widths} arguments.  Multicard datasets are more complicated. In the best case, the file contains one line per card per respondent; then, the user can extract the needed variables by adding only the \code{var_cards} and \code{total_cards} arguments. When this condition is violated---there is not a line for every card for every respondent, or there are extra lines---the function will throw an error and request the user specify the additional arguments \code{card_pattern} and \code{respondent_pattern}.
#' 
#' See \code{\link[readroper]{read_rpr}} for an alternate implementation.
#'
#' @return A data frame containing any variables specified in the \code{var_names} argument, plus a numeric \code{respondent} identifier and as many string \code{card} variables (\code{card1}, \code{card2}, ...) as specified by the \code{total_cards} argument.
#'
#' @examples
#' \dontrun{
#' # a single-card file
#' gallup1982 <- read_ascii(file = "roper_data/USAIPO1982-1197G/USAIPO1982-1197G.dat",
#'    var_names = c("q09j", "weight"),
#'    var_positions = c(38, 1),
#'    var_widths = c(1, 1))
#'    
#' # a multi-card file, with extra lines make the card_pattern and respondent_pattern arguments necessary
#' gallup1996 <- read_ascii(file = "roper_data/USAIPOCNUS1996-9603008/USAIPOCNUS1996-9603008.dat",
#'    var_names = c("q43a", "q44", "weight"),
#'    var_cards = c(6, 6, 1),
#'    var_positions = c(62, 64, 13),
#'    var_widths = c(1, 1, 3),
#'    total_cards = 7,
#'    card_pattern = "(?<=^.{10})\\d",
#'    respondent_pattern = "(?<=^\\s{2})\\d{4}")
#' }
#' 
#' @importFrom readr read_lines parse_guess
#' @importFrom tibble as_tibble
#' @importFrom dplyr '%>%' mutate filter
#' @importFrom tidyr spread
#' @importFrom stringr str_extract str_replace
#' 
#' @export
read_ascii <- function(file,
                       total_cards = 1,
                       var_names,
                       var_cards = 1,
                       var_positions,
                       var_widths,
                       card_pattern,
                       respondent_pattern) {

  . <- value <- NULL   # satisfy R CMD check
     
  if ((length(read_lines(file)) %% total_cards) != 0 & (missing(card_pattern) | missing(respondent_pattern))) {
    stop("The number of lines in the file is not a multiple of the number of cards in the file.  Please specify card_pattern and respondent_pattern", call. = FALSE)
  }
  
  if (length(var_cards) == 1 & !missing(var_names)) {
    var_cards = rep(var_cards, length(var_names))
  }

  if (missing(card_pattern)) {
    df <- read_lines(file) %>%
      as_tibble() %>%
      mutate(card = paste0("card", rep_len(seq_len(total_cards), nrow(.))),
             respondent = rep(seq(to = nrow(.)/total_cards), each = total_cards)) %>%
      spread(key = "card", value = "value")
  } else {
    df <- read_lines(file) %>%
      as_tibble() %>% 
      mutate(card = paste0("card", str_extract(value, card_pattern))) %>% 
      filter(!card == "cardNA") %>% 
      mutate(respondent = str_extract(value, respondent_pattern)) %>%
      spread(key = "card", value = "value")
  }

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
                                                    "\\1") %>% 
                                          str_replace("^\\s+$", ""))
    }
  }
  
  return(df)
}
