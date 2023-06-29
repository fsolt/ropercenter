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
#' roper_download("USAIPO1982-1197G", # Gallup Poll for June 25-28, 1982
#'                download_dir = tempdir())  # remember to specify a directory for your download
#'                       
#' gallup1982 <- read_ascii(file = file.path(tempdir(), "USAIPO1982-1197G",
#'                                           "1197.dat"),
#'                          var_names = c("q09j", "weight"),
#'                          var_positions = c(38, 1),
#'                          var_widths = c(1, 1))
#'    
#' # a multi-card file, with extra lines that make the card_pattern and
#'   respondent_pattern arguments necessary
#' roper_download("USAIPOCNUS1996-9603008", # Gallup/CNN/USA Today Poll: Politics/1996 Election
#'                download_dir = tempdir())  # remember to specify a directory for your download
#' 
#' gallup1996 <- read_ascii(file = file.path(tempdir(), "USAIPOCNUS1996-9603008",
#'                                           "a9603008.dat"),
#'                          var_names = c("q43a", "q44", "weight"),
#'                          var_cards = c(6, 6, 1),
#'                          var_positions = c(62, 64, 13),
#'                          var_widths = c(1, 1, 3),
#'                          total_cards = 7,
#'                          card_pattern = "(?<=^.{10})\\d", 
#'                                         # (a digit, preceded by the start of the line
#'                                         # and ten other characters)
#'                          respondent_pattern = "(?<=^\\s{2})\\d{4}")
#'                                        # (# four digits, preceded by the start of the line
#'                                        # and two whitespace characters)
#' }
#' 
#' @importFrom readr read_lines parse_guess
#' @importFrom tibble as_tibble
#' @importFrom dplyr '%>%' mutate filter select summarise group_by if_else row_number first
#' @importFrom tidyr spread fill separate pivot_longer nest
#' @importFrom stringr str_extract str_replace str_trim str_replace_all str_c
#' @importFrom purrr as_vector
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
      dplyr::mutate(card = paste0("card", rep_len(seq_len(total_cards), nrow(.))),
                    respondent = rep(seq(to = nrow(.)/total_cards), each = total_cards)) %>%
      spread(key = "card", value = "value")
  } else {
    df <- read_lines(file) %>%
      as_tibble() %>% 
      dplyr::mutate(card = paste0("card", str_extract(value, card_pattern))) %>% 
      filter(!card == "cardNA") %>% 
      dplyr::mutate(respondent = str_extract(value, respondent_pattern)) %>%
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
      if (var_widths[i] < 4) {
        is.na(df[[var_names[i]]]) <- which(!nchar(df[[var_names[i]]]) == var_widths[i])
      }
    }
  }
  
  return(df)
}

setup_file <- "../Data/dcpo_surveys/icpsr_files/cnes_files/ICPSR_03969/DS0001/03969-0001-Setup.sps"
setup <- read_lines(setup_file) %>%
  as_tibble() %>% 
  dplyr::mutate(section = str_extract(value, "^\\*.+")) %>%
  tidyr::fill(section) 

data_list <- setup %>%
  filter(str_detect(value, "\\w+\\s\\d+-\\d+")) %>%
  dplyr::mutate(value = stringr::str_trim(value)) %>% 
  tidyr::separate(value, 
                  sep = "\\s+(?=\\w+\\s\\d+-\\d+)", 
                  into = LETTERS[1:5], 
                  fill = "right") %>%
  tidyr::pivot_longer(LETTERS[1:5]) %>%
  filter(!is.na(value)) %>% 
  tidyr::separate(value, 
                  sep = "[\\s-]+",
                  into = c("variable", "first_col", "last_col", "notes"),
                  fill = "right") %>% 
  dplyr::mutate(first_col = as.numeric(first_col),
                last_col = as.numeric(last_col),
                width = last_col - first_col + 1) %>%
  dplyr::select(variable, first_col, last_col, width)

ascii_dataset <- read_ascii("../Data/dcpo_surveys/icpsr_files/cnes_files/ICPSR_03969/DS0001/03969-0001-Data.txt",
                            var_names = data_list$variable,
                            var_positions = data_list$first_col,
                            var_widths = data_list$width)

variable_labels <- setup %>% 
  filter(str_detect(section, "LABELS")) %>%
  dplyr::mutate(section = str_extract(value, ".*LABELS.*")) %>%
  tidyr::fill(section) %>%
  filter(str_detect(section, "VARIABLE")) %>%
  dplyr::mutate(var = str_extract(value, "^[^'\"]*") %>% 
                  stringr::str_trim(),
                label = str_extract(value, "\".*\""),
                label2 = dplyr::if_else(is.na(label), str_extract(value, "'.*'"), label) %>% 
                  stringr::str_replace_all("^[\"']|[\"']$", "") %>% 
                  stringr::str_trim(),
                id = dplyr::if_else(var != "", dplyr::row_number(), NA_integer_)) %>% 
  tidyr::fill(id) %>% 
  dplyr::group_by(id) %>%
  dplyr::summarise(variable = dplyr::first(var),
                   label = stringr::str_c(label2, collapse = " ")) %>% 
  filter(!is.na(label)) %>% 
  dplyr::select(-id)

var_labels <- split(variable_labels$label, variable_labels$variable)

ascii_dataset2 <- ascii_dataset %>% 
  labelled::set_variable_labels(.labels = var_labels)

value_labels <- setup %>% 
  filter(str_detect(section, "LABELS")) %>%
  dplyr::mutate(section = str_extract(value, ".*LABELS.*")) %>%
  tidyr::fill(section) %>%
  filter(str_detect(section, "VALUE")) %>%
  dplyr::mutate(variable = dplyr::if_else(str_detect(value, "^\\s*\\d"), NA_character_, stringr::str_trim(value)),
                value2 = dplyr::if_else(is.na(variable),
                                        str_replace(value, "(\\d+)\\s.*", "\\1") %>% 
                                          stringr::str_trim(),
                                        NA_character_),
                label2 = dplyr::if_else(is.na(variable),
                                        str_replace(value, "\\d+\\s(.*)", "\\1") %>% 
                                          str_replace("/", "") %>% 
                                          stringr::str_trim(),
                                        NA_character_)) %>% 
  tidyr::fill(variable) %>% 
  filter(!is.na(value2)) %>% 
  dplyr::select(variable, value2, label2) %>% 
  tidyr::nest(labels = c(value2, label2))

for(i in 1:nrow(value_labels)) {
  x_labs <- value_labels[i, "labels"][[1]][[1]]
  labelled::val_labels(ascii_dataset2[, value_labels$variable[i]]) <- split(as.numeric(x_labs$value2), x_labs$label2) %>% 
    purrr::as_vector()
}

