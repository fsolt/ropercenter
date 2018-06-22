#' Download datasets from the Roper Center
#'
#' \code{roper_download} provides a programmatic and reproducible means to download datasets 
#'   from the Roper Center's data archive
#'
#' @param file_id The unique identifier (or optionally a vector of these identifiers)
#'  for the dataset(s) to be downloaded (see details).
#' @param email,password Your Roper Center email and password (see details)
#' @param reset If TRUE, you will be asked to re-enter your Roper Center email and password.
#' @param download_dir The directory (relative to your working directory) to
#'   which files from the Roper Center will be downloaded.
#' @param msg If TRUE, outputs a message showing which data set is being downloaded.
#' @param convert If TRUE, converts downloaded file(s) to .RData format.
#'
#' @details 
#'  To avoid requiring others to edit your scripts to insert their own email and  
#'  password or to force them to do so interactively, the default is set to fetch 
#'  this information from the user's .Rprofile.  Before running \code{roper_download}, 
#'  then, you should be sure to add these options to your .Rprofile substituting your 
#'  info for the example below:
#'
#'  \code{
#'   options("roper_email" = "juanita-herrara@uppermidwest.edu",
#'          "roper_password" = "password123!")
#'  }
#'
#' @return The function returns downloaded files.
#'
#' @examples
#' \dontrun{
#'  roper_download(file_id = c("CNCIPO1996-96010", "CNCIPO2000-02"))
#' }
#' 
#' @import rvest
#' @importFrom httr content cookies set_cookies
#' @importFrom xml2 read_html
#' @importFrom dplyr '%>%' first nth
#' @importFrom stringr str_replace str_replace_all
#' @importFrom purrr walk
#' @importFrom haven read_por
#' @importFrom foreign read.spss
#' 
#' @export
roper_download <- function(file_id, 
                           email = getOption("roper_email"),
                           password = getOption("roper_password"),
                           reset = FALSE,
                           download_dir = "roper_data",
                           msg = TRUE,
                           convert = TRUE) {
  
  # detect login info
  if (reset) {
    email <- password <- NULL
  }
  
  if (is.null(email)) {
    roper_email <- readline(prompt = "The Roper Center requires your user account information.  Please enter your email address: \n")
    options("roper_email" = roper_email)
    email <- getOption("roper_email")
  }
  
  if (is.null(password)) {
    roper_password <- readline(prompt = "Please enter your Roper Center password: \n")
    options("roper_password" = roper_password)
    password <- getOption("roper_password")
  }
  
  # enter username and password
  signin <- "http://ropercenter.cornell.edu/CFIDE/cf/action/login/signin.cfm"
  p0 <- html_session(signin)
  
  f0 <- html_form(p0) %>%
    first() %>% 
    set_values("username" = email,
               "password" = password)
  
  c0 <- httr::cookies(p0)$value
  names(c0) <- httr::cookies(p0)$name
  p1 <- suppressMessages(submit_form(session = p0, form = f0, config = httr::set_cookies(.cookies = c0)))
  
  # Loop through files
  walk(file_id, function(item) { 
    if (msg) message("Downloading Roper Center file: ", item, sprintf(" (%s)", Sys.time()))
    
    # create item directory
    item_dir <- file.path(download_dir, item)
    if (!dir.exists(item_dir)) dir.create(item_dir, recursive = TRUE)
    
    # build url
    url <- paste0("http://ropercenter.cornell.edu/CFIDE/cf/action/catalog/abstract.cfm?type=&start=&id=&archno=", item, "&abstract=")
    
    # navigate to download page, get data
    item_page <- p1 %>% 
      jump_to(url) 
    
    data_links <- item_page %>% 
      xml2::read_html() %>% 
      html_nodes(xpath = "//a[contains(.,'SPSS file')]") %>% 
      html_attr("href") %>% 
      trimws() 
    if (identical(data_links, character(0))) {
      data_links <- item_page %>% 
        xml2::read_html() %>% 
        html_nodes(xpath = "//a[contains(.,'ASCII file')]") %>% 
        html_attr("href") %>% 
        trimws()
      spss <- FALSE
      data_links0 <- data_links
    } else {
      spss <- TRUE
    }
    data_links <- paste0("https://ropercenter.cornell.edu", data_links)

    if (spss) { 
      for (i in seq(data_links)) {
        data_link <- data_links[i]
        dl_data <- item_page %>% 
          jump_to(data_link)
        
        if (length(data_links)==1) {
          data_file <- paste0(item, ".por")
        } else {
          data_file <- item_page %>% 
            xml2::read_html() %>% 
            html_nodes(xpath = "//a[contains(.,'SPSS file')]") %>%
            nth(i) %>% 
            html_text() %>%
            trimws() %>% 
            str_replace(" SPSS file", "") %>% 
            str_replace_all("\\s", "_") %>% 
            paste0(".por")
        }
        writeBin(httr::content(dl_data$response, "raw"), file.path(item_dir, data_file))
        
        # convert data to .RData
        if (convert == TRUE) {
          tryCatch( 
            {x <- tryCatch(haven::read_por(file.path(item_dir, data_file)),
                           error = function(e) {
                             foreign::read.spss(file.path(item_dir, data_file),
                                                to.data.frame = TRUE,
                                                use.value.labels = FALSE)
                           })
            save(x, file = stringr::str_replace(file.path(item_dir, data_file), "\\.por$", ".RData"))},
            error = function(e) warning(paste("Conversion from .por to .RData failed for", item))
          )
        }
      }
    } else {
      if (identical(data_links0, character(0))) {
        warning(paste("No SPSS or ASCII data file available for", item))
      } else {
        dl_data <- item_page %>% 
          jump_to(data_links[1])
        
        data_file <- paste0(item, ".dat")
        writeBin(httr::content(dl_data$response, "raw"), file.path(item_dir, data_file))
      }
    }
    
    # get codebook
    pdf_link <- item_page %>% 
      xml2::read_html() %>% 
      html_node(xpath = "//a[contains(.,'PDF file')]") %>% 
      html_attr("href") %>% 
      trimws()
    pdf_link <- paste0("https://ropercenter.cornell.edu", pdf_link)
    
    dl_pdf <- item_page %>% 
      jump_to(pdf_link)
    
    pdf_file <- paste0(item, "_cb.pdf")
    writeBin(httr::content(dl_pdf$response, "raw"), file.path(item_dir, pdf_file))
  })
}  
