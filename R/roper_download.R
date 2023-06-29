#' Download datasets from the Roper Center
#'
#' \code{roper_download} provides a programmatic and reproducible means to download  
#'   datasets from the Roper Center's data archive
#'
#' @param file_id The unique identifier (or optionally a vector of these identifiers)
#'   for the dataset(s) to be downloaded (see details).  Both new Archive Numbers and old
#'   Historical Archive Numbers, if the latter are available, may be used.  
#' @param affiliation,email,password Your Roper Center affiliation, email, and password 
#'   (see details)
#' @param reset If TRUE, you will be asked to re-enter your Roper Center affiliation, 
#'   email, and password.
#' @param download_dir The directory (relative to your working directory) to
#'   which files from the Roper Center will be downloaded.
#' @param msg If TRUE, outputs a message showing which data set is being downloaded.
#' @param convert If TRUE, converts downloaded file(s) to .RData format.
#' @param delay If the speed of your connection to the Roper Center data archive is particularly slow, 
#'   \code{roper_download} may encounter problems.  Increasing the \code{delay} parameter
#'   may help.
#'   
#' @details 
#'  To avoid requiring others to edit your scripts to insert their own affiliation, 
#'  email, and password or to force them to do so interactively, the default is set 
#'  to fetch this information from the user's .Rprofile.  Before running 
#'  \code{roper_download}, then, you should be sure to add these options to your 
#'  .Rprofile (\code{usethis::edit_r_profile()} is one particularly easy way),
#'  substituting your own info for the example below:
#'
#'  \code{
#'   options("roper_affiliation" = "Upper Midwest University",
#'           "roper_email" = "juanita-herrara@uppermidwest.edu",
#'           "roper_password" = "password123!")
#'  }
#'
#' @return The function returns nothing, but has the side effect of downloading
#' all files of the datasets identified in the file_id argument.
#'
#' @examples
#' \dontrun{
#'  roper_download(file_id = c("31117412", "USPEW2015-GOVERNANCE"),
#'                 download_dir = tempdir()) # remember to specify a directory for your download
#' }
#' 
#' @import RSelenium
#' @importFrom stringr str_detect str_subset str_replace str_extract_all
#' @importFrom magrittr '%>%'
#' @importFrom rio export
#' @importFrom haven read_dta read_por
#' @importFrom foreign read.spss
#' @importFrom netstat free_port
#' 
#' @export
roper_download <- function(file_id,
                           affiliation = getOption("roper_affiliation"),
                           email = getOption("roper_email"),
                           password = getOption("roper_password"),
                           reset = FALSE,
                           download_dir = "roper_data",
                           msg = TRUE,
                           convert = TRUE,
                           delay = 2) {
  . <- NULL # satisfy CRAN check
  
  file_id <- as.character(file_id)
  
  # detect login info
  if (reset) {
    affiliation <- email <- password <- NULL
  }
  
  if (is.null(affiliation)) {
    roper_affiliation <- readline(prompt = "The Roper Center requires your user account information.  Please enter the member organization that you are affiliated with: \n")
    options("roper_affiliation" = roper_affiliation)
    affiliation <- getOption("roper_affiliation")
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
  
  # build path to chrome's default download directory
  if (Sys.info()[["sysname"]]=="Linux") {
    default_dir <- file.path("home", Sys.info()[["user"]], "Downloads")
  } else {
    default_dir <- file.path("", "Users", Sys.info()[["user"]], "Downloads")
  }
  
  # create specified download directory if necessary
  if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
  
  # initialize driver
  if (msg) message("Initializing RSelenium driver")
  rD <- rsDriver(browser="firefox", 
                       port = free_port(),
                       verbose = FALSE,
                       chromever = NULL)
  remDr <- rD$client
  
  # sign in
  signin <- "https://ropercenter.cornell.edu/ipoll/login"
  remDr$navigate(signin)
  Sys.sleep(delay)

  remDr$findElement(using = "id", "react-select-2-input")$sendKeysToElement(list(affiliation, key = "enter"))
  remDr$findElement(using = "id", "username")$sendKeysToElement(list(email))
  remDr$findElement(using = "id", "password")$sendKeysToElement(list(password))
  remDr$findElement(using = "css", ".btn.btn-primary.float-right")$clickElement()
  if (try(unlist(remDr$findElement(using = "class", "accept-container")$getElementAttribute('id')), silent = TRUE) == "") { # check for cookies pop-up
    remDr$findElement(using = "css", ".accept-container .btn-primary")$clickElement() # accept cookies 
  }  
  
  Sys.sleep(delay)
  
  # Loop through files
  for (i in seq(file_id)) { 
    item <- file_id[[i]]
    if (msg) message("Downloading Roper Center file: ", item, sprintf(" (%s)", Sys.time()))
    
    # get list of current default download directory contents
    dd_old <- list.files(default_dir)
    
    # navigate to study page
    url <- paste0("https://ropercenter.cornell.edu/ipoll/study/", item)
    remDr$navigate(url)
    Sys.sleep(delay)
    if (try(unlist(remDr$findElement(using = "class", "accept-container")$getElementAttribute('id')), silent = TRUE) == "") { # check for cookies pop-up
      remDr$findElement(using = "css", ".accept-container .btn-primary")$clickElement() # accept cookies 
    }  
    
    # switch to download tab
    remDr$findElement(using = "css", "#download-tab")$clickElement()
    
    download_links <- remDr$getPageSource()[[1]] %>% 
      stringr::str_extract_all('rc-download-btn[^"]*') %>% 
      `[[`(1) %>% 
      paste0("#", .)
    
    download_files <- remDr$getPageSource()[[1]] %>% 
      stringr::str_extract_all('(?<=\\()[^)]+\\.[a-z]{3,4}(?=\\))') %>% 
      `[[`(1)
    
    # download all files
    for (j in seq_along(download_links)) {
      new_dd_old <- list.files(default_dir)
      remDr$findElement(using = "css", download_links[j])$clickElement() # initiate data download
      Sys.sleep(delay * .61)
      
      element <- try(unlist(remDr$findElement(using = "css", "#rc-downloads-tc-modal-accept-btn")$getElementAttribute('id')), silent = TRUE)
      if (element == "rc-downloads-tc-modal-accept-btn") { # check for terms pop-up
        remDr$findElement(using = "css", "#rc-downloads-tc-modal-accept-btn")$clickElement() # accept terms 
      }
      
      # check that download has completed
      dd_new <- setdiff(list.files(default_dir), new_dd_old)
      
      jj = 0
      tryCatch(
        while(length(dd_new) == 0) { # has download started?
          Sys.sleep(abs(rnorm(1)))
          dd_new <- setdiff(list.files(default_dir), new_dd_old)
          message("waiting for download to start")
          if (jj < 10)
            jj = jj + 1
          else {
            message("attempting to start download again")
            remDr$findElement(using = "css", download_links[j])$clickElement() # initiate data download
            Sys.sleep(sum(abs(rnorm(delay))))
            
            element <- try(unlist(remDr$findElement(using = "css", "#rc-downloads-tc-modal-accept-btn")$getElementAttribute('id')), silent = TRUE)
            if (element == "rc-downloads-tc-modal-accept-btn") { # check for terms pop-up
              remDr$findElement(using = "css", "#rc-downloads-tc-modal-accept-btn")$clickElement() # accept terms 
              Sys.sleep(1)
            }
            Sys.sleep(abs(rnorm(1)))
            jj = 0
          }
        }, error = function(e) 1
      )
      while(any(stringr::str_detect(dd_new, "\\.part$"))) { # has download finished?
        Sys.sleep(1)
        dd_new <- setdiff(list.files(default_dir), new_dd_old)
        message("waiting for download to finish")
      }
      Sys.sleep(sum(abs(rnorm(delay*2))))
    }
    dd_new <- setdiff(list.files(default_dir), dd_old)
    
    # move to specified directory
    dir.create(file.path(download_dir, item), showWarnings = FALSE)
    for (ii in seq_along(dd_new)) {
      file.rename(from = file.path(default_dir, dd_new[ii]), to = file.path(download_dir, item, dd_new[ii]))
      unlink(file.path(default_dir, dd_new[ii]))
    }
    
    # convert to .RData
    spss <- 0
    if (convert == TRUE) {
      if (any(stringr::str_detect(dd_new, "dta|DTA"))) {
        tryCatch(
          haven::read_dta(file.path(download_dir, item, stringr::str_subset(dd_new, "\\.dta|DTA")), encoding = "latin1") %>%
            rio::export(stringr::str_replace(file.path(download_dir, item, stringr::str_subset(dd_new, "\\.(dta|DTA)")), "\\.(dta|DTA)", ".RData")),
          error = function(e) spss <- 1
        )
      }
      if (any(stringr::str_detect(dd_new, "dta|DTA")) == FALSE | spss == 1) {
        tryCatch( 
          {data_file <- stringr::str_subset(dd_new, "\\.por")
          x <- tryCatch(haven::read_por(file.path(download_dir, item, data_file)),
                        error = function(e) {
                          foreign::read.spss(file.path(download_dir, item, data_file),
                                             to.data.frame = TRUE,
                                             use.value.labels = FALSE)
                        })
          save(x, file = stringr::str_replace(file.path(download_dir, item, data_file), "\\.por$", ".RData"))},
          error = function(e) warning(paste("Conversion from .por to .RData failed for", item))
        )
      }
    }
  }
  
  # Close driver
  remDr$close()
  rD[["server"]]$stop()
}  

