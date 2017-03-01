#' Download datasets from the Roper Center
#'
#' \code{roper_download} provides a programmatic and reproducible means to download datasets 
#'   from the Roper Center data archive
#'
#' @param file_id The unique identifier (or optionally a vector of these identifiers)
#'  for the dataset(s) to be downloaded (see details).
#' @param email,password Your Roper Center email and password (see details)
#' @param reset If TRUE, you will be asked to re-enter your Roper Center email and password.
#' @param download_dir The directory (relative to your working directory) to
#'   which files from the Roper Center will be downloaded.
#' @param msg If TRUE, outputs a message showing which data set is being downloaded.
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
#' @import RSelenium
#' @importFrom stringr str_detect
#' @importFrom haven read_por
#' @importFrom rio export
#' @importFrom tools file_path_sans_ext
#' 
#' @export
roper_download <- function(file_id, 
                           email = getOption("roper_email"),
                           password = getOption("roper_password"),
                           reset = FALSE,
                           download_dir = "roper_data",
                           msg = TRUE) {
  
  # Detect login info
  if (reset){
    email <- password <- NULL
  }
  
  if (is.null(email)){
    roper_email <- readline(prompt = "The Roper Center requires your user account information.  Please enter your email address: \n")
    options("roper_email" = roper_email)
    email <- getOption("roper_email")
  }
  
  if (is.null(password)){
    roper_password <- readline(prompt = "Please enter your Roper Center password: \n")
    options("roper_password" = roper_password)
    password <- getOption("roper_password")
  }
  
  # Initialize driver
  if(msg) message("Initializing RSelenium driver")
  fprof <- makeFirefoxProfile(list(
    browser.download.dir = file.path(getwd(), download_dir),
    browser.download.folderList = 2L,
    browser.download.manager.showWhenStarting = FALSE,
    pdfjs.disabled=TRUE,
    plugin.scan.plid.all = FALSE,
    plugin.scan.Acrobat = "99.0",
    browser.helperApps.neverAsk.saveToDisk = "application/x-zip, application/por, application/pdf"))
  rD <- rsDriver(browser = "firefox", extraCapabilities = fprof, verbose = FALSE)
  remDr <- rD[["client"]]

  # Sign in
  signin <- "http://ropercenter.cornell.edu/CFIDE/cf/action/login/signin.cfm"
  remDr$navigate(signin)
  remDr$findElement(using = "name", "username")$sendKeysToElement(list(email))
  remDr$findElement(using = "name", "password")$sendKeysToElement(list(password))
  remDr$findElement(using = "name", "signin")$clickElement()
  Sys.sleep(3)
    
  # Loop through files
  for (i in seq(file_id)) { 
      item <- file_id[[i]]
      if(msg) message("Downloading Roper Center file: ", item, sprintf(" (%s)", Sys.time()))

      # Get list of current download directory contents
      if (!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
      dd_old <- list.files(download_dir)
            
      # build url
      url <- paste0("http://ropercenter.cornell.edu/CFIDE/cf/action/catalog/abstract.cfm?type=&start=&id=&archno=", item, "&abstract=")
      
      # navigate to download page, start download
      remDr$navigate(url)
      remDr$findElement(using = "partial link text", "SPSS file")$clickElement()
      
      # check that download has completed
      dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
      wait <- TRUE
      tryCatch(
        while(all.equal(str_detect(dd_new, "\\.part$"), logical(0))) {
          Sys.sleep(1)
          dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
        }, error = function(e) 1 )
      while(any(str_detect(dd_new, "\\.part$"))) {
        Sys.sleep(1)
        dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
      }
      
      # get codebook
      dd_old <- list.files(download_dir)
      remDr$findElement(using = "partial link text", "PDF file")$clickElement()
      
      # check that download has completed
      dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
      wait <- TRUE
      tryCatch(
        while(all.equal(str_detect(dd_new, "\\.part$"), logical(0))) {
          Sys.sleep(1)
          dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
        }, error = function(e) 1 )
      while(any(str_detect(dd_new, "\\.part$"))) {
        Sys.sleep(1)
        dd_new <- list.files(download_dir)[!list.files(download_dir) %in% dd_old]
      }
      
  }
  
  # Close driver
  remDr$close()
  rD[["server"]]$stop()
}  

