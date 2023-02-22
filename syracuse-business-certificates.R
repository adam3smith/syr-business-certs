

# Setup -------------------------------------------------------------------

library(RSelenium)
library(here)

# Set parameters
link <-
  "https://cotthosting.com/NYOnondagaExternal/User/Login.aspx?ReturnUr=%2fNYOnondagaExternal"
fromDate <- list("12012022")
thruDate <- list("12312022")
startAt <- 1


# Start driver
driver <-
  rsDriver(browser = "firefox", chromever = NULL) # can also be chrome
remote_driver <- driver[["client"]]

Sys.sleep(5)

# Enter search mask & search ----------------------------------------------

# Navigate to digital documents page
remote_driver$navigate(link)
Sys.sleep(2)
# Click on Guest Login
remote_driver$findElement(using = "id", value = "ctl00_cphMain_blkLogin_btnGuestLogin")$clickElement()
Sys.sleep(2)
# Accept the terms
remote_driver$findElement(using = "id", value = "ctl00_btnEmergencyMessagesClose")$clickElement()
Sys.sleep(2)
# Go to Date Range Search
remote_driver$findElement(using = "id", value = "ctl00_NavMenuIdxRec_btnNav_IdxRec_Date_NEW")$clickElement()
Sys.sleep(2)
#Specify Index Type Business Certificate
remote_driver$findElement(using = "css selector", value = "#ctl00_cphMain_tcMain_tpNewSearch_ucSrchDates_lbIndexTypes option[value='CRT']")$clickElement()

#Specify Kind Business Certificate
remote_driver$findElement(using = "css selector", value = "#ctl00_cphMain_tcMain_tpNewSearch_ucSrchDates_lbKinds option[value='46|CRT']")$clickElement()


#Enter from and through date
remote_driver$findElement(using = "id", value = "ctl00_cphMain_tcMain_tpNewSearch_ucSrchDates_txtFiledFrom")$sendKeysToElement(fromDate)

remote_driver$findElement(using = "id", value = "ctl00_cphMain_tcMain_tpNewSearch_ucSrchDates_txtFiledThru")$sendKeysToElement(thruDate)

# Click search button
remote_driver$findElement(using = "id", value = "ctl00_cphMain_tcMain_tpNewSearch_ucSrchDates_btnSearch")$clickElement()

Sys.sleep(3)

# Loop through results ----------------------------------------------------


# create empty dataframe
businessInfo <- data.frame()

# get all elements linking to files
files <-
  remote_driver$findElements(using = "css", value = "a[id*='lbDocument_FileNumberWithSuffix']")

for (i in startAt:length(files)) {
  # check we're on the results page
  message("Running cycle number ", i)
  iterations <- 0
  onResultsPage <- FALSE
  while (!onResultsPage & iterations < 10) {
    tryCatch({
      files <-
        remote_driver$findElements(using = "css", value = "a[id*='lbDocument_FileNumberWithSuffix']")
      
      
      if (length(files) > 2)  {
        onResultsPage <<- TRUE
      }
    },
    error = function(e) {
      iterations <<- iterations + 1
      Sys.sleep(0.5)
    })
  }
  
  if (!onResultsPage & iterations == 10) {
    stop(paste0(
      "Can't reload results page. Something has gone wrong during cycle ",
      i
    ))
  }
  
  # click on result
  files[[i]]$clickElement()
  
  # check we're on item page
  Sys.sleep(2)
  
  iterations <- 0
  onCertPage <- FALSE
  
  while (!onCertPage & iterations < 10) {
    tryCatch({
      test <-
        remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvDetails1 tr>td:nth-child(1)")$getElementText()
      
      if (length(test) > 0)  {
        onCertPage <<- TRUE
      }
    },
    error = function(e) {
      iterations <<- iterations + 1
      Sys.sleep(0.5)
    })
  }
  
  if (!onCertPage & iterations == 10) {
    message(
      "Cant get to certificate info on cycle ",
      i,
      ". Writing NA to data, saving file, and trying to return to results"
    )
    businessInfo <-
      rbind(businessInfo,
            data.frame(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA))
    tmp <- remote_driver$getPageSource()[[1]]
    write(tmp, file = here("data", "files", paste0("error_", i, ".html")))
    remote_driver$findElement(using = "id", value = "btnReturn")$clickElement()
    next
  }
  
  # Save individual fields
  instrNumber <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvDetails1 tr>td:nth-child(1)")$getElementText()
  print(instrNumber[[1]])
  dateFiled <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvDetails1 tr>td:nth-child(6)")$getElementText()
  
  # Business Infor
  businessName <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties1 tr>td:nth-child(1)")$getElementText()
  address1 <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties1 tr>td:nth-child(2)")$getElementText()
  address2 <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties1 tr>td:nth-child(3)")$getElementText()
  city <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties1 tr>td:nth-child(4)")$getElementText()
  state <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties1 tr>td:nth-child(5)")$getElementText()
  zip <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties1 tr>td:nth-child(6)")$getElementText()
  ownerName <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties2 tr>td:nth-child(1)")$getElementText()
  ownerAddress1 <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties2 tr>td:nth-child(2)")$getElementText()
  ownerAddress2 <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties2 tr>td:nth-child(3)")$getElementText()
  ownerCity <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties2 tr>td:nth-child(4)")$getElementText()
  ownerState <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties2 tr>td:nth-child(5)")$getElementText()
  ownerZip <-
    remote_driver$findElement(using = "css", value = "#ctl00_cphMain_gvParties2 tr>td:nth-child(6)")$getElementText()
  
  # add to businessInfo dataframe
  businessInfo <-
    rbind(
      businessInfo,
      data.frame(
        instrNumber[[1]],
        dateFiled[[1]],
        businessName[[1]],
        address1[[1]],
        address2[[1]],
        city[[1]],
        state[[1]],
        zip[[1]],
        ownerName[[1]],
        ownerAddress1[[1]],
        ownerAddress2[[1]],
        ownerCity[[1]],
        ownerState[[1]],
        ownerZip[[1]]
      )
    )
  
  
  #save results page into the data/files folder
  tmp <- remote_driver$getPageSource()[[1]]
  write(tmp, file = here("data", "files", paste0(instrNumber, "_cerficiate.html")))
  
  # return to search results
  message("returning to search results")
  remote_driver$findElement(using = "id", value = "btnReturn")$clickElement()
  Sys.sleep(2)
}


# Closing out -------------------------------------------------------------


#create filename based on input dates
filename <-
  paste0(
    "syr-business-certificates_",
    as.Date(c(fromDate[[1]]), format = "%m%d%Y"),
    "_",
    as.Date(c(thruDate[[1]]), format = "%m%d%Y",),
    ".csv"
  )

write.csv(businessInfo, here("data", filename))

# close the browser
driver$server$stop()

# driver$server$log()
