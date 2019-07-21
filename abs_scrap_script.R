# --------------
# Name: scrap_script.R
# Author: Prashidha Kharel
# Description: This script downloads the ABS quick stat webpage for each LGAs and saves it to "HTML_ARCHIVE" folder
# It then scrapes each of the webpages to extract important summary statistics for each LGA
# Finally the aggregated data is saved as csv.


library(tidyverse)
library(rvest)


# FUNCTIONS ------------------------

# Scraps the url using phantomjs and saves it to Archive folder
scrap_to_file <- function(url, filepath = './HTML_ARCHIVE/1.html') {
  writeLines(sprintf("var page = require('webpage').create();
  page.open('%s', function () {
      console.log(page.content); //page source
      phantom.exit();
  });", url), con="scrape.js")
  
  system(paste0("./phantomjs scrape.js > ", filepath), intern = T)
}

# DOWNLOAD FILES ----------------------
# Scrap and download all LGA webpages

# read the list of LGA
lga_codes <- read_csv("./CSV/LGA_2016_AUS_FILTERED.csv")

# ABS's quickstat url without the LGA code
url_prefex <- "https://quickstats.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/LGA"

# Loop through each lga and download the abs quickstats webpage
for (i_lga in 1:nrow(lga_codes)){
  # Extract i_lga row information
  lga_name = lga_codes$LGA_NAME_2016[i_lga]
  lga_code = lga_codes$LGA_CODE_2016[i_lga]
  lga_url = paste0(url_prefex, lga_code)
  lga_filepath = paste0("./HTML_ARCHIVE/", lga_code, ".html")
  
  # download website if file doesn't exist
  if (!file.exists(lga_filepath)){
    scrap_to_file(lga_url, lga_filepath)
  }
  
  if (i_lga %% 50 == 0) {
    print(paste0(i_lga, " files downloaded."))
  }
}

  
