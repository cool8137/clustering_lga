# --------------
# Name: scrap_script.R
# Author: Prashidha Kharel
# Description: This script downloads the ABS quick stat webpage for each LGAs and saves it to "HTML_ARCHIVE" folder
# It then scrapes each of the webpages to extract important summary statistics for each LGA
# Finally the aggregated data is saved as csv.


library(tidyverse)
library(rvest)
library(htmltab)


# FUNCTIONS ------------------------

# Downloads the url using phantomjs and saves it to Archive folder
download_url <- function(url, filepath = './HTML_ARCHIVE/1.html') {
  writeLines(sprintf("var page = require('webpage').create();
  page.open('%s', function () {
      console.log(page.content); //page source
      phantom.exit();
  });", url), con="scrape.js")
  
  system(paste0("./phantomjs scrape.js > ", filepath), intern = T)
  
  if (file.size(filepath) < 150000) {
    Sys.sleep(1+runif(1))
    system(paste0("./phantomjs scrape.js > ", filepath), intern = T)
  }
  if (file.size(filepath) < 150000) {
    Sys.sleep(1+runif(1))
    system(paste0("./phantomjs scrape.js > ", filepath), intern = T)
  }
}

# Load the scrap_html function
# It extracts table information from the html file
# Inputs:
#   doc - path to the html file
# Outputs:
#   the_df - one row of data as required.
source("./scrap_html_function.R")

# DOWNLOAD FILES and Extract information ----------------------
# Scrap and download all LGA webpages to HTML_ARCHIVE

# read the list of LGA
lga_codes <- read_csv("./CSV/LGA_2016_AUS_FILTERED.csv")

# ABS's quickstat url without the LGA code
url_prefex <- "https://quickstats.censusdata.abs.gov.au/census_services/getproduct/census/2016/quickstat/LGA"

# A matrix to save extracted information for each lgas
lga_mat <- c()

# Loop through each lga and download the abs quickstats webpage
for (i_lga in 1:nrow(lga_codes)){
  # Extract i_lga row information
  lga_name = lga_codes$LGA_NAME_2016[i_lga]
  lga_code = lga_codes$LGA_CODE_2016[i_lga]
  lga_url = paste0(url_prefex, lga_code, "?opendocument")
  lga_filepath = paste0("./HTML_ARCHIVE/", lga_code, ".html")
  
  # download website if file doesn't exist
  if (!file.exists(lga_filepath)){
    download_url(lga_url, lga_filepath)
  }
  
  # Extract each row of information
  lga_info <- scrap_html(lga_filepath)
  row.names(lga_info) <- lga_name
  
  # Add info to main data frame
  lga_mat <- rbind(lga_mat, lga_info)
  
  if (i_lga %% 50 == 0) {
    print(paste0(i_lga, " files downloaded."))
  }
}

# Add row names to column
lga_mat <- rownames_to_column(lga_mat, "LGA Name")

# Save the lga matrix as csv
write_csv(lga_mat, "./CSV/LGA_STAT_TABLE.csv")
