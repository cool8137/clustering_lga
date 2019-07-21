
library(tidyverse)
library(rvest)


# SCRAP TO FILE FUNCTION ------------------------

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
# Scrap and download all NSW LGA files

# read the list of NSW LGA
lga_codes <- read_csv("./INPUTS/LGA_2016_NSW_FILTERED.csv")

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

  
