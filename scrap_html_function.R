# Author: Prashidha Kharel

# This function extracts table information from the downloaded ABS QuickStats webpages
# It then returns a row of extracted information

scrap_html <- function(doc) {
  # Inputs:
  # doc - path to the html file
  
  # Outputs:
  # the_df - one row of data as required.
  
  check_table <- function(doc, which, checkname){
    # extract table and check name in first column heading
    the_tab <- tryCatch(htmltab(doc, which = which), error = function(x) NA)
    if (is.data.frame(the_tab)){
      if (grepl(checkname, names(the_tab)[1])) {
        return(the_tab)
      } else {
        return(NA)
      }
    } else {
      return(NA)
    }
    
  }
  
  get_table_info <- function(the_tab, row_names, value_col = 3){
    # selects the values for given row_names in the table
    if (!is.data.frame(the_tab)){
      return(rep(NA, length(row_names)))
    } else {
      return(the_tab[match(row_names, the_tab[, 1]), value_col])
    }
  }
  
  table_names <- c()
  table_values <- c()
  
  # Gender
  this_tab <- check_table(doc, which = 4, checkname = "People")
  this_names <- c("Male","Female")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  # Age [combine age groups to form 10 years wide age brackets]
  this_tab <- check_table(doc, which = 5, checkname = "Age")
  age_names <- c("0-4 years", "5-9 years", "10-14 years",
                 "15-19 years", "20-24 years", "25-29 years", "30-34 years",
                 "35-39 years", "40-44 years", "45-49 years", "50-54 years",
                 "55-59 years", "60-64 years", "65-69 years", "70-74 years",
                 "75-79 years", "80-84 years", "85 years and over")
  age_values <- as.numeric(get_table_info(this_tab, age_names))
  median_age <- as.numeric(get_table_info(this_tab, "Median age", 2))
  
  this_names <- c("Median Age", "0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "85+")
  this_values <- c(median_age, sum(age_values[1:3]), 
                   sum(age_values[4:5]),
                   sum(age_values[6:7]),
                   sum(age_values[8:9]),
                   sum(age_values[10:11]),
                   sum(age_values[12:13]),
                   sum(age_values[14:15]),
                   sum(age_values[16:17]),
                   sum(age_values[18]))
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  # Marriage - 6th table
  this_tab <- check_table(doc, which = 6, checkname = "Registered marital status")
  this_names <- c("Married")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  # Level of highest educational attainment - 9th table
  this_tab <- check_table(doc, which = 9, checkname = "Level of highest educational attainment")
  this_names <- c("Bachelor Degree level and above", "Advanced Diploma and Diploma level")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  #   Country of birth - 9th table
  this_tab <- check_table(doc, which = 11, checkname = "Country of birth")
  this_names <- c("Australia")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  #   Religious affiliation - 15th table
  this_tab <- check_table(doc, which = 15, checkname = "Religious affiliation")
  this_names <- c("No Religion, so described", "Catholic")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  #   Language - 16th table
  this_tab <- check_table(doc, which = 16, checkname = "Language")
  this_names <- c("English only spoken at home")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  #   Employment - 17th table
  this_tab <- check_table(doc, which = 17, checkname = "Employment")
  this_names <- c("Worked full-time", "Worked part-time")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  #   Occupation - 19th table
  this_tab <- check_table(doc, which = 19, checkname = "Occupation")
  this_names <- c("Professionals", "Clerical and Administrative Workers", "Technicians and Trades Workers")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  #   Median weekly incomes - 21th table
  this_tab <- check_table(doc, which = 21, checkname = "Median weekly incomes")
  this_names <- c("Personal", "Family")
  this_values <- get_table_info(this_tab, this_names, 2)
  this_names <- c("PersonalMedianIncomeWeekly", "FamilyMedianIncomeWeekly")
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  #   Travel to work - 22th table
  this_tab <- check_table(doc, which = 22, checkname = "Travel to work")
  this_names <- c("People who travelled to work by public transport", "People who travelled to work by car as driver or passenger")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  #   Family composition - 25th table
  this_tab <- check_table(doc, which = 25, checkname = "Family composition")
  this_names <- c("Couple family without children", "Couple family with children")
  this_values <- get_table_info(this_tab, this_names)
  table_names <- c(table_names, this_names)
  table_values <- c(table_values, this_values)
  
  the_df <- as.data.frame(t(table_values))
  names(the_df) <- table_names
  
  return(the_df)
  
}
