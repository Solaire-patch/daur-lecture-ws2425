#___________________________________________________________________________####
#   Data Importing                                                          ####

library(tidyverse)
library(readxl)

files <- list(
  input = list(
    enoe_xlsx = "data/raw/enoe/enoe.xlsx",
    enoe_csv = "data/raw/enoe/enoe.csv"
  ),
  output = list(
    csv = "data/raw/enoe.csv",
    xlsx = "data/raw/enoe.xlsx"
  )
)


#___________________________________________________________________________####
#   Importing .xlsx                                                         ####

# How can we check which sheets are in an excel file?
?excel_sheets

files$input$enoe_xlsx %>% 
  excel_sheets()

# Read in the enoe file
df_enoe_xlsx <- files$input$enoe_xlsx %>% 
  read_excel(
    skip = 3,
    na = c("", "NA", "N/A")
  )

df_enoe_xlsx

# All columns are interpreted as values. Based on our knowledge of the data
# set, that should not be the case. For all numeric columns, we want to check
# unique values (-> set na argument as vector above)
df_enoe_xlsx$fence %>% 
  table()


#___________________________________________________________________________####
#   Importing .csv                                                          ####

df_enoe_csv <- files$input$enoe_csv %>% 
  read_csv(
    skip = 3,
    na = c("", "NA", "N/A")
  )


#___________________________________________________________________________####
#   data.frame vs. tibble                                                   ####

df_example <- data.frame(
  id = 1:50,
  educ = runif(50, 9, 13),
  income = rnorm(50, mean = 50000, sd = 5000)
)

# Returns `NULL`
df_example$gender


tbl_example <- tibble(
  id = 1:50,
  educ = runif(50, 9, 13),
  income = rnorm(50, mean = 50000, sd = 5000)
)

# Returns `NULL` but also warns us that the column does not exist
tbl_example$gender


#___________________________________________________________________________####
#   Exporting .csv                                                          ####

df_enoe_csv %>% 
  write_csv(files$output$csv)


#___________________________________________________________________________####
#   Export .xlsx?                                                           ####

?readxl # -> no exporting function...

# install.packages("openxlsx")
openxlsx::write.xlsx(
  df_enoe_xlsx,
  files$output$xlsx
)

