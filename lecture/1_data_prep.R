#___________________________________________________________________________####
# Clean the Happiness Data                                                  ####

files <- list(
  input = list(
    happiness = "data/raw/happiness.csv"
  ),
  output = list(
    happiness = "data/processed/happiness_cleaned.csv"
  )
)


# Read in the happiness data
df_happiness <- read.csv(files$input$happiness)

# Select columns needed for the model
select_cols_vec <- c("vhappy", "income", "educ", "owngun", "female")
df_happiness_proc <- df_happiness[,select_cols_vec]

# Create identifier column for individuals
df_happiness_proc$id <- 1:nrow(df_happiness_proc)

# Recode owngun variable to 0/1
df_clean$owngun <- ifelse(df_clean$owngun == "yes", 1, 0)
summary(df_clean)

# Remove all rows containing NAs for numerical columns
df_clean <- df_clean[
  !is.na(df_clean$educ) & 
    !is.na(df_clean$owngun) & 
    !is.na(df_clean$female) & 
    !is.na(df_clean$vhappy),
]

# Recode missings in income to "Missing"
df_clean$income[is.na(df_clean$income)] <- "Missing"

# Export cleaned data set
write.csv(df_clean, files$output$happiness)
