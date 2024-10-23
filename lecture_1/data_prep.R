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

# Remove all rows containing NAs
df_clean <- na.omit(df_happiness_proc)
summary(df_clean)

# Recode owngun variable to 0/1
df_clean$owngun <- ifelse(df_clean$owngun == "yes", 1, 0)
summary(df_clean)

# Export cleaned data set
write.csv(df_clean, files$output$happiness)
