#___________________________________________________________________________####
#   Lecture 2: Functional Programming                                       ####

# Define function to standardize numeric vectors
standardize <- function(x) {
  x_mean <- mean(x)
  x_sd <- sd(x)
  
  (x - x_mean) / x_sd
}


# Create a data vector with 50 random draws from a normal distribution with mean
# 5 and std. dev. 10 (columns x, y, z) and a random sample of letters. In the
# following, we want to create standardized columns for x, y, and z, i.e.
# center around mean 0 and with a std. dev. of 1.
df <- data.frame(
  x = rnorm(50, 5, 10),
  y = rnorm(50, 5, 10),
  z = rnorm(50, 5, 10),
  char = sample(letters, 50, replace = T)
)


# Option 1: Use a foor-loop to create standardized vectors for x, y, and z (new 
# column names are std_x_for, std_y_for, std_z_for)
for (col in c("x", "y", "z")) {
  message("Current increment: ", col)
  
  # Define new column names for standardized columns
  new_col_name <- paste0("std_", col, "_for")
  
  df[[new_col_name]] <- standardize(df[[col]])
}


# Option 2: Use the lapply function to create standardized vectors for x, y, and 
# z (new column names are std_x_apply, std_y_apply, std_z_apply)
df[,paste0("std_", c("x", "y", "z"), "_apply")] <- lapply(
  df[,c("x", "y", "z")], 
  standardize
)


#___________________________________________________________________________####
#   Make code more readable: Using the Pipe-Operator ( %>% )                ####

#install.packages("magrittr")
library(magrittr)

# Task: Standardize and take the square of x, y, and z using the pipe operator.
# Assign the results back to the data frame.

# Pipe operator keyboard shortcut: ctrl + shift + M
df[,paste0("sq_std_", c("x", "y", "z"))] <- df[,c("x", "y", "z")] %>% 
  lapply(standardize) %>% 
  lapply(function(x) x^2)
