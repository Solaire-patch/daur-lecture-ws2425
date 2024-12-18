#___________________________________________________________________________####
#   Data Preparation                                                        ####

# This script prepares an analysis-ready data set from the provided raw ENOE
# data set

library(tidyverse)

files <- list(
  input = list(
    enoe = "data/raw/enoe.csv",
    fence_dir = "data/raw/fence_construction/"
  ),
  output = list(
    estimation_sample = "data/processed/estimation_sample.csv"
  )
)


#___________________________________________________________________________####
#   Importing                                                               ####

df_enoe <- files$input$enoe %>% 
  read_csv(
    skip = 3,
    na = c("", "NA", "N/A"),
    show_col_types = F
  )

df_fence_constr <- files$input$fence_dir %>%
  list.files() %>% 
  set_names(nm = str_extract(., "[0-9]{4,4}")) %>% 
  map_df(
    ~ read_csv(str_c(files$input$fence_dir, .), show_col_types = F),
    .id = "year"
  )


#___________________________________________________________________________####
#   Data Cleaning: Missings in Fence Construction                           ####

# The fence construction data contains several missing values. A conservative
# assumption would be to assume that a missing value in a quarter would take
# on the previous quarter's value. For this, the data needs to be lengthened
# first.

df_fence_constr_proc <- df_fence_constr %>%
  pivot_longer(starts_with("Q"), names_to = "quarter", values_to = "fence") %>% 
  mutate(
    quarter = as.numeric(str_remove(quarter, "Q")),
    year = as.numeric(year)
  ) %>% 
  # Remove quarters outside data range (Q3 2003 until Q3 2013) and missing
  # municipality observations
  filter(
    !(year == 2003 & quarter %in% c(1, 2)),
    !(year == 2013 & quarter == 4),
    !is.na(municipality)
  ) %>% 
  # Create period column, where 1 refers to the third quarter of 2003
  mutate(period = ((year - 2003) + (quarter - 1) / 4) * 4 - 1) %>% 
  # Replace missings with previous value within municipalities
  arrange(municipality, period) %>% 
  mutate(
    fence = {
      this_fence <- fence
      
      # Replace missings in starting period with 0
      if (is.na(this_fence[1])) {
        this_fence[1] <- 0
      }
      
      # Loop through the fence vector to assign previous values if the dummy is
      # missing
      for (i in 2:length(fence)) {
        if (is.na(fence[i])) {
          this_fence[i] <- this_fence[i - 1]
        }
      }
      
      this_fence
    },
    # Create indicator for treatment exposure
    treated = ifelse(any(fence == 1), 1, 0),
    .by = municipality
  ) %>% 
  # Keep only relevant information
  select(municipality, period, treated, fence)


#___________________________________________________________________________####
#   Data Cleaning: ENOE Survey                                              ####

df_enoe_proc <- df_enoe %>% 
  # Choose variables relevant for identification and process them for empirical
  # evaluation
  select(
    id, municipality, migrate, year, quarter, age, sex, marital_status, educ
  ) %>% 
  mutate(
    # Create period variable as in the fence construction data
    quarter = as.numeric(str_remove(quarter, "Q")),
    year = as.numeric(year),
    period = ((year - 2003) + (quarter - 1) / 4) * 4 - 1,
    # Process control variables for empirical implementation; Note: missings
    # are kept "as-is"
    migrate = if_else(migrate == "Yes", 1, 0, missing = NA),
    female = if_else(sex == "Female", 1, 0, missing = NA),
    married = if_else(marital_status == "Married", 1, 0, missing = NA)
  ) %>% 
  select(id, migrate, municipality, period, age, educ, female, married) %>% 
  # Remove observations for which either the outcome, municipality or period is
  # missing as these are not usable in the analysis; also remove missings in
  # socio-economic characteristics used in estimation
  filter(
    if_all(
      c(migrate, municipality, period, age, educ, female, married), 
      ~ !is.na(.)
    )
  )


#___________________________________________________________________________####
#   Join and Process Treatment Information                                  ####

# Join both data sets to create a main sample used for the analysis. To inspect
# parallel trends, a relative period is created, i.e. periods until treatment
# exposure, where -1 refers to the last pre-treatment period. This is necessary
# due to the staggered treatment exposure.

# See section on staggered DiD in the `fixest` documentation:
# https://lrberge.github.io/fixest/articles/fixest_walkthrough.html

# Create data set with treatment periods
df_treatment_periods <- df_fence_constr_proc %>% 
  filter(fence == 1) %>% 
  filter(period == min(period), .by = municipality) %>% 
  select(municipality, treat_period = period)

df_treat_proc <- df_fence_constr_proc %>% 
  left_join(df_treatment_periods, by = "municipality") %>% 
  mutate(
    # Set treatment period arbitrarily high for the control group
    treat_period = if_else(treated == 0, 1000, treat_period), 
    relative_period = if_else(
      treated == 0,
      -1000, # Set time to treatment arbitrarily high for the control group
      period - treat_period
    )
  )

df_main <- df_enoe_proc %>% 
  left_join(df_treat_proc, by = c("period", "municipality"))


#___________________________________________________________________________####
#   Export                                                                  ####

df_main %>% 
  write_csv(files$output$estimation_sample)
