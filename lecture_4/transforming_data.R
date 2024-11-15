#___________________________________________________________________________####
#   Transforming Data                                                       ####

# This script will read in the raw data set used in Feigenberg (2020) and to 
# transform it to our needs. Feigenberg (2020) explores the effect the fence
# construction at the US-Mexican border had on the migration decision of
# Mexican individuals. 

library(tidyverse)

files <- list(
  input = list(
    enoe = "data/raw/enoe/enoe.csv"
  )
)


#___________________________________________________________________________####
#   Importing                                                               ####

# Import raw ENOE data set (CSV) containing individual-level information on
# potential migrants
df_enoe <- files$input$enoe %>% 
  read_csv(
    skip = 3, # First rows in the CSV are empty / contain a header
    na = c("", "NA", "N/A") # Missings were inconsistenly coded
  )


#___________________________________________________________________________####
#   Data Preparation                                                        ####

# For the data preparation, we first take a look at the data set so that we know
# what data is stored and how the data is structured
summary(df_enoe)


# For some columns in the data frame, it makes sense to check unique values,
# e.g. the marital status of individuals. We can do this using the `table()`
# command
table(df_enoe$marital_status)


# We can see that there are several characteristics in the data set that need
# some manipulation so that we can work with the data:

# - `period`: Information on the time dimension in the panel is stored in two
#   separate columns (`year` and `quarter`). These have to be combined.

# - `marital_status`/`empl_status`: Both variables store categorical information
#   that we may want to recode. First, any `NA` values in the two columns could
#   be recoded to the category "Missing" since that information alone could be
#   an interesting factor level that allows us to retain the observations (as
#   opposed to removing them entirely). Second, in many cases we would only want
#   to know whether individuals are married/employed which is why we should
#   encode corresponding binary indicators (aka dummy variables).

# - `sex`: Since this a variable with only two levels, we can recode it to a
#   dummy variable `female` that indicates whether an individual is female or
#   not.

# - `age`/`educ`/`income`: An easy way to model a nonlinear relationship between
#   some continuous variable x and an outcome y is to reduce the continuous
#   variable to some categorical variable. This categorization into brackets of
#   x should be based off either theoretical considerations or distributive 
#   exploration (e.g. visual inspection of relation between outcome and 
#   covariate). If you see some stepwise relationship between x and y, binning
#   x into categories could be a good option. We will choose these kind of 
#   arbitrarily here but could always revise them later when inspecting the
#   data more thoroughly. For this, let us take a look at the distribution of
#   `educ` (i.e. years of schooling) using base R's `hist()`-function:
hist(df_enoe$educ)

#   We can see clear spikes in years of schooling at 6, 9, 12, 17, and 19 years. 
#   Presumably, these are primary school, middle school, high school, college,
#   and university graduates. It would be reasonable to bin years of schooling
#   accordingly.


# For the data manipulation, we use the `mutate()`-command from the `dplyr`
# package. This function allows us to specify new variables (or overwrite
# existing ones) by providing name-value-pairs. To not overwrite the raw
# data set, we assign the mutated data frame to a new object in the environment.

df_enoe_proc <- df_enoe %>% 
  mutate(
    # Create new period columns in the data set using two options:
    # (1) Concatenate the year and quarter columns to create a new categorical
    #     column that uniquely identifies a period (i.e. quarter of a year)
    # (2) Take the quarter value (i.e. "3" from "Q3") to create a continuous
    #     period column, e.g. 2004.75 for the third quarter in 2004. This uses
    #     the `str_sub` command from `stringr` to get the second position of
    #     the `quarter` string. This character is then concerted to a numeric
    period = str_c(year, "-", quarter),
    period_num = year + as.numeric(str_sub(quarter, 2, 2))/4,
    # Convert missings in categorical columns to the category "Missing" using
    # the `across` command. The lambda expression here means that if a value in
    # the column specified by the first argument's vector is missing, set it
    # to "Missing", else keep the current value so to not overwrite any 
    # non-missing existing data
    across(c(marital_status, empl_status), ~ ifelse(is.na(.), "Missing", .)),
    # Recode these status variables in new columns to "Yes"/"No" dummies. We may
    # want to do this only for non-"Missing" categories. In this case, we would
    # use a `case_when` command similar to the binning of continuous variables
    # below. For the employment status, we check whether the observation's value
    # is found in character vector. We could also have specified
    # `empl_status == "Full-time" | empl_status == "Part-time"` instead.
    married = ifelse(marital_status == "Married", "Yes", "No"),
    employed = ifelse(empl_status %in% c("Full-time", "Part-time"), "Yes", "No"),
    female = ifelse(sex == "Female", "Yes", "No"),
    # Since dummy variables are best stored as 0/1 values (due to easier 
    # computations), let's recode all "Yes"/"No" variables accordingly. The
    # `.names` argument let's us specify new column names for the columns 
    # created in `across`. In our case, the suffix "_num" is appended to the
    # column name.
    across(
      all_of(c("married", "employed", "migrate", "female")),
      ~ ifelse(. == "Yes", 1, 0),
      .names = "{.col}_num"
    ),
    # Bin continuous variables into brackets using the `case_when` command
    # that let's you write nested if-else statements more concisely
    age_grouped = case_when(
      age < 20 ~ "under 20",
      age >= 20 & age < 30 ~ "20-29",
      age >= 30 & age < 40 ~ "30-39",
      age >= 40 & age < 50 ~ "40-49",
      age >= 50 & age < 60 ~ "50-59",
      age >= 60 ~ "over 59"
    ),
    income_grouped = case_when(
      income == 0 ~ "0",
      income < 1000 ~ "1-999",
      income >= 1000 & income < 2000 ~ "1000-1999",
      income >= 2000 & income < 3000 ~ "2000-2999",
      income >= 3000 & income < 4000 ~ "3000-3999",
      income >= 4000 & income < 5000 ~ "4000-4999",
      income >= 5000 & income < 6000 ~ "5000-5999",
      income >= 6000 & income < 7000 ~ "6000-6999",
      income >= 7000 & income < 8000 ~ "7000-7999",
      income >= 8000 & income < 9000 ~ "8000-9999",
      income >= 9000 & income < 10000 ~ "9000-9999",
      income >= 10000 ~ "above 9999"
    ),
    educ_grouped = case_when(
      educ < 6 ~ "under 6",
      educ >= 6 & educ < 9 ~ "6-9",
      educ >= 9 & educ < 12 ~ "9-12",
      educ >= 12 & educ < 17 ~ "12-17",
      educ >= 17 & educ < 19 ~ "17-19",
      educ >= 19 ~ "over 18"
    )
  )


# Further data manipulation: Let us also create a new variable based on the 
# income that let's us determine whether an individual is above or below the 
# poverty line using 60% of the median wage as a threshold. If we do this
# computation on the uncleaned data set, however, our threshold may suffer
# from data quality issues, e.g. strong outliers that are likely errors in
# data collection. Before continuing with our data preparation, we should 
# therefore first look at the quality of our data


##__________________________________________________________________________####
##  Filtering Observations                                                  ####

# In this step, we will take a look at quick look at how reasonable our data
# is so that we can identify data entries with errors. Since our data is rather
# simple, we will focus our attention on the income.

# Check the distribution of income visually. The below histogram of absolute
# income values already tells us that there are large outliers at the head of
# the distribution. 
hist(df_enoe_proc$income)

# We should explore these outliers in more depth. A useful way in doing so, is
# to look at the quantiles on the right side of the distribution, e.g. quantiles
# in a sequence from 0.9 to 1 in increments of 1 percentage points. Note that
# we have to remove missings for this computation (see argument `na.rm`).
quantile(df_enoe_proc$income, seq(0.9, 1, 0.01), na.rm = T)

# Our quantile check tells us that there is a jump in income from 21500 in the
# 99% quantile to 749999.80 in the 100% quantile (i.e. the maximum value). While
# there could be someone in the data earning such a high wage, this could also
# just be a mistake in the data collection. Also, individuals with such a high
# wage probably do not to the portion of the population we are interested in
# studying. Due to their very high value compared to the general population, 
# their influence in a regression would be overstated. It would therefore be 
# reasonable to only keep observations within the 99% quantile.

# Another variable we will need to check is the municipality identifier. Since
# the treatment is assigned on municipality level, we can not work with 
# individuals for which the information on their municipality of residence is
# missing. Similar reasons apply to the observation period.

# `filter` takes one or more expressions that return a logical and keeps only
# observations from the data set for which these expressions evaluate to `TRUE`.
# If several expressions are given, all are connected by an AND statement.

df_enoe_proc %>% # Note we do not overwrite the data set here (we do that below)
  filter(
    income <= quantile(income, 0.99, na.rm = T),
    if_all(c(municipality, year, quarter), ~ !is.na(.))
  )

# Note: that when filtering income based on the 99% quantile we also remove
# observations with missing income. If you want to keep those observations,
# you will have to add an OR statement to the income filter condition.

df_enoe_proc <- df_enoe_proc %>% 
  filter(
    income <= quantile(income, 0.99, na.rm = T) | is.na(income),
    if_all(c(municipality, year, quarter), ~ !is.na(.))
  )


# Another note: `if_all` gets you a logical vector per column on which it is
# applied (similar to how `across` works). These are then connected by an AND
# statement. If instead you want an OR statement here, i.e. keep observations
# for which any of these columns is non-missing, use `if_any` instead.


##__________________________________________________________________________####
##  Determine Poverty                                                       ####

# Now that we have removed unrealistic income values (and observations that will
# not enter our model due to missings in relevant identifiers), we can
# create our poverty indicator. We keep our naming convention of indicating
# dummy variables by the suffix "_num" for consistency with our code above.

# Instead of `ifelse` we will use `dplyr`'s `if_else` function here. In case
# you have wondered "What happens if my condition evaluates to NA?", with
# `if_else` you can set a self-chosen value for that case using the `missing`
# argument. It doesn't really matter here because base R's `ifelse` already
# resolves those to NA but it is nice to know, I guess.

df_enoe_proc %>% 
  mutate(
    poverty_num = if_else(
      income < median(income, na.rm = T), 
      1, 
      0,
      missing = NA
    )
  )


remove observations above the
quantile(df_enoe_proc$income, seq(0.999, 1, 0.0001), na.rm = T)








# This already looks 
hist(log(df_enoe_proc$income))




df_enoe_proc %>% names()






# Concatenate year and quarter to create identifier for the period of
# observation
period = str_c(year, "-", quarter),
# Numeric period identifier (perhaps for calculations)
period_num = year + as.numeric(str_sub(quarter, 2, 2))/4,
# Binary indicators
poverty = ifelse(income < .6 * median(income, na.rm = T), "Yes", "No"),
empl = ifelse(empl_status %in% c("Full-time", "Part-time"), "Yes", "No"),
married = ifelse(marital_status == "Married", "Yes", "No"),
across(
  all_of(c("poverty", "married", "empl", "migrate")),
  ~ ifelse(. == "Yes", 1, 0),
  .names = "{.col}_num"
),
.before = 2



#___________________________________________________________________________####
#   Mutating Columns                                                        ####

df_enoe_proc <- df_enoe %>% 
  mutate(
    age_grouped = case_when(
      age < 20 ~ "under 20",
      age >= 20 & age < 30 ~ "20-29",
      age >= 30 & age < 40 ~ "30-39",
      age >= 40 & age < 50 ~ "40-49",
      age >= 50 & age < 60 ~ "50-59",
      age >= 60 ~ "over 59"
    ),
    income_grouped = case_when(
      income == 0 ~ "0",
      income < 1000 ~ "1-999",
      income >= 1000 & income < 2000 ~ "1000-1999",
      income >= 2000 & income < 3000 ~ "2000-2999",
      income >= 3000 & income < 4000 ~ "3000-3999",
      income >= 4000 & income < 5000 ~ "4000-4999",
      income >= 5000 & income < 6000 ~ "5000-5999",
      income >= 6000 & income < 7000 ~ "6000-6999",
      income >= 7000 & income < 8000 ~ "7000-7999",
      income >= 8000 & income < 9000 ~ "8000-9999",
      income >= 9000 & income < 10000 ~ "9000-9999",
      income >= 10000 ~ "above 9999"
    ),
    # Concatenate year and quarter to create identifier for the period of
    # observation
    period = str_c(year, "-", quarter),
    # Numeric period identifier (perhaps for calculations)
    period_num = year + as.numeric(str_sub(quarter, 2, 2))/4,
    # Binary indicators
    poverty = ifelse(income < .6 * median(income, na.rm = T), "Yes", "No"),
    empl = ifelse(empl_status %in% c("Full-time", "Part-time"), "Yes", "No"),
    married = ifelse(marital_status == "Married", "Yes", "No"),
    across(
      all_of(c("poverty", "married", "empl", "migrate")),
      ~ ifelse(. == "Yes", 1, 0),
      .names = "{.col}_num"
    ),
    .before = 2
  )


#___________________________________________________________________________####
#   Filtering Observations                                                  ####

df_enoe_proc <- df_enoe_proc %>% 
  filter(!is.na(fence), !is.na(municipality))


#___________________________________________________________________________####
#   Define Treatment Status                                                 ####

# Create data set with treatment status of municipalities
df_municipality_treated <- df_enoe_proc %>% 
  select(municipality, fence) %>% 
  filter(!is.na(municipality)) %>% 
  distinct() %>%
  group_by(municipality) %>% 
  mutate(
    treatment = ifelse(any(fence) == 1, 1, 0),
    .keep = "unused"
  ) %>% 
  distinct()

df_enoe_proc <- df_enoe_proc %>% 
  left_join(
    df_municipality_treated, 
    by = "municipality",
    relationship = "many-to-one"
  )


#___________________________________________________________________________####
#   Summarizing Variables                                                   ####

df_summary <- df_enoe_proc %>% 
  summarize(
    across(
      c(married_num, empl_num, migrate_num),
      ~ mean(., na.rm = T),
      .names = "{.col}__mean"
    ),
    across(
      c(income, age),
      list(mean = ~ mean(., na.rm = T), sd = ~ sd(., na.rm = T)),
      .names = "{.col}__{.fn}"
    ),
    .by = treatment
  )

df_summary %>% 
  pivot_longer(
    -treatment,
    names_to = c("variable", "statistic"),
    values_to = "value",
    names_sep = "__"
  ) %>% 
  pivot_wider(
    names_from = c("statistic", "treatment"),
    values_from = "value"
  )









