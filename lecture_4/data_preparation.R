#___________________________________________________________________________####
#   Transforming Data                                                       ####

# This script will read in the raw data set used in Feigenberg (2020) and to 
# transform it to our needs. Feigenberg (2020) explores the effect the fence
# construction at the US-Mexican border had on the migration decision of
# Mexican individuals. 

library(tidyverse)

files <- list(
  input = list(
    enoe = "data/raw/enoe/enoe.csv",
    fence_construction = "data/raw/fence_construction/csv/"
  ),
  output = list(
    enoe = "data/processed/enoe.csv"
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
    ),
    # Replace NA in columns that end on "_grouped" (i.e. grouped income, 
    # education, and age) with "Missing"
    across(ends_with("_grouped"), ~ ifelse(is.na(.), "Missing", .))
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

# Note that when filtering income based on the 99% quantile we also remove
# observations with missing income. If you want to keep those observations, e.g.
# when using income brackets with category "Missing", you will have to add an 
# OR statement to the income filter condition:

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
# resolves those to NA but it is nice to know that the option for default
# values exists.

df_enoe_proc <- df_enoe_proc %>% 
  mutate(
    poverty_num = if_else(
      income < median(income, na.rm = T), 
      1, 
      0,
      missing = NA
    )
  )


#___________________________________________________________________________####
#   Joining Data                                                            ####

# We already have the `fence` variable in our data set but let's remove that
# column to join information on fence construction using the data sets in the
# folder "data/raw/fence_construction" of the slides repository
# (see https://github.com/empwifo/dauR-slides)

df_enoe_proc <- df_enoe_proc %>% 
  select(-fence) # Removes the column (see below for more on `select`)


##__________________________________________________________________________####
##  Preparation of Fence Construction Data                                  ####

# There are several csv-files in the fence_construction-folder. Each file
# contains for a specific year the information whether fence construction 
# started in Q1, Q2, Q3, or Q4. All files are structured similarly, so using
# what we learning in the lecture on programming, we can read all files in
# using the `map()` command. For this, we first list all csv files in the
# fence_construction-folder. Note that we included the path to the csv files
# in the list at the top of the script.

df_fence_construction <- files$input$fence_construction %>% 
  list.files() %>% 
  # Sets names for the list; names are the file names with ".csv" removed 
  # (dots have to be escaped using double backslashes)
  set_names(nm = str_remove(., "\\.csv")) %>% 
  map(
    ~ read_csv(
      # Bind the path to the csv file in front of the file names (the dot 
      # represents the file name / list entry on which the function is applied)
      str_c(files$input$fence_construction, .)
    )
  )


# We now have a list of data frames with each entry corresponding to a certain
# year. This data should of course be merged into a single data frame. We do so
# using the `bind_rows` command that can be applied to multiple data frames but
# also a list of data frames. It appends the data frames row-wise. Using the
# `.id` argument we can let R create a new column for the list names. That way
# we also get the information on which year a row corresponds to.

df_fence_construction <- df_fence_construction %>% 
  bind_rows(.id = "year")


# The data frame contains several issues. Let's start with the first: `year`
# is not a numeric, so let's convert its data type in a `mutate` call

df_fence_construction <- df_fence_construction %>% 
  mutate(year = as.numeric(year))


# Next problem: The data is not stored in a format that is very useful to us
# because it is too long, i.e. each row corresponds to a year but the data we
# want to merge it to has as rows observations from a quarter. Let's bring the
# data into a long format using `pivot_longer`. For this, we specify which
# columns we want to lengthen, in what newly created column the former column 
# names should be stored, and what the column in which the values are stored
# should be named

df_fence_construction <- df_fence_construction %>% 
  pivot_longer(starts_with("Q"), names_to = "quarter", values_to = "fence")


# We know that the earliest data entry in the ENOE survey is from Q3 2003 and
# the last is Q2 2013, so let's filter the data accordingly. Let us also
# remove missings in municipality.

df_fence_construction <- df_fence_construction %>% 
  filter(
    !(year == 2003 & quarter %in% c("Q1", "Q2")),
    !(year == 2013 & quarter %in% c("Q3", "Q4")),
    !is.na(municipality)
  )


# When inspecting the data, we see that several quarters have missing 
# observations in our treatment variable. A conservative assumption would be
# that fence construction did not start in these quarters. Let us therefore
# impute their value with their leading quarters value. Note that for this
# computation, we need some grouping because we have to make sure that we only
# do this imputation within municipalities. Also, we need to arrange our data
# first by year and quarter.

df_fence_construction <- df_fence_construction %>% 
  arrange(year, quarter) %>% 
  # Grouping let's us conduct computations within groups of data instead of
  # across all variables. When printing the data set to the console, you can see
  # that the tibble is grouped by municipality which has 23 groups
  group_by(municipality) %>% 
  mutate(
    fence = ifelse(
      is.na(fence),
      lag(fence, default = 0),
      fence
    )
  )


# Note: `lag` gives us the previous value within a vector. Since the first 
# element in a vector does not have previous value, we set it to 0 by `default`.
# If you want to get the next value in a vector, you can use `lead()`.


# You might have noticed that we could have written all these transformations
# in one pipe. For a clean and readable code, let's do that quickly here:

df_fence_construction <- files$input$fence_construction %>% 
  list.files() %>% 
  set_names(nm = str_remove(., "\\.csv")) %>% 
  map(~ read_csv(str_c(files$input$fence_construction, .))) %>% 
  bind_rows(.id = "year") %>% 
  mutate(year = as.numeric(year)) %>% 
  pivot_longer(starts_with("Q"), names_to = "quarter", values_to = "fence") %>% 
  arrange(year, quarter) %>% 
  group_by(municipality) %>% 
  mutate(
    fence = ifelse(
      is.na(fence),
      lag(fence, default = 0),
      fence
    )
  )


# Please note that as a general rule, you should always import your data at the 
# top of the script! I only imported the data here as to not confuse you
# at the top.


##__________________________________________________________________________####
##  Joining Fence Construction with ENOE                                    ####

# We join the fence construction data set with the ENOE data by simply merging
# over columns present in both data sets (in this case year, quarter, and
# municipality). The join is made over column names so if the names in both
# data sets do not match, just rename them to be consistent or take a look
# at the documentation for the `by` argument

df_enoe_proc %>% 
  left_join(df_fence_construction, by = c("year", "quarter", "municipality"))

# We can also include some failsafe checks in here, e.g. by pre-specifying
# the relationship between our data sets. We know that we have several 
# individuals recorded within municipalities and year-quarters, so we know that
# "many" observations in df_enoe_proc have to be joined with "one" observation
# in df_fence_construction, hence `relationship = "many-to-one"`. If this R 
# finds other kind of joins, e.g. "many-to-many" during the matching, an error
# will be thrown so that we know that something's not right.

df_enoe_proc <- df_enoe_proc %>% 
  left_join(
    df_fence_construction, 
    by = c("year", "quarter", "municipality"),
    relationship = "many-to-one"
  )


# We did a left join here because all we need to know is whether observations
# in the ENOE data set were treated. A full join would have extended the ENOE
# data with empty columns in ENOE characteristics for all 
# municipality-year-quarter that are not found in the ENOE data set (and we do
# not want that). An inner join would also not have been appropriate here 
# because observations in the ENOE data set that are not matched with an
# observation in the fence construction data set would have been dropped in the
# join, leaving us no way of checking non-randomness in missing information on
# fence construction.


# Now that we have joined information on whether an individual was treated,
# we can determine which observations are in the treatment and which in the
# control group. Let us define the treatment group as individuals located in
# a municipality that sometime in the observation period was exposed to the
# treatment, i.e. the construction of a border fence. This is pretty simple 
# because all we have to do is create a new column that takes on the value 1 if
# a municipality suffices the condition `fence == 1` at any point in the data.
# We can do this by grouping our observations again, but this time we will make
# use of `mutate()`'s short-hand `.by` argument.

df_enoe_proc <- df_enoe_proc %>% 
  mutate(treatment = if_else(any(fence == 1), 1, 0), .by = municipality)


#___________________________________________________________________________####
#   Summarizing Data                                                        ####

# Usually, we would leave the summarizing of data to be done in a separate
# script, at least if it is for data exploration and not transformation. Since
# this will be rather short, we will do this here but keep in mind that in your
# group project you should treat data preparation and exploration separately.

# When looking at our data, what we are probably most interested in is the
# difference between our treatment and control group. So, for summary statistics
# let us take a look at mean values and standard deviations.

# Using the `summarize()` command we can easily create summary statistics over
# multiple (grouped) columns. In our case, we would like to take a look at
# the proportion of individuals that migrated, were married, employed etc., as
# well as the distribution of income, age, and education.

# Instead of providing a single function in `across`, wen can also provide a
# (named) list of anonymous functions as seen for income and age below.

df_sum_stats <- df_enoe_proc %>% 
  summarize(
    across(
      c(migrate_num, female_num, married_num, employed_num),
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


# This data set is obviously not in the correct format as it is too wide for
# us and therefore too messy. Let's tidy it by creating a data frame with
# each row corresponding to a variable. The columns in the target data frame
# should therefore containing mean/standard deviation for the treatment and 
# control group.

# For the lengthening of our data, we can make use of the separator "__" (double
# underscore) that we specified above to separate the column name and the 
# function name. ".value" in `names_to` refers to `mean` and `sd`, respectively.

df_sum_stats <- df_sum_stats %>% 
  pivot_longer(
    -treatment, # All columns except treatment
    names_to = c("variable", ".value"),
    names_sep = "__"
  )


# We know only have to bring our data into a wider format to store the treatment
# information column-wise. We can do this using `pivot_wider`

df_sum_stats <- df_sum_stats %>% 
  pivot_wider(names_from = treatment, values_from = c(mean, sd))


# Let us quickly re-order the column for better readability

df_sum_stats <- df_sum_stats %>% 
  relocate(variable, mean_1, sd_1, mean_0, sd_0)


# Of course, we could have done all this in a single pipe:

df_sum_stats <- df_enoe_proc %>% 
  summarize(
    across(
      c(migrate_num, female_num, married_num, employed_num),
      ~ mean(., na.rm = T),
      .names = "{.col}__mean"
    ),
    across(
      c(income, age),
      list(mean = ~ mean(., na.rm = T), sd = ~ sd(., na.rm = T)),
      .names = "{.col}__{.fn}"
    ),
    .by = treatment
  ) %>% 
  pivot_longer(
    -treatment, # All columns except treatment
    names_to = c("variable", ".value"),
    names_sep = "__"
  ) %>% 
  pivot_wider(names_from = treatment, values_from = c(mean, sd)) %>% 
  relocate(variable, mean_1, sd_1, mean_0, sd_0)


##__________________________________________________________________________####
##  Simple Extension to Categorical Data                                    ####

# If we want to include categorical data in our summary statistics table,
# we could have first created dummary variables for the categorical data. For
# example, we likely would want to include information on the years of schooling
# in our summary statistics, so let us use `dummy_cols()` from the `fastDummies`
# package before summarizing the data. This function creates dummy variables
# for the specified columns with the corresponding columns names consisting
# of the variable name and the level name (e.g. `educ_grouped_12-17`).

# All we have to do after creating the dummy variables is to add 
# `starts_with("educ_grouped_")` in our `across` command and we also get the 
# portion of individuals in the respective education brackets.

df_enoe_proc %>% 
  fastDummies::dummy_cols("educ_grouped") %>% 
  summarize(
    across(
      c(
        migrate_num, female_num, married_num, employed_num, 
        starts_with("educ_grouped_")
      ),
      ~ mean(., na.rm = T),
      .names = "{.col}__mean"
    ),
    across(
      c(income, age),
      list(mean = ~ mean(., na.rm = T), sd = ~ sd(., na.rm = T)),
      .names = "{.col}__{.fn}"
    ),
    .by = treatment
  ) %>% 
  pivot_longer(
    -treatment, # All columns except treatment
    names_to = c("variable", ".value"),
    names_sep = "__"
  ) %>% 
  pivot_wider(names_from = treatment, values_from = c(mean, sd)) %>% 
  relocate(variable, mean_1, sd_1, mean_0, sd_0)


# Note: You can also get the numbers of observations in the `summarize` command,
# e.g. through the `n()` function that returns the number of rows of your data
# set. With the number of observations, the mean, and the standard deviation
# you could easily conduct simple t-tests to test for group differences in
# observed characteristics.


#___________________________________________________________________________####
#   Some Clean-Up                                                           ####

# Now that we have prepared our data set, we may want to clean it up a bit, e.g.
# change the ordering of columns. As already seen above, a simple command for
# this is `relocate()`. If you want to change the name of columns, you can use
# `rename()`. For selecting columns (or removing them), use the `select()` 
# command. For more information on these, please check the lecture slides.


#___________________________________________________________________________####
#   Export                                                                  ####

# At the bottom of your script, you should export data frames created in your
# data preparation. I store the prepared ENOE data set in the data/processed/
# folder of my repository

df_enoe_proc %>% 
  write_csv(files$output$enoe)
