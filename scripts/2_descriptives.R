#___________________________________________________________________________####
#   Descriptive Statistics for Estimation Sample                            ####

library(tidyverse)
library(kableExtra)
library(scales)

files <- list(
  input = list(
    estimation_sample = "data/processed/estimation_sample.csv"
  ),
  output = list(
    table_dir = "tables/",
    figure_dir = "figures/"
  )
)


#___________________________________________________________________________####
#   Data Import                                                             ####

df_sample <- files$input$estimation_sample %>% 
  read_csv(show_col_types = F)


#___________________________________________________________________________####
#   Table with Summary Statistics                                           ####

df_sum_stats <- df_sample %>% 
  summarise(
    across(c(migrate, female, married), mean, .names = "mean__{col}"),
    across(
      c(age, educ), 
      list(mean = ~ mean(.), sd = ~ sd(.)), 
      .names = "{fn}__{col}"
    ),
    .by = treated
  ) %>% 
  pivot_longer(
    -treated, 
    names_pattern = "(.*)__(.*)", 
    names_to = c("statistic", "variable")
  ) %>% 
  pivot_wider(
    names_from = c(treated, statistic),
    names_sep = "__",
    values_from = value
  ) %>% 
  mutate(
    label = case_when(
      variable == "migrate" ~ "Migrated",
      variable == "female" ~ "Female",
      variable == "married" ~ "Married",
      variable == "age" ~ "Age",
      variable == "educ" ~ "Years of schooling"
    ),
    order = case_when(
      variable == "migrate" ~ 1,
      variable == "female" ~ 2,
      variable == "married" ~ 3,
      variable == "age" ~ 4,
      variable == "educ" ~ 5
    ),
    .after = variable
  )

df_nobs <- df_sample %>% 
  summarise(mean = n(), .by = treated) %>% 
  pivot_longer(
    -treated,
    names_to = "statistic" # to align under mean column of summary statistics
  ) %>% 
  pivot_wider(
    names_from = c(treated, statistic),
    names_sep = "__",
    values_from = value
  ) %>% 
  mutate(label = "$N$")

# Create LaTex table to include in the presentation; object could alternatively 
# also be stored as .Rds file and then imported in the presentation (if you do
# not set the format argument)
sum_stats_table <- df_sum_stats %>% 
  arrange(order) %>% 
  select(-variable, -order) %>% 
  bind_rows(df_nobs) %>% 
  mutate(
    across(
      where(is.double),
      ~ case_when(
        label == "$N$" ~ label_number(accuracy = 1)(.),
        . > 1 ~ label_number(accuracy = 0.01)(.),
        . <= 1 ~ label_percent(accuracy = 0.01, suffix = "\\%")(.)
      )
    ),
    across(everything(), ~ if_else(is.na(.), "", .))
  ) %>% 
  kbl(
    col.names = c("", rep(c("Mean", "Std. Dev."), 2)),
    escape = F,
    format = "latex",
    align = "lcccc",
    booktabs = T,
    linesep = c(
      "\\addlinespace", "\\addlinespace", "\\addlinespace", "\\addlinespace", 
      "\\addlinespace\\midrule"
    ),
    caption = "Summary Statistics",
    label = "sum-stats"
  ) %>% 
  add_header_above(
    c("", "Municipalities w/ fence" = 2, "Municipalities w/o fence" = 2)
  )


#___________________________________________________________________________####
#   Export                                                                  ####

sum_stats_table %>% 
  save_kable(str_c(files$output$table_dir, "summary_statistics.tex"))

# Alternative approach: Export the prepared descriptive statistics as CSV, read
# that data during runtime of Quarto presentation and move the table creation
# to a code chunk in Quarto
