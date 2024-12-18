#___________________________________________________________________________####
#   Identification: TWFE Estimates                                          ####

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

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


# We consider year-to-year responses in migration behavior. Relative periods 
# there have to be summarized into years instead of quarters. This is easily
# achieved by binning e.g. periods -4 to -1 into year -1. The computation is
# straightforward: divide relative periods by 4 (the number of quarters within
# a year) and take the floor of the resulting numbers. This is only needed for
# treated observations because for non-treated, the relative year is set
# arbitrarily high.

df_sample_proc <- df_sample %>% 
  mutate(
    relative_year = if_else(
      treated == 1,
      floor(relative_period / 4),
      relative_period
    )
  )

#___________________________________________________________________________####
#   TWFE Models                                                             ####

twfe_formula <- migrate ~ 
  fence +
  csw0(age + educ + female + married) | # Stepwise adding of controls
  as.factor(period) + municipality # Fixed effects

twfe_lpms <- feols(
  twfe_formula,
  data = df_sample_proc,
  vcov = cluster ~ municipality
)

twfe_logits <- feglm(
  twfe_formula,
  data = df_sample_proc,
  vcov = cluster ~ municipality,
  family = "binomial"
)

# Assign new names for models
names(twfe_lpms) <- c("simple", "extended")
names(twfe_logits) <- c("simple", "extended")


#___________________________________________________________________________####
#   Event Study Design                                                      ####

event_formula <- migrate ~ 
  i(relative_year, ref = c(-1, -1000), bin = list("-4" = -5:-4, "4" = 4:7)) + 
  csw0(age + educ + female + married) | # Stepwsie adding of controls
  period + municipality # Fixed effects

event_lpms <- feols(
  event_formula,
  data = df_sample_proc,
  vcov = cluster ~ municipality
)

event_logits <- feglm(
  event_formula,
  data = df_sample_proc,
  vcov = cluster ~ municipality,
  family = "binomial"
)

# Assign new names for models
names(event_lpms) <- c("simple", "extended")
names(event_logits) <- c("simple", "extended")


#___________________________________________________________________________####
#   Results                                                                 ####

##  Regression Tables                                                       ####

# Set parameters for tables
stars <- c("***" = .01, "**" = .05, "*" = .1)

coef_labels <- c(
  fence = "Fence Construction", age = "Age", female = "Female", 
  educ = "Years of Schooling", married = "Married"
)

gof_labels <- tribble(
  ~ raw, ~ clean, ~ fmt,
  #"r2.within", "$R^2$ within", 8,
  #"r2.within.adjusted", "$R^2_{adj}$ within", 8,
  "nobs", "Observations", 0,
)

# Checkmarks to indicate FE used
fe_checkmarks <- tribble(
  ~ ...0, ~ ...1, ~ ...2, ~ ...3, ~ ...4,
  "Municipality FE", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
  "Year-quarter FE", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$",
)

attr(fe_checkmarks, "position") <- c(11, 12)

reg_table <- msummary(
  list(twfe_lpms, twfe_logits),
  output = "kableExtra",
  stars = stars,
  coef_map = coef_labels,
  gof_map = gof_labels,
  escape = F,
  col.names = c("", "(1)", "(2)", "(3)", "(4)"),
  title = "Impact of Fence Construction on US-Mexico Migration",
  notes = "Parentheses shows standard errors clustered on municipality-level.",
  add_rows = fe_checkmarks,
  format = "latex"
) %>% 
  add_header_above(
    c("", "OLS" = 2, "Logit" = 2)
  ) %>% 
  kable_styling(latex_options = "scale_down")

reg_table


##  Event Study Plot                                                        ####

#' Custom function to retrieve visualization-ready Event Study results
#' 
#' @param model Object of class "fixest"
#' @param var Name of the relative period column
#' @param alpha Confidence level, defaults to 0.95 
#' 
#' @returns A `tibble` with the relative period estimates, standard errors and
#' confidence interval
get_event_study_data <- function(model, var, alpha = 0.95) {
  require(magrittr)
  require(tibble)
  require(dplyr)
  require(stringr)
  require(fixest)
  require(lmtest)
  
  if (class(model) != "fixest")
    stop("`model` has to be of class 'fixest'")
  
  coef_table <- model %>% 
    coeftable() %>% 
    as_tibble(rownames = "term")
  
  idx <- str_starts(coef_table$term, var)
  
  coef_table[idx,] %>% 
    mutate(
      ci_level = alpha,
      ci_lower = coefci(model, level = alpha)[idx,1],
      ci_upper = coefci(model, level = alpha)[idx,2],
      term = as.numeric(str_remove(term, str_c(var, "\\:\\:")))
    ) %>% 
    select(
      term, estimate = Estimate, std_err = `Std. Error`, starts_with("ci_")
    ) %>% 
    rename_with(~ var, .cols = term)
}

event_study_data <- list(
  lpm = event_lpms,
  logit = event_logits
) %>% 
  map_df(
    ~ map_df(., get_event_study_data, var = "relative_year", .id = "spec"),
    .id = "model"
  )

event_plots <- event_study_data %>% 
  filter(spec == "extended") %>% 
  split(~ model) %>% 
  map(
    ~ ggplot(., aes(x = relative_year)) +
      geom_hline(yintercept = 0, color = "#8dae10", linetype = "longdash") +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = .5) +
      geom_point(aes(y = estimate)) +
      scale_x_continuous(breaks = -4:4) +
      labs(y = "Coefficient", x = "Relative Year") +
      theme_bw() +
      theme(text = element_text(size = 8))
  )

event_plots


#___________________________________________________________________________####
#   Export                                                                  ####

reg_table %>% 
  save_kable(str_c(files$output$table_dir, "reg_table.tex"))

event_plots %>% 
  map2(
    names(.),
    ~ ggsave(
      str_c(files$output$figure_dir, "event_plot_", .y, ".png"),
      plot = .x,
      width = 1000,
      height = 750,
      units = "px"
    )
  )
