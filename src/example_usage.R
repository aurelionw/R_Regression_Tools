# ========== EXAMPLE USAGE OF THE R FUNCTIONS ==========

# This file contains complete test cases for all R functions in the repository.
# The required CSV files are located in the directory: src/csv-data/

# ------------ ANALYSIS OF VARIANCE (ANOVA) ------------

source("src/own_ANOVA.R")

# Case 1: Iris dataset (Games-Howell expected)
result <- aurelio_einfach_anova(iris, "Sepal.Length", "Species")
print(result$plot)

# Case 2: Tukey HSD test with simulated data
set.seed(1)
group <- rep(c("A", "B", "C"), each = 30)
value <- c(rnorm(30, 10, 2), rnorm(30, 12, 2), rnorm(30, 14, 2))
df_tukey <- data.frame(group, value)
res_tukey <- aurelio_einfach_anova(df_tukey, "value", "group")

# Case 3: Kruskal-Wallis test with skewed distributions
set.seed(2)
value <- c(rexp(30, 1/10), rexp(30, 1/12), rexp(30, 1/14))
df_kruskal <- data.frame(group, value)
res_kruskal <- aurelio_einfach_anova(df_kruskal, "value", "group")

# Case 4: Games-Howell with unequal variances
set.seed(3)
value <- c(rnorm(30, 10, 1), rnorm(30, 12, 5), rnorm(30, 14, 1))
df_games <- data.frame(group, value)
res_games <- aurelio_einfach_anova(df_games, "value", "group")

# Case 5: Real CSV data
abortion_data_anova <- read.csv2("src/csv-data/abortion_data.csv")
abortion_anova <- aurelio_einfach_anova(abortion_data_anova, "Abortion", "Relig_Group")

# ------------ SIMPLE LINEAR REGRESSION ------------

source("src/own_Regression.R")

abortion_data <- read.csv2("src/csv-data/abortion_data.csv")
regression_abortion <- aurelio_lineareregression(abortion_data, "Religion", "Abortion")

# ------------ MULTIPLE LINEAR REGRESSION ------------

source("src/own_multipleregression.R")

election_data <- read.csv2("src/csv-data/election_data.csv")
multiple_result <- aurelio_multilineareregression(
  election_data, list("Growth", "Inflation"), "Vote"
)

# ------------ DUMMY VARIABLE CREATION ------------

source("src/own_dummycreator.R")

income_data <- read.csv2("src/csv-data/einkommensdaten.csv")
dummy_result <- aurelio_dummy_variable_creator(income_data, c("Ausbildung"), list(1, 2, 3))

# ------------ MULTIPLE REGRESSION WITH INTERACTIONS ------------

source("src/own_multipleregression_interactions.R")

# Example 1: Single interaction term
attitudes_data <- read.csv2("src/csv-data/einstellungsproblem.csv")
result_interaction_1 <- aurelio_multilineareregression_with_interactions(
  attitudes_data, list("Test*Rasse"), "Arbeitsleistung"
)

# Example 2: Interaction using colon notation
result_interaction_2 <- aurelio_multilineareregression_with_interactions(
  election_data, list("Growth:Incumbent", "Inflation"), "Vote"
)

# Example 3: Full interaction with asterisk
result_interaction_3 <- aurelio_multilineareregression_with_interactions(
  election_data, list("Growth*Incumbent", "Inflation"), "Vote"
)

# Example 4: mtcars with categorical predictor
mtcars$gear <- factor(mtcars$gear)
multi_gear <- aurelio_multilineareregression_with_interactions(mtcars, "gear", "mpg")

# Example 5: Interaction of two factors
mtcars$am <- factor(mtcars$am)
mtcars$vs <- factor(mtcars$vs)
multi_factors <- aurelio_multilineareregression_with_interactions(mtcars, list("am*vs", "hp"), "mpg")

# ------------ COMPLEX EXAMPLE: 102 OBSERVATIONS ------------

income_data <- read.csv2("src/csv-data/einkommensdaten.csv")
income_data$Management <- factor(income_data$Management)
income_data$Ausbildung <- factor(income_data$Ausbildung)

# Optional: remove outlier row 33
df_cleaned <- income_data[-33, ]

complex_result <- aurelio_multilineareregression_with_interactions(
  income_data, list("Management*Ausbildung", "Erfahrung"), "Einkommen"
)
