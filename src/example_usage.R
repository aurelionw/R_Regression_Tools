
# ========== BEISPIEL-VERWENDUNG DER R-FUNKTIONEN ==========

# Diese Datei enthält vollständige Tests für alle R-Funktionen im Repository.
# Die benötigten CSV-Dateien liegen im Verzeichnis: src/csv-data/

# ------------ ANALYSIS OF VARIANCE (ANOVA) ------------

source("src/own_ANOVA.R")

# Fall 1: Iris-Datensatz (Games-Howell)
result <- aurelio_einfach_anova(iris, "Sepal.Length", "Species")
print(result$plot)

# Fall 2: Tukey-Test mit simulierten Daten
set.seed(1)
gruppe <- rep(c("A", "B", "C"), each = 30)
wert <- c(rnorm(30, 10, 2), rnorm(30, 12, 2), rnorm(30, 14, 2))
df_tukey <- data.frame(gruppe, wert)
res_tukey <- aurelio_einfach_anova(df_tukey, "wert", "gruppe")

# Fall 3: Kruskal-Wallis-Test mit schiefen Verteilungen
set.seed(2)
wert <- c(rexp(30, 1/10), rexp(30, 1/12), rexp(30, 1/14))
df_kruskal <- data.frame(gruppe, wert)
res_kruskal <- aurelio_einfach_anova(df_kruskal, "wert", "gruppe")

# Fall 4: Games-Howell mit unterschiedlicher Varianz
set.seed(3)
wert <- c(rnorm(30, 10, 1), rnorm(30, 12, 5), rnorm(30, 14, 1))
df_games <- data.frame(gruppe, wert)
res_games <- aurelio_einfach_anova(df_games, "wert", "gruppe")

# Fall 5: Echte CSV-Daten
abortion_data_anova <- read.csv2("src/csv-data/abortion_data.csv")
abortion_anova <- aurelio_einfach_anova(abortion_data_anova, "Abortion", "Relig_Group")

# ------------ EINFACHE LINEARE REGRESSION ------------

source("src/own_Regression.R")

abortion_data <- read.csv2("src/csv-data/abortion_data.csv")
regression_abortion <- aurelio_lineareregression(abortion_data, "Religion", "Abortion")

# ------------ MULTIPLE LINEARE REGRESSION ------------

source("src/own_multipleregression.R")

election_data <- read.csv2("src/csv-data/election_data.csv")
multiplereression_election_result <- aurelio_multilineareregression(
  election_data, list("Growth", "Inflation"), "Vote"
)

# ------------ DUMMY-VARIABLEN ------------

source("src/own_dummycreator.R")

einkommensdaten <- read.csv2("src/csv-data/einkommensdaten.csv")
dummy_var <- aurelio_dummy_variable_creator(einkommensdaten, c("Ausbildung"), list(1, 2, 3))

# ------------ MULTIPLE REGRESSION MIT INTERAKTION ------------

source("src/own_multipleregression_interactions.R")

einstellungsproblem <- read.csv2("src/csv-data/einstellungsproblem.csv")
multipleregression_einstellungsproblem_with_interactions <- aurelio_multilineareregression_with_interactions(
  einstellungsproblem, list("Test*Rasse"), "Arbeitsleistung"
)

election_data <- read.csv2("src/csv-data/election_data.csv")
multiplereression_election_with_interactions <- aurelio_multilineareregression_with_interactions(
  election_data, list("Growth:Incumbent", "Inflation"), "Vote"
)

multiplereression_election_with_interactions2 <- aurelio_multilineareregression_with_interactions(
  election_data, list("Growth*Incumbent", "Inflation"), "Vote"
)

# Beispiel mit mtcars
mtcars$gear <- factor(mtcars$gear)
multiwithinter <- aurelio_multilineareregression_with_interactions(mtcars, "gear", "mpg")

mtcars$am <- factor(mtcars$am)
mtcars$vd <- factor(mtcars$vs)
mixed <- aurelio_multilineareregression_with_interactions(mtcars, list("am*vs", "hp"), "mpg")

# ------------ KOMPLEXES BEISPIEL: 102 EINKOMMENSDATEN ------------

einkommensdaten <- read.csv2("src/csv-data/einkommensdaten.csv")
einkommensdaten$Management <- factor(einkommensdaten$Management)
einkommensdaten$Ausbildung <- factor(einkommensdaten$Ausbildung)

df_neu <- einkommensdaten[-33,]
mixed2 <- aurelio_multilineareregression_with_interactions(
  einkommensdaten, list("Management*Ausbildung", "Erfahrung"), "Einkommen"
)
