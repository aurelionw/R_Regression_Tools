#' Multiple Lineare Regression mit grafischer Ausgabe
#'
#' Diese Funktion führt eine multiple lineare Regression durch und erstellt verschiedene
#' diagnostische Plots zur Beurteilung der Modellgüte. Dazu zählen u.a. die Residuenanalyse,
#' die Untersuchung auf Homoskedastizität, Multikollinearität sowie Korrelationsanalysen.
#'
#' @param df                          ... Data Frame, der die Daten für die Analyse enthält.
#' @param unabhVar_list               ... Charaktervektor mit den Namen der unabhängigen Variablen.
#' @param abhVar                      ... Charakterstring mit dem Namen der abhängigen (Ziel-)Variable.
#' @param signifikanzniveau_alpha     ... Signifikanzniveau für Hypothesentests (Default: 0.05)
#'
#' @return Liste mit folgenden Elementen:
#' \item{multilineare_regression}{Das Regressionsmodell als Objekt vom Typ "lm".}
#' \item{summary_multilineare_regression}{Zusammenfassung des Regressionsmodells.}
#' \item{multilineare_regression_residuals}{Residuen des Modells.}
#' \item{shapiro_test}{Ergebnis des Shapiro-Wilk-Tests auf Normalverteilung der Residuen.}
#' \item{multilineare_regression_residuals_normalverteilung}{Boolescher Wert, ob Normalverteilung angenommen werden kann.}
#' \item{breusch_pagan_test}{Ergebnis des Breusch-Pagan-Tests auf Heteroskedastizität.}
#' \item{homoskedazitaet}{Boolescher Wert, ob Homoskedastizität angenommen werden kann.}
#' \item{korrelationsmatrix}{Korrelationsmatrix der unabhängigen Variablen.}
#' \item{hohe_korrelation_name}{Vektor mit Namen stark korrelierter Variablenpaare (|r| > 0.8).}
#' \item{multikollinearitaet}{Named numeric vector mit den berechneten Variance Inflation Factors (VIF) für jede unabhängige Variable.}
#' \item{problematische_vif_variablen}{Character vector mit Namen der unabhängigen Variablen, deren VIF-Werte größer oder gleich 5 sind.}
#' \item{vif_warnung}{Logical, TRUE wenn mindestens ein VIF-Wert größer oder gleich 10 ist (Hinweis auf ernsthafte Multikollinearität).}
#' \item{plot_qq_resid}{Q-Q-Plot der Residuen.}
#' \item{plot_resid_vs_fitted}{Plot: Residuen vs. Fitted Values.}
#' \item{plot_korrelationsmatrix}{Plot der Korrelationsmatrix (ggcorrplot).}
#' \item{plot_histogram_resid}{Histogramm der Residuen mit Dichtekurve.}
#' \item{plot_scale_location}{Scale-Location-Plot.}
#' \item{plot_influencePlot}{Influence Plot nach Cook's Distance.} Aus technischen Gründen nict automatisch angezeigt. Aufruf: \code{result\$plot_influencePlot()} in der Console
#' \item{plot_vif}{Visualisierung der VIF-Werte zur Multikollinearitätsdiagnose.}
#' 
#'
#' @author Aurelio Nwamusse <aurelio.nwamusse@stud.h-da.de>, Hochschule Darmstadt
#'
#' @examples
#' data <- read.csv2("election_data.csv")
#' aurelio_multilineareregression(data, list("Growth", "Inflation"), "Vote")
#'
#' @import ggplot2
#' @import ggcorrplot
#' @import car
#'
#' @export



aurelio_multilineareregression <- function(df,unabhVar_list,abhVar, signifikanzniveau_alpha = 0.05) {
  # ------------ LOKALES LADEN DER PAKETE ------------
  requireNamespace("ggplot2")
  requireNamespace("ggcorrplot")
  requireNamespace("car")
  #requireNamespace("")
  
  
  # ------------ EINGABE-TEST -------------
  #Überprüfung, ob Data-Frame
  if (!is.data.frame(df)) {
    stop("Fehler: 'df' muss ein Data-Frame sein ")
  }
  
  #?berpr?fung der abh?ngigen Variable
  if (missing(abhVar) || !(abhVar %in% names(df))) {
    stop("Fehler: 'abhVar' fehlt oder existiert nicht im Data-Frame")
  }
  
  #Überprüfung der unabhängiige Variable
  if (missing(unabhVar_list) || !all(unabhVar_list %in% names(df))) {
    stop("Fehler: Eine oder mehrere unabhängige Variablen fehlen oder existieren nicht im Data-Frame")
  }
  
  # Sicherstellen, dass abhVar numerisch ist
  if (!is.numeric(df[[abhVar]])) {
    warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", abhVar))
    df[[abhVar]] <- as.numeric(as.character(df[[abhVar]]))
    if (any(is.na(df[[abhVar]]))) {
      stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden.", abhVar))
    }
  }
  
  # Sicherstellen, dass unabhVar numerisch ist
  for (var in unabhVar_list) {
    if (!is.numeric(df[[var]])) {
      warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", var))
      df[[var]] <- as.numeric(as.character(df[[var]]))
      if (any(is.na(df[[var]]))) {
        stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden.", var))
      }
    }
  }
  
  # Falls Nutzer versehentlich eine Liste übergibt, in character-Vektor umwandeln
  if (is.list(unabhVar_list)) {
    unabhVar_list <- unlist(unabhVar_list)
  }
  
  
  
  result <- list()
  result$multilineare_regression <- NA
  result$summary_multilineare_regression <- NA
  result$multilineare_regression_residuals <- NA
  result$shapiro_test <- NA
  result$multilineare_regression_residuals_normalverteilung <- NA
  
  result$breusch_pagan_test <- NA
  result$homoskedazitaet <- NA
 
  
  result$korrelationsmatrix <-NA
  result$multikollinearitaet <- NA
  result$hohe_korrelation_name <- NA
  
  result$multikollinearitaet <- NA
  result$problematische_vif_variablen <- NA_character_
  result$vif_warnung <- NA
  
  
  
  #Plots
  result$plot_qq_resid <- NA
  result$plot_resid_vs_fitted <- NA
  result$plot_korrelationsmatrix <- NA
  result$plot_histogramm_residuen <- NA
  result$plot_scale_location <- NA
  result$plot_vif
  result$plot_influencePlot <- NA 
  
  
  # ------------ REGRESSIONSMODELL AUFSTELLEN ------------
  formel <- as.formula(paste(abhVar, '~', paste(unabhVar_list, collapse = " + ")))
  
  multiple_regression <- lm(formel, data = df)
  result$multilineare_regression <- multiple_regression
  result$summary_multilineare_regression <- summary(result$multilineare_regression)
  
  
  # ------------ RESIDUENANALYSE - SHAPIRO-WILK ------------
  result$multilineare_regression_residuals <- multiple_regression$residuals
  shapiro_wilk_result <- shapiro.test(result$multilineare_regression_residuals)
  result$shapiro_test <- shapiro_wilk_result
  result$multilineare_regression_residuals_normalverteilung <- shapiro_wilk_result$p.value >= signifikanzniveau_alpha
  
  
  # ------------ HOMOSKEDAZITÄT PRÜFEN ------------
  bp_test <- lmtest::bptest(result$multilineare_regression)
  result$breusch_pagan_test <- bp_test
  result$homoskedazitaet <- bp_test$p.value >= signifikanzniveau_alpha
  
  # ------------ KORRELATIONSMATRIX BESTIMMEN ------------
  reduced_data <- df[, unabhVar_list]
  reduced_data <- reduced_data[sapply(reduced_data, function(x) is.numeric(x) && sd(x, na.rm = TRUE) > 0)]
  korrelationsmatrix <- cor(reduced_data, use = "pairwise.complete.obs")
  result$korrelationsmatrix <- korrelationsmatrix
  
  
  hohe_korrelationspaare <- which(abs(korrelationsmatrix) >= 0.8 & abs(korrelationsmatrix) < 1, arr.ind = TRUE)
  hohe_korrelation_liste <- unique(t(apply(hohe_korrelationspaare, 1, sort)))
  hohe_korrelation_name <- apply(hohe_korrelation_liste, 1, function(idx) {
    paste0(colnames(korrelationsmatrix)[idx[1]], " & ", colnames(korrelationsmatrix) [idx[2]])
  })
  
  result$hohe_korrelation_name <- hohe_korrelation_name
  
  
  
  # ------------ TEST AUF MULTIKOLLINEARITÄT ------------
  vif_values <- car::vif(multiple_regression)
  
  result$multikollinearitaet <- vif_values
  result$problematische_vif_variablen <- names(vif_values[vif_values >= 5])
  result$vif_warnung <- any(vif_values >= 10)
  
  
  # ------------ DESKRIPTIVE STATISTIK (PLOTS) ------------
  
  #Wahrscheinlichlichkeitsnetz der Residuen - Q-Q-Plot
  resid_df <- data.frame(resid = multiple_regression$residuals)
  result$plot_qq_resid <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = resid)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Wahrscheinlichkeitsnetz für Normalverteilung (Q-Q Plot)",
                  x = "Theoretische Quantile",
                  y = "Residuen") +
  ggplot2::theme_minimal()
  

  
  #Residuals vs Fitted
  plot_df <- data.frame(Fitted = multiple_regression$fitted.values, Residuals = multiple_regression$residuals)
  result$plot_resid_vs_fitted <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(title = "Residuen vs Fitted",
                  x = "Fitted Values", y = "Residuen") +
    ggplot2::theme_minimal()

  #Korrelationsmatrix
  plot_km <- ggcorrplot::ggcorrplot(korrelationsmatrix, hc.order = TRUE, type = "lower", lab = TRUE)
  result$plot_korrelationsmatrix <- plot_km
  
  #Influence Plot (nur optional)
  result$plot_influencePlot <- function() {
    car::influencePlot(multiple_regression)
  }
  
  
  
  #VIF
  vif_values <- car::vif(multiple_regression)
  vif_df <- data.frame(variable = names(vif_values), VIF = as.numeric(vif_values))
  
  result$plot_vif <- ggplot2::ggplot(vif_df, ggplot2::aes(x = reorder(variable, - VIF), y = VIF, fill = VIF)) +
    ggplot2::geom_col(width =  0.6) +
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "orange", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "steelblue") +
    ggplot2::labs(title = "Variance Inflation Factor (VIF)",x = "Unabhängige Variable", y = "VIF-Wert", fill = "VIF") + #Markierung nochmal checken (x-Achse)
    
    ggplot2::theme_minimal()
  
  
  #Histogramm der Residuen mit der überlagerten Dichtefunktion und Bins-Berechnung mithilfe der 'Freedman-Diaconis-Regel'

  bin_width <- 2 * IQR(result$multilineare_regression_residuals, na.rm = TRUE)/length(result$multilineare_regression_residuals)^(1/3)
  bin_size <- ceiling((max(result$multilineare_regression_residuals, na.rm = TRUE) - min(result$multilineare_regression_residuals, na.rm = TRUE))/bin_width)
  
  result$plot_histogramm_residuen <- ggplot2::ggplot(resid_df, ggplot2::aes(x = resid)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..),bins = bin_size, fill = "skyblue", color = "black") +
    ggplot2::geom_density(color = "red", size = 1.2) +
    ggplot2::labs(title = "Histogramm der Residuen",
                  x = "Residuen", y = "Häufigkeit") +
    ggplot2::theme_minimal()
  
  
  #Scale-Location-Plot (Spread-Level)
    scale_location_df <- data.frame(fitted = multiple_regression$fitted.values,
                                    sqrt_std_resid = sqrt(abs(scale(result$multilineare_regression_residuals))))
    
    result$plot_scale_location <- ggplot2::ggplot(scale_location_df,ggplot2::aes(x = fitted, y = sqrt_std_resid)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "loess", color = "red") +
      ggplot2::labs(title = "Scale-Location-Plot", x = "Fitted Values", y = "√|Standardisierte Residuen|") +
      ggplot2::theme_minimal()
  
  
  return(result)
}
