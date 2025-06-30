#' Einfache Lineare Regression mit grafischer Ausgabe
#' 
#' Die Funktion führt automatisch die Einfache Lineare Regression durch, überprüft die Residuen auf Normalverteilung 
#' und plottet das Wahrscheinlichkeitsnetz für Normalverteilungen der Rsiduen,
#' die Punkte mit Regressionslinie, Residuals vs Fitted-Values 
#' 
#' 
#' 
#' 
#' @param df          ... Data Frame mit den Beobachtungen, die die Gruppierungsvariable enthält (typeof Data Frame)
#' @param abhVar      ... Name der abh?ngigen Variable (typeof as.numeric)
#' @param unabhVar    ... Name der unabhängigen Variable (typeof as.numeric)
#' 
#' 
#' @return Eine Liste mit:
#' \item{regression}{LIST, Ergebnis der Regression}
#' \item{summary_regression}{LIST, zusammengefasstes Ergebnis der Regression}
#' \item{shapiro_test}{LIST, Ergebnis des Shapiro-Tests}
#' \item{residuals_normalverteilung}{BOOL, Normalverteilung der Residuen gegeben oder nicht}
#' \item{plot_regression}{ggplot2-Objekt, Scatterplot mit Regressionslinie }
#' \item{plot_qq_resid}{ggplot2-Objekt, Wahrscheinlichkeitsnetz der Residuen}
#' \item{plot_resid_vs_fitted}{ggplot2-Objekt, Scatterplot der Residuen vs Fitted-Values}
#' 
#' 
#' @author Aurelio Nwamusse <aurelio.nwamusse@stud.h-da.de>, Hochschule Darmstadt
#' 
#' 
#' @examples
#' 
#' 
#' @import ggplot2
#' 
#' 
#' @export


aurelio_lineareregression <- function(df, unabhVar, abhVar ) {
  
  # ------------ Lokales laden der Pakete ------------
  requireNamespace("ggplot2")


  
  
  # ------------ EINGABE-TEST ------------
  
  #Überprüfung, ob Data-Frame
  if (!is.data.frame(df)) {
    stop("Fehler: 'df' muss ein Data-Frame sein ")
  }
  
  #?berpr?fung der abh?ngigen Variable
  if (missing(abhVar) || !(abhVar %in% names(df))) {
    stop("Fehler: 'abhVar' fehlt oder existiert nicht im Data-Frame")
  }
  
  #Überprüfung der unabhängiige Variable
  if (missing(unabhVar) || !(unabhVar %in% names(df))) {
    stop("Fehler: 'unabhVar' fehlt oder existiert nicht im Data-Frame")
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
  if (!is.numeric(df[[unabhVar]])) {
    warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", unabhVar))
    df[[unabhVar]] <- as.numeric(as.character(df[[unabhVar]]))
    if (any(is.na(df[[unabhVar]]))) {
      stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden.", unabhVar))
    }
  }
  

  
  
  # ------------ ERGEBNISLISTE ERSTELLEN ------------
  result <- list()
  result$regression <- NA
  result$summary_regression <- NA
  result$regression_residuals <- NA
  result$shapiro_test <- NA
  result$residuals_normalverteilung <- NA
  
  result$plot_regression <- NA
  result$plot_qq_resid <- NA
  result$plot_resid_vs_fitted <- NA

  
  
  
  
  # ------------ REGRESSIONSMODELL AUFSTELLEN ------------ 
  formel <- as.formula(paste(abhVar, '~', unabhVar))
  
  regression <- lm(formel, data = df)
  result$regression <- regression
  result$summary_regression <- summary(regression)
  
  
  # ------------ RESIDUUENANALYSE - SHAPIRO-WILK ------------
  result$regression_residuals <- regression$residuals
  shapiro_wilk_result <- shapiro.test(regression$residuals)
  result$shapiro_test <- shapiro_wilk_result
  result$residuals_normalverteilung <- shapiro_wilk_result$p.value >= 0.05
  
  
  # ------------ DESKRIPTIVE STATISTIK (PLOTS) ------------
  
  # Scatterplot mit Regressionslinie 
  result$plot_regreession <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[unabhVar]], y = .data[[abhVar]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red") +
    ggplot2::labs(title = 'Scatterplot mit Regressionslinie',
                  x = sprintf(" %s ", unabhVar),
                  y = sprintf(" %s ", abhVar)
                  ) + 
    ggplot2::theme_minimal()
  
  # Wahrscheinlichleitz Netz der Residuen - Q-Q Plot
  resid_df <- data.frame(resid = regression$residuals)
  result$plot_qq_resid <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = resid)) + 
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Wahrscheinlichkeitsnetz für Normalverteilungen (Q-Q Plot)", 
                  x = "Theoretische Quantile", 
                  y = "Residuen") +
    ggplot2::theme_minimal()
  
  
  # Residuen vs. Fitted
  plot_df <- data.frame(Fitted = regression$fitted.values, Residuals = regression$residuals )
  result$plot_resid_vs_fitted <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(title = "Residuen vs. Fitted",
                  x = "Fitted Values", y = "Residuen") +
    ggplot2::theme_minimal()
  
  
  
  
  return(result)
  
  
}