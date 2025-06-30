#' Simple Linear Regression with Visual Diagnostics
#'
#' This function performs a simple linear regression, automatically checks the residuals for normality,
#' and generates several diagnostic plots, including a Q-Q plot of residuals, 
#' a scatterplot with the regression line, and a residuals vs. fitted values plot.
#'
#'
#' @param df         A data frame containing the observations. (type: data.frame)
#' @param abhVar     The name of the dependent variable. (type: character, numeric column)
#' @param unabhVar   The name of the independent variable. (type: character, numeric column)
#'
#'
#' @return A list containing:
#' \item{regression}{The linear model object (lm).}
#' \item{summary_regression}{The summary of the regression (coefficients, R², etc.).}
#' \item{shapiro_test}{Shapiro-Wilk test results for residuals.}
#' \item{residuals_normalverteilung}{Boolean indicating whether residuals are normally distributed.}
#' \item{plot_regression}{ggplot2 object: scatterplot with regression line.}
#' \item{plot_qq_resid}{ggplot2 object: Q-Q plot of residuals.}
#' \item{plot_resid_vs_fitted}{ggplot2 object: residuals vs. fitted values plot.}
#'
#'
#' @author Aurelio Nwamusse <aurelio.nwamusse@stud.h-da.de>, Hochschule Darmstadt
#'
#' @examples
#' # aurelio_linearregression(df, "y", "x")
#'
#' @import ggplot2
#'
#' @export



aurelio_lineareregression <- function(df, unabhVar, abhVar ) {
  
  # ------------ Load required packages locally ------------
  requireNamespace("ggplot2")


  
  
  # ------------ Validate function inputs ------------
  
  #Check if 'df' is a data frame
  if (!is.data.frame(df)) {
    stop("Fehler: 'df' muss ein Data-Frame sein ")
  }
  
  #Check if dependent variable is provided and exists in the data frame
  if (missing(abhVar) || !(abhVar %in% names(df))) {
    stop("Fehler: 'abhVar' fehlt oder existiert nicht im Data-Frame")
  }
  
  Check if independent variable is provided and exists
  if (missing(unabhVar) || !(unabhVar %in% names(df))) {
    stop("Fehler: 'unabhVar' fehlt oder existiert nicht im Data-Frame")
  }
  
  # Ensure dependent variable is numeric
  if (!is.numeric(df[[abhVar]])) {
    warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", abhVar))
    df[[abhVar]] <- as.numeric(as.character(df[[abhVar]]))
    if (any(is.na(df[[abhVar]]))) {
      stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden.", abhVar))
    }
  }
  
  # Ensure independent variable is numeric
  if (!is.numeric(df[[unabhVar]])) {
    warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", unabhVar))
    df[[unabhVar]] <- as.numeric(as.character(df[[unabhVar]]))
    if (any(is.na(df[[unabhVar]]))) {
      stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden.", unabhVar))
    }
  }
  

  
  
  # ------------ Initialize result list ------------
  result <- list()
  result$regression <- NA
  result$summary_regression <- NA
  result$regression_residuals <- NA
  result$shapiro_test <- NA
  result$residuals_normalverteilung <- NA
  
  result$plot_regression <- NA
  result$plot_qq_resid <- NA
  result$plot_resid_vs_fitted <- NA

  
  
  
  
  # ------------ Fit linear regression model ------------ 
  formel <- as.formula(paste(abhVar, '~', unabhVar))
  
  regression <- lm(formel, data = df)
  result$regression <- regression
  result$summary_regression <- summary(regression)
  
  
  # ------------ Residual diagnostics: Shapiro-Wilk test ------------
  result$regression_residuals <- regression$residuals
  shapiro_wilk_result <- shapiro.test(regression$residuals)
  result$shapiro_test <- shapiro_wilk_result
  result$residuals_normalverteilung <- shapiro_wilk_result$p.value >= 0.05
  
  
  # ------------ Visualization: regression diagnostics ------------
  
  # Scatterplot with regression line 
  result$plot_regreession <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[unabhVar]], y = .data[[abhVar]])) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "red") +
    ggplot2::labs(title = 'Scatterplot mit Regressionslinie',
                  x = sprintf(" %s ", unabhVar),
                  y = sprintf(" %s ", abhVar)
                  ) + 
    ggplot2::theme_minimal()
  
  # Q-Q plot of residuals
  resid_df <- data.frame(resid = regression$residuals)
  result$plot_qq_resid <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = resid)) + 
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Wahrscheinlichkeitsnetz für Normalverteilungen (Q-Q Plot)", 
                  x = "Theoretische Quantile", 
                  y = "Residuen") +
    ggplot2::theme_minimal()
  
  
  # Residuals vs. fitted values plot
  plot_df <- data.frame(Fitted = regression$fitted.values, Residuals = regression$residuals )
  result$plot_resid_vs_fitted <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(title = "Residuen vs. Fitted",
                  x = "Fitted Values", y = "Residuen") +
    ggplot2::theme_minimal()
  
  
  
  
  return(result)
  
  
}
