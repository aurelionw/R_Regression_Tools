#' Multiple Linear Regression with Diagnostic Visualizations
#'
#' This function performs a multiple linear regression and generates several
#' diagnostic plots to assess model quality. These include residual analysis,
#' checks for homoscedasticity, multicollinearity, and correlation structure among predictors.
#'
#' @param df                          A data frame containing the input data.
#' @param unabhVar_list               A character vector with the names of the independent variables.
#' @param abhVar                      A character string with the name of the dependent (target) variable.
#' @param signifikanzniveau_alpha     Significance level for hypothesis tests (default: 0.05).
#'
#' @return A list with the following elements:
#' \item{multilineare_regression}{The fitted linear regression model (object of class \code{lm}).}
#' \item{summary_multilineare_regression}{Summary of the regression model.}
#' \item{multilineare_regression_residuals}{Residuals of the model.}
#' \item{shapiro_test}{Shapiro-Wilk test for normality of residuals.}
#' \item{multilineare_regression_residuals_normalverteilung}{Logical value indicating whether normality of residuals can be assumed.}
#' \item{breusch_pagan_test}{Breusch-Pagan test for heteroskedasticity.}
#' \item{homoskedazitaet}{Logical value indicating whether homoscedasticity can be assumed.}
#' \item{korrelationsmatrix}{Correlation matrix of the independent variables.}
#' \item{hohe_korrelation_name}{Vector of variable name pairs with high correlation (|r| > 0.8).}
#' \item{multikollinearitaet}{Named numeric vector with the computed Variance Inflation Factors (VIFs) for each predictor.}
#' \item{problematische_vif_variablen}{Character vector of predictors with VIF ≥ 5 (indicating problematic multicollinearity).}
#' \item{vif_warnung}{Logical value, TRUE if any VIF ≥ 10 (indicating serious multicollinearity).}
#' \item{plot_qq_resid}{Q-Q plot of residuals.}
#' \item{plot_resid_vs_fitted}{Plot of residuals versus fitted values.}
#' \item{plot_korrelationsmatrix}{Correlation matrix plot (from \code{ggcorrplot}).}
#' \item{plot_histogram_resid}{Histogram of residuals with density curve.}
#' \item{plot_scale_location}{Scale-Location plot (sqrt of standardized residuals vs. fitted values).}
#' \item{plot_influencePlot}{Cook's Distance influence plot. Not shown automatically; call manually via \code{result\$plot_influencePlot()}.}
#' \item{plot_vif}{Barplot showing VIF values to assess multicollinearity.}
#'
#' @author Aurelio Nwamusse <aurelio.nwamusse@stud.h-da.de>, Darmstadt University of Applied Sciences
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
  
  # ------------ LOAD REQUIRED PACKAGES LOCALLY ------------
  requireNamespace("ggplot2")
  requireNamespace("ggcorrplot")
  requireNamespace("car")
  #requireNamespace("")
  
  
  # ------------ INPUT VALIDATION -------------
  # Check if df is a data frame
  if (!is.data.frame(df)) {
    stop("Fehler: 'df' muss ein Data-Frame sein ")
  }
  
  # Check if dependent variable exists
  if (missing(abhVar) || !(abhVar %in% names(df))) {
    stop("Fehler: 'abhVar' fehlt oder existiert nicht im Data-Frame")
  }
  
  # Check if all independent variables exist
  if (missing(unabhVar_list) || !all(unabhVar_list %in% names(df))) {
    stop("Fehler: Eine oder mehrere unabhängige Variablen fehlen oder existieren nicht im Data-Frame")
  }
  
  # Ensure the dependent variable is numeric
  if (!is.numeric(df[[abhVar]])) {
    warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", abhVar))
    df[[abhVar]] <- as.numeric(as.character(df[[abhVar]]))
    if (any(is.na(df[[abhVar]]))) {
      stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden.", abhVar))
    }
  }
  
  # Ensure all independent variables are numeric
  for (var in unabhVar_list) {
    if (!is.numeric(df[[var]])) {
      warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", var))
      df[[var]] <- as.numeric(as.character(df[[var]]))
      if (any(is.na(df[[var]]))) {
        stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden.", var))
      }
    }
  }
  
  # Convert list to character vector if needed
  if (is.list(unabhVar_list)) {
    unabhVar_list <- unlist(unabhVar_list)
  }
  
  

  # ------------ CREATE RESULT LIST ------------
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
  
  
  # ------------ FIT MULTIPLE LINEAR REGRESSION MODEL ------------
  formel <- as.formula(paste(abhVar, '~', paste(unabhVar_list, collapse = " + ")))
  
  multiple_regression <- lm(formel, data = df)
  result$multilineare_regression <- multiple_regression
  result$summary_multilineare_regression <- summary(result$multilineare_regression)
  
  
  # ------------ TEST FOR NORMALITY OF RESIDUALS (SHAPIRO-WILK) ------------
  result$multilineare_regression_residuals <- multiple_regression$residuals
  shapiro_wilk_result <- shapiro.test(result$multilineare_regression_residuals)
  result$shapiro_test <- shapiro_wilk_result
  result$multilineare_regression_residuals_normalverteilung <- shapiro_wilk_result$p.value >= signifikanzniveau_alpha
  
  
  # ------------ CHECK FOR HOMOSCEDASTICITY (Breusch-Pagan Test)------------
  bp_test <- lmtest::bptest(result$multilineare_regression)
  result$breusch_pagan_test <- bp_test
  result$homoskedazitaet <- bp_test$p.value >= signifikanzniveau_alpha
  
  # ------------ COMPUTE CORRELATION MATRIX AMONG INDEPENDENT VARIABLES ------------
  reduced_data <- df[, unabhVar_list]
  reduced_data <- reduced_data[sapply(reduced_data, function(x) is.numeric(x) && sd(x, na.rm = TRUE) > 0)]
  korrelationsmatrix <- cor(reduced_data, use = "pairwise.complete.obs")
  result$korrelationsmatrix <- korrelationsmatrix
  
  # ------------ DETECT STRONGLY CORRELATED PAIRS (|r| >= 0.8) ------------
  hohe_korrelationspaare <- which(abs(korrelationsmatrix) >= 0.8 & abs(korrelationsmatrix) < 1, arr.ind = TRUE)
  hohe_korrelation_liste <- unique(t(apply(hohe_korrelationspaare, 1, sort)))
  hohe_korrelation_name <- apply(hohe_korrelation_liste, 1, function(idx) {
    paste0(colnames(korrelationsmatrix)[idx[1]], " & ", colnames(korrelationsmatrix) [idx[2]])
  })
  
  result$hohe_korrelation_name <- hohe_korrelation_name
  
  
  
  # ------------ MULTICOLLINEARITY CHECK (VIF) ------------
  vif_values <- car::vif(multiple_regression)
  
  result$multikollinearitaet <- vif_values
  result$problematische_vif_variablen <- names(vif_values[vif_values >= 5])
  result$vif_warnung <- any(vif_values >= 10)
  
  
  # ------------ DESCRIPTIVE DIAGNOSTIC PLOTS ------------
  
  # Q-Q Plot for residuals
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

  # Correlation Matrix Plot
  plot_km <- ggcorrplot::ggcorrplot(korrelationsmatrix, hc.order = TRUE, type = "lower", lab = TRUE)
  result$plot_korrelationsmatrix <- plot_km
  
  # Cook's Distance Influence Plot
  result$plot_influencePlot <- function() {
    car::influencePlot(multiple_regression)
  }
  
  
  
  #VIF Barplot
  vif_values <- car::vif(multiple_regression)
  vif_df <- data.frame(variable = names(vif_values), VIF = as.numeric(vif_values))
  
  result$plot_vif <- ggplot2::ggplot(vif_df, ggplot2::aes(x = reorder(variable, - VIF), y = VIF, fill = VIF)) +
    ggplot2::geom_col(width =  0.6) +
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "orange", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "steelblue") +
    ggplot2::labs(title = "Variance Inflation Factor (VIF)",x = "Unabhängige Variable", y = "VIF-Wert", fill = "VIF") + #Markierung nochmal checken (x-Achse)
    
    ggplot2::theme_minimal()
  
  
  #Histogram of residuals (Freedman-Diaconis binning)

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
