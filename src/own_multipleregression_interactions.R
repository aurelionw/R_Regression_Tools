#' Multiple Linear Regression with Interaction Terms and Visual Diagnostics
#'
#' This function performs a multiple linear regression with interaction terms and provides various
#' diagnostic plots to assess model quality. These include residual analysis, tests for homoscedasticity,
#' multicollinearity checks, and correlation analyses.
#'
#' @param df                          A data frame containing the data for analysis.
#' @param unabhVar_list               A character vector with the names of the independent variables, including interaction terms (e.g. "x1*x2").
#' @param abhVar                      A character string indicating the dependent (target) variable.
#' @param signifikanzniveau_alpha     Significance level for hypothesis testing (default: 0.05)
#'
#' @return A list with the following elements:
#' \item{multilineare_regression}{The regression model object of type "lm".}
#' \item{summary_multilineare_regression}{Summary of the regression model.}
#' \item{multilineare_regression_residuals}{Residuals of the model.}
#' \item{shapiro_test}{Result of the Shapiro-Wilk test for normality of residuals.}
#' \item{multilineare_regression_residuals_normalverteilung}{Boolean indicating whether normality can be assumed.}
#' \item{breusch_pagan_test}{Result of the Breusch-Pagan test for heteroskedasticity.}
#' \item{homoskedazitaet}{Boolean indicating whether homoscedasticity can be assumed.}
#' \item{korrelationsmatrix}{Correlation matrix of the independent variables.}
#' \item{hohe_korrelation_name}{Character vector of highly correlated variable pairs (|r| > 0.8).}
#' \item{multikollinearitaet}{Named numeric vector containing the calculated Variance Inflation Factors (VIFs).}
#' \item{problematische_vif_variablen}{Names of variables with VIF values ≥ 5.}
#' \item{vif_warnung}{Logical; TRUE if any VIF ≥ 10 (indicating serious multicollinearity).}
#' \item{plot_qq_resid}{Q-Q plot of the residuals.}
#' \item{plot_resid_vs_fitted}{Plot of residuals vs. fitted values.}
#' \item{plot_korrelationsmatrix}{Correlation matrix plot (via ggcorrplot).}
#' \item{plot_histogram_resid}{Histogram of residuals with density curve.}
#' \item{plot_scale_location}{Scale-location (spread-level) plot.}
#' \item{plot_influencePlot}{Cook's Distance influence plot. Not rendered by default – run \code{result\$plot_influencePlot()} manually in console.}
#' \item{plot_vif}{Bar plot of VIF values to detect multicollinearity.}
#' \item{plot_interaktion_list}{List of interaction plots for each interaction term.}
#' \item{plot_interaktion}{Single interaction plot (if one interaction with a factor exists).}
#' \item{plot_interactions_faceted}{Faceted interaction plot comparing multiple interactions.}
#' \item{plot_johnson_neyman_list}{List of Johnson-Neyman plots for continuous-continuous interactions.}
#' \item{plot_konfint}{Plot of confidence intervals for regression coefficients at (1 - alpha)%.}
#'
#' @author Aurelio Nwamusse <aurelio.nwamusse@stud.h-da.de>, Hochschule Darmstadt
#'
#' @examples
#' data <- read.csv2("election_data.csv")
#' aurelio_multilineareregression(data, list("Growth", "Inflation", "Growth*Inflation"), "Vote")
#'
#' @import ggplot2
#' @import ggcorrplot
#' @import car
#' @import interactions
#' @import lmtest
#'
#' @export




aurelio_multilineareregression_with_interactions <- function(df,unabhVar_list,abhVar, signifikanzniveau_alpha = 0.05) {
  # ------------ LOAD REQUIRED PACKAGES LOCALLY ------------
  requireNamespace("ggplot2")
  requireNamespace("ggcorrplot")
  requireNamespace("car")
  requireNamespace("lmtest")
  requireNamespace("interactions")
  
  
  
  # ------------ HELPER FUNCTION: Extract base variables from formulas ------------
  extrahiere_basisvariablen <- function(varformeln) {
    clean_vars <- gsub("(log|scale|I)\\([^\\)]*\\)", "", varformeln) # Funktionen entfernen
    clean_vars <- gsub("[^[:alnum:]_]", " ", clean_vars)             # Sonderzeichen entfernen
    clean_vars <- unlist(strsplit(clean_vars, "\\s+"))               # aufsplitten
    clean_vars <- clean_vars[clean_vars != ""]                       # leere raus
    unique(clean_vars)
  }
  
  
  
  # ------------ INPUT VALIDATION -------------
  # Ensures that 'df' is a data frame
  if (!is.data.frame(df)) {
    stop("Fehler: 'df' muss ein Data-Frame sein ")
  }
  
  # Check if all base variables exist in the data frame
  basis_variablen <- extrahiere_basisvariablen(unabhVar_list)
  if (!all(basis_variablen %in% names(df))) {
    fehlende <- basis_variablen[!(basis_variablen %in% names(df))]
    stop(paste("Fehler: Diese Variablen fehlen im Data Frame:", paste(fehlende, collapse = ", ")))
  }
  
  
  # Check if dependent variable exists
  if (missing(abhVar) || !(abhVar %in% names(df))) {
    stop("Fehler: 'abhVar' fehlt oder existiert nicht im Data-Frame")
  }

  
  # Convert dependent variable to numeric if necessary
  if (!is.numeric(df[[abhVar]])) {
    warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", abhVar))
    df[[abhVar]] <- as.numeric(as.character(df[[abhVar]]))
    if (any(is.na(df[[abhVar]]))) {
      stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden.", abhVar))
    }
  }
  
   # Convert independent variables to numeric or factor
  for (var in basis_variablen) {
    if (!is.numeric(df[[var]]) && !is.factor(df[[var]])) {
      warning(sprintf("Hinweis: '%s' ist weder numerisch noch ein Faktor. Versuch automatische Umwandlung.", var))
      df[[var]] <- as.numeric(as.character(df[[var]]))
      if (any(is.na(df[[var]]))) {
        stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden.", var))
      }
    }
  }
  
  # Convert list to character vector if necessary
  if (is.list(unabhVar_list)) {
    unabhVar_list <- unlist(unabhVar_list)
  }
  
  
  # ------------ INITIALIZE RESULT LIST ------------
  result <- list()
  
  
  result$multilineare_regression <- NA
  result$summary_multilineare_regression <- NA
  result$multilineare_regression_residuals <- NA
  
 
  result$shapiro_test <- NA
  result$multilineare_regression_residuals_normalverteilung <- NA
  
  
  result$breusch_pagan_test <- NA
  result$homoskedazitaet <- NA
  
  
  result$korrelationsmatrix <- NA
  result$hohe_korrelation_name <- NA
  result$plot_korrelationsmatrix <- NA
  
  
  result$multikollinearitaet <- NA
  result$problematische_vif_variablen <- NA_character_
  result$vif_warnung <- NA
  result$plot_vif <- NA
  vif_values <- NULL
  
  
  result$plot_qq_resid <- NA
  result$plot_resid_vs_fitted <- NA
  result$plot_histogramm_residuen <- NA
  result$plot_scale_location <- NA
  
  
  result$plot_influencePlot <- NA
  
  
  result$plot_interaktion_list <- list()
  result$plot_interaktion <- NA
  result$plot_interactions_faceted <- NA
  
   
  result$plot_johnson_neyman_list <- list()
  
  result$plot_konfint <- NA
  
  
  
  # ------------ FIT MULTIPLE LINEAR REGRESSION MODEL ------------
  formel <- as.formula(paste(abhVar, '~', paste(unabhVar_list, collapse = " + ")))
  
  multiple_regression <- lm(formel, data = df)
  result$multilineare_regression <- multiple_regression
  result$summary_multilineare_regression <- summary(result$multilineare_regression)
  
  
  # ------------ NORMALITY TEST OF RESIDUALS (SHAPIRO-WILK) ------------
  result$multilineare_regression_residuals <- multiple_regression$residuals
  shapiro_wilk_result <- shapiro.test(result$multilineare_regression_residuals)
  result$shapiro_test <- shapiro_wilk_result
  result$multilineare_regression_residuals_normalverteilung <- shapiro_wilk_result$p.value >= signifikanzniveau_alpha
  
  
  # ------------ HOMOSCEDASTICITY TEST (BREUSCH-PAGAN) ------------
  bp_test <- lmtest::bptest(result$multilineare_regression)
  result$breusch_pagan_test <- bp_test
  result$homoskedazitaet <- bp_test$p.value >= signifikanzniveau_alpha
  
  # ------------ CORRELATION MATRIX ------------
  reduced_data <- df[, basis_variablen]
  reduced_data <- Filter(function(x) is.numeric(x) && sd(x, na.rm = TRUE) > 0, reduced_data)
  
  if (length(reduced_data) >= 2) {
    korrelationsmatrix <- cor(as.data.frame(reduced_data), use = "pairwise.complete.obs")
    result$korrelationsmatrix <- korrelationsmatrix
    
    # Extract strongly correlated variable pairs
    kor_pairs <- which(abs(korrelationsmatrix) >= 0.8 & abs(korrelationsmatrix) < 1, arr.ind = TRUE)
    kor_names <- apply(unique(t(apply(kor_pairs, 1, sort))), 1, function(i) {
      paste0(colnames(korrelationsmatrix)[i[1]], " & ", colnames(korrelationsmatrix)[i[2]])
    })
    
    result$hohe_korrelation_name <- kor_names
    result$plot_korrelationsmatrix <- ggcorrplot::ggcorrplot(korrelationsmatrix, hc.order = TRUE, type = "lower", lab = TRUE)
  } else {
    result$korrelationsmatrix <- NA
    result$hohe_korrelation_name <- character(0)
    result$plot_korrelationsmatrix <- NA
  }
  
  
  
  
  # ------------ MULTICOLLINEARITY TEST (VIF) ------------
  vif_values <- tryCatch({
    vif_temp <- car::vif(multiple_regression)
    if (length(vif_temp) > 0) vif_temp else NULL
  }, error = function(e) {
    warning("VIF konnte nicht berechnet werden: ", conditionMessage(e))
    NULL
  })
  
  result$multikollinearitaet <- vif_values
  result$problematische_vif_variablen <- if (!is.null(vif_values)) names(vif_values[vif_values >= 5]) else NA_character_
  result$vif_warnung <- if (!is.null(vif_values)) any(vif_values >= 10) else FALSE
  
  # ------------ VIF-PLOT ------------
  if (!is.null(vif_values)) {
    if (is.matrix(vif_values)) {
      vif_df <- data.frame(variable = rownames(vif_values), VIF = as.numeric(vif_values[, 1]))
    } else if (is.vector(vif_values)) {
      vif_df <- data.frame(variable = names(vif_values), VIF = as.numeric(vif_values))
    } else {
      vif_df <- data.frame()
    }
    
    result$plot_vif <- ggplot2::ggplot(vif_df, ggplot2::aes(x = reorder(variable, -VIF), y = VIF, fill = VIF)) +
      ggplot2::geom_col(width = 0.6) +
      ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "orange", linewidth = 1) +
      ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
      ggplot2::scale_fill_gradient(low = "lightblue", high = "steelblue") +
      ggplot2::labs(title = "Variance Inflation Factor (VIF)", x = "Unabhängige Variable", y = "VIF-Wert", fill = "VIF") +
      ggplot2::theme_minimal()
  } else {
    result$multikollinearitaet <- NA
    result$problematische_vif_variablen <- NA_character_
    result$vif_warnung <- FALSE
    result$plot_vif <- NA
  }
  
  
  # ------------ ADDITIONAL DIAGNOSTIC PLOTS ------------
  
  # QQ Plot of residuals
  resid_df <- data.frame(resid = multiple_regression$residuals)
  result$plot_qq_resid <- ggplot2::ggplot(resid_df, ggplot2::aes(sample = resid)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Wahrscheinlichkeitsnetz für Normalverteilung (Q-Q Plot)",
                  x = "Theoretische Quantile",
                  y = "Residuen") +
    ggplot2::theme_minimal()
  
  
  
  #Residuals vs Fitted plot
  plot_df <- data.frame(Fitted = multiple_regression$fitted.values, Residuals = multiple_regression$residuals)
  result$plot_resid_vs_fitted <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Fitted, y = Residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(title = "Residuen vs Fitted",
                  x = "Fitted Values", y = "Residuen") +
    ggplot2::theme_minimal()
  
  #correlation matrix
  if (!is.null(result$korrelationsmatrix) && !all(is.na(result$korrelationsmatrix))) {
    plot_km <- ggcorrplot::ggcorrplot(result$korrelationsmatrix, hc.order = TRUE, type = "lower", lab = TRUE)
    result$plot_korrelationsmatrix <- plot_km
  } else {
    result$plot_korrelationsmatrix <- NA
  }
  
  
  #Influence Plot
  result$plot_influencePlot <- function() {
    car::influencePlot(multiple_regression)
  }
  
  
  
  #VIF
  if (length(vif_values) == 0 || is.null(vif_values)) {
    warning("VIF konnte nicht berechnet werden oder ergibt keine gültigen Werte.")
    vif_df <- data.frame()
  } else if (is.matrix(vif_values)) {
    vif_numeric <- as.numeric(vif_values[, 1])
    vif_df <- data.frame(variable = rownames(vif_values), VIF = vif_numeric)
  } else if (is.vector(vif_values)) {
    vif_df <- data.frame(variable = names(vif_values), VIF = as.numeric(vif_values))
  } else {
    warning("Unerwarteter VIF-Datentyp – keine VIF-Visualisierung möglich.")
    vif_df <- data.frame()
  }
  

  
  result$plot_vif <- ggplot2::ggplot(vif_df, ggplot2::aes(x = reorder(variable, - VIF), y = VIF, fill = VIF)) +
    ggplot2::geom_col(width =  0.6) +
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "orange", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
    ggplot2::scale_fill_gradient(low = "lightblue", high = "steelblue") +
    ggplot2::labs(title = "Variance Inflation Factor (VIF)",x = "Unabhängige Variable", y = "VIF-Wert", fill = "VIF") + #Markierung nochmal checken (x-Achse)
    
    ggplot2::theme_minimal()
  
  
  # Histogram of residuals (Freedman-Diaconis binning)
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
  
  
  #Ineraction plot 
  interaktionen <- grep("\\*", unabhVar_list, value = TRUE) #RegEx - Trennung durch *
  
  interaktion_plots <- list()
  
  for (term in interaktionen) {
    vars_plot <- unlist(strsplit(term, "\\*"))
    if (length(vars_plot) == 2) {
      x_var <- vars_plot[1]
      group_var <- vars_plot[2]
      
      p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]],
                                            y = .data[[abhVar]],
                                            color = .data[[group_var]],
                                            group = .data[[group_var]])) +
        ggplot2::stat_summary(fun = mean, geom = "point", size = 3) +
        ggplot2::stat_summary(fun = mean, geom = "line") +
        ggplot2::labs(title = paste("Interaktion:", term), x = x_var, y = abhVar) +
        ggplot2::theme_minimal()
      
      
      interaktion_plots[[term]] <- p
    }
  }
  
  result$plot_interaktion_list <- interaktion_plots
  
  
  interaktionen <- grep("\\*", unabhVar_list, value = TRUE)
  mindestens_ein_faktor <- any(sapply(unlist(strsplit(interaktionen, "\\*")), function(var) is.factor(df[[var]])))
  
  
  if (length(interaktionen) == 1 && mindestens_ein_faktor) {
    vars_plot <- unlist(strsplit(interaktionen[1], "\\*"))
    if (length(vars_plot) == 2) {
      var1 <- vars_plot[1]
      var2 <- vars_plot[2]
      
      df$x_var <- df[[var1]]
      df$group_var <- df[[var2]]
      
      mean_df <- aggregate(df[[abhVar]],
                           by = list(x = df$x_var, group = df$group_var),
                           FUN = mean, na.rm = TRUE)
      names(mean_df)[3] <- abhVar
      
      result$plot_interaktion <- ggplot2::ggplot(mean_df,
                                                 ggplot2::aes(x = x, y = get(abhVar), color = group, group = group)) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::labs(
          title = paste("Interaktion:", var1, "*", var2),
          x = var1,
          y = abhVar,
          color = var2
        ) +
        ggplot2::theme_minimal()
    }
    
  } else if (length(interaktionen) > 1 && mindestens_ein_faktor) {
    plot_data <- data.frame()
    
    for (term in interaktionen) {
      vars_plot <- unlist(strsplit(term, "\\*"))
      if (length(vars_plot) == 2) {
        df$temp_x <- df[[vars_plot[1]]]
        df$temp_group <- df[[vars_plot[2]]]
        df$temp_interaktion <- term
        plot_data <- rbind(plot_data, df[, c("temp_x", "temp_group", abhVar, "temp_interaktion")])
      }
    }
    
    result$plot_interactions_faceted <- ggplot2::ggplot(plot_data,
                                                        ggplot2::aes(x = temp_x, y = .data[[abhVar]], color = temp_group, group = temp_group)) +
      ggplot2::stat_summary(fun = mean, geom = "point") +
      ggplot2::stat_summary(fun = mean, geom = "line") +
      ggplot2::facet_wrap(~ temp_interaktion) +
      ggplot2::labs(title = "Mehrere Interaktionen im Vergleich", x = "x", y = abhVar) +
      ggplot2::theme_minimal()
  }
  
  
  
  #Johnson-Neyman-plot
  interaktionen <- grep("\\*", unabhVar_list, value = TRUE)
  result$plot_johnson_neyman_list <- list()
  
  for (term in interaktionen) {
    vars_inter <- unlist(strsplit(term, "\\*"))
    
    sind_numerisch <- all(sapply(vars_inter, function(var) is.numeric(df[[var]])))
    
    if (length(vars_inter) == 2 && sind_numerisch) {
      plot_jn <- interactions::johnson_neyman(model = multiple_regression,
                                              pred = vars_inter[1],
                                              modx = vars_inter[2],
                                              alpha = signifikanzniveau_alpha,
                                              plot = TRUE)
      result$plot_johnson_neyman_list[[term]] <- plot_jn$plot
    } else {
      result$plot_johnson_neyman_list[[term]] <- paste("Kein Johnson-Neyman-Plot möglich für", term, 
                                                       "– mindestens ein Term ist kein metrischer Prädiktor.")
    }
  }
  
  
  
  #CI-plot
  confint_df <- as.data.frame(confint(multiple_regression, level = 1 - signifikanzniveau_alpha))
  confint_df$Estimate <- coef(multiple_regression)
  confint_df$Variable <- rownames(confint_df)
  names(confint_df)[1:2] <- c("Lower", "Upper")
  
  result$plot_konfint <- ggplot2::ggplot(confint_df, ggplot2::aes(y = reorder(Variable, Estimate), x = Estimate)) +
    ggplot2::geom_point(color = "steelblue", size = 3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = Lower, xmax = Upper), height = 0.2, color = "steelblue") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    ggplot2::labs(
      title = paste0("Konfidenzintervalle der Regressionskoeffizienten (", round((1 - signifikanzniveau_alpha) * 100), "%)"),
      x = "Schätzwert (mit KI)",
      y = "Variable"
    ) +
    ggplot2::theme_minimal()
  
  
  return(result)
}
