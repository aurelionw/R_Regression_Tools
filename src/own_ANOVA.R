#' Simple ANOVA Function with Automatic Test Selection and Visual Output
#'
#' This function automatically performs a one-way analysis of variance (ANOVA) or Kruskal-Wallis test, depending on the results of normality and homogeneity tests.
#' It supports the following decision logic:
#' 
#' - **Case 1:** Non-normal distribution ⟶ Kruskal-Wallis test + Dunn post-hoc
#' - **Case 2:** Normal distribution with equal variances ⟶ ANOVA + Tukey HSD
#' - **Case 3:** Normal distribution with unequal variances ⟶ ANOVA + Games-Howell
#' 
#' The function also returns multiple publication-ready plots, including:
#' - Q–Q plot of residuals
#' - Grouped boxplot
#' - Error bar plot (Mean ± SE)
#' - Simultaneous confidence intervals (Tukey, Dunn, or Games-Howell)
#' 
#' All tests and plots are executed automatically based on the data structure.
#'
#' @param df A data frame containing the observations and grouping variable.
#' @param abhVar Name (character) of the dependent variable.
#' @param gruppVar Name (character) of the grouping variable.
#' @param conf.level Confidence level used for Tukey/Games-Howell intervals (default: 0.95).
#'
#' @return A list containing:
#' \item{shapiro_test}{Result of the Shapiro-Wilk test.}
#' \item{normalverteilung}{Boolean indicating whether residuals are normally distributed.}
#' \item{varianzhomogenitaet}{Boolean indicating whether equal variances are present (Levene test).}
#' \item{test}{Character string specifying which test was used ("Kruskal-Wallis", "ANOVA + varianzhomogen", "ANOVA + varianzinhomogen").}
#' \item{posthoc}{Post-hoc test used ("Dunn", "Tukey HSD", "Games-Howell").}
#' \item{posthoc_ergebnisse}{Result of the post-hoc analysis.}
#' \item{plot_qq}{ggplot object: Q–Q plot of residuals.}
#' \item{plot_boxplot}{ggplot object: Grouped boxplot of the dependent variable.}
#' \item{plot_fehlerbalkendiagramm}{ggplot object: Error bar plot (Mean ± SE).}
#' \item{plot_simultane95kis}{ggplot object: 95% simultaneous confidence intervals.}
#'
#' @author Aurélio Nwamusse <aurelio.nwamusse@stud.h-da.de>, Hochschule Darmstadt
#'
#' @examples
#' # Example usage:
#' # result <- aurelio_einfach_anova(df = my_data, abhVar = "score", gruppVar = "group")
#'
#' @import ggplot2 dplyr userfriendlyscience FSA ggpubr car
#'
#' @export


aurelio_einfach_anova <- function(df, abhVar, gruppVar, conf.level = 0.95) {
  
  # ------------ Load required packages locally ------------
  requireNamespace("ggplot2")
  requireNamespace("dplyr")
  requireNamespace("userfriendlyscience")
  requireNamespace("FSA")
  requireNamespace("ggpubr")
  requireNamespace("car")
  

  # ------------ validate function inputs ------------
  
  #Check if 'df' is a data frame
  if (!is.data.frame(df)) {
    stop("Fehler: 'df' muss ein Data-Frame sein ")
  }
  
  #Check if dependent variable is provided and exists in the data frame
  if (missing(abhVar) || !(abhVar %in% names(df))) {
    stop("Fehler: 'abhVar' fehlt oder existiert nicht im Data-Frame")
  }
  
  #Check if grouping variable is provided and exists in the data frame
  if (missing(gruppVar) || !(gruppVar %in% names(df))) {
    stop("Fehler: 'gruppVar' fehlt oder existiert nicht im Data-Frame")
  }
  
  # Check if grouping variable is a factor; convert if necessary
  if (!is.factor(df[[gruppVar]])) {
    warning("Hinweis: 'gruppVar' ist kein Faktor und wird automatisch in ein Faktor umgewandelt")
    df[[gruppVar]] <- as.factor(df[[gruppVar]])
  }
  
  # Ensure that the dependent variable is numeric; try to convert if not
  if (!is.numeric(df[[abhVar]])) {
    warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", abhVar))
    df[[abhVar]] <- as.numeric(as.character(df[[abhVar]]))
    
    # Re-check if conversion was successful
    if (any(is.na(df[[abhVar]]))) {
      stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden. Enthält nicht-numerische Werte.", abhVar))
    }
  }
  
  
  formel <- as.formula(paste(abhVar, "~", gruppVar)) 
  
  # ------------ Initialize result list ------------
  result <- list()
  result$shapiro_test <- NA
  result$anova <- NA
  result$normalverteilung <- NA
  result$varianzhomogenitaet <- NA
  result$levene_test <- NA
  result$test <- NA_character_
  result$posthoc <- NA_character_
  result$posthoc_ergebnisse <- NA
  result$ergebnisse <- NA
  
  result$plot_qq <- NA
  result$plot_boxplot <- NA
  result$plot_fehlerbalkendiagramm <- NA
  result$plot_simultane95kis <- NA
  
  
  
  #Create ANOVA model
  anova_model <- aov(formel, data = df)
  result$anova <- anova_model
  
  
  # ------------ Test for normality (Shapiro-Wilk) ------------
  
  #Shapiro-Wilk
  shapiro_wilk_result <- shapiro.test(resid(anova_model))
  result$shapiro_test <- shapiro_wilk_result
  result$normalverteilung <- shapiro_wilk_result$p.value >= 0.05
  
  #**Case 1:** Non-normal distribution ⟶ Kruskal-Wallis test + Dunn post-hoc
  if (!result$normalverteilung) {
    result$test <- "Kruskal-Wallis"
    result$ergebnisse <- kruskal.test(formel, data = df)
    result$posthoc <- "Dunn (Bonferoni)"
    result$posthoc_ergebnisse <- FSA:: dunnTest(formel, data = df, method = "bonferroni")
    #return(result)
  }
  
  
  
  if (result$normalverteilung == TRUE) {

    # ------------ Homogenity of variance (Levene-Test) ------------
    levene_result <- car:: leveneTest(formel, data = df)
    result$levene_test <- levene_result
    result$varianzhomogenitaet <- levene_result$`Pr(>F)`[1] >= 0.05
    
     # Case 2: Normal + homogeneous variance -> ANOVA + Tukey HSD 
    if(result$varianzhomogenitaet) {
      result$test = "ANOVA + varianzhomogen"
      result$ergebnisse <- summary(anova_model)
      result$posthoc <- "Tukey HSD"
      tukey <- TukeyHSD(anova_model, conf.level = conf.level)
      result$posthoc_ergebnisse <- tukey
    }
    
    
    # Case 3: Normal + heteroscedasticity -> ANOVA + Games-Howell
    if(!result$varianzhomogenitaet) {
      result$test = "ANOVA + varianzinhomogen"
      result$ergebnisse <- summary(anova_model)
      result$posthoc <- "Games-Howell"
      
      y <- df[[abhVar]]
      x <- df[[gruppVar]]
      
      games_howell <- userfriendlyscience::oneway(x = as.factor(x), y = y, posthoc = "games-howell")
      result$posthoc_ergebnisse <- games_howell
  }
  }

  
  # ------------ Descriptive statistics and plots ------------
  
 # Q-Q Plot of residuals to visualize normality
  result_df <- data.frame(resid = resid(anova_model))
  result$plot_qq <- ggplot2::ggplot(result_df, ggplot2::aes(sample = resid)) + 
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Wahrscheinlichkeitsnetz für Normalverteilungen (Q-Q Plot)", 
         x = "Theoretische Quantile", y = "Residuen")
  
  
  # Boxplot of dependent variable across groups
  result$plot_boxplot <- ggplot2::ggplot(df, ggplot2::aes(x= .data[[gruppVar]], y = .data[[abhVar]])) +
    ggplot2::geom_boxplot(fill = "lightblue", color = "black") +
    ggplot2::labs(title = "Boxplot je Gruppe",
                  x = gruppVar, y = abhVar) +
    ggplot2::theme_minimal()
  
  if (result$test == "Kruskal-Wallis") return(result)
  
  # If Kruskal-Wallis was used, return result early (no CI plots)
  #if (result$test != "Kruskal-Wallis") {
    
   # Mean ± SE Plot (error bars)
    summary_stats <- df %>%
      dplyr::group_by(.data[[gruppVar]]) %>%
      dplyr::summarise(
        mean = mean(.data[[abhVar]], na.rm = TRUE),
        se = sd(.data[[abhVar]], na.rm = TRUE) / sqrt(dplyr::n())
      )
    
    plot_title_se <- sprintf("Fehlerbalkendiagramm (Mean ± SE, %s)", result$posthoc)
    
    # 3. Plot erstellen
    result$plot_fehlerbalkendiagramm <- ggplot2::ggplot(summary_stats, ggplot2::aes(x = .data[[gruppVar]], y = mean)) +
      ggplot2::geom_point(size = 3) +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
      ggplot2::labs(
        title = plot_title_se,
        x = gruppVar,
        y = sprintf("Mittelwert von %s", abhVar)
      ) +
      ggplot2::theme_minimal()
  #}
  


  
  
  # Simultaneous 95% Confidence Intervals (Tukey or Games-Howell)
  plot_title_ci <- sprintf("Simultane 95%%-KIs (%s)", result$posthoc)
  
  # Prepare data depending on post-hoc test
  if (result$posthoc == "Tukey HSD") {
    plot_df <- as.data.frame(result$posthoc_ergebnisse[[gruppVar]])
    plot_df$comparison <- rownames(plot_df)
    names(plot_df)[names(plot_df) == "lwr"] <- "ci.lo"
    names(plot_df)[names(plot_df) == "upr"] <- "ci.hi"
  } else if (result$posthoc == "Games-Howell") {
    plot_df <- as.data.frame(result$posthoc_ergebnisse$comparisons)
    plot_df$comparison <- rownames(plot_df)
  }
  
  # Create CI plot
  result$plot_simultane95kis <- ggplot2::ggplot(plot_df, ggplot2::aes(y = comparison, x = diff)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci.lo, xmax = ci.hi), height = 0.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = plot_title_ci,
                  x = "Differenz der Mittelwerte", y = "Gruppenvergleich") +
    ggplot2::theme_minimal()


  return(result)
}
