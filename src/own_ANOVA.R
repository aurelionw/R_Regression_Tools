#' Einfache ANOVA mit grafischer Ausgabe
#' 
#' Die Funktion überprüft automatisch auf Normalverteilung und Varianzhomogenität 
#' und wählt den richtigen Test aus. Es werden 3 Fälle betrachtet:
#'
#' Fall 1: Nicht normalverteilt                      -> Kruskal + DUNN
#' Fall 2: Normalverteilt + Varianzhomogenit?t       -> ANOVA + Tukey HSD 
#' Fall 3: Normalverteilt + Varianzinhomogenit?t     -> ANOVA + Games-Howell                      
#' 
#' Anschließend gibt die Funktion folgende Abbildungen aus:
#' Fehlerbalkendiagramm
#' Wahrscheinlichkeitsnetz
#' Simultane 95%-Kis (nach Tukey, Dunn oder Games-Howell)
#' ... (Erweiterbar)
#' 
#' 
#' 
#' @param df          ... Data Frame mit den Beobachtungen, die die Gruppierungsvariable enthält (typeof Data Frame)
#' @param abhVar      ... Name der abh?ngigen Variable (typeof String)
#' @param gruppVar    ... Name der Gruppierungsvariable (typeof String)
#' 
#' 
#' @return Eine Liste mit:
#' \item{shapiro_test}{LIST, Ergebnis des Shapiro-Test}
#' \item{normalverteilung}{BOOL, Normalverteilung gegeben oder nicht}
#' \item{varianzhomogenität}{BOOL, Varianzhomogenität gegeben oder nicht (Levene)}
#' \item{test}{CHARACTER, Name des durchgeführten Tests (Kruskal, ANOVA + varianzhomogen, ANOVA + varianzinhomogen)}
#' \item{posthoc}{Name des Post-hoc-Tests (Dunn, Tukey HSD, Games-Howell)}
#' \item{posthoc_ergebnisse}{LIST, Ergebnisse des Post-hoc-Tests}
#' \item{plot_qq}{ggplot2-Objekt, grafische Darstellung der Quantile zweier statistischer Variablen}
#' \item{plot_boxplot}{ggplot2-Objekt, grafische Darstellung als Boxplot}
#' \item{plot_fehlerbalkendiagramm}{ggplot2-Objekt, grafische Darstellung von Mittelwerten mit den dazugehörigen Schätzintervall}
#' \item{simultane95kis}{ggplot2-Objekt, grafische Darstellung der Konfidenzintervalle}
#' 
#' 
#' @author Aurelio Nwamusse <aurelio.nwamusse@stud.h-da.de>, Hochschule Darmstadt
#' 
#' 
#' @examples
#' 
#' 
#' @import ggplot2 dplyr userfriendlyscience FSA ggpubr car  
#' 
#' 
#' @export


aurelio_einfach_anova <- function(df, abhVar, gruppVar, conf.level = 0.95) {
  
  # ------------ Lokales laden der Pakete ------------
  requireNamespace("ggplot2")
  requireNamespace("dplyr")
  requireNamespace("userfriendlyscience")
  requireNamespace("FSA")
  requireNamespace("ggpubr")
  requireNamespace("car")
  

  # ------------ ?berpr?fung der einzelnen Einaben ------------
  
  #?berpr?fung von df
  if (!is.data.frame(df)) {
    stop("Fehler: 'df' muss ein Data-Frame sein ")
  }
  
  #?berpr?fung der abh?ngigen Variable
  if (missing(abhVar) || !(abhVar %in% names(df))) {
    stop("Fehler: 'abhVar' fehlt oder existiert nicht im Data-Frame")
  }
  
  #?berpr?fung von gruppVar
  #Existenz
  if (missing(gruppVar) || !(gruppVar %in% names(df))) {
    stop("Fehler: 'gruppVar' fehlt oder existiert nicht im Data-Frame")
  }
  
  #Gruppierbarkeit bzw. Tauglichkeitstest
  if (!is.factor(df[[gruppVar]])) {
    warning("Hinweis: 'gruppVar' ist kein Faktor und wird automatisch in ein Faktor umgewandelt")
    df[[gruppVar]] <- as.factor(df[[gruppVar]])
  }
  
  # Sicherstellen, dass abhängige Variable numerisch ist
  if (!is.numeric(df[[abhVar]])) {
    warning(sprintf("Hinweis: '%s' ist nicht numerisch. Versuch automatische Umwandlung.", abhVar))
    df[[abhVar]] <- as.numeric(as.character(df[[abhVar]]))
    
    # Nochmals prüfen
    if (any(is.na(df[[abhVar]]))) {
      stop(sprintf("Fehler: '%s' konnte nicht in numerisch umgewandelt werden. Enthält nicht-numerische Werte.", abhVar))
    }
  }
  
  
  formel <- as.formula(paste(abhVar, "~", gruppVar)) 
  
  # ------------ ERGEBNISLISTE ERSTELLEN ------------
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
  
  
  
  #ANOVA-Modell erstellen
  anova_model <- aov(formel, data = df)
  result$anova <- anova_model
  
  
  # ------------ Test auf Normalvertei?ung ------------
  
  #Shapiro-Wilk
  shapiro_wilk_result <- shapiro.test(resid(anova_model))
  result$shapiro_test <- shapiro_wilk_result
  result$normalverteilung <- shapiro_wilk_result$p.value >= 0.05
  
  #Fall 1: nicht normalverteilt -> Kruskal + Dunn ABÄNDERN WEGEN PLOT
  
  if (!result$normalverteilung) {
    result$test <- "Kruskal-Wallis"
    result$ergebnisse <- kruskal.test(formel, data = df)
    result$posthoc <- "Dunn (Bonferoni)"
    result$posthoc_ergebnisse <- FSA:: dunnTest(formel, data = df, method = "bonferroni")
    #return(result)
  }
  
  
  
  if (result$normalverteilung == TRUE) {
    # ------------ VARIANZHOMOGENITAET ÜBERPRÜFEN ------------
    levene_result <- car:: leveneTest(formel, data = df)
    result$levene_test <- levene_result
    result$varianzhomogenitaet <- levene_result$`Pr(>F)`[1] >= 0.05
    
    #  Fall 2: Normalverteilt + Varianzhomogenit?t -> ANOVA + Tukey HSD 
    if(result$varianzhomogenitaet) {
      result$test = "ANOVA + varianzhomogen"
      result$ergebnisse <- summary(anova_model)
      result$posthoc <- "Tukey HSD"
      tukey <- TukeyHSD(anova_model, conf.level = conf.level)
      result$posthoc_ergebnisse <- tukey
    }
    
    
    # Fall 3: Normalverteilt + Varianzinhomogenit?t -> ANOVA + Games-Howell
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

  
  # ------------ DESKRIPTIVE STATISTIK (PLOTS) ------------
  
  # Wahrscheinlichkeitsnetz Q-Q Plot (Data-Frame - resid(anova_model))
  result_df <- data.frame(resid = resid(anova_model))
  result$plot_qq <- ggplot2::ggplot(result_df, ggplot2::aes(sample = resid)) + 
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = "red") +
    ggplot2::labs(title = "Wahrscheinlichkeitsnetz für Normalverteilungen (Q-Q Plot)", 
         x = "Theoretische Quantile", y = "Residuen")
  
  
  # Boxplot (Data-Frame)
  result$plot_boxplot <- ggplot2::ggplot(df, ggplot2::aes(x= .data[[gruppVar]], y = .data[[abhVar]])) +
    ggplot2::geom_boxplot(fill = "lightblue", color = "black") +
    ggplot2::labs(title = "Boxplot je Gruppe",
                  x = gruppVar, y = abhVar) +
    ggplot2::theme_minimal()
  
  if (result$test == "Kruskal-Wallis") return(result)
  
  # Fehlerbalkendiagramm (Tukey, Games-Howell)
  #if (result$test != "Kruskal-Wallis") {
    
    # 1. Zusammenfassen: Mittelwert + SE pro Gruppe
    summary_stats <- df %>%
      dplyr::group_by(.data[[gruppVar]]) %>%
      dplyr::summarise(
        mean = mean(.data[[abhVar]], na.rm = TRUE),
        se = sd(.data[[abhVar]], na.rm = TRUE) / sqrt(dplyr::n())
      )
    
    # 2. Dynamischer Titel mit sprintf()
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
  


  
  
  # Simultane 95%-KIs nach (Tukey, Games-Howell)
      # 1. Titel bauen
  plot_title_ci <- sprintf("Simultane 95%%-KIs (%s)", result$posthoc)
  
  # 2. Daten vorbereiten
  if (result$posthoc == "Tukey HSD") {
    plot_df <- as.data.frame(result$posthoc_ergebnisse[[gruppVar]])
    plot_df$comparison <- rownames(plot_df)
    names(plot_df)[names(plot_df) == "lwr"] <- "ci.lo"
    names(plot_df)[names(plot_df) == "upr"] <- "ci.hi"
  } else if (result$posthoc == "Games-Howell") {
    plot_df <- as.data.frame(result$posthoc_ergebnisse$comparisons)
    plot_df$comparison <- rownames(plot_df)
  }
  
  # 3. Plot
  result$plot_simultane95kis <- ggplot2::ggplot(plot_df, ggplot2::aes(y = comparison, x = diff)) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = ci.lo, xmax = ci.hi), height = 0.2) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = plot_title_ci,
                  x = "Differenz der Mittelwerte", y = "Gruppenvergleich") +
    ggplot2::theme_minimal()


  return(result)
}