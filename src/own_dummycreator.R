#' Dummy-Variablen erstellen.
#' 
#' Diese Funktion erzeugt Dummy-Variablen, die beispielsweise bei der Regressionsanalyse 
#' verwendet werden können 
#'
#'
#' @param df                   ... Data Frame, der die Daten für die Analyse enthält.
#' @param dummyspalte          ... Dummyspaltenname, für die Erzeugung von Dummy-Variablen
#' @param referenz_level_list  ... Die einzelnen Referenzwerte
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' @return df_aufgearbeitet

#'
#' @author Aurelio Nwamusse <aurelio.nwamusse@stud.h-da.de>, Hochschule Darmstadt
#'
#' @examples
#' data <- read.csv2("election_data.csv")
#' aurelio_multilineareregression(data, list("Growth", "Inflation"), "Vote")
#'
#' @import ggplot2

#' @export

aurelio_dummy_variable_creator <- function(df, vars, ref_levels = list(), drop_first = TRUE, prefix = TRUE) {
  for (var in vars) {
    # Faktor erzwingen
    df[[var]] <- as.factor(df[[var]])
    
    # Referenzstufe setzen, falls in ref_levels angegeben
    if (var %in% names(ref_levels)) {
      ref <- ref_levels[[var]]
      if (!(ref %in% levels(df[[var]]))) {
        stop(paste0("Referenzwert '", ref, "' ist kein Level in Variable '", var, "'"))
      }
      df[[var]] <- relevel(df[[var]], ref = ref)
    }
    
    # Dummy-Matrix ohne Intercept (also alle Levels als Spalten)
    dummy_mat <- model.matrix(~ . - 1, data = df[var])
    
    # Spaltennamen anpassen: ggf. Prefix hinzufügen
    if (prefix) {
      colnames(dummy_mat) <- gsub("^", paste0(var, "_"), colnames(dummy_mat))
    }
    
    # Drop first level wenn nötig
    if (drop_first) {
      dummy_mat <- dummy_mat[, -1, drop = FALSE]
    }
    
    # Kombinieren und Originalvariable entfernen
    df <- cbind(df, dummy_mat)
    df[[var]] <- NULL
  }
  return(df)
}