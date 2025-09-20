library(semPlot)

# Nur das Messmodell definieren, orthogonal = TRUE macht den Rest
simple_model <- '
  O =~ of3b + of1 + of2 + of5
  C =~ cf2b + cf3b + cf3 + cf5
  E =~ ef3b + ef2 + ef4 + ef5
  A =~ af1b + af1 + af3 + af6
  N =~ nf1 + nf4 + nf6
'

orthogonal_fit <- cfa(simple_model, data = data_aggregated, 
                      orthogonal = TRUE)

fitmeasures(orthogonal_fit, c("chisq", "df", "pvalue", "cfi", "tli", 
                       "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

semPaths(sem_fit, "std", layout = "tree", 
         curvePivot = TRUE, fade = FALSE)