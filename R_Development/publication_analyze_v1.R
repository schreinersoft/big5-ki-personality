library(tidyverse)
library(psych)
library(lavaan)
library(knitr)
library(semTools)
library(corrplot)
library(patchwork)
library(purrr)

source("connect_database.R")
source("functions.R")
source("BFI-2-Names-EN.R")

# combine datasets
essays <- tbl(con, "essays") %>% select(-text, -author) %>% collect()
openai <- tbl(con, "openai_analyzation") %>% 
  select(-updated_at) %>% 
  collect()
# join only rows with values
openai_joined <- left_join(essays, openai, by = c("id" = "essay_id")) %>% 
  collect() %>% 
  drop_na(of1)

o_facets <- paste0("of", 1:3)
c_facets <- paste0("cf", 1:3)
e_facets <- paste0("ef", 1:3)
a_facets <- paste0("af", 1:3)
n_facets <- paste0("nf", 1:3)

#e_facets <- e_facets[e_facets != "ef2"]  # schritt 2
#e_facets <- e_facets[e_facets != "ef1"]  # schritt 3

all_facets <- c(o_facets, c_facets, e_facets, a_facets, n_facets)
all_names <- facet_names[all_facets]
facet_list <- list(o_facets, c_facets, e_facets, a_facets, n_facets)



# Cronbachs alpha der Facetten
for (facets in facet_list) {
  alpha <- openai_joined %>% 
    select(all_of(facets)) %>% 
    as_tibble() %>% 
    psych::alpha()
  print(alpha)
}


cor_matrix <- cor(openai_joined[, all_facets], use = "complete.obs")
# Round to 2 decimal places
cor_matrix_rounded <- round(cor_matrix, 2)
print(cor_matrix_rounded)
corrplot(cor_matrix_rounded, method = "color", type = "upper", 
         addCoef.col = "black", tl.cex = 0.8)
heatmap(cor_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100),
        main = "Correlation Matrix")



facets <- openai_joined %>% 
  select(all_of(all_facets)) %>% 
  drop_na() %>% 
  as_tibble()


# Faktorenanalyse
model <- '
  Ofactor =~ of1 + of2 + of3
  Cfactor =~ cf1 + cf2 + cf3
  Efactor =~ ef3
  Afactor =~ af1 + af2 + af3
  Nfactor =~ nf1 + nf2 + nf3
'



#fit <- cfa(model, data = facets, 
#           estimator = "GLS")
fit <- cfa(model, data = facets, 
           estimator = "ML")
           #,
           #se="bootstrap",
           #bootstrap = 2000) # see CFA.md

summary(fit, fit.measures = TRUE, standardized = TRUE)
parameterEstimates(fit)
standardizedSolution(fit)  # -1... +1
reliability(fit) 
inspect(fit, "cor.lv")
modificationIndices(fit, sort = TRUE)
residuals(fit, type = "standardized")

# PCA
pcModel <- principal(facets, nfactors = 5, rotate = "varimax")
pcModel

pcModel$loadings


scree_data <- data.frame(
  Component = 1:length(pcModel$values),
  Eigenvalue = pcModel$values
)

# Scree Plot !
screePlot <- ggplot(scree_data, aes(x = Component, y = Eigenvalue)) +
  geom_point(size = 3) +
  geom_line() +
  labs(
    title = "Scree Plot",
    x = "Primärkomponente",
    y = "Eigenwert"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 1:length(pcModel$values))
screePlot
ggsave("graphics/screeplot_v1.png", plot = screePlot, dpi=300, width = 8, height = 5)


# Eigenvalues und Varianzaufklärung
eigenvalues <- pcModel$values[1:5]
var_explained <- eigenvalues / sum(eigenvalues) * 100
cumvar_explained <- cumsum(var_explained)

# Übersichtstabelle
summary_pca <- data.frame(
  Faktor = paste("Faktor", 1:5),
  Eigenvalue = round(eigenvalues, 3),
  Varianz_Prozent = round(var_explained, 2),
  Kumulative_Varianz = round(cumvar_explained, 2)
)

summary_table <- summary_pca %>%
  flextable() %>%
  set_header_labels(
    Faktor = "Faktor",
    Eigenvalue = "Eigenwert",
    Varianz_Prozent = "Varianz (%)",
    Kumulative_Varianz = "Kumulativ (%)"
  ) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  # Faktoren mit Eigenwert > 1 hervorheben
  bg(i = ~ Eigenvalue > 1.0, bg = "#f0f9ff") %>%
  autofit() %>%
  align(j = 2:4, align = "center", part = "all")

save_as_docx(summary_table, path = "pca_summary.docx")
print(summary_table)
