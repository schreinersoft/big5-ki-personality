library(officer)
### Collector for all models, data and analyzations

#root folder for graphics
root_folder <- "C:/Users/Bernd Schreiner/OneDrive/@@@APOLLON/@@Thesis KI/Auswertungen"
source("sources/output_folders.R")

# root folder for supplement
root_folder <- "supplement"


source("sources/connect_database.R")
source("sources/graphics_functions.R")
source("sources/tables_functions.R")
source("sources/transformation_functions.R")
source("sources/supplement_functions.R")
source("sources/output_folders.R")

source("sources/combined_names_EN_DE.R")


create_dir <- function(new_dir) {
  ifelse(!dir.exists(file.path(new_dir)),
         dir.create(file.path(new_dir)),
         "Directory Exists")
}

supp_format <- function(ft){
  result <- ft%>% 
    fontsize(size = 9, part = "all") %>%
    color(color = "black", part="all")
  return (result)
}

publish_essays_histograms <- function() {
  supplement_output_folder <- paste(root_folder, "/", measurement_version, sep="")
  create_dir(supplement_output_folder)

  doc <- body_add_par(doc, "Statistiken Essay 42", style = "Zwischenüberschrift")
  ft_essay_42 <- supp_analyse_essay_item(data, measurement_version, 42)
  doc <- body_add_flextable(doc, supp_format(ft_essay_42))
  
  doc <- body_add_par(doc, "Histogramme Essay 42", style = "Zwischenüberschrift")
  filename <- paste(supplement_output_folder,"/histograms_essay_42.jpg",sep="")
  gg_essay_42 <- create_essay_histograms(data, measurement_version, 42)
  width <- 130
  height <- 170
  ggsave(filename=filename, plot=gg_essay_42, dpi=300, width=width, height=height, units = "mm")
  doc <- body_add_img(doc, src = filename, width=width, height=height, unit = "mm")
  
}

publish_all <- function() {
  supplement_output_folder <- paste(root_folder, "/", measurement_version, sep="")
  create_dir(supplement_output_folder)
  
  ft_factors <- supp_analyze_factors(data_aggregated, measurement_version)
  ft_facets <- supp_analyze_facets(data_aggregated, measurement_version)
  ft_loadings <- supp_analyze_factor_loadings(data_aggregated, measurement_version)
  ft_factor_correlations <- supp_analyze_factor_correlations(data_aggregated, measurement_version)
  ft_facet_correlations <- supp_analyze_facet_correlations(data_aggregated, measurement_version)
  gg_screeplot <- supp_analyze_screeplot(data_aggregated, measurement_version)
  gg_factors <- create_factor_densities(data_aggregated, measurement_version)
  gg_facets <- create_facet_densities(data_aggregated, measurement_version)
  

  doc <- body_add_par(doc, "Faktorstatistiken und Verteilungen", style = "Zwischenüberschrift")
  doc <- body_add_flextable(doc, supp_format(ft_facets))
  width <- 180
  height <- 120
  filename <- paste(supplement_output_folder,"/factor_densities.jpg",sep="")
  ggsave(filename=filename, plot=gg_factors, dpi=300, width=width, height=height, units = "mm")
  doc <- body_add_img(doc, src = filename, width=width, height=height, unit = "mm")
  
  
  doc <- body_add_par(doc, "Facettenstatistiken und Verteilungen", style = "Zwischenüberschrift")
  doc <- body_add_par(doc, "K-S : Kolmogorov-Smirnov-Test, S-W : Shapiro-Wilk-Test", style="annotation text")
  doc <- body_add_flextable(doc, supp_format(ft_facets))
  width <- 180
  height <- 240
  filename <- paste(supplement_output_folder,"/facet_densities.jpg",sep="")
  ggsave(filename=filename, plot=gg_facets, dpi=300, width=width, height=height, units = "mm")
  doc <- body_add_img(doc, src = filename, width=width, height=height, unit = "mm")
  
  doc <- body_add_par(doc, "Faktorkorrelationen", style = "Zwischenüberschrift")
  doc <- body_add_flextable(doc, supp_format(ft_factor_correlations))
  
  doc <- body_add_par(doc, "Facettenkorrelationen", style = "Zwischenüberschrift")
  doc <- body_add_flextable(doc, supp_format(ft_facet_correlations))
  
  
  doc <- body_add_par(doc, "Faktorladungen und Kommunalitäten", style = "Zwischenüberschrift")
  doc <- body_add_flextable(doc, supp_format(ft_loadings))
  doc <- body_add_par(doc, "Scree Plot", style = "Zwischenüberschrift")
  doc <- body_add_img(doc, src = paste(supplement_output_folder, "/screeplot_", measurement_version, ".png", sep=""), width = 8, height = 5)
}




main_versions = c("v1.0",
                  "v2.0",
                  "v3.0",
                  "v4.1",
                  "v5.0")
sub_versions = c("v1.1",
                 "v2.1","v2.2",
                 "v4.1")




# Create a new Word document from template with correct paragraph styles

doc <- read_docx(paste(root_folder, "/Supplement_styletemplate.docx", sep=""))
doc <- body_remove(doc)


################################################# V1.0
measurement_version <- "v1.0"

data <- tbl(con, "openai_analyzation") %>% 
  select(-temperature) %>% 
  collect() %>% 
  rename(
    of1b = of1,
    of2b = of2,
    of3b = of3,
    cf1b = cf1,
    cf2b = cf2,
    cf3b = cf3,
    ef1b = ef1,
    ef2b = ef2,
    ef3b = ef3,
    af1b = af1,
    af2b = af2,
    af3b = af3,
    nf1b = nf1,
    nf2b = nf2,
    nf3b = nf3
  )
data_aggregated <- aggregate_model(data)

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_essays_histograms()
publish_all()


################################################# V1.1
measurement_version <- "v1.1"
data_aggregated <- data %>% 
  select(-ef1b, -af2b, -nf3b) %>% 
  aggregate_model()

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_all()


################################################# V1.2
measurement_version <- "v1.2"
data_aggregated <- data %>% 
  select(-af3b, -nf3b) %>% 
  aggregate_model()

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_all()


################################################ V1.3
measurement_version <- "v1.3"
data_aggregated <- data %>% 
  select(-cf1b, -af3b, -nf3b) %>% 
  aggregate_model()

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_all()

################################################# V2.0
measurement_version <- "v2.0"

# v2 in publication is v3 in data XXX
data <- tbl(con, "openai_analyzation_v3") %>% 
  select(-updated_at) %>%
  filter(essay_id <= 250) %>% 
  collect() %>% 
  drop_na("of1")

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_essays_histograms()
publish_all()


################################################# V2.1
measurement_version <- "v2.1"

data_aggregated <- data %>% 
  select(-of3, -of4,
         -cf1,
         -ef1, -ef3,
         -af2,
         -nf2, -nf5) %>% 
  aggregate_model()

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_all()

################################################# V2.2
measurement_version <- "v2.2"

data_aggregated <- data %>% 
  select(-of3, -of4,
         -cf1,
         -ef1, -ef3, -ef6,
         -af2,
         -nf2, -nf5) %>% 
  aggregate_model()

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_all()


################################################# V2.3
measurement_version <- "v2.3"

data_aggregated <- data %>% 
  select(-of3, -of4,
         -cf1, 
         -ef2, -ef3, -ef6, 
         -af2, 
         -nf2, -nf5) %>% 
  aggregate_model()

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_all()


################################################# V3.0
measurement_version <- "v3.0"

# DANGER !!! v2 in publication is v3 XXX
data <- tbl(con, "openai_analyzation_v2") %>% select(-updated_at) %>%
  collect() %>% 
  drop_na("of1")

data_aggregated <- data %>% 
  aggregate_model()

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_essays_histograms()
publish_all()


################################################# V4.1
measurement_version <- "v4.1"
data <- tbl(con, "google_analyzation") %>% select(-updated_at) %>%
  collect() %>% 
  drop_na("of1")

data_aggregated <- data %>% 
  filter(temperature == 0) %>% 
  aggregate_model()

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_essays_histograms()
publish_all()


################################################# V5.X
measurement_version <- "v5.X"
o_facets <- c("of3b", "of1", "of2", "of5")
c_facets <- c("cf2b", "cf3b", "cf3", "cf5")
e_facets <- c("ef2", "ef3b", "ef4", "ef5")
a_facets <- c("af1b", "af1", "af3", "af6")
n_facets <- c("nf1", "nf4", "nf6")
data_bfi <- tbl(con, "openai_analyzation") %>% 
  select(essay_id, of3, cf2, cf3, ef3, af1) %>% 
  collect() %>% 
  group_by(essay_id) %>%
  summarise(
    of3 = mean(of3, na.rm = TRUE),
    cf2 = mean(cf2, na.rm = TRUE),
    cf3 = mean(cf3, na.rm = TRUE),
    ef3 = mean(ef3, na.rm = TRUE),
    af1 = mean(af1, na.rm = TRUE),
    .groups = "drop") %>% 
  rename(of3b = of3,
         cf2b = cf2,
         cf3b = cf3,
         ef3b = ef3,
         af1b = af1,
         essay_idb = essay_id)

data_neo <- tbl(con, "openai_analyzation_v3") %>%
  select(essay_id, of1, of2, of5, cf3, cf5, ef2, ef4, ef5, af1, af3, af6, nf1, nf4, nf6) %>% 
  collect() %>% 
  group_by(essay_id) %>%
  summarise(
    of1 = mean(of1, na.rm = TRUE),
    of2 = mean(of2, na.rm = TRUE),
    of5 = mean(of5, na.rm = TRUE),
    cf3 = mean(cf3, na.rm = TRUE),
    cf5 = mean(cf5, na.rm = TRUE),
    ef2 = mean(ef2, na.rm = TRUE),
    ef4 = mean(ef4, na.rm = TRUE),
    ef5 = mean(ef5, na.rm = TRUE),
    af1 = mean(af1, na.rm = TRUE),
    af3 = mean(af3, na.rm = TRUE),
    af6 = mean(af6, na.rm = TRUE),
    nf1 = mean(nf1, na.rm = TRUE),
    nf4 = mean(nf4, na.rm = TRUE),
    nf6 = mean(nf6, na.rm = TRUE),
    .groups = "drop")
data_aggregated <- left_join(data_bfi, data_neo, by = c("essay_idb" = "essay_id")) %>%
  rowwise() %>% 
  mutate(
    o_llm = mean(c_across(all_of(o_facets)), na.rm = TRUE),
    c_llm = mean(c_across(all_of(c_facets)), na.rm = TRUE),
    e_llm = mean(c_across(all_of(e_facets)), na.rm = TRUE),
    a_llm = mean(c_across(all_of(a_facets)), na.rm = TRUE),
    n_llm = mean(c_across(all_of(n_facets)), na.rm = TRUE)
  ) %>% 
  rename(essay_id = essay_idb)



################################################# V5.0
measurement_version <- "v5.0"
data <- tbl(con, "openai_analyzation_v5") %>% 
  select(-updated_at) %>%
  filter(model=="gpt-5-mini-2025-08-07") %>% 
  filter(essay_id <= 250) %>% 
  collect()

data_aggregated <- aggregate_model(data) %>% 
  select(where(~ all(!is.na(.))))

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_essays_histograms()
publish_all()


################################################# V5.1
measurement_version <- "v5.1"
data <- tbl(con, "openai_analyzation_v5") %>% 
  select(-updated_at) %>%
  select(-af1) %>% 
  filter(model=="gpt-5-mini-2025-08-07") %>% 
  filter(essay_id <= 250) %>% 
  collect()

data_aggregated <- aggregate_model(data) %>% 
  select(where(~ all(!is.na(.))))

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_all()


################################################# Noise
measurement_version <- "noise"
noise <- tbl(con, "noise") %>% 
  select(hash) %>% 
  collect()

data_aggregated <- aggregate_model(data) %>% 
  select(where(~ all(!is.na(.))))

doc <- body_add_par(doc, paste("Datenauswertung Version ", substr(measurement_version, 2, 100), sep=""), style = "Anhang")
publish_all()



# Save the document XXX
print(doc, target = paste(root_folder, "/Supplements.docx", sep=""))
