library(officer)

############# find out style names
doc <- read_docx("C:/Users/Bernd Schreiner/OneDrive/@@@APOLLON/@@Thesis KI/Thesis_Anhänge.docx")

# Get all available styles
styles <- styles_info(doc)

# View all paragraph styles
paragraph_styles <- styles[styles$style_type == "paragraph",]
print(paragraph_styles$style_id)

# Or view just the style names
paragraph_style_names <- paragraph_styles$style_name
print(paragraph_style_names)








