# Bibliographic Coupling

## Import Spreadsheets into RStudio

Use the script below to import the two spreadsheets with the target researchers and the potential collaborators.

```r
install.packages("readxl")

# Upload the publications for the target researchers and potential collaborators, with the file paths separated with a comma
data_folder <- ("path_to_target_researchers.xlsx", "path_to_potential_collaborators.xlsx"
)

# Replace `target_researchers.xlsx` with the name of the target researchers file
cohort <- readxl::read_excel(
  path = file.path(data_folder, "target_researchers.xlsx"),
  sheet = "Cohort-csv",
  col_types = "text"
)

# Replace `potential_collaborators.xlsx` with the name of the potential collaborators file
campus <- readxl::read_excel(
  path = file.path(data_folder, "potential_collaborators.xlsx"),
  sheet = 1,
  col_types = "text"
)

c(cohort_rows = nrow(cohort), campus_rows = nrow(campus))
```
