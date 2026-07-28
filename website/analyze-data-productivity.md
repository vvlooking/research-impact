# Productivity

## Checklist

- [ ] Calculate author indexes (individual reports only)
- [ ] Calculate the number of documents published per year
- [ ] Calculate the average number of documents published per year
- [ ] Create figure: "Average Productivity per Active Year"
- [ ] Calculate the number and percentage of document types
- [ ] Calculate the number of documents by authorship order (i.e., sole, first, and last author)

## Author Indexes
Identify the researcher's i10-index using their Google Scholar profile. Calculate the researcher's h-index, g-index, and m-index using the script below. Update the file path for 'file_path_to_standardized_dataset.xlsx` and specify the researcher's name in the variable, `researcher_name <- LAST NAME INITIAL`.

```r
# Load libraries
library(bibliometrix)
library(readxl)
library(dplyr)
library(stringr)

# File location
input_file <- "file_path_to_standardized_dataset.xlsx" # Update file path to the de-deduplicated standardized dataset

# Import the Publications worksheet

publications <- read_excel(
  input_file,
  sheet = "Publications"
)

# Confirm that the required columns exist
required_columns <- c(
  "AU_key",
  "TC",
  "PY",
  "SO",
  "DI",
  "TI"
)

missing_columns <- setdiff(
  required_columns,
  names(publications)
)

if (length(missing_columns) > 0) {
  stop(
    "The workbook is missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Prepare a bibliometrix-compatible data frame

M <- publications |>
  filter(
    !is.na(TI),
    !is.na(AU_key)
  ) |>
  transmute(
    AU = AU_key |>
      str_replace_all("\\s*;\\s*", ";") |>
      str_to_upper(),

    SO = as.character(SO),
    PY = as.numeric(PY),
    TC = as.numeric(TC),
    DI = as.character(DI),
    TI = as.character(TI)
  ) |>
  mutate(
    TC = coalesce(TC, 0)
  )

# Calculate the researcher's h-index

researcher_name <- "LAST NAME INITIAL" # Add researcher's name

impact <- Hindex(
  M,
  field = "author",
  elements = researcher_name,
  sep = ";",
  years = Inf
)

# Display h-index, g-index, m-index, citations, and papers
print(impact$H)
```
