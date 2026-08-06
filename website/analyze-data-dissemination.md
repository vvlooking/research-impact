# Dissemination

## Checklist

- [ ] Export journal metrics from Journal Citation Reports and Scopus
- [ ] Calculate the number of unique journals and then identify the most common journals
- [ ] Identify journals in the top 10% of their category by JIF percentile
- [ ] Calculate the number and percentage of documents in journals by quartile (using a pivot table in Excel)



List journal impact factors
Mark journals in the top 10% of their category by JIF percentile
Calculate the number and percentage of journal quartiles (Excel pivot table)
Create figure: "Percentage of Documents Published in Journals by JIF Quartile"
Create figure: "Articles by Journal Citation Reports (JCR) Category"
Calculate JCR categories (Excel pivot table)


## Calculate the Number of Unique Journals

Calculate the number of journals in which the researcher/group has published in by running the following script. Update the file path for `path_to_standardized_file`.

```r
# Load libraries
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Load file
input_file <- "file_path_to_standardized_file" # Update file path to standardized Excel file

# Import data
publications <- read_excel(input_file) |>
  mutate(publication_row_id = row_number())

# Confirm that SO_key exists
if (!"SO_key" %in% names(publications)) {
  stop(
    "The input workbook does not contain a column named 'SO_key'."
  )
}

# Clean journal names
publication_journals <- publications |>
  mutate(
    journal = str_squish(SO_key)
  ) |>
  filter(
    !is.na(journal),
    journal != ""
  )

# Count unique journals
number_unique_journals <- publication_journals |>
  summarise(
    unique_journals = n_distinct(journal)
  ) |>
  pull(unique_journals)

message(
  "Number of unique journals: ",
  number_unique_journals
)
```

Then, to identify which journals the researcher/group publishes in most often, add the following to the end of the script:

```r
# Select top 15 journals
top_15_journals <- journal_counts |>
  slice_head(n = 15)

print(top_15_journals)
```
