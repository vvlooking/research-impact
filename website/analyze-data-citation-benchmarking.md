# Citation Benchmarking

## Checklist

- [ ] Calculate citation counts across documents
- [ ] Calculate relative citation ratio and field citation ratio across documents
- [ ] Identify top articles by field citation ratio (in Excel)
- [ ] Export, clean, and de-duplicate citing documents
- [ ] Identify top affiliations of citing documents
- [ ] Identify top countries of citing documents
- [ ] Identify top journals of citing documents
- [ ] Identify altmetric impact (e.g., book metrics, institutional repository downloads, public engagement and readership)

## Calculate Citation Counts across Documents
Calculate total citation count, mean citation count, and number of documents with at least one citation using the following script. Update the file path for `file_name`.

```r
# Load libraries
library(readr)
library(dplyr)
library(readxl)

# Read  data
df <- read_excel("file_name") # Update file path

df <- df %>%
  rename(TC = `TC...35`)
  
summary(df$TC)

nrow(df)

tc_overview <- df %>%
  summarise(
    publications_total = n(),
    citations_total = sum(TC, na.rm = TRUE),
    citations_min = min(TC, na.rm = TRUE),
    citations_median = median(TC, na.rm = TRUE),
    citations_mean = mean(TC, na.rm = TRUE),
    citations_max = max(TC, na.rm = TRUE),
    publications_with_citations = sum(TC > 0, na.rm = TRUE),
    pct_with_citations = round(mean(TC > 0, na.rm = TRUE) * 100, 1)
  )

tc_overview

tc_distribution <- df %>%
  mutate(
    bucket = case_when(
      TC == 0 ~ "0",
      TC <= 5 ~ "1–5",
      TC <= 10 ~ "6–10",
      TC <= 25 ~ "11–25",
      TRUE ~ "26+"
    )
  ) %>%
  count(bucket) %>%

  mutate(percent = round(n / sum(n) * 100, 1))

tc_distribution
```

## Calculate Field Citation Ratio and Relative Citation Ratio across Documents
Use the following script to calculate field citation ratio and relative citation ratio for available documents. Update the file path for `file_name`.

```r
# Load libraries
library(dplyr)
library(readxl)

# Read file
df <- read_excel("file_name") # Update file name

# Make sure metrics are numeric
df <- df %>%
  mutate(
    TC  = as.numeric(TC),
    RCR = as.numeric(RCR),
    FCR = as.numeric(FCR)
  )

# Overview function
metric_overview <- function(data, col, positive_threshold = 0) {
  data %>%
    summarise(
      publications_total = n(),
      metric_total   = sum({{ col }}, na.rm = TRUE),
      metric_min     = min({{ col }}, na.rm = TRUE),
      metric_median  = median({{ col }}, na.rm = TRUE),
      metric_mean    = mean({{ col }}, na.rm = TRUE),
      metric_max     = max({{ col }}, na.rm = TRUE),
      publications_with_value = sum({{ col }} > positive_threshold, na.rm = TRUE),
      pct_with_value = round(mean({{ col }} > positive_threshold, na.rm = TRUE) * 100, 1)
    )
}

# Distribution function
metric_distribution <- function(data, col, type = c("tc", "ratio")) {
  type <- match.arg(type)

  if (type == "tc") {
    data %>%
      mutate(
        bucket = case_when(
          is.na({{ col }}) ~ "Missing",
          {{ col }} == 0 ~ "0",
          {{ col }} <= 5 ~ "1–5",
          {{ col }} <= 10 ~ "6–10",
          {{ col }} <= 25 ~ "11–25",
          TRUE ~ "26+"
        )
      ) %>%
      count(bucket) %>%
      mutate(percent = round(n / sum(n) * 100, 1))
  } else {
    data %>%
      mutate(
        bucket = case_when(
          is.na({{ col }}) ~ "Missing",
          {{ col }} == 0 ~ "0",
          {{ col }} < 1 ~ "<1.0",
          {{ col }} < 2 ~ "1.0–<2.0",
          {{ col }} < 5 ~ "2.0–<5.0",
          {{ col }} < 10 ~ "5.0–<10.0",
          TRUE ~ "10+"
        )
      ) %>%
      count(bucket) %>%
      mutate(percent = round(n / sum(n) * 100, 1))
  }
}

# TC
tc_overview <- metric_overview(df, TC)
tc_distribution <- metric_distribution(df, TC, type = "tc")

# RCR
rcr_overview <- metric_overview(df, RCR)
rcr_distribution <- metric_distribution(df, RCR, type = "ratio")

# FCR
fcr_overview <- metric_overview(df, FCR)
fcr_distribution <- metric_distribution(df, FCR, type = "ratio")

tc_overview
rcr_overview
fcr_overview

tc_distribution
rcr_distribution
fcr_distribution
```

## Export, Clean, and De-Duplicate Citing Documents
In the "Exports" folder, create a subfolder titled "Citing Documents" that includes subfolders titled “Citing Original” and “Citing Working."

Export Scopus data in a CSV format. Export citation information, bibliographical information, abstract & keywords, funding details, and other information. Save the file with a standard naming convention: last name/department-scopus-citing-original (e.g., smith-scopus-citing-original; marketing-scopus-citing-original). Save the original export in the “Citing Original” subfolder, and then add a copy to the “Citing Working” folder and update the file name (e.g., smith-scopus-citing-working; marketing-scopus-citing-working).

Export Web of Science data in a BibTeX format. Export the full record and cited references. Save the file with a standard naming convention: last name/department-isi-citing-original (e.g., smith-isi-citing-original.csv; marketing-isi-citing-original.csv). Save the original export in the “Citing Original” subfolder, and then add a copy to the “Citing Working” folder and update the file name (e.g., smith-isi-citing-working; marketing-isi-citing-working).

Use the following script to convert the original Web of Science (bibtex) file into a csv. Update the file paths for `C:/path/to/your/wos_file.bib` and `C:/path/to/your/wos_converted.csv`. Save a copy to the “Citing Working” folder and update the file name (e.g., smith-isi-citing-working; marketing-isi-citing-working).

```r
library(bibliometrix)
wos_file <- "C:/path/to/your/wos_file.bib" # update path name
W <- convert2df(wos_file,
                dbsource = "wos",
                format = "bibtex")
write.csv(W,
          "C:/path/to/your/wos_converted.csv", # update path name
          row.names = FALSE)
```

In the Scopus working csv files, update the column names to the following:

- Authors = AU
- Title = TI
- Source Title = SO
- Cited by = TC
- Year = PY
- DOI = DI
- Affiliations = affiliations

Then, add the column “DB” to the Scopus and Web of Science working files and note the database name (i.e., Scopus, ISI). Retain only the following columns in the working files: DB, DI, TI, SO, PY, AU, TC, and affiliations.

Repeat the steps outlined on the [Clean Data in OpenRefine webpage](https://vvlooking.github.io/research-impact/data-cleaning-openrefine.html) to combine and clean the full citing documents dataset.

Create a working copy of the file exported from OpenRefine and use a standard naming convention (e.g., smith-combined-citing-deduplicated; marketing-combined-citing-deduplicated). Save the file in the “Citing Working” subfolder. Then, delete duplicate documents, retaining the record from the database with the most comprehensive indexing, by sorting first by the DOI and then the title. Ensure the number of de-duplicated documents equals the number of total citations reported. After the data has been prepared, create a duplicate copy of the file, titled "last-name/department-citing-noselfcitations.xlsx" and delete self-citations.

Using the "last-name/department-citing-noselfcitations.xlsx" file, standardize institutions by following the directions on the [Update Institution Crosswalk webpage](https://vvlooking.github.io/research-impact/data-cleaning-crosswalk.html).

## Identify Top Affiliations of Citing Documents

Calculate the number of unique citing institutions by running the following script. Update the file paths for input_file.xlsx, path_to_institutions.csv, and path_to_institution-counts.xlsx (this will be a new file, “institution counts”, that will be exported into the Outputs/ folder).

```r
# Load libraries

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

# Load files

input_file <- "input_file.xlsx" # Update path to standardized citing Excel file
institutions_file <- "path_to_institutions.csv" # Update path to institution crosswalk
output_file <- "path_to_institution-counts.xlsx" # Update path to output folder

# Import Excel file

publications <- read_excel(input_file) |>
  mutate(publication_row_id = row_number())

# Confirm that the required column exists
if (!"institution_ids" %in% names(publications)) {
  stop(
    "The input workbook does not contain a column named ",
    "'institution_ids'."
  )
}

# Convert institution_id to long form

publication_institutions <- publications |>
  select(
    publication_row_id,
    institution_ids
  ) |>
  separate_longer_delim(
    institution_ids,
    delim = ";"
  ) |>
  mutate(
    institution_id = str_squish(institution_ids)
  ) |>
  filter(
    !is.na(institution_id),
    institution_id != ""
  ) |>
  distinct(
    publication_row_id,
    institution_id
  )
  
# Count unique institutions

number_unique_institutions <- publication_institutions |>
  summarise(
    unique_institutions = n_distinct(institution_id)
  ) |>
  pull(unique_institutions)

message(
  "Number of unique institutions: ",
  number_unique_institutions
)

# Count publications per institution

institution_counts <- publication_institutions |>
  count(
    institution_id,
    name = "unique_publications",
    sort = TRUE
  )

# Add canonical names and identifers

institutions <- read_csv(
  institutions_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

institution_counts <- institution_counts |>
  left_join(
    institutions |>
      select(
        institution_id,
        canonical_name,
        entity_type,
        state_code,
        country_code,
        ipeds_unitid,
        ror_id
      ),
    by = "institution_id"
  ) |>
  select(
    rank = unique_publications,
    institution_id,
    canonical_name,
    unique_publications,
    entity_type,
    state_code,
    country_code,
    ipeds_unitid,
    ror_id
  ) |>
  arrange(
    desc(unique_publications),
    canonical_name
  ) |>
  mutate(
    rank = row_number()
  )
```

Identify the top collaborating institutions and export the results by adding the following script:

```r
# Identify top 15 intitutions

top_15_institutions <- institution_counts |>
  slice_head(n = 15)

print(top_15_institutions)

# Summary

summary_table <- tibble(
  measure = c(
    "Publications in input file",
    "Publications with at least one matched institution",
    "Unique institutions",
    "Institution-publication combinations"
  ),
  value = c(
    nrow(publications),
    n_distinct(publication_institutions$publication_row_id),
    number_unique_institutions,
    nrow(publication_institutions)
  )
)  # Closes tibble()

# Export results

write_xlsx(
  list(
    Summary = summary_table,
    `Top 15 Institutions` = top_15_institutions,
    `All Institution Counts` = institution_counts
  ),
  output_file
)

message("Results written to: ", output_file)
```

## Identify Top Countries of Citing Documents

Calculate the number of unique citing countries across publications by adding the following script:

```r
# Validate the country column
if (!"country_code" %in% names(institutions)) {
  stop(
    "institutions.csv does not contain a column named ",
    "'country_code'."
  )
}

# Add institution metadata to publication-institution pairs
publication_institution_details <- publication_institutions |>
  left_join(
    institutions |>
      select(
        institution_id,
        country_code
      ),
    by = "institution_id"
  )

# Create one row per publication and country
publication_countries <- publication_institution_details |>
  filter(
    !is.na(country_code),
    country_code != ""
  ) |>
  distinct(
    publication_row_id,
    country_code
  )

# Count unique countries
number_unique_countries <- publication_countries |>
  summarise(
    unique_countries = n_distinct(country_code)
  ) |>
  pull(unique_countries)

message(
  "Number of unique countries: ",
  number_unique_countries
)

# Count publications per country
country_counts <- publication_countries |>
  count(
    country_code,
    name = "unique_publications",
    sort = TRUE
  ) |>
  mutate(
    rank = row_number()
  ) |>
  select(
    rank,
    country_code,
    unique_publications
  )

print(country_counts)
```

Identify the top citing countries by adding the following script:

```r
# Select top 15 countries
top_15_countries <- country_counts |>
  slice_head(n = 15)

print(top_15_countries)

# Summary
country_summary_table <- tibble(
  measure = c(
    "Publications in input file",
    "Publications with at least one identified country",
    "Unique countries",
    "Publication-country combinations"
  ),
  value = c(
    nrow(publications),
    n_distinct(publication_countries$publication_row_id),
    number_unique_countries,
    nrow(publication_countries)
  )
)

# Update Excel file

write_xlsx(
  list(
    Summary = country_summary_table,

    `Top 15 Institutions` =
      top_15_institutions,

    `All Institution Counts` =
      institution_counts,

    `Country Summary` =
      country_summary_table,

    `Top 15 Countries` =
      top_15_countries,

    `All Country Counts` =
      country_counts
  ),
  output_file
)

message("Results written to: ", output_file)
```

## Identify Top Journals of Citing Documents

Identify the journals citing documents are mostly published in by running the following script. Update the file paths for input_file.xlsx, and path_to_journal-counts.xlsx (this will be a new file, “journal counts”, that will be exported into the Outputs/ folder).

```r
# Load libraries
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Load files
input_file <- "input_file.xlsx" # Update path to standardized citing Excel file
output_file <- "path_to_journal-counts.xlsx" # Update path to output folder

# Import Excel file
publications <- read_excel(input_file) |>
  mutate(publication_row_id = row_number())

# Confirm that SO_key exists
if (!"SO_key" %in% names(publications)) {
  stop(
    "The input workbook does not contain a column named ",
    "'SO_key'."
  )
}

# Clean journal names
publication_journals <- publications |>
  select(
    publication_row_id,
    SO_key
  ) |>
  mutate(
    journal = str_squish(SO_key)
  ) |>
  filter(
    !is.na(journal),
    journal != ""
  ) |>
  distinct(
    publication_row_id,
    journal
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

# Count publications per journal
journal_counts <- publication_journals |>
  count(
    journal,
    name = "unique_publications",
    sort = TRUE
  ) |>
  mutate(
    rank = row_number()
  ) |>
  select(
    rank,
    journal,
    unique_publications
  )

# Select top 15 journals
top_15_journals <- journal_counts |>
  slice_head(n = 15)

print(top_15_journals)

# Summary
journal_summary_table <- tibble(
  measure = c(
    "Publications in input file",
    "Publications with journal data",
    "Unique journals"
  ),
  value = c(
    nrow(publications),
    n_distinct(publication_journals$publication_row_id),
    number_unique_journals
  )
)

# Export results
write_xlsx(
  list(
    `Journal Summary` = journal_summary_table,
    `Top 15 Journals` = top_15_journals,
    `All Journal Counts` = journal_counts
  ),
  output_file
)

message("Results written to: ", output_file)
```
