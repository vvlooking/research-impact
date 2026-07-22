# Update Institution Crosswalk

## Checklist

- [ ] Create a folder structure for the crosswalk
- [ ] Create a dated copy of `institutions.csv` and `institution_aliases.csv`, and add them to the `data/` folder
- [ ] Add the cleaned Excel file to the `input/` folder
- [ ] Add `02_standardize_affiliations.R` to the `scripts/` subfolder and update the `input_file` and `output_file` paths
- [ ] Run the standardization script
- [ ] Check the match-status summary
- [ ] Open `unmatched_affiliations.csv`
- [ ] Map new variants to existing institutions in the dated `institution_aliases.csv` file
- [ ] Add new institutions, when necessary, to the dated `institutions.csv` file
- [ ] Re-run `02_standardize_affiliations.R`

## Create a Folder Structure

Create a parent folder, "institution-crosswalk" and include the following sub-folders  it: data, input, output, review, scripts.

## Create a Dated Copy of the Crosswalk Files

Make copies of the crosswalk files with today's date and add them to the "data/" subfolder (e.g., `institutions-07.16.26.csv`, `institutions_aliases-07.16.26.csv`)

## Add the Cleaned Excel file to the input/ Subfolder

Upload the cleaned de-duplicated Excel file into the "input/" subfolder. The Excel file should include at least the following columns: `DB`, `affiliations`, `affiliations_key`.

## Create and Update "Standarize Affiliations" Script

In RStudio, create the script `02_standardize_affiliations.R`. Then, update the following lines with the correct file paths: `path_to_deduplicated_dataset.xlsx`, `path_to_dated_institutions.csv`, `path_to_dated_institution_aliases.csv`, `path_to_unmatched_affiliations.csv`, and `path_to_deduplicated_dataset-standardized.xlsx`.

```r
# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(writexl)

# Import data
input_file <- "path_to_dedeuplicated dataset.xlsx" # Update file path to cleaned de-duplicated OpenRefine dataset

institutions_file <-"path_to_dated institutions.csv" # Update file path to institutions crosswalk

aliases_file <- "path_to_date_institution_aliases.csv" # Update file path to institution aliases

unmatched_file <- "path_to_unmatched_affiliations.csv" # Update file path to unmatched affiliations spreadsheet

output_file <- "path_to_deduplicated_dataset-standarized.xlsx" # Update file path to output folder

# Combine unique nonblank values into one semicolon-separated string.
collapse_unique <- function(x) {

  values <- x |>
    as.character() |>
    na_if("") |>
    na.omit() |>
    unique() |>
    sort()

  if (length(values) == 0) {
    NA_character_
  } else {
    str_c(values, collapse = "; ")
  }
}

# Confirm that a data frame contains the required columns.
check_required_columns <- function(data, required, data_name) {

  missing_columns <- setdiff(
    required,
    names(data)
  )

  if (length(missing_columns) > 0) {
    stop(
      data_name,
      " is missing these required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
}

# Import publication data
publications <- read_excel(input_file) |>
  mutate(
    publication_row_id = row_number(),

    source = recode(
      DB,
      "ISI" = "Web of Science",
      .default = DB
    )
  )


# Confirm that the OpenRefine export has the necessary columns.
check_required_columns(
  publications,
  required = c(
    "DB",
    "affiliations",
    "affiliations_key"
  ),
  data_name = "The publication workbook"
)

# Convert affiliations to long form
affiliations_long <- publications |>
  select(
    publication_row_id,
    source,
    affiliations,
    affiliations_key
  ) |>
  separate_longer_delim(
    affiliations_key,
    delim = ";"
  ) |>
  mutate(
    observed_key = str_squish(affiliations_key)
  ) |>
  filter(
    !is.na(observed_key),
    observed_key != ""
  )

# Import the institutional crosswalk

institutions <- read_csv(
  institutions_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

aliases <- read_csv(
  aliases_file,
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)


# Confirm that institutions.csv has the necessary columns.
check_required_columns(
  institutions,
  required = c(
    "institution_id",
    "canonical_name",
    "ipeds_unitid",
    "ror_id",
    "entity_type",
    "state_code",
    "country_code"
  ),
  data_name = "institutions.csv"
)


# Confirm that institution_aliases.csv has the necessary columns.
check_required_columns(
  aliases,
  required = c(
    "source",
    "observed_key",
    "institution_id",
    "review_status"
  ),
  data_name = "institution_aliases.csv"
)


# Keep only approved mappings.
aliases <- aliases |>
  filter(review_status == "approved")


# Stop if an approved alias is missing an institution ID.
approved_without_id <- aliases |>
  filter(
    is.na(institution_id),
    institution_id == ""
  )

if (nrow(approved_without_id) > 0) {
  stop(
    "At least one approved alias is missing an institution_id."
  )
}


# Stop if the same source and alias combination appears more than once.
duplicate_aliases <- aliases |>
  count(
    source,
    observed_key,
    name = "row_count"
  ) |>
  filter(row_count > 1)

if (nrow(duplicate_aliases) > 0) {

  print(duplicate_aliases)

  stop(
    "Duplicate source and observed_key combinations were found ",
    "in institution_aliases.csv."
  )
}


# Stop if an alias points to an institution that is absent from institutions.csv.
orphan_aliases <- aliases |>
  anti_join(
    institutions,
    by = "institution_id"
  )

if (nrow(orphan_aliases) > 0) {

  print(
    orphan_aliases |>
      select(
        source,
        observed_key,
        institution_id
      )
  )

  stop(
    "At least one approved alias points to an institution_id ",
    "that is missing from institutions.csv."
  )
}

# Match affiliations to institutions

matched_affiliations <- affiliations_long |>
  left_join(
    aliases |>
      select(
        source,
        observed_key,
        institution_id
      ),
    by = c(
      "source",
      "observed_key"
    )
  ) |>
  left_join(
    institutions,
    by = "institution_id"
  )

# Create an unmatched affiliation review file

nmatched <- matched_affiliations |>
  filter(is.na(institution_id)) |>
  count(
    source,
    observed_key,
    sort = TRUE,
    name = "occurrences"
  ) |>
  mutate(
    institution_id = NA_character_,
    review_status = "pending",
    notes = NA_character_
  )


write_csv(
  unmatched,
  unmatched_file,
  na = ""
)

# Create one row per publication and institution
publication_institutions <- matched_affiliations |>
  filter(!is.na(institution_id)) |>
  distinct(
    publication_row_id,
    institution_id,
    .keep_all = TRUE
  )

# Create publication-level standardized column

match_summary <- matched_affiliations |>
  group_by(publication_row_id) |>
  summarise(
    match_status = case_when(
      all(!is.na(institution_id)) ~ "complete",
      any(!is.na(institution_id)) ~ "partial",
      TRUE ~ "unmatched"
    ),

    unmatched_affiliation_count =
      sum(is.na(institution_id)),

    .groups = "drop"
  )


institution_summary <- publication_institutions |>
  group_by(publication_row_id) |>
  summarise(
    canonical_affiliations =
      collapse_unique(canonical_name),

    institution_ids =
      collapse_unique(institution_id),

    ipeds_unitids =
      collapse_unique(ipeds_unitid),

    ror_ids =
      collapse_unique(ror_id),

    entity_type =
      collapse_unique(entity_type),

    state_code =
      collapse_unique(state_code),

    country_code =
      collapse_unique(country_code),

    institution_count =
      n_distinct(institution_id),

    .groups = "drop"
  )


standardized_summary <- match_summary |>
  left_join(
    institution_summary,
    by = "publication_row_id"
  )

# Add standardized results to the original publication records

standardized_publications <- publications |>
  left_join(
    standardized_summary,
    by = "publication_row_id"
  ) |>
  mutate(
    match_status = if_else(
      is.na(match_status),
      "no_affiliation_data",
      match_status
    ),

    unmatched_affiliation_count = coalesce(
      unmatched_affiliation_count,
      0L
    ),

    institution_count = coalesce(
      institution_count,
      0L
    )
  )
  
# Create a publication-institution table

publication_institution_detail <- publication_institutions |>
  select(
    publication_row_id,
    source,
    observed_key,
    institution_id,
    canonical_name,
    entity_type,
    state_code,
    country_code,
    ipeds_unitid,
    ror_id
  ) |>
  arrange(
    publication_row_id,
    canonical_name
  )

# Create a quality control summary

quality_control <- standardized_publications |>
  count(
    match_status,
    name = "publication_count"
  ) |>
  mutate(
    percent = publication_count / sum(publication_count)
  )

# Export the standardized file

write_xlsx(
  list(
    Publications =
      standardized_publications,

    `Publication Institutions` =
      publication_institution_detail,

    `Quality Control` =
      quality_control
  ),
  output_file
)


message("Standardized workbook written to: ", output_file)

message("Unmatched review file written to: ", unmatched_file)
```

## Run the Standardization Script

Run the standardization script to split the `affiliations_key` into individual affiliation strings, match those strings against `institution_aliases.csv`, retrieve canonical names and identifiers from `institutions.csv`, produce a standardized Excel file, and produce a new `unmatched_affiliations.csv` file.

## Review the Matching Rate

Add the following code to the end of the standardization script:

```r
print(quality_control)


standardized_publications |>
  filter(match_status != "complete") |>
  select(
    DB,
    DI,
    TI,
    affiliations,
    affiliations_key,
    canonical_affiliations,
    institution_ids,
    entity_type,
    state_code,
    country_code,
    match_status,
    unmatched_affiliation_count
  ) |>
  View()
```

## Classify Every Unmatched Affiliation

Review the `unmatched_affiliations.csv` file. If an unmatched institution maps to an institution already in the `institutions.csv` file, add a row to the `institutions_aliases.csv` file. 

For example:

| institution_id | observed_key | source | review_status |
|---|---|---|---|
| INST000001 | University of South Carolina Columbia | Scopus | Approved |

If an unmatched institution does not map to an institution already in the `institutions.csv` file, add a row to the `institutions.csv` file.

| institution_id | canonical_name | entity_type | parent_id | state_code | country_code | ipeds_unitid |
|---|---|---|---|---|---|---|
| INST000001 | university of south carolina | university | INST000001 | sc | usa | 218663 |

After adding necessary rows to the `institutions.csv` file, check for duplicates based on affiliation IDs, such as IPEDS.

## Re-run the Standardization Script

Re-run the standardization script and check the match status. Repeat the review and re-run process until the remaining unmatched affiliations are resolved. Note that some affiliations may need to be edited in the cleaned Excel file manually.
