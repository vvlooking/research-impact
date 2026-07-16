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

Create a parent folder, `institution-crosswalk` and include the following sub-folders  it: data, input, output, review, scripts.

## Create a Dated Copy of the Crosswalk Files

Make copies of the crosswalk files with today's date and add them to the `data/` subfolder (e.g., `institutions-07.16.26.csv`, `institutions_aliases-07.16.26.csv`)

## Add the Cleaned Excel file to the input/ Subfolder

Upload the cleaned de-duplicated Excel file into the `input/` subfolder. The Excel file should include at least the following columns: `DB`, `affiliations`, `affiliations_key`.

## Create and Update "Standarize Affiliations" Script

In RStudio, create the script `02_standardize_affiliations.R`. Then, update the following lines with the correct file paths: `path_to_deduplicated_dataset`, `path_to_dated_institutions.csv`, `path_to_dated_institution_aliases.csv`, `path_to_unmatched_affiliations.csv`, and `path_to_deduplicated_dataset-standardized.xlsx`.

```r
# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(writexl)

collapse_unique <- function(x) {
  values <- x |>
    na.omit() |>
    unique() |>
    sort()

  if (length(values) == 0) {
    NA_character_
  } else {
    str_c(values, collapse = "; ")
  }
}

# Import publication data
publications <- read_excel(
  "path_to_deduplicated_dataset" # Update file path to de-duplicated Excel file
) |>
  mutate(
    publication_row_id = row_number(),
    source = recode(
      DB,
      "ISI" = "Web of Science",
      .default = DB
    )
  )

# Convert the publication-level affiliation column to long form
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

# Import crosswalk tables
institutions <- read_csv(
  "path_to_dated_institutions.csv", # Update path to dated "institutions.csv" file in the data/ subfolder
  col_types = cols(.default = col_character())
)

aliases <- read_csv(
  "path_to_dated_institution_aliases.csv", # Update path to dated "institution_aliases.csv" file in the data/subfolder
  col_types = cols(.default = col_character())
) |>
  filter(review_status == "approved")

# Check columns
names(aliases)
names(institutions)


# Match source plus affiliation key
matched_affiliations <- affiliations_long |>
  left_join(
    aliases |>
      select(source, observed_key, institution_id),
    by = c("source", "observed_key")
  ) |>
  left_join(
    institutions,
    by = "institution_id"
  )

# Create an updated unmatched-review file
unmatched <- matched_affiliations |>
  filter(is.na(institution_id)) |>
  count(source, observed_key, sort = TRUE)

write_csv(
  unmatched,
  "path_to_unmatched_affiliations.csv", # Update path to "unmatched affiliations.csv" in the review/ subfolder
  na = ""
)

# Prevent repeated authors from causing repeated institution matches
publication_institutions <- matched_affiliations |>
  filter(!is.na(institution_id)) |>
  distinct(
    publication_row_id,
    institution_id,
    .keep_all = TRUE
  )

# Produce publication-level standardized columns
standardized_summary <- matched_affiliations |>
  group_by(publication_row_id) |>
  summarise(
    match_status = case_when(
      all(!is.na(institution_id)) ~ "complete",
      any(!is.na(institution_id)) ~ "partial",
      TRUE ~ "unmatched"
    ),
    unmatched_affiliation_count = sum(is.na(institution_id)),
    .groups = "drop"
  ) |>
  left_join(
    publication_institutions |>
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
        institution_count =
          n_distinct(institution_id),
        .groups = "drop"
      ),
    by = "publication_row_id"
  )

# Add results without overwriting original affiliation columns
standardized_publications <- publications |>
  left_join(
    standardized_summary,
    by = "publication_row_id"
  )

write_xlsx(
  standardized_publications,
  "path_to_deduplicated_dataset-standardized.xlsx" # Update path to standardized output file in output/ subfolder (add -standardized.xlsx to the original file name)
)
```

## Run the Standardization Script

Run the standardization script to split the affiliations_key into individual affiliation strings, match those strings against `institution_aliases.csv`, retrieve canonical names and identifiers from `institutions.csv`, produce a standardized Excel file, and produce a new `unmatched_affiliations.csv` file.

## Review the Matching Rate

Add the following code to the end of the standardization script:

```r
# Review the results

standardized_publications |>
  count(match_status)

# Check examples
standardized_publications |>
  filter(match_status != "complete") |>
  select(
    DB,
    DI,
    TI,
    affiliations,
    affiliations_key,
    canonical_affiliations,
    match_status
  ) |>
  View()
```

## Classify Every Unmatched Affiliation

Review the 'unmatched_affiliations.csv` file. If an unmatched institution maps to an institution already in the `institutions.csv` file, add a row to the `institutions.aliases.csv` file. 

For example:

| institution_id | observed_key | source | review_status |
|---|---|---|---|
| INST000001 | University of South Carolina Columbia | Scopus | Approved |

If an unmatched institution does not map to an institution already in the "institutions.csv" file, add a row to the "institutions.csv" file.

| institution_id | canonical_name | entity_type | parent_id | state_code | country_code | ipeds_unitid |
|---|---|---|---|---|---|---|
| INST000001 | university of south carolina | university | INST000001 | sc | usa | 218663 |

After adding necessary rows to the "institutions.csv" file, check for duplicates based on affiliation IDs, such as IPEDS.

## Re-run the Standardization Script

Re-run the standardization script and check the match status by adding the following code to the end of the script:

```r
standardized_publications |>
  count(match_status)
```

Repeat the review and re-run process until the remaining unmatched affiliations are resolved.
