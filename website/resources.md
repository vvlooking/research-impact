# Additional Resources

## Create an Institution Crosswalk

### Checklist

- [ ] Create a project folder
- [ ] Create the `01_create_affiliation_inventory.R` script
- [ ] Create the `institution.csv` file
- [ ] Review the `affiliation_inventory.csv` file
- [ ] Create the `02_standardize_affiliations.R` script
- [ ] Review the results in the `unmatched_affiliations.csv` file
- [ ] Add new institutions to `institution.csv` and their observed names in `institution_aliases.csv`

### Create a Project Folder

Create the folder `institution-crosswalk/` and add the following subfolders: `data`, `input`, `output`, `review`, and `scripts`.

### Create the `01_create_affiliation_inventory.R` Script

Using RStudio, create a new script titled `01_create_affiliation_inventory.R` to extract distinct `affiliation_key` values and produce `affiliation_inventory.csv` in the review/ subfolder. Update the file paths for the input and output files: `input/file-name.xlsx` and `review/affiliation_inventory.csv`.

```r
install.packages(c(
  "readxl",
  "dplyr",
  "tidyr",
  "stringr",
  "readr",
  "writexl"
))

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

input_file <- "input/file-name.xlsx" # Update the file path to the de-deduplicated Excel file

publications <- read_excel(input_file) |>
  mutate(
    publication_row_id = row_number(),
    source = recode(
      DB,
      "ISI" = "Web of Science",
      .default = DB
    )
  )

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
    # affiliations_key is already cleaned in OpenRefine.
    # Only remove extra spaces here.
    observed_key = str_squish(affiliations_key)
  ) |>
  filter(
    !is.na(observed_key),
    observed_key != ""
  )

affiliation_inventory <- affiliations_long |>
  group_by(source, observed_key) |>
  summarise(
    occurrences = n(),
    example_record = first(publication_row_id),
    example_affiliations = first(affiliations),
    .groups = "drop"
  ) |>
  arrange(source, desc(occurrences), observed_key) |>
  mutate(
    institution_id = NA_character_,
    review_status = "pending",
    notes = NA_character_
  )

write_csv(
  affiliation_inventory,
  "review/affiliation_inventory.csv", # Update the file path to the review/ subfolder
  na = ""
)
```

### Create the Institution Crosswalk

Create an `institutions.csv` file with the following headings: `institution_id`, `canonical_name`, `entity_type`, `parent_id`, `state_code`, `country_code`, `ipeds_unitid`, `herd_inst_id`, `ncses_inst_id`, `ror_id`, `notes`. Upload the file to the `data/` subfolder.

### Review the Affiliation Inventory

Open the `affiliation_inventory.csv` from the `review` subfolder. For every distinct `observation_key`, identify the actual institution, ensure the institution has a row in `institutions.csv`, enter its `institution.id`, and change its `review_status` to `approved`. Once finished, save a copy in the `data` subfolder as `institutions_aliases.csv`.


### Create the `02_standardize_affiliations.R` Script

Using RStudio, create a script titled `02_standardize_affiliations.R`. Update the file paths for `input/file-name.xlsx`, `data/institutions.csv`, 'data/institution_aliases.csv', `review/unmatched_affiliations.csv`, and `output/file-name-standardized.xlsx'.

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
  "input/file-name.xlsx" # Update the file path to deduplicated Excel file
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
  "data/institutions.csv", # Update the path to the institutions.csv file
  col_types = cols(.default = col_character())
)

aliases <- read_csv(
  "data/institution_aliases.csv", # Update the path to the institution_aliases.csv file
  col_types = cols(.default = col_character())
) |>
  filter(review_status == "approved")

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
  "review/unmatched_affiliations.csv", # Update the file path to the review/ subfolder and name the file `unmatched_affiliations.csv`
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
  "output/file-name-standardized.xlsx" # Update the file path to the output/ subfolder and add -standardized.xlsx to the original deduplicated file name
)
```

### Review the Results

Check the match status:

```r
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

Open `unmatched_affiliations.csv` in the `review/` subfolder and examine the results. Add each new institution to `institutions.csv` and add the observed name to `institution_aliases.csv` and add the alias as approved. Re-run the script again as necessary.
