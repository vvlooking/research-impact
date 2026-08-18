# Collaboration

## Checklist

- [ ] Calculate the number of unique co-authors
- [ ] Calculate the average number of co-authors per publication
- [ ] Identify the top collaborating authors
- [ ] Calculate the number of unique institutions
- [ ] Identify the top collaborating institutions
- [ ] Identify the top collaborating units at USC
- [ ] Calculate the number of unique countries
- [ ] Identify the top collaborating countries
- [ ] Calculate the number of unique states
- [ ] Identify the top collaborating states
- [ ] Create a collaboration network visualization (authors) in VOSViewer
- [ ] Create a collaboration network visualization (USC) in VOSViewer
- [ ] Create a collaboration network visualization (institutions) in VOSViewer
- [ ] Create a collaboration network visualization (states) in Flourish (via Canva)
- [ ] Create a collaboration network visualization (countries) in Flourish (via Canva)

## Collaborating Authors

Calculate the number of unique co-authors across publications by running the following script. Update the file path for `file_name.xlsx` and update the faculty names to exclude under `faculty_exclude`.

```r
# Load libraries
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)

# Read data
df <- read_excel("file_name") # Add file path

# Faculty to exclude (update names)
faculty_exclude <- c(
  "Doe J",
  "Smith J",
  "Jones R"
)

# Clean and filter author names
authors_long <- df %>%
  select(AU_key) %>%
  filter(!is.na(AU_key) & AU_key != "") %>%
  separate_rows(AU_key, sep = "\\s*;\\s*") %>%
  mutate(
    AU_key = str_trim(AU_key),
    AU_key = str_replace_all(AU_key, "\\s+", " ")
  ) %>%
  filter(
    AU_key != "",
    !(AU_key %in% faculty_exclude)   # <-- exclude all faculty names
  )

# Count unique co-author names
unique_authors <- authors_long %>%
  distinct(AU_key)

n_unique_authors <- nrow(unique_authors)

cat("Number of unique co-author names (excluding faculty list):",
    n_unique_authors, "\n")

# View the unique names
unique_authors %>%
  arrange(AU_key) %>%
  View()

# Frequency of each co-author
author_counts <- authors_long %>%
  count(AU_key, sort = TRUE)

author_counts
```

For **individual reports**, calculate the average number of co-authors per publication by running the following script. Update the file path for `file_name.xlsx` and update the author's name in `Last Name F`.

```r
# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Read data
df <- read_excel("file_name") # Add file path

# Add a document ID
df <- df %>%
  mutate(doc_id = row_number())

# Split authors into long format
authors_long <- df %>%
  select(doc_id, AU_key) %>%
  filter(!is.na(AU_key) & AU_key != "") %>%
  separate_rows(AU_key, sep = "\\s*;\\s*") %>%
  mutate(
    AU_key = str_trim(AU_key),
    AU_key = str_replace_all(AU_key, "\\s+", " ")
  ) %>%
  filter(AU_key != "")

# Count authors per document
coauthor_counts <- authors_long %>%
  group_by(doc_id) %>%
  summarise(
    total_authors = n_distinct(AU_key),
    focal_author_present = any(AU_key == "Last Name F"), # Add author name here
    coauthors = ifelse(focal_author_present,
                       total_authors - 1,
                       total_authors),
    .groups = "drop"
  )

# Calculate summary statistics
summary_stats <- coauthor_counts %>%
  summarise(
    documents = n(),
    mean_coauthors = round(mean(coauthors), 2),
    median_coauthors = median(coauthors),
    min_coauthors = min(coauthors),
    max_coauthors = max(coauthors)
  )

summary_stats
```

For **group reports**, calculate the average number of co-authors per publication by running the following script. Update the file path for `file_name.xlsx` and update the faculty names to exclude under `faculty_exclude`.

```r
## Average number of co-authors per document (Group Reports)

# Load libraries
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# Read data
df <- read_excel("file_name")  # Add file path

# ---- Faculty to exclude ----
faculty_exclude <- c(
  "Doe J",
  "Smith M",
  "Dear, J"
)

# Add document ID
df <- df %>%
  mutate(doc_id = row_number())

# Split authors into long format
authors_long <- df %>%
  select(doc_id, AU_key) %>%
  filter(!is.na(AU_key) & AU_key != "") %>%
  separate_rows(AU_key, sep = "\\s*;\\s*") %>%
  mutate(
    AU_key = str_trim(AU_key),
    AU_key = str_replace_all(AU_key, "\\s+", " ")
  ) %>%
  filter(
    AU_key != "",
    !(AU_key %in% faculty_exclude)   # <-- REMOVE FACULTY HERE
  )

# Count remaining authors per document
coauthor_counts <- authors_long %>%
  group_by(doc_id) %>%
  summarise(
    coauthors = n_distinct(AU_key),
    .groups = "drop"
  )

# Ensure documents with ONLY faculty don't disappear
coauthor_counts <- df %>%
  select(doc_id) %>%
  left_join(coauthor_counts, by = "doc_id") %>%
  mutate(coauthors = ifelse(is.na(coauthors), 0, coauthors))

# Summary statistics
summary_stats <- coauthor_counts %>%
  summarise(
    documents = n(),
    mean_coauthors = round(mean(coauthors), 2),
    median_coauthors = median(coauthors),
    min_coauthors = min(coauthors),
    max_coauthors = max(coauthors)
  )

summary_stats
```

## Collaborating Institutions

Calculate the number of unique institutions across publications by running the following script. Update the file paths for `input_file.xlsx`, `path_to_institutions.csv`, and `path_to_institution-counts.xlsx` (this will be a new file, "institution counts", that will be exported into the Outputs/ folder).

```r
# Load libraries

library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

# Load files

input_file <- "input_file.xlsx" # Update path to standardized Excel file
institutions_file <- "path_to_institutions.csv" # Update path to institution crosswalk
output_file <- "path_to_institution-counts.xlsx" # Update path to output folder

home_institution_id <- "INST000001"

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
    institution_id != "",
    institution_id != home_institution_id
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

## Collaborating States

Calculate the number of unique states across publications by adding the following script:

```r
# Validate the state column

if (!"state_code" %in% names(institutions)) {
  stop(
    "institutions.csv does not contain a column named ",
    "'state_code'."
  )
}

# Add state information

publication_institution_details <- publication_institutions |>
  left_join(
    institutions |>
      select(
        institution_id,
        canonical_name,
        entity_type,
        state_code,
        country_code
      ),
    by = "institution_id"
  )
  

# Create one row per publication and state

publication_states <- publication_institution_details |>
  filter(
    !is.na(state_code),
    state_code != ""
  ) |>
  filter(country_code == "usa") |>
  distinct(
    publication_row_id,
    state_code
  )

# Count unique states

number_unique_states <- publication_states |>
  summarise(
    unique_states = n_distinct(state_code)
  ) |>
  pull(unique_states)

message(
  "Number of unique states: ",
  number_unique_states
)

# Count publications per state

state_counts <- publication_states |>
  count(
    state_code,
    name = "unique_publications",
    sort = TRUE
  ) |>
  mutate(
    rank = row_number()
  ) |>
  select(
    rank,
    state_code,
    unique_publications
  )
```


Identify the top collaborating states and export the results by adding the following script:

```r  
# Top 15 states

top_15_states <- state_counts |>
  slice_head(n = 15)

print(top_15_states)

# Summary table

state_summary_table <- tibble(
  measure = c(
    "Publications in input file",
    "Publications with at least one identified U.S. state",
    "Unique states",
    "Publication-state combinations"
  ),
  value = c(
    nrow(publications),
    n_distinct(publication_states$publication_row_id),
    number_unique_states,
    nrow(publication_states)
  )
)

# Update Excel export

write_xlsx(
  list(
    Summary = summary_table,
    `Top 15 Institutions` = top_15_institutions,
    `All Institution Counts` = institution_counts,
    `State Summary` = state_summary_table,
    `Top 15 States` = top_15_states,
    `All State Counts` = state_counts
  ),
  output_file
)

message("Results written to: ", output_file)
```

## Top Collaborating Countries

Calculate the number of unique countries across publications by adding the following script:

```r
# Validate the country column
if (!"country_code" %in% names(institutions)) {
  stop(
    "institutions.csv does not contain a column named ",
    "'country_code'."
  )
}

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
```

Identify the top collaborating countries and export the results by adding the following script:

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
    Summary = summary_table,

    `Top 15 Institutions` =
      top_15_institutions,

    `All Institution Counts` =
      institution_counts,

    `State Summary` =
      state_summary_table,

    `Top 15 States` =
      top_15_states,

    `All State Counts` =
      state_counts,

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

## Collaborating Entities

Calculate the number of unique entities across publications by adding the following script:

```r
# Validate the column
if (!"entity_type" %in% names(institutions)) {
  stop(
    "institutions.csv does not contain a column named ",
    "'entity_type'."
  )
}

# Create one row per publication and entity type

publication_entity_types <- publication_institution_details |>
  filter(
    !is.na(entity_type),
    entity_type != ""
  ) |>
  distinct(
    publication_row_id,
    entity_type
  )
  
# Count unique entity types
number_unique_entity_types <- publication_entity_types |>
  summarise(
    unique_entity_types = n_distinct(entity_type)
  ) |>
  pull(unique_entity_types)

message(
  "Number of unique entity types: ",
  number_unique_entity_types
)

# Count publications per entity type
entity_type_counts <- publication_entity_types |>
  count(
    entity_type,
    name = "unique_publications",
    sort = TRUE
  ) |>
  mutate(
    rank = row_number()
  ) |>
  select(
    rank,
    entity_type,
    unique_publications
  )
```

Identify the top collaborating entities and export the results by adding the following script:

```r
# Select the top entity types

top_15_entity_types <- entity_type_counts |>
  slice_head(n = 15)

print(top_15_entity_types)

# Summary

entity_type_summary_table <- tibble(
  measure = c(
    "Publications in input file",
    "Publications with at least one identified entity type",
    "Unique entity types",
    "Publication-entity type combinations"
  ),
  value = c(
    nrow(publications),
    n_distinct(publication_entity_types$publication_row_id),
    number_unique_entity_types,
    nrow(publication_entity_types)
  )
)

# Update Excel export

write_xlsx(
  list(
    Summary =
      summary_table,

    `Top 15 Institutions` =
      top_15_institutions,

    `All Institution Counts` =
      institution_counts,

    `State Summary` =
      state_summary_table,

    `Top 15 States` =
      top_15_states,

    `All State Counts` =
      state_counts,

    `Country Summary` =
      country_summary_table,

    `Top 15 Countries` =
      top_15_countries,

    `All Country Counts` =
      country_counts,

    `Entity Type Summary` =
      entity_type_summary_table,

    `Top 15 Entity Types` =
      top_15_entity_types,

    `All Entity Type Counts` =
      entity_type_counts
  ),
  output_file
)

message("Results written to: ", output_file)
```

## Create a Collaboration Network Visualization (Authors) in VOSViewer

Create a duplicate copy of the `deduplicated-standardized.xlsx` file, save it as a csv, and rename it as `-vosviewer-authors.csv`. Create a new column, "Authors," and use the formula below (replacing `A2` with the corresponding cell number in the `AU_key` column). Apply the formula to the remaining cells. Make sure the cells are pasted as values before saving the file.

```r
=TEXTJOIN("; ",TRUE,MAP(TEXTSPLIT(A2,"; "),LAMBDA(n,LET(parts,TEXTSPLIT(TRIM(n)," "),last,TAKE(parts,,1),initials,DROP(parts,,1),last&" "&TEXTJOIN(".",TRUE,MID(TEXTJOIN("",TRUE,initials),SEQUENCE(LEN(TEXTJOIN("",TRUE,initials))),1))&"."))))
```

In VOSViewer, click "Create," then "Create a Map Based on Bibliometric Data." Select "Read Data from Bibliographic Database Files," select Scopus, and upload the edited csv file. Select "Co-Authorship" as the type of analysis and "Authors" as the unit of analysis.

## Create a Collaboration Network Visualization (USC) in VOSViewer

Create a duplicate copy of the `deduplicated-standardized.xlsx` file, save it as a csv, and rename it as -vosviewer-uscunits.csv. Rename the "usc_affiliations" column to "Affiliations" and delete the "affiliations" column.

In VOSViewer, click "Create," then "Create a Map Based on Bibliometric Data." Select "Read Data from Bibliographic Database Files," select Scopus, and upload the edited csv file. Select "Co-Authorship" as the type of analysis and "Organizations" as the unit of analysis. Before selecting "Finish," right-click on the organizations and export all selected organizations. Name the txt file as "usc-thesaurus.txt."

In Excel, click "Data," then "From Text/CSV." Load the txt file. Delete all the columns except for "Affiliations." Rename the column "Affiliations" as "label." Name the next column "replace by." For "university of south carolina," add the researcher's or department's name in the "replace by" column. Copy and paste the respective unit names in the "replace by" column. Save the file as a csv and title it "-vosviewer-usc-thesaurus.csv".

In VOSViewer, click "Create," then "Create a Map Based on Bibliometric Data." Select "Read Data from Bibliographic Database Files," select Scopus, and upload the edited csv file. Select "Co-Authorship" as the type of analysis and "Organizations" as the unit of analysis. Upload the thesaurus file.

## Create a Collaboration Network Visualization (Institutions) in VOSViewer

Create a duplicate copy of the `deduplicated-standardized.xlsx` file, save it as a csv, and rename it as `-vosviewer-institutions.csv`. Rename the "canonical_affiliations" column to "Affiliations" and delete the "affiliations" column.

In VOSViewer, click "Create," then "Create a Map Based on Bibliometric Data." Select "Read Data from Bibliographic Database Files," select Scopus, and upload the edited csv file. Select "Co-Authorship" as the type of analysis and "Organizations" as the unit of analysis. Before selecting "Finish," right-click on the organizations and export all selected organizations. Name the txt file as "institution-thesaurus.txt."

In Excel, click "Data," then "From Text/CSV." Load the txt file. Delete all the columns except for "Affiliations." Rename the column "Affiliations" as "label." Name the next column "replace by." For "university of south carolina," add the researcher's or department's name in the "replace by" column. Copy and paste the respective institution names in the "replace by" column. Save the file as a csv and title it "-vosviewer-institutions-thesaurus.csv".

In VOSViewer, click "Create," then "Create a Map Based on Bibliometric Data." Select "Read Data from Bibliographic Database Files," select Scopus, and upload the edited csv file. Select "Co-Authorship" as the type of analysis and "Organizations" as the unit of analysis. Upload the thesaurus file.

## Create a Collaboration Network Visualization (States) in Flourish (via Canva)

Prepare the standardized citing Excel file for visualization by running the following script. Update the file path for input_file.

```r
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

input_file <- "file_path_to_standardized_dataset" # Update file path

publications <- read_excel(
  input_file,
  sheet = "Publications"
)

institutions <- read_excel(
  input_file,
  sheet = "Publication Institutions"
)
```

Extract U.S. publication-state relationships:

```r
document_states <- institutions %>%
  mutate(
    country_code = str_to_lower(str_squish(country_code)),
    state_code = str_to_upper(str_squish(state_code))
  ) %>%

  # Retain U.S. institutions only
  filter(
    str_detect(
      country_code,
      "(^|;)\\s*usa\\s*(;|$)"
    )
  ) %>%

  select(
    publication_row_id,
    original_state_value = state_code
  ) %>%

  # Safety step for cells containing multiple states
  separate_rows(
    original_state_value,
    sep = "\\s*;\\s*"
  ) %>%

  mutate(
    original_state_value = str_to_upper(
      str_squish(original_state_value)
    )
  ) %>%

  filter(
    !is.na(original_state_value),
    original_state_value != ""
  ) %>%

  # Count each publication once per state
  distinct(
    publication_row_id,
    original_state_value,
    .keep_all = TRUE
  )
```

Create a state-coordinate lookup:

```r
state_lookup <- tibble(
  state_code = state.abb,
  state_name = state.name,
  latitude = state.center$y,
  longitude = state.center$x,
  region = as.character(state.region)
)
```

Add Washington D.C. and common U.S. territories in case they occur in the data and attach state information:

```r
additional_locations <- tribble(
  ~state_code, ~state_name,               ~latitude, ~longitude, ~region,
  "DC",        "District of Columbia",      38.9072,   -77.0369, "South",
  "PR",        "Puerto Rico",               18.2208,   -66.5901, "Territory",
  "VI",        "U.S. Virgin Islands",        18.3358,   -64.8963, "Territory",
  "GU",        "Guam",                      13.4443,   144.7937, "Territory",
  "AS",        "American Samoa",            -14.2710,  -170.1322, "Territory",
  "MP",        "Northern Mariana Islands",   15.0979,   145.6739, "Territory"
)

state_lookup <- bind_rows(
  state_lookup,
  additional_locations
)

document_states <- document_states %>%
  left_join(
    state_lookup,
    by = c("original_state_value" = "state_code")
  )

# Examine the lookup

View(state_lookup)
```

Check for state codes that did not match:

```r
unmatched_states <- document_states %>%
  filter(is.na(state_name)) %>%
  count(original_state_value, sort = TRUE)

unmatched_states
```

Add publication details:

```r
publication_metadata <- publications %>%
  select(
    publication_row_id,
    source,
    title = TI,
    authors = AU,
    year = PY,
    journal = SO,
    doi = DI
  )

document_states <- document_states %>%
  left_join(
    publication_metadata,
    by = "publication_row_id"
  ) %>%
  transmute(
    publication_row_id,
    source,
    title,
    authors,
    year,
    journal,
    doi,
    original_state_value,
    state_code = original_state_value,
    state_name,
    latitude,
    longitude,
    region
  ) %>%
  arrange(
    state_name,
    publication_row_id
  )
```

Review the state counts:

```r
state_counts <- document_states %>%
  filter(!is.na(state_name)) %>%
  group_by(
    state_code,
    state_name,
    region
  ) %>%
  summarise(
    citing_document_count = n_distinct(publication_row_id),
    .groups = "drop"
  ) %>%
  arrange(desc(citing_document_count))

state_counts
```

Next, create the Flourish arcs sheet:

```r
flourish_state_arcs <- state_counts %>%
  mutate(
    source_code = paste0("US_", state_code),
    destination_code = "USC",

    popup_text = paste0(
      state_name,
      ": ",
      citing_document_count,
      " citing document",
      if_else(citing_document_count == 1, "", "s")
    )
  ) %>%
  select(
    source_code,
    destination_code,
    citing_document_count,
    region,
    popup_text
  )

flourish_state_arcs
```

Next, create the Locations sheet:

```r
flourish_state_locations <- state_counts %>%
  left_join(
    state_lookup,
    by = c("state_code", "state_name", "region")
  ) %>%
  transmute(
    code = paste0("US_", state_code),
    location_name = state_name,
    latitude,
    longitude,
    region
  ) %>%

  # Add the destination
  bind_rows(
    tibble(
      code = "USC",
      location_name = "University of South Carolina, Columbia",
      latitude = 33.9971,
      longitude = -81.0274,
      region = "Destination"
    )
  ) %>%
  arrange(location_name)

flourish_state_locations

# validate the location codes

codes_used_in_arcs <- unique(c(
  flourish_state_arcs$source_code,
  flourish_state_arcs$destination_code
))

missing_location_codes <- setdiff(
  codes_used_in_arcs,
  flourish_state_locations$code
)

missing_location_codes

# check for missing coordinates

flourish_state_locations %>%
  filter(is.na(latitude) | is.na(longitude))
```

Export the prepared file, updating the file path for `output_file`:

```r
output_file <- "file_path_to_collaborating_states.xlsx" # Update file path

write_xlsx(
  list(
    "Flourish State Arcs" = flourish_state_arcs,
    "Locations" = flourish_state_locations,
    "Document States" = document_states,
    "State Counts" = state_counts
  ),
  path = output_file
)

message("Created: ", output_file)
```

After the file has been prepared, log into Flourish through Canva. Choose the template, “Connection Map (under “Arc Maps”). Upload Flourish Arcs into the main Data sheet and select:

- Source location: `source code`
- Destination location: `destination_code`
- Value: `citing_document_count`
- Category: `region`
- Info for popups: `popup_text`

In the Flourish Locations datasheet, upload the Excel `Locations` sheet and select:

- Code: `code`
- Name: `location_name`
- Latitude: `latitude`
- Longitude: `longitude`

## Create a Collaboration Network (Countries) in Flourish (via Canva)

Prepare the standardized Excel file for visualization by running the following script. Update the file path for `input_file`.

```r
# Install packages

install.packages(c(
  "readxl",
  "writexl",
  "dplyr",
  "tidyr",
  "stringr",
  "tibble"
))

# Load libraries

library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)

# Import file

input_file <- ("file_path.xlsx") # Update file path

publications <- read_excel(
  input_file,
  sheet = "Publications"
)

institutions <- read_excel(
  input_file,
  sheet = "Publication Institutions"
)

quality_control <- read_excel(
  input_file,
  sheet = "Quality Control"
)
```

Then, clean and separate the countries by adding the following script:

```r

document_countries <- institutions %>%
  select(
    publication_row_id,
    original_country_value = country_code
  ) %>%

  # A safety step in case any cell still contains semicolons
  separate_rows(
    original_country_value,
    sep = "\\s*;\\s*"
  ) %>%

  mutate(
    original_country_value = str_squish(
      str_to_lower(original_country_value)
    ),

    # Clean known errors and non-country values
    country_key = case_when(
      original_country_value == "switerzland" ~ "switzerland",
      original_country_value == "amsterdam"   ~ "netherlands",
      TRUE ~ original_country_value
    )
  ) %>%

  filter(
    !is.na(country_key),
    country_key != ""
  ) %>%

  # Count a publication only once per country
  distinct(
    publication_row_id,
    country_key,
    .keep_all = TRUE
  )
```

Create the coordinate lookup:

```r
country_lookup <- tribble(
  ~country_key,       ~source_code, ~country,          ~latitude, ~longitude, ~region,
  "australia",        "AUS",        "Australia",        -25.2744,  133.7751,  "Oceania",
  "austria",          "AUT",        "Austria",           47.5162,   14.5501,  "Europe",
  "canada",           "CAN",        "Canada",            56.1304, -106.3468,  "North America",
  "china",            "CHN",        "China",             35.8617,  104.1954,  "Asia",
  "czech republic",   "CZE",        "Czech Republic",    49.8175,   15.4730,  "Europe",
  "denmark",          "DNK",        "Denmark",           56.2639,    9.5018,  "Europe",
  "egypt",            "EGY",        "Egypt",             26.8206,   30.8025,  "Africa",
  "england",          "ENG",        "England",           52.3555,   -1.1743,  "Europe",
  "finland",          "FIN",        "Finland",           61.9241,   25.7482,  "Europe",
  "france",           "FRA",        "France",            46.2276,    2.2137,  "Europe",
  "germany",          "DEU",        "Germany",           51.1657,   10.4515,  "Europe",
  "greece",           "GRC",        "Greece",            39.0742,   21.8243,  "Europe",
  "hong kong",        "HKG",        "Hong Kong",         22.3193,  114.1694,  "Asia",
  "india",            "IND",        "India",             20.5937,   78.9629,  "Asia",
  "ireland",          "IRL",        "Ireland",           53.1424,   -7.6921,  "Europe",
  "italy",            "ITA",        "Italy",             41.8719,   12.5674,  "Europe",
  "japan",            "JPN",        "Japan",             36.2048,  138.2529,  "Asia",
  "lithuania",        "LTU",        "Lithuania",         55.1694,   23.8813,  "Europe",
  "netherlands",      "NLD",        "Netherlands",       52.1326,    5.2913,  "Europe",
  "new zealand",      "NZL",        "New Zealand",      -40.9006,  174.8860,  "Oceania",
  "norway",           "NOR",        "Norway",            60.4720,    8.4689,  "Europe",
  "pakistan",         "PAK",        "Pakistan",          30.3753,   69.3451,  "Asia",
  "philippines",      "PHL",        "Philippines",       12.8797,  121.7740,  "Asia",
  "qatar",            "QAT",        "Qatar",             25.3548,   51.1839,  "Asia",
  "scotland",         "SCT",        "Scotland",          56.4907,   -4.2026,  "Europe",
  "south africa",     "ZAF",        "South Africa",     -30.5595,   22.9375,  "Africa",
  "south korea",      "KOR",        "South Korea",       35.9078,  127.7669,  "Asia",
  "spain",            "ESP",        "Spain",             40.4637,   -3.7492,  "Europe",
  "sweden",           "SWE",        "Sweden",            60.1282,   18.6435,  "Europe",
  "switzerland",      "CHE",        "Switzerland",       46.8182,    8.2275,  "Europe",
  "thailand",         "THA",        "Thailand",          15.8700,  100.9925,  "Asia",
  "tunisia",          "TUN",        "Tunisia",           33.8869,    9.5375,  "Africa",
  "usa",              "USA",        "United States",     39.8283,  -98.5795,  "North America"
)
```

Then, join the countries to the publication details:

```r
publication_metadata <- publications %>%
  select(
    publication_row_id,
    source,
    title = TI,
    authors = AU,
    year = PY,
    journal = SO,
    doi = DI
  )

document_countries <- document_countries %>%
  left_join(
    country_lookup,
    by = "country_key"
  ) %>%
  left_join(
    publication_metadata,
    by = "publication_row_id"
  ) %>%
  select(
    publication_row_id,
    source,
    title,
    authors,
    year,
    journal,
    doi,
    original_country_value,
    country,
    source_code,
    latitude,
    longitude,
    region
  )
```

Check for unmatched country values:

```r
unmatched_countries <- document_countries %>%
  filter(is.na(source_code)) %>%
  distinct(original_country_value) %>%
  arrange(original_country_value)

unmatched_countries
```

Create the arcs sheet for Flourish, with one weighted arc per country:

```r
flourish_arcs <- document_countries %>%
  filter(!is.na(source_code)) %>%
  group_by(
    source_code,
    country,
    region
  ) %>%
  summarise(
    citing_document_count = n_distinct(publication_row_id),
    .groups = "drop"
  ) %>%
  mutate(
    destination_code = "USC",

    popup_text = paste0(
      country,
      ": ",
      citing_document_count,
      " citing document",
      if_else(citing_document_count == 1, "", "s")
    )
  ) %>%
  select(
    source_code,
    destination_code,
    citing_document_count,
    region,
    popup_text
  ) %>%
  arrange(
    desc(citing_document_count),
    source_code
  )

flourish_arcs
```

Then, create the locations sheet for Flourish:

```r
flourish_locations <- document_countries %>%
  filter(!is.na(source_code)) %>%
  distinct(
    code = source_code,
    location_name = country,
    latitude,
    longitude,
    region
  ) %>%

  # Add the destination
  bind_rows(
    tibble(
      code = "USC",
      location_name = "University of South Carolina, Columbia",
      latitude = 33.9971,
      longitude = -81.0274,
      region = "Destination"
    )
  ) %>%
  arrange(location_name)

flourish_locations
```

Next, verify that all arc codes have coordinates. The script below should return "character(0)".

```r
arc_codes <- unique(c(
  flourish_arcs$source_code,
  flourish_arcs$destination_code
))

missing_location_codes <- setdiff(
  arc_codes,
  flourish_locations$code
)

missing_location_codes
```

Finally, export the Excel file. Update the file path to `path_to_collaborating_countries.xlsx`.

```r
output_file <- ("path_to_flourish_countries.xlsx")

write_xlsx(
  list(
    "Flourish Arcs" = flourish_arcs,
    "Locations" = flourish_locations,
    "Document Countries" = document_countries,
    "Publications" = publications,
    "Publication Institutions" = institutions,
    "Quality Control" = quality_control
  ),
  path = output_file
)

message("Created: ", output_file)
```

After the file has been prepared, log into Flourish through Canva. Choose the template, "Connection Map (under "Arc Maps"). Upload `Flourish Arcs` into the main Data sheet and select:

- Source location: `source_code`
- Destination location: `destination_code`
- Value: `citing_document_count`
- Category: `region`
- Info for popups: `popup_text`

Then, upload `Locations` into Flourish's Locations sheet and select:

- Code: `code`
- Name: `location_name`
- Latitude: `latitude`
- Longitude: `longitude`
