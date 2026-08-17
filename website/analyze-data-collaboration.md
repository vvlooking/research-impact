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
- [ ] Create a collaboration network visualization (countries) in Flourish (via Canva)
- [ ] Create a collaboration network visualization (states) in Flourish (via Canva)


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

## Create a Collaboration Network Visualization (Institutions) in VOSViewer

Create a duplicate copy of the `deduplicated-standardized.xlsx` file, save it as a csv, and rename it as `-vosviewer-institutions.csv`. Rename the "canonical_affiliations" column to "Affiliations" and delete the "affiliations" column

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

Add Washington D.C. and common U.S. territories in case they occur in the data:

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

# Examine the lookup

View(state_lookup)
```

Attach state information:

```r
unmatched_states <- document_states %>%
  filter(is.na(state_name)) %>%
  count(original_state_value, sort = TRUE)

unmatched_states
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
output_file <- "file_path.xlsx" # Update file path

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
