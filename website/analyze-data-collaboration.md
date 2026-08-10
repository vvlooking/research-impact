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
    institution_id != ""
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

## Create a Collaboration Network Visualization (Countries) in Tableau

Start by preparing the data in RStudio. If necessary, install the required packages:

```r
install.packages(
  c(
    "readxl",
    "dplyr",
    "tidyr",
    "stringr",
    "writexl",
    "sf",
    "rnaturalearth",
    "countrycode"
  )
)
```

Then, add the following script, updating the file paths for `input_file` to the standardized dataset and `output_file` to the Working/ folder. For every publication, this script will identify its unique countries or states, remove repeated occurrences of the same location within that publication, create every unique pair of locations, and count how many publications contain that pair.

```r
# Load libraries
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)
library(sf)
library(rnaturalearth)
library(countrycode)

# 1. Set file paths

input_file <- "path_to_standardized_file.xlsx" # Update file path to standardized dataset
output_file <- "path_to_tableau-geographic-collaboration.xlsx" # Update file path to Working/ folder

# Change this if the publication data are not on the first sheet
input_sheet <- 1


# 2. Column names

country_column <- "country_code"
state_column <- "state_code"


# 3. Import publication data

publications <- read_excel(
  input_file,
  sheet = input_sheet
)

# Add a publication identifier if one does not already exist
if (!"publication_row_id" %in% names(publications)) {
  publications <- publications |>
    mutate(publication_row_id = row_number())
}

# Confirm that the required columns exist
required_columns <- c(
  "publication_row_id",
  country_column,
  state_column
)

missing_columns <- setdiff(
  required_columns,
  names(publications)
)

if (length(missing_columns) > 0) {
  stop(
    "The following required columns are missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

# 4. Separate location codes

separate_location_codes <- function(data, location_column) {

  data |>
    select(
      publication_row_id,
      location_codes = all_of(location_column)
    ) |>
    separate_longer_delim(
      location_codes,
      delim = ";"
    ) |>
    mutate(
      location_code = location_codes |>
        str_squish() |>
        str_to_upper()
    ) |>
    filter(
      !is.na(location_code),
      location_code != "",
      location_code != "NA"
    ) |>
    distinct(
      publication_row_id,
      location_code
    ) |>
    select(
      publication_row_id,
      location_code
    )
}


# 5. Prepare country codes

publication_countries <- separate_location_codes(
  publications,
  country_column
)

# Convert two-character country codes to ISO three-character codes.
# Existing three-character codes remain unchanged.
publication_countries <- publication_countries |>
  mutate(
    location_code = case_when(
      str_length(location_code) == 2 ~
        countrycode(
          location_code,
          origin = "iso2c",
          destination = "iso3c",
          warn = TRUE
        ),

      TRUE ~ location_code
    )
  ) |>
  filter(
    !is.na(location_code),
    location_code != ""
  ) |>
  distinct(
    publication_row_id,
    location_code
  )


# 6. Create country coordinate lookup 

world_boundaries <- ne_countries(
  scale = "small",
  returnclass = "sf"
) |>
  filter(
    iso_a3 != "-99",
    name_long != "Antarctica"
  )

# Transform before identifying a representative point
country_points <- world_boundaries |>
  st_transform(6933) |>
  st_point_on_surface() |>
  st_transform(4326)

country_coordinates <- st_coordinates(country_points)

country_lookup <- country_points |>
  st_drop_geometry() |>
  transmute(
    location_code = str_to_upper(iso_a3),
    location_name = name_long,
    latitude = country_coordinates[, 2],
    longitude = country_coordinates[, 1]
  ) |>
  distinct(
    location_code,
    .keep_all = TRUE
  )


# 7. Create a U.S. coordinate lookup

state_lookup <- tibble(
  location_code = state.abb,
  location_name = state.name,
  latitude = state.center$y,
  longitude = state.center$x
) |>
  bind_rows(
    tibble(
      location_code = c(
        "DC",
        "PR",
        "VI",
        "GU",
        "AS",
        "MP"
      ),
      location_name = c(
        "District of Columbia",
        "Puerto Rico",
        "U.S. Virgin Islands",
        "Guam",
        "American Samoa",
        "Northern Mariana Islands"
      ),
      latitude = c(
        38.9072,
        18.2208,
        18.3358,
        13.4443,
        -14.2710,
        15.0979
      ),
      longitude = c(
        -77.0369,
        -66.5901,
        -64.8963,
        144.7937,
        -170.1322,
        145.6739
      )
    )
  )


# 8. Prepare state codes

publication_states <- separate_location_codes(
  publications,
  state_column
) |>
  # Retain only codes found in the U.S. state lookup
  semi_join(
    state_lookup,
    by = "location_code"
  )


# 9. Create map tables

create_map_tables <- function(
    publication_locations,
    location_lookup
) {

  # Attach coordinates and names
  located_publications <- publication_locations |>
    inner_join(
      location_lookup,
      by = "location_code"
    ) |>
    distinct(
      publication_row_id,
      location_code,
      .keep_all = TRUE
    )

  # Count unique publications involving each location
  nodes <- located_publications |>
    count(
      location_code,
      location_name,
      latitude,
      longitude,
      name = "location_publications",
      sort = TRUE
    )

  # Create every unordered pair of locations appearing
  # on the same publication
  links <- located_publications |>
    select(
      publication_row_id,
      from = location_code
    ) |>
    inner_join(
      located_publications |>
        select(
          publication_row_id,
          to = location_code
        ),
      by = "publication_row_id",
      relationship = "many-to-many"
    ) |>
    filter(from < to) |>
    distinct(
      publication_row_id,
      from,
      to
    ) |>
    count(
      from,
      to,
      name = "collaborations",
      sort = TRUE
    )

  # Add origin coordinates
  links <- links |>
    left_join(
      location_lookup |>
        transmute(
          from = location_code,
          from_name = location_name,
          from_latitude = latitude,
          from_longitude = longitude
        ),
      by = "from"
    )

  # Add destination coordinates
  links <- links |>
    left_join(
      location_lookup |>
        transmute(
          to = location_code,
          to_name = location_name,
          to_latitude = latitude,
          to_longitude = longitude
        ),
      by = "to"
    ) |>
    mutate(
      path_id = str_c(from, to, sep = "__")
    )

  # Tableau needs two rows for each path:
  # one for the origin and one for the destination.
  map_data <- bind_rows(

    links |>
      transmute(
        path_id,
        path_order = 1L,
        location_code = from,
        location_name = from_name,
        latitude = from_latitude,
        longitude = from_longitude,
        collaborations
      ),

    links |>
      transmute(
        path_id,
        path_order = 2L,
        location_code = to,
        location_name = to_name,
        latitude = to_latitude,
        longitude = to_longitude,
        collaborations
      )

  ) |>
    left_join(
      nodes |>
        select(
          location_code,
          location_publications
        ),
      by = "location_code"
    ) |>
    arrange(
      path_id,
      path_order
    )

  list(
    nodes = nodes,
    links = links,
    map_data = map_data
  )
}


# 10. Create country tables

country_results <- create_map_tables(
  publication_countries,
  country_lookup
)


# Identify country codes that did not receive coordinates
unrecognized_countries <- publication_countries |>
  distinct(location_code) |>
  anti_join(
    country_lookup,
    by = "location_code"
  )


# 11. Create state tables

state_results <- create_map_tables(
  publication_states,
  state_lookup
)


# Identify state codes that did not receive coordinates
unrecognized_states <- separate_location_codes(
  publications,
  state_column
) |>
  distinct(location_code) |>
  anti_join(
    state_lookup,
    by = "location_code"
  )


# 12. Create summary

summary_table <- tibble(
  measure = c(
    "Publications in input file",
    "Countries represented",
    "Country collaboration links",
    "States represented",
    "State collaboration links"
  ),
  value = c(
    nrow(publications),
    nrow(country_results$nodes),
    nrow(country_results$links),
    nrow(state_results$nodes),
    nrow(state_results$links)
  )
)


# 13. Export Tableau workbook

write_xlsx(
  list(
    Summary = summary_table,
    Country_Map = country_results$map_data,
    Country_Nodes = country_results$nodes,
    Country_Links = country_results$links,
    State_Map = state_results$map_data,
    State_Nodes = state_results$nodes,
    State_Links = state_results$links,
    Unrecognized_Countries = unrecognized_countries,
    Unrecognized_States = unrecognized_states
  ),
  output_file
)

message(
  "Tableau workbook written to: ",
  output_file
)
```

Open the Excel file and inspect these sheets: Country_Map, Country_Nodes, Country_Links, State_Map, State_Nodes, State_Links, Unrecognized_Countries, Unrecognized_States. Both Uncrecognized sheets should be empty. If not, review the listed codes.

Next, Open Tableau Public.
