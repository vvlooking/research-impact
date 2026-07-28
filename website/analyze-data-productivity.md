# Productivity

## Checklist

- [ ] Calculate author indexes (individual reports only)
- [ ] Calculate the number of documents published per year (in Excel)
- [ ] Calculate the average number of documents published per year
- [ ] Create table: "Number of Publications Produced per Year" (in Word)
- [ ] Create figure: "Average Productivity per Active Year"
- [ ] Calculate the number and percentage of document types (in Excel) and present them in a table (in Word)
- [ ] Calculate the number of documents by authorship order (i.e., sole, first, and last author)

## Author Indexes
Identify the researcher's i10-index using their Google Scholar profile. Calculate the researcher's h-index, g-index, and m-index using the script below. Update the file path for `file_path_to_standardized_dataset.xlsx` and specify the researcher's name in the variable, `researcher_name <- LAST NAME INITIAL`.

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

## Calculate the Average Number of Documents Published per Year
Calculate the average number of documents published each year by running the script below. Update the file paths, `path_to_standardized_dataset.xlsx` and `path_to_figures_folder.png`

```r
# Load libraries
library(readxl)
library(dplyr)

# Import spreadsheet (Excel)
df <- read_excel("path_to_standardized_dataset.xlsx") # Update file path to the de-deduplicated standardized dataset

pubs_per_year <- df %>%
  mutate(PY = as.integer(PY)) %>%
  count(PY, name = "Publications")

pubs_per_year

avg_publications_per_year <- mean(pubs_per_year$Publications)

avg_publications_per_year
```

## Create Figure: "Average Productivity per Active Year"
Create a bar graph to visualize the number of publications produced each year. In the script below, update the file path for `file_path_to_standardized_dataset.xlsx` and `path_to_figures_folder/average-productivity-per-year.png`, and then update the years for `START_YEAR` and `END_YEAR`.

```r
# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Import spreadsheet (Excel)
df <- read_excel("file_path_to_standardized_dataset.xlsx") # Update file path to the de-deduplicated standardized dataset


yearly_counts <- df %>%
  filter(PY >= START_YEAR, PY <= END_YEAR) %>% # Update the publication start and end years
  mutate(PY = as.integer(PY)) %>%
  count(PY, name = "Publications") %>%
  complete(PY = START_YEAR:END_YEAR, fill = list(Publications = 0)) %>% # Update the publication start and end years
  arrange(PY)

# Average publications per ACTIVE year (exclude years with 0 pubs)
avg_pubs_active_year <- yearly_counts %>%
  filter(Publications > 0) %>%
  summarise(avg = mean(Publications)) %>%
  pull(avg)

avg_pubs_active_year

# Create figure
p <- ggplot(yearly_counts, aes(x = PY, y = Publications)) +
  geom_col(fill = "#73000a") +
  geom_hline(yintercept = avg_pubs_active_year, linetype = "dashed", linewidth = 1) +
  scale_x_continuous(breaks = seq(START_YEAR, END_YEAR, by = 1)) + # Update the publication start and end years
  labs(
    x = "Publication Year",
    y = "Number of Publications",
    ) +
  theme_minimal(base_size = 14)

p

ggsave(
  filename = "path_to_figures_folder/average-productivity-per-year.png", # Update file path to Figures folder
  plot     = p,
  width    = 7,
  height   = 4,
  dpi      = 300
)
```

## Calculate the Number of Documents by Authorship Order
First, identify the number of sole-authored publications using the script below. Update the file path for `file_path_to_standardized_dataset.xlsx`.

```r
# Load libraries
library(readxl)
library(dplyr)
library(stringr)

# Import spreadsheet (Excel)
df <- read_excel("file_path_to_standardized_dataset.xlsx") # Update file path to the de-deduplicated standardized dataset

solo_authored <- df %>%
  filter(!is.na(AU_key) & AU_key != "") %>%
  mutate(
    n_authors = str_count(AU_key, ";") + 1
  ) %>%
  filter(n_authors == 1)

# Number of solo-authored articles
n_solo <- nrow(solo_authored)

n_solo

total_articles <- df %>%
  filter(!is.na(AU_key) & AU_key != "") %>%
  nrow()

pct_solo <- n_solo / total_articles * 100

pct_solo

solo_authored %>%
  select(AU_key, PY) %>%
  head(10)
```

Then, identify the number of co-authored publications on which the researcher(s) served as first and/or last author. Update `faculty_string` with the relevant names.

```r
# First author

faculty_string <- "Doe J; Smith J" # Adjust names

faculty <- str_split(faculty_string, ";\\s*")[[1]] %>%
  str_squish()

coauthored_df <- df %>%
  filter(!is.na(AU_key) & AU_key != "") %>%
  mutate(
    author_count = str_count(AU_key, ";") + 1,
    first_author = str_trim(str_extract(AU_key, "^[^;]+"))
  ) %>%
  filter(author_count > 1)

first_author_counts <- coauthored_df %>%
  filter(first_author %in% faculty) %>%
  count(first_author, sort = TRUE)

first_author_counts

# Last Author

faculty_string <- "Doe J; Smith J" # Adjust names

faculty <- str_split(faculty_string, ";\\s*")[[1]] %>%
  str_squish()

coauthored_df <- df %>%
  filter(!is.na(AU_key) & AU_key != "") %>%
  mutate(
    author_count = str_count(AU_key, ";") + 1,
    last_author = str_trim(str_extract(AU_key, "[^;]+$"))
  ) %>%
  filter(author_count > 1)

last_author_counts <- coauthored_df %>%
  filter(last_author %in% faculty) %>%
  count(last_author, sort = TRUE)

last_author_counts
```
