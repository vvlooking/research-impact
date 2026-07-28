# Productivity

## Checklist

- [ ] Calculate author indexes (individual reports only)
- [ ] Calculate the number of documents published per year (in Excel)
- [ ] Calculate the average number of documents published per year
- [ ] Create table: "Number of Publications Produced per Year" (in Word)
- [ ] Create figure: "Average Productivity per Active Year"
- [ ] Calculate the number and percentage of document types
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
Calculate the average number of documents published per year by running the script below. Update the file path, `file_path_to_standardized_dataset.xlsx`.

```r
# Load libraries
library(readxl)
library(dplyr)

# Import spreadsheet (Excel)
df <- read_excel("file_path_to_standardized_dataset.xlsx") # Update file path to the de-deduplicated standardized dataset

pubs_per_year <- df %>%
  mutate(PY = as.integer(PY)) %>%
  count(PY, name = "Publications")

pubs_per_year

avg_publications_per_year <- mean(pubs_per_year$Publications)

avg_publications_per_year
```

## Create Figure: "Average Productivity per Active Year"
Create a bar graph to visualize the number of publications produced each year. In the script below, update the file path for `file_path_to_standardized_dataset.xlsx`.


## Figure (Average Productivity per Active Year)

# Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Import spreadsheet (Excel)
df <- read_excel("file_path_to_standardized_dataset.xlsx") # Update file path to the de-deduplicated standardized dataset


yearly_counts <- df %>%
  filter(PY >= 2019, PY <= 2025) %>% # adjust years
  mutate(PY = as.integer(PY)) %>%
  count(PY, name = "Publications") %>%
  complete(PY = START_YEAR:END_YEAR, fill = list(Publications = 0)) %>% # adjust years
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
  scale_x_continuous(breaks = seq(2019, 2025, by = 1)) + # adjust years
  labs(
    x = "Publication Year",
    y = "Number of Publications",
    ) +
  theme_minimal(base_size = 14)

p

ggsave(
  filename = "file_name", # adjust file name
  plot     = p,
  width    = 7,
  height   = 4,
  dpi      = 300
)
