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

## Retrieve CiteScores for Journals

In Google Colab, run the following Python script. Update the CiteScore year as necessary.

```r
import getpass
import json
import re
import time

import pandas as pd
import requests

try:
    from google.colab import files
    IN_COLAB = True
except ImportError:
    IN_COLAB = False


# Change only this value between runs (e.g., 2024; 2025)
TARGET_YEAR = 2024

# A CiteScore year uses the target year plus the preceding three years.
WINDOW_START = TARGET_YEAR - 3

CITATIONS_COLUMN = (
    f"{WINDOW_START}-{str(TARGET_YEAR)[-2:]} Citations"
)
DOCUMENTS_COLUMN = (
    f"{WINDOW_START}-{str(TARGET_YEAR)[-2:]} Documents"
)
OUTPUT_FILE = f"scopus_journal_metrics_{TARGET_YEAR}.csv"


if IN_COLAB:
    uploaded = files.upload()
    if not uploaded:
        raise RuntimeError("No CSV was uploaded.")
    input_path = next(iter(uploaded))
else:
    input_path = "Sample ISSNs.csv"

input_df = pd.read_csv(input_path, dtype=str, keep_default_na=False)
issn_columns = [c for c in input_df.columns if c.strip().lower() == "issn"]
if not issn_columns:
    raise ValueError(f"No ISSN column found. Columns present: {list(input_df.columns)}")
ISSN_COLUMN = issn_columns[0]

API_KEY = getpass.getpass("Elsevier/Scopus API key (hidden): ").strip()
if not API_KEY:
    raise ValueError("An API key is required.")
INST_TOKEN = getpass.getpass("Institution token (optional; Enter to skip): ").strip()

REQUEST_TIMEOUT_SECONDS = 45
MAX_RETRIES = 5
PAUSE_BETWEEN_UNIQUE_ISSNS = 0.15


def normalize_issn(value):
    text = re.sub(r"[^0-9Xx]", "", str(value or "")).upper()
    return text if re.fullmatch(r"[0-9]{7}[0-9X]", text) else ""


def objects(value):
    yield value
    if isinstance(value, dict):
        for child in value.values():
            yield from objects(child)
    elif isinstance(value, list):
        for child in value:
            yield from objects(child)


def scalar(value):
    if isinstance(value, (str, int, float)) and not isinstance(value, bool):
        return value
    if isinstance(value, dict):
        for key in ("$", "value", "_", "#text"):
            if key in value:
                return scalar(value[key])
    return None


def first_key(root, keys):
    wanted = {k.lower() for k in keys}
    for node in objects(root):
        if isinstance(node, dict):
            for key, value in node.items():
                if key.lower() in wanted:
                    result = scalar(value)
                    if result not in (None, ""):
                        return result
    return None


def number(value):
    if value in (None, ""):
        return None
    try:
        result = float(str(value).replace(",", "").replace("%", "").strip())
        return int(result) if result.is_integer() else result
    except (TypeError, ValueError):
        return None


def direct_year(node):
    """Read a year directly attached to a JSON object."""
    if not isinstance(node, dict):
        return None
    lowered = {str(k).lower(): v for k, v in node.items()}
    for key in ("@year", "year", "metricyear", "citescorecurrentmetricyear"):
        value = number(lowered.get(key))
        if isinstance(value, int) and 1900 <= value <= 2100:
            return value
    return None


def target_year_scopes(root, target_year):
    """Return all JSON objects explicitly labelled with target_year."""
    return [
        node for node in objects(root)
        if isinstance(node, dict) and direct_year(node) == target_year
    ]


def best_scope(scopes, desired_keys):
    """Choose the year-labelled scope containing the most desired fields."""
    wanted = {key.lower() for key in desired_keys}

    def score(scope):
        found = set()
        for node in objects(scope):
            if isinstance(node, dict):
                found.update(str(key).lower() for key in node)
        return len(found & wanted)

    return max(scopes, key=score) if scopes else {}


def highest_percentile(root):
    values = []
    for node in objects(root):
        if isinstance(node, dict):
            for key, value in node.items():
                if key.lower() in {"percentile", "highestpercentile"}:
                    parsed = number(scalar(value))
                    if parsed is not None and 0 <= parsed <= 100:
                        values.append(parsed)
    return max(values) if values else None


def metric_for_year(root, target_year, container_names, value_names):
    """Return SNIP/SJR only from an object carrying the requested year."""
    containers = {name.lower() for name in container_names}
    values = {name.lower() for name in value_names}
    matches = []
    for node in objects(root):
        if not isinstance(node, dict):
            continue
        if any(str(key).lower() in containers for key in node):
            for subnode in objects(node):
                if not isinstance(subnode, dict) or direct_year(subnode) != target_year:
                    continue
                for key, value in subnode.items():
                    if str(key).lower() in values:
                        parsed = number(scalar(value))
                        if parsed is not None:
                            matches.append(parsed)
    return matches[0] if matches else None


session = requests.Session()
session.headers.update({
    "Accept": "application/json",
    "X-ELS-APIKey": API_KEY,
    "X-ELS-ResourceVersion": "new",
    "User-Agent": "Colab-Scopus-Serial-Metrics/2.0",
})
if INST_TOKEN:
    session.headers["X-ELS-Insttoken"] = INST_TOKEN


def request_serial_title(issn):
    url = f"https://api.elsevier.com/content/serial/title/issn/{issn}"
    params = {"view": "CITESCORE", "httpAccept": "application/json"}
    last_error = "Request failed"
    for attempt in range(MAX_RETRIES):
        try:
            response = session.get(url, params=params, timeout=REQUEST_TIMEOUT_SECONDS)
        except requests.RequestException as exc:
            last_error = f"Network error: {exc}"
            if attempt == MAX_RETRIES - 1:
                raise RuntimeError(last_error) from exc
            time.sleep(2 ** attempt)
            continue

        if response.status_code == 200:
            return response.json()
        if response.status_code == 404:
            raise LookupError("ISSN not found")
        if response.status_code in (429, 500, 502, 503, 504):
            retry_after = response.headers.get("Retry-After")
            delay = float(retry_after) if retry_after and retry_after.isdigit() else 2 ** attempt
            last_error = f"HTTP {response.status_code}: {response.text[:300]}"
            if attempt < MAX_RETRIES - 1:
                time.sleep(delay)
                continue
        raise RuntimeError(f"HTTP {response.status_code}: {response.text[:500]}")
    raise RuntimeError(last_error)


def get_entry(payload):
    for node in objects(payload):
        if isinstance(node, dict) and "entry" in node:
            raw = node["entry"]
            entry = raw[0] if isinstance(raw, list) and raw else raw
            if isinstance(entry, dict):
                return entry
    return payload


def extract_metrics(payload, input_issn):
    entry = get_entry(payload)
    scopes = target_year_scopes(entry, TARGET_YEAR)
    year_scope = best_scope(
        scopes,
        [
            "citeScore", "citeScoreCurrentMetric", "citationCount",
            "citationsCount", "documentCount", "documentsCount",
            "percentCited", "percentile",
        ],
    )

    # The current year can also be represented by CiteScoreCurrentMetric at the
    # entry level rather than by a historical citeScoreYearInfo object.
    current_year = number(first_key(entry, ["citeScoreCurrentMetricYear"]))
    if not year_scope and current_year == TARGET_YEAR:
        year_scope = entry

    if not year_scope:
        available_years = sorted({
            direct_year(node) for node in objects(entry)
            if isinstance(node, dict) and direct_year(node) is not None
        })
        raise LookupError(
            f"CiteScore {TARGET_YEAR} not found; years present in response: "
            f"{available_years or 'none detected'}"
        )

    calculation = best_scope(
        [node for node in objects(year_scope) if isinstance(node, dict)],
        ["citationCount", "citationsCount", "documentCount", "documentsCount", "percentCited"],
    )

    if current_year == TARGET_YEAR:
        citescore = number(first_key(entry, ["citeScoreCurrentMetric"]))
    else:
        citescore = number(first_key(year_scope, ["citeScore", "citescoreValue", "metricValue"]))

    return {
        "Input ISSN": input_issn,
        "Source title": first_key(entry, ["dc:title", "source-title", "sourceTitle", "title"]),
        "CiteScore": citescore,
        "Highest percentile": highest_percentile(year_scope),
        CITATIONS_COLUMN: number(first_key(calculation, ["citationCount", "citationsCount"])),
        DOCUMENTS_COLUMN: number(first_key(calculation, ["documentCount", "documentsCount"])),
        "% Cited": number(first_key(calculation, ["percentCited", "percentageCited"])),
        "SNIP": metric_for_year(entry, TARGET_YEAR, ["SNIPList", "SNIP"], ["SNIP", "snip"]),
        "SJR": metric_for_year(entry, TARGET_YEAR, ["SJRList", "SJR"], ["SJR", "sjr"]),
        "Publisher": first_key(entry, ["dc:publisher", "publisher", "publisher-name"]),
        "Metric year": TARGET_YEAR,
        "Status": "OK",
        "Error": "",
    }


cache = {}
results = []

for row_number, raw_issn in enumerate(input_df[ISSN_COLUMN], start=2):
    display_issn = str(raw_issn).strip()
    issn = normalize_issn(display_issn)
    if not issn:
        results.append({
            "Input ISSN": display_issn,
            "Status": "INVALID_ISSN",
            "Error": f"Row {row_number}: expected eight ISSN characters",
        })
        continue

    if issn not in cache:
        try:
            cache[issn] = extract_metrics(request_serial_title(issn), display_issn)
        except Exception as exc:
            cache[issn] = {
                "Input ISSN": display_issn,
                "Metric year": TARGET_YEAR,
                "Status": "ERROR",
                "Error": str(exc),
            }
        time.sleep(PAUSE_BETWEEN_UNIQUE_ISSNS)

    result = cache[issn].copy()
    result["Input ISSN"] = display_issn
    results.append(result)
    print(f"{len(results):>4}/{len(input_df)}  {display_issn}: {result.get('Status')}")

output_columns = [
    "Input ISSN", "Source title", "CiteScore", "Highest percentile",
    CITATIONS_COLUMN, DOCUMENTS_COLUMN, "% Cited", "SNIP", "SJR",
    "Publisher", "Metric year", "Status", "Error",
]
output_df = pd.DataFrame(results).reindex(columns=output_columns)
output_df.to_csv(OUTPUT_FILE, index=False, encoding="utf-8-sig")

print(f"\nSaved {len(output_df)} rows to {OUTPUT_FILE}")
print(output_df["Status"].value_counts(dropna=False).to_string())
if IN_COLAB:
    display(output_df.head(10))
    files.download(OUTPUT_FILE)
else:
    print(output_df.head(10).to_string(index=False))
```

## Identify Publication Share in the Top 5% and 25% of Journals by CiteScore

```r
# Calculate publication shares in the top 5% and top 25% of journals by CiteScore.

# Select the Scopus journal-metrics CSV when prompted.
input_file <- "file_path" # Add file here

# Thresholds based on the Scopus "Highest percentile" field.
top_5_threshold <- 95
top_25_threshold <- 75

# Read identifiers and headers exactly as supplied.
publications <- read.csv(
  input_file,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8-BOM"
)

percentile_column <- "Highest percentile"

if (!percentile_column %in% names(publications)) {
  stop(
    paste0(
      "The file does not contain a column named '", percentile_column,
      "'. Columns found: ", paste(names(publications), collapse = ", ")
    )
  )
}

# Convert values such as "95" or "95%" to numbers.
percentile_text <- trimws(as.character(publications[[percentile_column]]))
percentile_text[percentile_text == ""] <- NA_character_
highest_percentile <- suppressWarnings(
  as.numeric(gsub("%", "", percentile_text, fixed = TRUE))
)

# Values outside the valid percentile range are treated as missing.
invalid_range <- !is.na(highest_percentile) &
  (highest_percentile < 0 | highest_percentile > 100)
highest_percentile[invalid_range] <- NA_real_

publications[[percentile_column]] <- highest_percentile

# Missing percentiles do not qualify, but their publication rows remain in the denominator. This preserves the publication-share interpretation.
publications$Top_5_percent_by_CiteScore <-
  !is.na(highest_percentile) & highest_percentile >= top_5_threshold

publications$Top_25_percent_by_CiteScore <-
  !is.na(highest_percentile) & highest_percentile >= top_25_threshold

publications$CiteScore_percentile_group <- ifelse(
  is.na(highest_percentile),
  "Missing percentile",
  ifelse(
    highest_percentile >= top_5_threshold,
    "Top 5%",
    ifelse(highest_percentile >= top_25_threshold, "Top 25% (not top 5%)", "Below top 25%")
  )
)

total_publications <- nrow(publications)
publications_with_percentile <- sum(!is.na(highest_percentile))
top_5_publications <- sum(publications$Top_5_percent_by_CiteScore)
top_25_publications <- sum(publications$Top_25_percent_by_CiteScore)

if (total_publications == 0) {
  stop("The input file contains no publication rows.")
}

summary_results <- data.frame(
  Metric = c(
    "Publication share in the Top 5% of Journals by CiteScore",
    "Publication share in the Top 25% of Journals by CiteScore"
  ),
  Qualifying_publications = c(top_5_publications, top_25_publications),
  Total_publications = total_publications,
  Publication_share = c(
    top_5_publications / total_publications,
    top_25_publications / total_publications
  ),
  Publication_share_percent = c(
    sprintf("%.1f%%", 100 * top_5_publications / total_publications),
    sprintf("%.1f%%", 100 * top_25_publications / total_publications)
  ),
  stringsAsFactors = FALSE
)

# Save outputs next to the source CSV.
input_directory <- dirname(normalizePath(input_file))
input_stem <- tools::file_path_sans_ext(basename(input_file))
classified_file <- file.path(
  input_directory,
  paste0(input_stem, "_with_citescore_groups.csv")
)
summary_file <- file.path(
  input_directory,
  paste0(input_stem, "_citescore_publication_shares.csv")
)

write.csv(publications, classified_file, row.names = FALSE, na = "")
write.csv(summary_results, summary_file, row.names = FALSE, na = "")

cat("\nCiteScore journal publication shares\n")
cat("------------------------------------\n")
cat("Total publication rows:", total_publications, "\n")
cat("Rows with a usable percentile:", publications_with_percentile, "\n")
cat("Rows with a missing/invalid percentile:",
    total_publications - publications_with_percentile, "\n\n")
print(summary_results, row.names = FALSE)
cat("\nClassified publications saved to:\n", classified_file, "\n", sep = "")
cat("\nSummary saved to:\n", summary_file, "\n", sep = "")
```
