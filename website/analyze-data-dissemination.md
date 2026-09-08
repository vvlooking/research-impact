# Dissemination

## Checklist

- [ ] Export journal metrics from Journal Citation Reports and Scopus
- [ ] Add journal metrics to standardized dataset
- [ ] Calculate the number of unique journals and then identify the most common journals
- [ ] Identify journals in the top 10% of their category by JIF percentile
- [ ] Calculate the number and percentage of documents in journals by quartile (using a pivot table in Excel)
- [ ] Create figure: "Percentage of Documents Published in Journals by JIF Quartile" (using Flourish via Canva)
- [ ] Calculate JCR categories (using a pivot table in Excel)
- [ ] Create figure: "Articles by Journal Citation Reports (JCR) Category" (using Flourish via Canva)

## Export Journal Metrics from Journal Citation Reports and Scopus

In Journal Citation Reports, click "Journals." Filter by ISSN and paste in all ISSNs from the standardized Excel file. Customize the indicators to include Total Citations, JIF, JIF Rank, JCI, JCI Rank, JCI Quartile, JCI Percentile, Eigenfactor, Normalized Eigenfactor, JIF Percentile, and JIF Quartile. Then, export as a csv file and name it "lastname/department-jcr.csv".

In Google Colab, open the Secrets panel using the key icon on the left. Create a secret named SCOPUS_API_KEY. Put the Scopus API key in the Value field and enable Notebook access. Then, run the following script:

```r
import getpass
import json
import math
import re
import time
from pathlib import Path

import pandas as pd
import requests

try:
    from google.colab import files
    IN_COLAB = True
except ImportError:
    IN_COLAB = False

if IN_COLAB:
    uploaded = files.upload()
    if not uploaded:
        raise RuntimeError("No CSV was uploaded.")
    input_path = next(iter(uploaded))
else:
    # When testing locally, replace this with the path to your CSV.
    input_path = "Sample ISSNs.csv"

input_df = pd.read_csv(input_path, dtype=str, keep_default_na=False)
issn_columns = [c for c in input_df.columns if c.strip().lower() == "issn"]
if not issn_columns:
    raise ValueError(f"No ISSN column found. Columns present: {list(input_df.columns)}")
ISSN_COLUMN = issn_columns[0]


# Enter credentials and configure the request
API_KEY = getpass.getpass("Elsevier/Scopus API key (hidden): ").strip()
if not API_KEY:
    raise ValueError("An API key is required.")

# Usually only the API key is needed. If Elsevier supplied an institutional token,
# paste it when prompted; otherwise press Enter.
INST_TOKEN = getpass.getpass("Institution token (optional; press Enter to skip): ").strip()

OUTPUT_FILE = "scopus_journal_metrics.csv"
REQUEST_TIMEOUT_SECONDS = 45
MAX_RETRIES = 5
PAUSE_BETWEEN_UNIQUE_ISSNS = 0.15


def normalize_issn(value):
    """Return an eight-character ISSN without its display hyphen."""
    text = re.sub(r"[^0-9Xx]", "", str(value or "")).upper()
    return text if re.fullmatch(r"[0-9]{7}[0-9X]", text) else ""


def objects(value):
    """Yield every nested dict/list node."""
    yield value
    if isinstance(value, dict):
        for child in value.values():
            yield from objects(child)
    elif isinstance(value, list):
        for child in value:
            yield from objects(child)


def scalar(value):
    """Unwrap common Elsevier JSON scalar representations."""
    if isinstance(value, (str, int, float)) and not isinstance(value, bool):
        return value
    if isinstance(value, dict):
        for key in ("$", "value", "_", "#text"):
            if key in value:
                return scalar(value[key])
    return None


def first_key(root, keys):
    """Find the first scalar value for any key, case-insensitively."""
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
    if value is None or value == "":
        return None
    try:
        result = float(str(value).replace(",", "").replace("%", "").strip())
        return int(result) if result.is_integer() else result
    except (TypeError, ValueError):
        return None


def year_of(node):
    if not isinstance(node, dict):
        return None
    for key in ("@year", "year", "metricYear", "citeScoreCurrentMetricYear"):
        value = number(node.get(key))
        if isinstance(value, int) and 1900 <= value <= 2100:
            return value
    return None


def latest_metric_node(root, required_keys):
    """Choose the newest dict containing at least one requested field."""
    wanted = {k.lower() for k in required_keys}
    candidates = []
    for node in objects(root):
        if not isinstance(node, dict):
            continue
        node_keys = {k.lower() for k in node}
        if node_keys & wanted:
            # In Elsevier responses, a metric's year may live on a parent object.
            candidates.append((year_of(node) or -1, node))
    return max(candidates, key=lambda pair: pair[0])[1] if candidates else {}


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


def metric_for_latest_year(root, container_names, value_names):
    """Extract the newest SNIP/SJR value, preferring a named container."""
    named = []
    containers = {name.lower() for name in container_names}
    values = {name.lower() for name in value_names}
    for node in objects(root):
        if not isinstance(node, dict):
            continue
        if any(k.lower() in containers for k in node):
            for subnode in objects(node):
                if isinstance(subnode, dict):
                    for key, value in subnode.items():
                        if key.lower() in values:
                            parsed = number(scalar(value))
                            if parsed is not None:
                                named.append((year_of(subnode) or -1, parsed))
    if named:
        return max(named, key=lambda pair: pair[0])[1]
    return number(first_key(root, value_names))


session = requests.Session()
session.headers.update({
    "Accept": "application/json",
    "X-ELS-APIKey": API_KEY,
    "X-ELS-ResourceVersion": "new",
    "User-Agent": "Colab-Scopus-Serial-Metrics/1.0",
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


def extract_metrics(payload, input_issn):
    entries = first_key(payload, ["entry"])
    # first_key intentionally returns scalars; locate the actual entry object here.
    entry = None
    for node in objects(payload):
        if isinstance(node, dict) and "entry" in node:
            raw = node["entry"]
            entry = raw[0] if isinstance(raw, list) and raw else raw
            if isinstance(entry, dict):
                break
    if not isinstance(entry, dict):
        entry = payload

    calculation = latest_metric_node(
        entry,
        ["citationCount", "citationsCount", "documentCount", "documentsCount", "percentCited"],
    )
    metric_year = year_of(calculation)
    if metric_year is None:
        metric_year = number(first_key(entry, ["citeScoreCurrentMetricYear", "metricYear"]))

    return {
        "Input ISSN": input_issn,
        "Source title": first_key(entry, ["dc:title", "source-title", "sourceTitle", "title"]),
        "CiteScore": number(first_key(entry, ["citeScoreCurrentMetric", "citeScore", "citescore"])),
        "Highest percentile": highest_percentile(entry),
        "2022-25 Citations": number(first_key(calculation, ["citationCount", "citationsCount"])),
        "2022-25 Documents": number(first_key(calculation, ["documentCount", "documentsCount"])),
        "% Cited": number(first_key(calculation, ["percentCited", "percentageCited"])),
        "SNIP": metric_for_latest_year(entry, ["SNIPList", "SNIP"], ["SNIP", "snip"]),
        "SJR": metric_for_latest_year(entry, ["SJRList", "SJR"], ["SJR", "sjr"]),
        "Publisher": first_key(entry, ["dc:publisher", "publisher", "publisher-name"]),
        "Metric year": metric_year,
        "Status": "OK",
        "Error": "",
    }


# Retrieve metrics, save the CSV, and download it
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
            payload = request_serial_title(issn)
            cache[issn] = extract_metrics(payload, display_issn)
        except Exception as exc:
            cache[issn] = {
                "Input ISSN": display_issn,
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
    "2022-25 Citations", "2022-25 Documents", "% Cited", "SNIP", "SJR",
    "Publisher", "Metric year", "Status", "Error",
]
output_df = pd.DataFrame(results).reindex(columns=output_columns)
output_df.to_csv(OUTPUT_FILE, index=False, encoding="utf-8-sig")

print(f"\nSaved {len(output_df)} rows to {OUTPUT_FILE}")
print(output_df["Status"].value_counts(dropna=False).to_string())
display(output_df.head(10)) if IN_COLAB else print(output_df.head(10).to_string(index=False))

if IN_COLAB:
    files.download(OUTPUT_FILE)
```


## Add Journal Metrics to Standardized Dataset

Add the journal metrics from both Journal Citation Reports and Scopus to the standardized dataset, ensuring metrics are included for each journal article (journal metrics will likely be duplicated throughout the file).

## Calculate the Number of Unique Journals

Calculate the number of journals in which the researcher/group has published in by running the following script. Update the file path for `path_to_standardized_file`.

```r
# Load libraries
library(readxl)
library(dplyr)
library(stringr)
library(writexl)

# Load file
input_file <- "file_path_to_standardized_file" # Update file path to standardized Excel file

# Import data
publications <- read_excel(input_file) |>
  mutate(publication_row_id = row_number())

# Confirm that SO_key exists
if (!"SO_key" %in% names(publications)) {
  stop(
    "The input workbook does not contain a column named 'SO_key'."
  )
}

# Clean journal names
publication_journals <- publications |>
  mutate(
    journal = str_squish(SO_key)
  ) |>
  filter(
    !is.na(journal),
    journal != ""
  )

# Count the number of unique journals
number_unique_journals <- publication_journals |>
  summarise(
    unique_journals = n_distinct(journal)
  ) |>
  pull(unique_journals)

message(
  "Number of unique journals: ",
  number_unique_journals
)

# Count publications in each journal
journal_counts <- publication_journals |>
  count(
    journal,
    name = "publication_count",
    sort = TRUE
  )
```

Then, to identify which journals the researcher/group publishes in most often, add the following to the end of the script:

```r
# Select top 15 journals
top_15_journals <- journal_counts |>
  slice_head(n = 15)

print(top_15_journals)
```

## Identify Journals in the Top 10% of Their Category by JIF Percentile

Identify the top journals (by JIF percentile) by running the following script. Update the file path for `file_name` to the standardized Excel file.

```r
## Identify Journals in the Top 10% of Their Category by JIF Percentile

# Load libraries
library(dplyr)

# Load Excel file
df <- read_excel("file_name") # Add file path

# Print list of journals in the top 10% of their category
top10_journals <- df %>%
  filter(!is.na(`JIF Percentile`)) %>%
  filter(`JIF Percentile` >= 90) %>%
  distinct(SO) %>%
  arrange(SO)

top10_journals
print(top10_journals, n = Inf)

# Print number of articles published in journals in the top 10% of their category
top10_journals_counts <- df %>%
  filter(!is.na(`JIF Percentile`)) %>%
  filter(`JIF Percentile` >= 90) %>%
  count(SO, sort = TRUE)

top10_journals_counts
print(top10_journals_counts, n = Inf)
```
