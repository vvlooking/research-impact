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
from google.colab import userdata

API_KEY = userdata.get("SCOPUS_API_KEY") 

if not API_KEY:
    raise ValueError(
        "SCOPUS_API_KEY was not found. Add it under Colab → Secrets."
    )

    API_KEY = API_KEY.strip()
print("API key loaded successfully.")
```

Import the file exported from Journal Citations Reports. Update the `TARGET_YEAR` as necessary.

```r
## Upload ISSNs

import pandas as pd
from google.colab import files

uploaded = files.upload()
input_filename = next(iter(uploaded))

input_df = pd.read_excel(
    input_filename,
    dtype=str,
)

# Clean column names in case the CSV contains extra spaces.
input_df.columns = input_df.columns.str.strip()

if "ISSN" not in input_df.columns:
    raise ValueError(
        "The uploaded CSV must contain a column named ISSN. "
        f"Columns found: {input_df.columns.tolist()}"
    )

ISSNS = (
    input_df["ISSN"]
    .dropna()
    .astype(str)
    .str.strip()
)

# Remove blank cells.
ISSNS = ISSNS[ISSNS != ""].tolist()

TARGET_YEAR = 2025 # update year as needed

print(f"Loaded {len(ISSNS)} ISSNs.")
print("First five:", ISSNS[:5])
```

Run the request:

```r
import re
import requests
import xml.etree.ElementTree as ET

from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry


API_URL = "https://api.elsevier.com/content/serial/title/issn/{issn}"

OUTPUT_COLUMNS = [
    "Source title",
    "CiteScore",
    "Highest percentile",
    "2022-25 Citations",
    "2022-25 Documents",
    "% Cited",
    "SNIP",
    "SJR",
    "Publisher",
]


def normalize_issn(value):
    """
    Remove spaces, hyphens, and punctuation from an ISSN.
    """
    return re.sub(
        r"[^0-9Xx]",
        "",
        str(value),
    ).upper()


def local_name(tag):
    """
    Remove the namespace from an XML tag.
    """
    return tag.rsplit("}", 1)[-1]


def element_text(element):
    """
    Return cleaned text from an XML element.
    """
    if element is None or element.text is None:
        return None

    value = element.text.strip()

    return value if value else None


def descendants_by_name(parent, names):
    """
    Find XML descendants matching one or more tag names.
    """
    if parent is None:
        return []

    wanted = {
        name.lower()
        for name in names
    }

    return [
        element
        for element in parent.iter()
        if local_name(element.tag).lower() in wanted
    ]


def first_text(parent, names):
    """
    Return the first nonempty value matching any candidate tag.
    """
    for element in descendants_by_name(parent, names):
        value = element_text(element)

        if value is not None:
            return value

    return None


def get_year(element):
    """
    Find a year stored as an attribute or child element.
    """
    if element is None:
        return None

    # Look for a year attribute.
    for attribute_name, attribute_value in element.attrib.items():
        if local_name(attribute_name).lower() == "year":
            match = re.search(
                r"\d{4}",
                str(attribute_value),
            )

            if match:
                return int(match.group())

    # Look for a year child element.
    for child in element:
        child_name = local_name(child.tag).lower()

        if child_name in {
            "year",
            "citescoreyear",
            "metricyear",
            "citescorecurrentmetricyear",
        }:
            value = element_text(child)

            if value:
                match = re.search(
                    r"\d{4}",
                    value,
                )

                if match:
                    return int(match.group())

    return None


def choose_year_container(entry, target_year):
    """
    Find the detailed CiteScore block for TARGET_YEAR.
    """
    candidates = descendants_by_name(
        entry,
        [
            "citeScoreYearInfo",
            "citescore-year-info",
        ],
    )

    for candidate in candidates:
        if get_year(candidate) == target_year:
            return candidate

    return None


def latest_year_value(
    entry,
    list_names,
    value_names,
    target_year,
):
    """
    Get SNIP or SJR for TARGET_YEAR.

    If TARGET_YEAR is not available, return the newest available
    value and its actual year.
    """
    containers = descendants_by_name(
        entry,
        list_names,
    )

    search_root = (
        containers[0]
        if containers
        else entry
    )

    elements = descendants_by_name(
        search_root,
        value_names,
    )

    parsed_values = []

    for element in elements:
        value = element_text(element)
        year = get_year(element)

        if value is not None:
            parsed_values.append(
                (year, value)
            )

    if not parsed_values:
        return None, None

    exact_matches = [
        item
        for item in parsed_values
        if item[0] == target_year
    ]

    if exact_matches:
        return (
            exact_matches[-1][1],
            exact_matches[-1][0],
        )

    dated_values = [
        item
        for item in parsed_values
        if item[0] is not None
    ]

    if dated_values:
        newest = max(
            dated_values,
            key=lambda item: item[0],
        )

        return newest[1], newest[0]

    return parsed_values[-1][1], None


def extract_percentile(year_block):
    """
    Return the highest CiteScore subject percentile.
    """
    if year_block is None:
        return None

    values = []

    elements = descendants_by_name(
        year_block,
        [
            "percentile",
            "citeScorePercentile",
            "citescore-percentile",
        ],
    )

    for element in elements:
        value = element_text(element)

        if value is None:
            value = (
                element.attrib.get("percentile")
                or element.attrib.get("value")
            )

        if value is not None:
            try:
                numeric_value = float(
                    str(value)
                    .replace("%", "")
                    .strip()
                )

                values.append(numeric_value)

            except ValueError:
                pass

    return max(values) if values else None


def extract_percent_cited(year_block):
    """
    Extract or calculate the percentage of documents cited.
    """
    if year_block is None:
        return None

    # Use an explicit percent-cited value when available.
    value = first_text(
        year_block,
        [
            "percentCited",
            "percent-cited",
            "citeScorePercentCited",
        ],
    )

    if value is not None:
        try:
            return float(
                value.replace("%", "").strip()
            )

        except ValueError:
            return value

    # Some responses provide percent uncited instead.
    uncited = first_text(
        year_block,
        [
            "zeroCitesPercentSCE",
            "zeroCitesPercent",
            "percentUncited",
        ],
    )

    if uncited is not None:
        try:
            return 100.0 - float(
                uncited.replace("%", "").strip()
            )

        except ValueError:
            pass

    # Try calculating the percentage from cited documents.
    cited_documents = first_text(
        year_block,
        [
            "citedDocumentCount",
            "citedDocuments",
            "citeCountSCE",
        ],
    )

    total_documents = first_text(
        year_block,
        [
            "documentCount",
            "documents",
            "publicationCount",
        ],
    )

    try:
        cited_documents = float(cited_documents)
        total_documents = float(total_documents)

        if total_documents > 0:
            return (
                100.0
                * cited_documents
                / total_documents
            )

    except (TypeError, ValueError):
        pass

    return None


def find_entry(root):
    """
    Find the journal entry in the Scopus response.
    """
    for element in root.iter():
        if local_name(element.tag).lower() == "entry":
            return element

    return None


def make_session():
    """
    Create an HTTP session with automatic retry handling.
    """
    retry_policy = Retry(
        total=5,
        connect=5,
        read=5,
        status=5,
        backoff_factor=1.5,
        status_forcelist=[
            429,
            500,
            502,
            503,
            504,
        ],
        allowed_methods=["GET"],
        respect_retry_after_header=True,
    )

    session = requests.Session()

    session.mount(
        "https://",
        HTTPAdapter(max_retries=retry_policy),
    )

    session.headers.update({
        "X-ELS-APIKey": API_KEY,
        "Accept": "application/xml",
        "User-Agent": "Colab-Scopus-Journal-Metrics/1.0",
    })

    return session


def fetch_journal(session, original_issn):
    """
    Retrieve and parse journal metrics for one ISSN.
    """
    issn = normalize_issn(original_issn)

    empty_result = {
        column: None
        for column in OUTPUT_COLUMNS
    }

    diagnostic = {
        "Input ISSN": original_issn,
        "Normalized ISSN": issn,
        "Status": None,
        "Message": None,
        "CiteScore year": None,
        "SNIP year": None,
        "SJR year": None,
    }

    if len(issn) != 8:
        diagnostic["Status"] = "Invalid ISSN"
        diagnostic["Message"] = (
            "An ISSN must contain eight characters."
        )

        return empty_result, diagnostic

    response = session.get(
        API_URL.format(issn=issn),

        # Your API key supports the STANDARD view.
        params={"view": "STANDARD"},

        timeout=60,
    )

    diagnostic["Status"] = response.status_code

    if response.status_code == 404:
        diagnostic["Message"] = "ISSN not found."

        return empty_result, diagnostic

    if response.status_code in (401, 403):
        raise RuntimeError(
            "Scopus authentication or entitlement error "
            f"({response.status_code}) for ISSN {issn}: "
            f"{response.text[:1000]}"
        )

    response.raise_for_status()

    try:
        root = ET.fromstring(
            response.content
        )

    except ET.ParseError as error:
        diagnostic["Message"] = (
            "Could not parse the API response: "
            f"{error}"
        )

        return empty_result, diagnostic

    entry = find_entry(root)

    if entry is None:
        diagnostic["Message"] = (
            "The API returned no journal entry."
        )

        return empty_result, diagnostic

    # Journal information.
    title = first_text(
        entry,
        ["title"],
    )

    publisher = first_text(
        entry,
        ["publisher"],
    )

    # Latest completed CiteScore and year.
    current_metric = first_text(
        entry,
        ["citeScoreCurrentMetric"],
    )

    current_metric_year = first_text(
        entry,
        ["citeScoreCurrentMetricYear"],
    )

    diagnostic["CiteScore year"] = (
        current_metric_year
    )

    # Look for detailed information for TARGET_YEAR.
    year_block = choose_year_container(
        entry,
        TARGET_YEAR,
    )

    citescore = None
    citations = None
    documents = None
    percentile = None
    percent_cited = None

    if year_block is not None:
        citescore = first_text(
            year_block,
            [
                "citeScore",
                "citeScoreValue",
                "citeScoreCurrentMetric",
            ],
        )

        citations = first_text(
            year_block,
            [
                "citationCount",
                "citations",
                "citeCount",
                "citeCountSCE",
            ],
        )

        documents = first_text(
            year_block,
            [
                "documentCount",
                "documents",
                "publicationCount",
            ],
        )

        percentile = extract_percentile(
            year_block
        )

        percent_cited = extract_percent_cited(
            year_block
        )

    # The STANDARD response puts the completed CiteScore
    # directly under citeScoreCurrentMetric.
    if citescore is None:
        citescore = current_metric

    # Retrieve SNIP.
    snip, snip_year = latest_year_value(
        entry,
        ["SNIPList"],
        ["SNIP"],
        TARGET_YEAR,
    )

    # Retrieve SJR.
    sjr, sjr_year = latest_year_value(
        entry,
        ["SJRList"],
        ["SJR"],
        TARGET_YEAR,
    )

    diagnostic["SNIP year"] = snip_year
    diagnostic["SJR year"] = sjr_year

    if year_block is None:
        diagnostic["Message"] = (
            f"CiteScore {current_metric_year} was retrieved. "
            "The STANDARD view did not include detailed "
            "citation, document, percentile, or "
            "percent-cited data."
        )
    else:
        diagnostic["Message"] = "OK"

    result = {
        "Source title": title,
        "CiteScore": citescore,
        "Highest percentile": percentile,
        "2022-25 Citations": citations,
        "2022-25 Documents": documents,
        "% Cited": percent_cited,
        "SNIP": snip,
        "SJR": sjr,
        "Publisher": publisher,
    }

    return result, diagnostic


print("Step 3 loaded successfully.")
```

Test the first ISSN:

```r
# Test the first ISSN

session = make_session()

test_result, test_diagnostic = fetch_journal(
    session,
    ISSNS[0],
)

print("Result:")
print(test_result)

print("\nDiagnostic:")
print(test_diagnostic)
```

Export the results:

```r
# Export the results

import time
import pandas as pd

session = make_session()

rows = []
diagnostics = []

for number, issn in enumerate(ISSNS, start=1):
    print(f"[{number}/{len(ISSNS)}] Retrieving {issn}...")

    try:
        row, diagnostic = fetch_journal(
            session,
            issn,
        )

    except Exception as error:
        row = {
            column: None
            for column in OUTPUT_COLUMNS
        }

        diagnostic = {
            "Input ISSN": issn,
            "Normalized ISSN": normalize_issn(issn),
            "Status": "Error",
            "Message": str(error),
            "SNIP year": None,
            "SJR year": None,
        }

    rows.append(row)
    diagnostics.append(diagnostic)

    time.sleep(0.25)

metrics_df = pd.DataFrame(
    rows,
    columns=OUTPUT_COLUMNS,
)

diagnostics_df = pd.DataFrame(diagnostics)

numeric_columns = [
    "CiteScore",
    "Highest percentile",
    "2022-25 Citations",
    "2022-25 Documents",
    "% Cited",
    "SNIP",
    "SJR",
]

for column in numeric_columns:
    metrics_df[column] = pd.to_numeric(
        metrics_df[column],
        errors="coerce",
    )

metrics_df.to_csv(
    "scopus_journal_metrics_2025.csv",
    index=False,
    encoding="utf-8-sig",
)

diagnostics_df.to_csv(
    "scopus_journal_metrics_diagnostics.csv",
    index=False,
    encoding="utf-8-sig",
)

display(metrics_df)
display(diagnostics_df)
```

Finally, download the files:

```r
# Download the files

from google.colab import files

files.download("scopus_journal_metrics_2025.csv")
files.download("scopus_journal_metrics_diagnostics.csv")
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
