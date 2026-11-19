# Additional Analyses

## Bibliographic Coupling (Scopus)

### Import Spreadsheets into RStudio

Use the script below to import the two spreadsheets with the target researchers and the potential collaborators.

```r
install.packages("readxl")

# Replace `path_to_target_researchers.xlsx` with the path to the target researchers file. Update `Cohort-csv` to the name of the sheet, if necessary (e.g., 1).

cohort <- readxl::read_excel(
  path = "path_to_target_researchers.xlsx",
  sheet = "Cohort-csv",
  col_types = "text"
)

# Replace `path_to_potential_collaborators.xlsx` with the name of the potential collaborators file. Update the sheet name, if necessary.

campus <- readxl::read_excel(
  path = "path_to_potential_collaborators.xlsx",
  sheet = 1,
  col_types = "text"
)

# Check the number of rows in each spreadsheet
c(cohort_rows = nrow(cohort), campus_rows = nrow(campus))
```

### Check for Duplicate Publications and Missing Data

Use the script below to check whether the same Scopus paper ID (EID) appears more than once and whether any rows are missing an ID or a reference list.

```r
check_publications <- function(data) {
  has_eid <- !is.na(data$EID) & nzchar(trimws(data$EID))
  has_references <- !is.na(data$References) &
    nzchar(trimws(data$References))

  eids <- trimws(data$EID[has_eid])

  c(
    rows = nrow(data),
    unique_eids = length(unique(eids)),
    repeated_eid_rows = sum(duplicated(eids)),
    missing_eid = sum(!has_eid),
    missing_references = sum(!has_references)
  )
}

rbind(
  cohort = check_publications(cohort),
  campus = check_publications(campus)
)
```

### Set Aside Publications with Missing Reference Lists

Use the script below to create a working dataset for matching.

```r
has_references <- !is.na(campus$References) &
  nzchar(trimws(campus$References))

campus_for_matching <- campus[has_references, ]

c(
  cohort_papers = nrow(cohort),
  campus_papers_for_matching = nrow(campus_for_matching),
  campus_papers_without_references = sum(!has_references)
)
```

### Inspect How Scopus Separates References

Before splitting the reference lists, look at examples from both datasets to see what marks the end of a reference.

```r
show_reference_sample <- function(data, label) {
  for (i in seq_len(min(2, nrow(data)))) {
    cat("\n", label, " | EID: ", data$EID[i], "\n", sep = "")
    cat(substr(data$References[i], 1, 1000), "\n")
  }
}

show_reference_sample(cohort, "Cohort")
show_reference_sample(campus_for_matching, "Campus")
```

### Test a Reference-Splitting Rule on Two Publications

Test whether splitting after a parenthesized four-digit year works on a publication from each dataset.

```r
split_references_test <- function(reference_list) {
  pieces <- strsplit(
    reference_list,
    "(?<=\\([0-9]{4}\\));\\s+",
    perl = TRUE
  )[[1]]

  trimws(pieces)
}

campus_eid <- "2-s2.0-105020660481"
campus_row <- match(campus_eid, campus_for_matching$EID)

campus_test <- split_references_test(
  campus_for_matching$References[campus_row]
)
cohort_test <- split_references_test(cohort$References[1])

cat("Campus preview\n")
writeLines(head(campus_test, 4))

cat("\nCohort preview\n")
writeLines(head(cohort_test, 4))
```

### Check Where the Splitting Rule Might Miss a Referece

Before using the rule across the full dataset, check for cases where it might miss. Check a sample of 100 publications and print up to three pieces per dataset that contain more than one parenthesized year.

```r
audit_reference_splits <- function(data) {
  pieces_by_paper <- lapply(data$References, split_references_test)
  pieces <- unlist(pieces_by_paper, use.names = FALSE)

  year_matches <- gregexpr("\\([0-9]{4}\\)", pieces)
  year_counts <- lengths(regmatches(pieces, year_matches))
  review <- which(year_counts > 1)

  cat("Papers checked:", nrow(data), "\n")
  cat("Pieces with multiple parenthesized years:", length(review), "\n")

  for (i in head(review, 3)) {
    cat("\n", pieces[i], "\n", sep = "")
  }
}

set.seed(42)
campus_sample <- campus_for_matching[
  sample.int(nrow(campus_for_matching), 100),
]

cat("COHORT\n")
audit_reference_splits(cohort)

cat("\nCAMPUS SAMPLE\n")
audit_reference_splits(campus_sample)
```

### Test Two More Reference Endings

Extend the rule to split after a parenthesized year with an optional period and after a page range marked with pp. Run the following script on two examples before applying it more broadly.

```r
split_references_v2 <- function(reference_list) {
  boundary <- paste0(
    "(?:\\([0-9]{4}\\)\\.?|pp\\.\\s*[0-9]+\\s*[-–]\\s*[0-9]+)",
    "\\K;\\s+"
  )

  trimws(strsplit(reference_list, boundary, perl = TRUE)[[1]])
}

show_test_entries <- function(data, find_paper, show_entries) {
  row <- which(grepl(find_paper, data$References, fixed = TRUE))[1]
  entries <- split_references_v2(data$References[row])
  writeLines(grep(show_entries, entries, value = TRUE))
}

cat("Cohort example\n")
show_test_entries(cohort, "Brockie L.; Miller E.", "Brockie|Bukowski")

cat("\nCampus example\n")
show_test_entries(campus_sample, "Hayashi S., Brainlife.Io", "Hayashi|Martin D.|Renton A.I.")
```

## Test a Boundary for Preprints Without a Year

Create a new test function and print the three entries from the potential collaborators example.

```r
split_references_v3 <- function(reference_list) {
  ending <- paste0(
    "(?:\\([0-9]{4}\\)\\.?",
    "|pp\\.\\s*[0-9]+\\s*[-–]\\s*[0-9]+",
    "|,\\s*(?i:biorxiv|medrxiv|arxiv)\\.?)"
  )

  boundary <- paste0(ending, "\\K;\\s+")
  trimws(strsplit(reference_list, boundary, perl = TRUE)[[1]])
}

hayashi_row <- which(grepl(
  "Hayashi S., Brainlife.Io",
  campus_sample$References,
  fixed = TRUE
))[1]

example_list <- campus_sample$References[hayashi_row]
entries <- split_references_v3(example_list)

writeLines(grep(
  "^(Hayashi S\\.|Martin D\\.|Renton A\\.I\\.)",
  entries,
  value = TRUE
))
```

### Compare the Splitting Rules across Both Datasets

See how many additional references the revised rule identifies across both datasets. `Additional pieces` counts the extra pieces produced by the revised rule. `Papers with fewer splits` should be zero because the revised rule retains the original boundary. The counts will show how often the change matters.

```r
compare_splits <- function(data) {
  original <- lengths(lapply(data$References, split_references_test))
  revised <- lengths(lapply(data$References, split_references_v3))
  added <- revised - original

  c(
    papers_checked = nrow(data),
    original_pieces = sum(original),
    revised_pieces = sum(revised),
    additional_pieces = sum(added),
    papers_with_additional_splits = sum(added > 0),
    papers_with_fewer_splits = sum(added < 0)
  )
}

rbind(
  cohort = compare_splits(cohort),
  campus_sample = compare_splits(campus_sample)
)
```

### Spot-Check the New Splits

Print a short excerpt around each new split, with `split` where the rule would cut.

```r
show_new_boundaries <- function(data) {
  new_boundary <- paste0(
    "(?:\\([0-9]{4}\\)\\.",
    "|pp\\.\\s*[0-9]+\\s*[-–]\\s*[0-9]+",
    "|,\\s*(?i:biorxiv|medrxiv|arxiv)\\.?)",
    "\\K;\\s+"
  )

  matches <- gregexpr(new_boundary, data$References, perl = TRUE)
  rows <- which(vapply(matches, function(x) x[1] > 0, logical(1)))

  for (i in rows) {
    text <- data$References[i]
    starts <- as.integer(matches[[i]])
    ends <- starts + attr(matches[[i]], "match.length")

    cat("\n", data$EID[i], "\n", sep = "")
    for (j in seq_along(starts)) {
      before <- substr(text, max(1, starts[j] - 70), starts[j] - 1)
      after <- substr(text, ends[j], ends[j] + 89)
      cat(before, " <<< SPLIT >>> ", after, "\n", sep = "")
    }
  }
}

cat("COHORT\n")
show_new_boundaries(cohort)

cat("\nCAMPUS SAMPLE\n")
show_new_boundaries(campus_sample)
```

### Build Paper-Reference Table

Remove exact repeats within each publication so those entries don't inflate its overlap score.

```r
make_reference_table <- function(data) {
  references <- lapply(data$References, split_references_v3)

  result <- data.frame(
    EID = rep(data$EID, lengths(references)),
    reference_raw = unlist(references, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  result <- result[nzchar(result$reference_raw), ]
  unique(result)
}

cohort_references <- make_reference_table(cohort)
campus_references <- make_reference_table(campus_for_matching)

rbind(
  cohort = c(
    papers = length(unique(cohort_references$EID)),
    reference_rows = nrow(cohort_references)
  ),
  campus = c(
    papers = length(unique(campus_references$EID)),
    reference_rows = nrow(campus_references)
  )
)
```

### Create One Row Per Paper and Cited Reference

Apply the tested split references rule to both dataset while keeping the original reference text and removing exact repeat entries within each publication.

```r
make_reference_table <- function(data) {
  entries <- lapply(data$References, split_references_v3)

  refs <- data.frame(
    EID = rep(data$EID, times = lengths(entries)),
    reference = unlist(entries, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  refs <- refs[!is.na(refs$reference) & nzchar(refs$reference), ]
  unique(refs)
}

cohort_refs <- make_reference_table(cohort)
campus_refs <- make_reference_table(campus_for_matching)

c(
  cohort_reference_rows = nrow(cohort_refs),
  campus_reference_rows = nrow(campus_refs)
)
```
