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

### Normalize Reference Text for Matching

Give each reference a match key. `Repeated keys within paper` will show how many entries become duplicates after normalization.

```r
normalize_reference <- function(text) {
  text <- enc2utf8(text)
  text <- gsub("[\u2018\u2019]", "'", text, perl = TRUE)
  text <- gsub("[\u2010\u2011\u2012\u2013\u2014]", "-", text, perl = TRUE)
  text <- gsub("[[:space:]\u00A0]+", " ", text, perl = TRUE)
  text <- trimws(tolower(text))
  sub("\\.$", "", text)
}

cohort_refs$ref_key <- normalize_reference(cohort_refs$reference)
campus_refs$ref_key <- normalize_reference(campus_refs$reference)

count_reference_keys <- function(refs) {
  usable <- !is.na(refs$ref_key) & nzchar(refs$ref_key)
  pairs <- refs[usable, c("EID", "ref_key")]

  c(
    usable_rows = nrow(pairs),
    unique_paper_reference_keys = nrow(unique(pairs)),
    repeated_keys_within_paper = sum(duplicated(pairs))
  )
}

rbind(
  cohort = count_reference_keys(cohort_refs),
  campus = count_reference_keys(campus_refs)
)
```

### Find References Shared Across Datasets

Match paper-reference pairs by `ref_key` so that each resulting row will show which papers share which references. `Same paper rows removed` will indicate how many publications appear in both spreadsheets with the same Scopus ID (EID), with each shared reference in one of those self-comparisons contributed as a row. `Shared references rows` represents one reference shared by one target researcher publication and one potential collaborator publication. The same reference can contribute to several different pairs. `Distinct paper pairs` represents each researcher-collaborator pair. Each pair is counted once, regardless of how many references it shares.

```r
cohort_keys <- unique(cohort_refs[c("EID", "ref_key")])
campus_keys <- unique(campus_refs[c("EID", "ref_key")])

# Keep only campus references that also appear in the cohort.
campus_matching_keys <- campus_keys[
  campus_keys$ref_key %in% cohort_keys$ref_key,
]

names(cohort_keys)[1] <- "cohort_eid"
names(campus_matching_keys)[1] <- "campus_eid"

shared_refs <- merge(
  cohort_keys,
  campus_matching_keys,
  by = "ref_key",
  sort = FALSE
)

# Skip comparisons of a paper with itself across the two exports.
same_paper <- shared_refs$cohort_eid == shared_refs$campus_eid
same_paper_rows <- sum(same_paper)
shared_refs <- shared_refs[!same_paper, ]

c(
  same_paper_rows_removed = same_paper_rows,
  shared_reference_rows = nrow(shared_refs),
  distinct_paper_pairs = nrow(unique(
    shared_refs[c("cohort_eid", "campus_eid")]
  ))
)
```

### Count the Shared References for Each Publication Pair

Make a table with one row per publication pair. The number of shared references is the pair's raw bibliographic coupling score. The last line will show the five pairs with the highest raw counts.

```r
paper_pairs <- aggregate(
  ref_key ~ cohort_eid + campus_eid,
  data = shared_refs,
  FUN = length
)

names(paper_pairs)[3] <- "shared_reference_count"

paper_pairs <- paper_pairs[
  order(paper_pairs$shared_reference_count, decreasing = TRUE),
]

c(
  paper_pairs = nrow(paper_pairs),
  total_shared_references = sum(paper_pairs$shared_reference_count)
)

head(paper_pairs, 5)
```

### Normalize the Coupling Scores

Add cosine similarity alongside the raw count to divide the number of shared references by the geometric mean of the two reference-list lengths. The scores use the full set of unique reference keys for each paper, including references that didn't match. A high score can come from a small number of shared references when both lists are short.

```r
paper_pairs <- aggregate(
  ref_key ~ cohort_eid + campus_eid,
  data = shared_refs,
  FUN = length
)

names(paper_pairs)[3] <- "shared_reference_count"

paper_pairs <- paper_pairs[
  order(paper_pairs$shared_reference_count, decreasing = TRUE),
]

c(
  paper_pairs = nrow(paper_pairs),
  total_shared_references = sum(paper_pairs$shared_reference_count)
)

head(paper_pairs, 5)
```

### Inspect Author Affiliations on the Matched Publications

Identify which authors on the matched potential collaborators publications are eligible collaborators (i.e., have the same affiliation). Before building the author-paper table, check how Scopus links author names, IDs, and affiliations on a sample of papers.

```r
matched_eids <- unique(paper_pairs$campus_eid)

matched_campus <- campus_for_matching[
  match(matched_eids, campus_for_matching$EID),
  c("EID", "Author full names", "Authors with affiliations")
]

cat("Distinct matched campus papers:", nrow(matched_campus), "\n")

for (i in seq_len(min(3, nrow(matched_campus)))) {
  cat("\nEID: ", matched_campus$EID[i], "\n", sep = "")
  cat("Names and IDs: ", matched_campus$`Author full names`[i], "\n", sep = "")
  cat("Affiliations: ", matched_campus$`Authors with affiliations`[i],
      "\n", sep = "")
}
```

### Test the Author-to-ID Mapping

Use the short names in `Authors with Affiliations` to identify who is linked to USC, match those short names against the `Authors` column, and use their positions to retrieve the full names and Scopus IDs.

```r
test_eids <- c(
  "2-s2.0-105056140036",
  "2-s2.0-105019382083",
  "2-s2.0-85151121786"
)

test_papers <- campus_for_matching[
  match(test_eids, campus_for_matching$EID),
]

split_author_field <- function(text) {
  trimws(strsplit(text, ";", fixed = TRUE)[[1]])
}

for (i in seq_len(nrow(test_papers))) {
  paper <- test_papers[i, ]
  short_names <- split_author_field(paper$Authors)
  full_names <- split_author_field(paper$`Author full names`)
  author_ids <- split_author_field(paper$`Author(s) ID`)
  affiliations <- split_author_field(paper$`Authors with affiliations`)

  if (length(short_names) != length(full_names) ||
      length(short_names) != length(author_ids) ||
      anyDuplicated(short_names) ||
      !all(endsWith(full_names, paste0("(", author_ids, ")")))) {
    stop("Review the author lists for ", paper$EID)
  }

  usc_entries <- grep(
    ",\\s*University of South Carolina\\s*(,|$)",
    affiliations,
    value = TRUE,
    ignore.case = TRUE
  )

  usc_short_names <- trimws(sub(",.*$", "", usc_entries))
  positions <- match(usc_short_names, short_names)

  if (anyNA(positions)) {
    stop("Review unmatched USC author names for ", paper$EID)
  }

  cat("\n", paper$EID, "\n", sep = "")
  writeLines(unique(full_names[positions]))
}
```

### Build Author-Publication Table

Apply the same mapping to every matched publication. Use the following script to keep one row per publication and USC author ID and record a status for each publication to later review any unresolved cases.

```r
map_usc_authors <- function(paper) {
  required <- c(
    "Authors", "Author full names", "Author(s) ID",
    "Authors with affiliations"
  )
  fields <- unlist(paper[required], use.names = FALSE)

  if (any(is.na(fields) | !nzchar(trimws(fields)))) {
    return(list(status = "Missing author or affiliation field"))
  }

  short_names <- split_author_field(paper$Authors)
  full_names <- split_author_field(paper$`Author full names`)
  author_ids <- split_author_field(paper$`Author(s) ID`)
  affiliations <- split_author_field(paper$`Authors with affiliations`)

  usc_entries <- grep(
    ",\\s*University of South Carolina\\s*(,|$)",
    affiliations,
    value = TRUE,
    ignore.case = TRUE
  )

  if (length(usc_entries) == 0) {
    return(list(status = "No exact USC affiliation match"))
  }

  if (length(short_names) != length(full_names) ||
      length(short_names) != length(author_ids) ||
      anyDuplicated(short_names) ||
      !all(grepl("^[0-9]+$", author_ids)) ||
      !all(endsWith(full_names, paste0("(", author_ids, ")")))) {
    return(list(status = "Review author lists or IDs"))
  }

  usc_short_names <- trimws(sub(",.*$", "", usc_entries))
  positions <- match(usc_short_names, short_names)

  if (anyNA(positions)) {
    return(list(status = "Review unmatched USC author name"))
  }

  positions <- unique(positions)

  list(
    status = "Mapped",
    authors = data.frame(
      campus_eid = paper$EID,
      author_id = author_ids[positions],
      author_name = sub("\\s*\\([0-9]+\\)$", "", full_names[positions]),
      stringsAsFactors = FALSE
    )
  )
}

papers_to_map <- campus_for_matching[
  match(matched_eids, campus_for_matching$EID),
]

mapping <- lapply(seq_len(nrow(papers_to_map)), function(i) {
  map_usc_authors(papers_to_map[i, ])
})

usc_mapping_status <- data.frame(
  campus_eid = papers_to_map$EID,
  status = vapply(mapping, function(result) result$status, character(1))
)

usc_author_papers <- do.call(
  rbind,
  lapply(mapping[usc_mapping_status$status == "Mapped"], function(result) {
    result$authors
  })
)

print(table(usc_mapping_status$status))
c(
  author_paper_rows = nrow(usc_author_papers),
  distinct_author_ids = length(unique(usc_author_papers$author_id))
)
```

### Review the Publications That Didn't Map

Sample publications that did not have an exact USC affiliation match to see whether the export uses affiliations names the rule missed.

```r
no_exact_match <- usc_mapping_status$campus_eid[
  usc_mapping_status$status == "No exact USC affiliation match"
]

set.seed(42)
review_eids <- sample(no_exact_match, min(6, length(no_exact_match)))

review_papers <- papers_to_map[
  match(review_eids, papers_to_map$EID),
]

for (i in seq_len(nrow(review_papers))) {
  entries <- split_author_field(
    review_papers$`Authors with affiliations`[i]
  )
  nearby <- grep("South Carolina|\\bUSC\\b", entries,
                 value = TRUE, ignore.case = TRUE)

  cat("\n", review_papers$EID[i], "\n", sep = "")
  if (length(nearby) > 0) {
    writeLines(nearby)
  } else {
    cat("No South Carolina or USC wording in the author affiliations.\n")
    cat("Paper affiliations:", review_papers$Affiliations[i], "\n")
  }
}
```

### Expand the Affiliation Rule and Re-Run the Mapping

Include longer affiliations under the eligibility rule, as well as include joint affiliations.

```r
split_author_field <- function(text) {
  trimws(strsplit(text, ";", fixed = TRUE)[[1]])
}

usc_affiliation_pattern <- paste0(
  "\\bUniversity of South Carolina",
  "(?:\\s+(?:School of Medicine|College of Nursing)\\b[^,;]*)?",
  "\\s*(?:,|$)"
)

map_usc_authors <- function(paper) {
  required <- c(
    "Authors", "Author full names", "Author(s) ID",
    "Authors with affiliations"
  )
  fields <- unlist(paper[required], use.names = FALSE)

  if (any(is.na(fields) | !nzchar(trimws(fields)))) {
    return(list(status = "Missing author or affiliation field"))
  }

  short_names <- split_author_field(paper$Authors)
  full_names <- split_author_field(paper$`Author full names`)
  author_ids <- split_author_field(paper$`Author(s) ID`)
  affiliations <- split_author_field(paper$`Authors with affiliations`)

  usc_entries <- grep(
    usc_affiliation_pattern,
    affiliations,
    value = TRUE,
    ignore.case = TRUE,
    perl = TRUE
  )

  if (length(usc_entries) == 0L) {
    return(list(status = "No exact USC affiliation match"))
  }

  if (length(short_names) != length(full_names) ||
      length(short_names) != length(author_ids) ||
      anyDuplicated(short_names) > 0L ||
      !all(grepl("^[0-9]+$", author_ids)) ||
      !all(endsWith(full_names, paste0("(", author_ids, ")")))) {
    return(list(status = "Review author lists or IDs"))
  }

  usc_short_names <- trimws(sub(",.*$", "", usc_entries))
  positions <- match(usc_short_names, short_names)

  if (anyNA(positions)) {
    return(list(status = "Review unmatched USC author name"))
  }

  positions <- unique(positions)

  list(
    status = "Mapped",
    authors = data.frame(
      campus_eid = paper$EID,
      author_id = author_ids[positions],
      author_name = sub("\\s*\\([0-9]+\\)$", "", full_names[positions]),
      stringsAsFactors = FALSE
    )
  )
}

# Map authors only for campus papers with a reference match.
matched_eids <- unique(paper_pairs$campus_eid)

papers_to_map <- campus_for_matching[
  match(matched_eids, campus_for_matching$EID),
]

mapping <- lapply(seq_len(nrow(papers_to_map)), function(i) {
  map_usc_authors(papers_to_map[i, ])
})

usc_mapping_status <- data.frame(
  campus_eid = papers_to_map$EID,
  status = vapply(mapping, function(result) result$status, character(1))
)

usc_author_papers <- unique(do.call(
  rbind,
  lapply(mapping[usc_mapping_status$status == "Mapped"], function(result) {
    result$authors
  })
))

print(table(usc_mapping_status$status))

c(
  author_paper_rows = nrow(usc_author_papers),
  distinct_author_ids = length(unique(usc_author_papers$author_id))
)
```

### Check the Target Researchers Roster before Excluding Members

Verify Scopus author IDs for the target researchers. `Papers in export` will show how many of the publications lists each ID.

```r
cohort_roster <- data.frame(
  researcher = rep(
    c(
      "Feili Tu-Keefner",
      "Darin Johnson",
      "Jungmi Jun",
      "Tong Lin",
      "Brett Robertson",
      "Viviana Carolina Zambrano Rodriguez",
      "Vanessa Kitzie"
    ),
    times = c(1, 1, 2, 1, 2, 2, 1)
  ),
  author_id = c(
    "57192369452",
    "58763059300",
    "44161101900", "59883821600",
    "57776860800",
    "57221693601", "57192904871",
    "57824168800", "60597040200",
    "55227368100"
  ),
  stringsAsFactors = FALSE
)

cohort_paper_ids <- lapply(cohort[["Author(s) ID"]], split_author_field)

cohort_roster$papers_in_export <- vapply(
  cohort_roster$author_id,
  function(id) {
    sum(vapply(cohort_paper_ids, function(ids) id %in% ids, logical(1)))
  },
  integer(1)
)

print(cohort_roster, row.names = FALSE)
```

### Link Each Publication to Its Author

Check to see whether every publication has exactly one target researcher as an author.

```r
members_by_paper <- lapply(cohort_paper_ids, function(ids) {
  unique(cohort_roster$researcher[cohort_roster$author_id %in% ids])
})

cohort_members_per_paper <- lengths(members_by_paper)

cohort_author_papers <- data.frame(
  cohort_eid = rep(cohort$EID, times = cohort_members_per_paper),
  researcher = unlist(members_by_paper, use.names = FALSE),
  stringsAsFactors = FALSE
)

c(
  papers_with_no_cohort_author = sum(cohort_members_per_paper == 0),
  papers_with_one_cohort_author = sum(cohort_members_per_paper == 1),
  papers_with_multiple_cohort_authors = sum(cohort_members_per_paper > 1),
  researcher_paper_links = nrow(cohort_author_papers)
)

# Show the paper IDs of any rows that need review.
cohort$EID[cohort_members_per_paper == 0]
```

### Link the Publication Matches to Potential Collaborators

Connect each publication-pair score to the target researcher and to the mapped USC author on the potential collaborators publication. Exclude all target researchers' Scopus IDs from everyone's results before joining. The output will show how many distinct USC candidate IDs have at least one matching publication for each target researcher.

```r
eligible_usc_author_papers <- usc_author_papers[
  !usc_author_papers$author_id %in% cohort_roster$author_id,
]

researcher_paper_pairs <- merge(
  cohort_author_papers,
  paper_pairs,
  by = "cohort_eid",
  sort = FALSE
)

candidate_evidence <- merge(
  researcher_paper_pairs,
  eligible_usc_author_papers,
  by = "campus_eid",
  sort = FALSE
)

stopifnot(!any(candidate_evidence$author_id %in% cohort_roster$author_id))

# Count distinct candidates for each cohort member.
candidate_ids_by_researcher <- unique(
  candidate_evidence[c("researcher", "author_id")]
)

table(factor(
  candidate_ids_by_researcher$researcher,
  levels = unique(cohort_roster$researcher)
))
```

### Summarize the Evidence for Each Candidate

Counts show how many candidate IDs have at least one matching paper for each target researcher. Create one row per target researcher-candidate pair, keeping the strongest publication match and showing how many publication pairs support it. The cosine score and raw count will describe the best publication pair. `Pairs with 2plus shared` is a diagnostic, so candidates supported only by one-reference matches are still included.

```r
# Count supporting paper pairs for each researcher and candidate.
support <- aggregate(
  cbind(
    matching_paper_pairs = rep(1L, nrow(candidate_evidence)),
    pairs_with_2plus_shared = as.integer(
      candidate_evidence$shared_reference_count >= 2
    )
  ),
  by = candidate_evidence[c("researcher", "author_id")],
  FUN = sum
)

# Pick the strongest paper pair by cosine similarity.
# Break score ties using the raw shared-reference count.
evidence_ordered <- candidate_evidence[
  order(
    -candidate_evidence$cosine_similarity,
    -candidate_evidence$shared_reference_count,
    candidate_evidence$cohort_eid,
    candidate_evidence$campus_eid
  ),
]

best_pairs <- evidence_ordered[
  !duplicated(evidence_ordered[c("researcher", "author_id")]),
  c(
    "researcher", "author_id", "author_name",
    "cohort_eid", "campus_eid",
    "shared_reference_count", "cosine_similarity"
  )
]

candidate_summary <- merge(
  best_pairs,
  support,
  by = c("researcher", "author_id"),
  sort = FALSE
)

candidate_summary <- candidate_summary[
  order(
    candidate_summary$researcher,
    -candidate_summary$cosine_similarity,
    -candidate_summary$shared_reference_count,
    candidate_summary$author_id
  ),
]

stopifnot(nrow(candidate_summary) == nrow(candidate_ids_by_researcher))

head(
  candidate_summary[
    candidate_summary$researcher == "Vanessa Kitzie",
    c(
      "author_name", "shared_reference_count", "cosine_similarity",
      "matching_paper_pairs", "pairs_with_2plus_shared"
    )
  ],
  5
)
```

### Rank Candidates with at Least One Two-Reference Match

For the first short-list, use two shared references in a single paper pair as a starting threshold. Pick each candidates highest-scoring pair among the pairs that meet that threshold. This will let a candidate qualify even if their highest cosine score overall came from a one-reference match. `True` means the target researcher and candidate appear together on at least one paper in the export. `False` means the export contains no such paper.

```r
qualified_evidence <- evidence_ordered[
  evidence_ordered$shared_reference_count >= 2,
]

# evidence_ordered is already sorted by cosine, then shared count.
shortlist_best_pairs <- qualified_evidence[
  !duplicated(qualified_evidence[c("researcher", "author_id")]),
  c(
    "researcher", "author_id", "author_name",
    "cohort_eid", "campus_eid",
    "shared_reference_count", "cosine_similarity"
  )
]

candidate_shortlist <- merge(
  shortlist_best_pairs,
  support,
  by = c("researcher", "author_id"),
  sort = FALSE
)

candidate_shortlist <- candidate_shortlist[
  order(
    candidate_shortlist$researcher,
    -candidate_shortlist$cosine_similarity,
    -candidate_shortlist$shared_reference_count,
    candidate_shortlist$author_id
  ),
]

print(table(factor(
  candidate_shortlist$researcher,
  levels = unique(cohort_roster$researcher)
)))

head(
  candidate_shortlist[
    candidate_shortlist$researcher == "Vanessa Kitzie",
    c(
      "author_name", "author_id", "shared_reference_count",
      "cosine_similarity", "pairs_with_2plus_shared"
    )
  ],
  5
)
```

### Flag Existing Co-Authors

Flag candidates who have already co-authored a paper with a target researcher.

```r
# Identify the other authors on each cohort paper.
coauthor_ids_by_paper <- lapply(cohort_paper_ids, function(ids) {
  unique(ids[!is.na(ids) & nzchar(ids) &
               !ids %in% cohort_roster$author_id])
})

# Each cohort paper has exactly one linked cohort member.
researcher_by_paper <- cohort_author_papers$researcher[
  match(cohort$EID, cohort_author_papers$cohort_eid)
]

observed_coauthors <- unique(data.frame(
  researcher = rep(researcher_by_paper, lengths(coauthor_ids_by_paper)),
  author_id = unlist(coauthor_ids_by_paper, use.names = FALSE),
  stringsAsFactors = FALSE
))

candidate_shortlist$coauthor_in_cohort_export <- with(
  candidate_shortlist,
  paste(researcher, author_id, sep = "\t") %in%
    with(observed_coauthors, paste(researcher, author_id, sep = "\t"))
)

head(
  candidate_shortlist[
    candidate_shortlist$researcher == "Vanessa Kitzie",
    c(
      "author_name", "author_id", "shared_reference_count",
      "pairs_with_2plus_shared", "coauthor_in_cohort_export"
    )
  ],
  5
)
```

## Create the Shortlist for Potential New Collaborators

Create a shortlist that excludes previous co-authors.

```r
new_collaborator_shortlist <- candidate_shortlist[
  !candidate_shortlist$coauthor_in_cohort_export,
]

# Rank within each cohort member using the existing score order.
new_collaborator_shortlist$rank <- as.integer(ave(
  seq_len(nrow(new_collaborator_shortlist)),
  new_collaborator_shortlist$researcher,
  FUN = seq_along
))

print(table(factor(
  new_collaborator_shortlist$researcher,
  levels = unique(cohort_roster$researcher)
)))

head(
  new_collaborator_shortlist[
    new_collaborator_shortlist$researcher == "Vanessa Kitzie",
    c(
      "rank", "author_name", "author_id", "shared_reference_count",
      "cosine_similarity", "pairs_with_2plus_shared"
    )
  ],
  5
)
```

### Add the Paper Titles Behind Each Recommendation

Add paper titles to help see whether the ties come from the same paper and whether the topics are relevant to the target researcher's work.

```r
new_collaborator_shortlist$cohort_paper_title <- cohort$Title[
  match(new_collaborator_shortlist$cohort_eid, cohort$EID)
]

new_collaborator_shortlist$campus_paper_title <- campus_for_matching$Title[
  match(new_collaborator_shortlist$campus_eid, campus_for_matching$EID)
]

vanessa_top <- head(
  new_collaborator_shortlist[
    new_collaborator_shortlist$researcher == "Vanessa Kitzie", 
  ],
  5
)

for (i in seq_len(nrow(vanessa_top))) {
  cat(
    "\n", vanessa_top$rank[i], ". ", vanessa_top$author_name[i],
    "\n   Cohort paper: ", vanessa_top$cohort_paper_title[i],
    "\n   Campus paper: ", vanessa_top$campus_paper_title[i],
    "\n   Campus EID: ", vanessa_top$campus_eid[i], "\n",
    sep = ""
  )
}
```

### Inspect the References Behind Top Matches

See whether the shared references point to a common method or topic, or whether they reflect a parsing artifact or incidental overlap.

```r
top_pair <- vanessa_top[1, ]

pair_matches <- shared_refs[
  shared_refs$cohort_eid == top_pair$cohort_eid &
    shared_refs$campus_eid == top_pair$campus_eid,
]

cohort_entries <- cohort_refs[
  cohort_refs$EID == top_pair$cohort_eid,
]

campus_entries <- campus_refs[
  campus_refs$EID == top_pair$campus_eid,
]

for (i in seq_len(nrow(pair_matches))) {
  key <- pair_matches$ref_key[i]

  cat("\nShared reference ", i, "\n", sep = "")
  cat("Cohort export:\n")
  writeLines(unique(cohort_entries$reference[cohort_entries$ref_key == key]))
  cat("Campus export:\n")
  writeLines(unique(campus_entries$reference[campus_entries$ref_key == key]))
}
```

### Filter Out Year-Only Reference Fragments

Create clean working copies of the reference tables and count how many fragments this removes.

```r
is_year_only <- function(key) {
  !is.na(key) & grepl(
    "^(\\([0-9]{4}\\)|[0-9]{4})[.,]?$",
    trimws(key)
  )
}

cohort_year_only <- is_year_only(cohort_refs$ref_key)
campus_year_only <- is_year_only(campus_refs$ref_key)

cohort_refs_clean <- cohort_refs[!cohort_year_only, ]
campus_refs_clean <- campus_refs[!campus_year_only, ]

c(
  cohort_year_only_rows_removed = sum(cohort_year_only),
  campus_year_only_rows_removed = sum(campus_year_only),
  shared_match_rows_from_year_only_keys = sum(
    is_year_only(shared_refs$ref_key)
  )
)
```

### Recalculate Coupling Scores from the Cleaned References

Rebuild the paper pairs and recalculate cosine similarity using the cleaned reference-list lengths for both papers.

```r
cohort_keys <- unique(cohort_refs_clean[c("EID", "ref_key")])
campus_keys <- unique(campus_refs_clean[c("EID", "ref_key")])

campus_matching_keys <- campus_keys[
  campus_keys$ref_key %in% cohort_keys$ref_key,
]

names(cohort_keys)[1] <- "cohort_eid"
names(campus_matching_keys)[1] <- "campus_eid"

shared_refs <- merge(
  cohort_keys,
  campus_matching_keys,
  by = "ref_key",
  sort = FALSE
)

shared_refs <- shared_refs[
  shared_refs$cohort_eid != shared_refs$campus_eid,
]

paper_pairs <- aggregate(
  ref_key ~ cohort_eid + campus_eid,
  data = shared_refs,
  FUN = length
)
names(paper_pairs)[3] <- "shared_reference_count"

cohort_ref_counts <- table(cohort_keys$cohort_eid)
campus_ref_counts <- table(campus_keys$EID)

paper_pairs$cohort_ref_count <- as.integer(
  cohort_ref_counts[paper_pairs$cohort_eid]
)
paper_pairs$campus_ref_count <- as.integer(
  campus_ref_counts[paper_pairs$campus_eid]
)

paper_pairs$cosine_similarity <- with(
  paper_pairs,
  shared_reference_count / sqrt(cohort_ref_count * campus_ref_count)
)

paper_pairs <- paper_pairs[
  order(-paper_pairs$cosine_similarity, -paper_pairs$shared_reference_count),
]

c(
  shared_reference_rows = nrow(shared_refs),
  distinct_paper_pairs = nrow(paper_pairs),
  pairs_with_2plus_shared = sum(paper_pairs$shared_reference_count >= 2)
)
```

### Audit the Most Frequent Reference Matches

Check which reference keys contribute the most match rows. Each count will show how many cohort-campus paper pairs share that key.

```r
key_frequency <- sort(table(shared_refs$ref_key), decreasing = TRUE)
top_keys <- head(key_frequency, 15)

original_text <- cohort_refs_clean$reference[
  match(names(top_keys), cohort_refs_clean$ref_key)
]

for (i in seq_along(top_keys)) {
  cat(
    "\n", i, ". ", as.integer(top_keys[i]), " paper-pair matches\n",
    "   ", original_text[i], "\n",
    sep = ""
  )
}
```
