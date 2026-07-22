# Clean Data in OpenRefine

## Checklist
- [ ] Trim and normalize text
- [ ] Clean and normalize DOIs
- [ ] Normalize titles and sources for matching
- [ ] Normalize citations
- [ ] Normalize author names
- [ ] Separate and clean author affiliations
- [ ] Cluster and edit near-duplicates
- [ ] Export cleaned dataset

## Trim and Normalize Text
For the columns `TI`, `SO`, `AU`, and `affiliations`, select "Edit cells," then "Transform." Add the following script:
```r
  value.trim().replace(/\s+/," ")
```

## Clean and Normalize DOIs
For the column `DI`, select "Edit cells," then "Transform." Add the following script:
```r
value.toLowercase()
  .trim()
  .replace(/^https?:\/\/(dx\.)?doi\.org\//,"")
  value.replace(/[<>,"'\s]+$/,"")
```

## Normalize Titles and Sources for Matching
Add a column based on the `TI` column and name it "TI_key." Add a column based on the `SO` column and name it "SO_key". For each of the new columns, select "Edit cells," then "Transform." Add the following script:
```r
value.toLowercase()
  .trim()
  .replace(/[^a-z0-9 ]/,"")    
  .replace(/\s+/," ")
```

## Normalize Citations
For the column `TC`, select  "Edit cells," then "Transform." Add the following script:
```r
value.toString().replace(",","").toNumber()
```

## Normalize Author Names
Add a column based on the `AU` column and name it "AI_key". Select "Edit cells," then "Transform." Add the following script:
```r
join(
  forEach(
    split(value, ";"),
    v,
    if(
      trim(v).contains(","),
      toTitlecase(trim(v).replace(/\./, "").replace(/^(.*?),\s*([A-Za-z]).*$/, "$1 $2")),
      if(
        isNonBlank(match(trim(v), /^([A-Z][A-Z'\- ]+)\s+([A-Z])[A-Z.]*$/)),
        toTitlecase(trim(v).replace(/\./, "").replace(/^([A-Z][A-Z'\- ]+)\s+([A-Z])[A-Z.]*$/, "$1 $2")),
        if(
          isNonBlank(match(trim(v), /^(.+?)\s+([A-Za-z]).*$/)),
          toTitlecase(trim(v).replace(/\./, "").replace(/^(.+?)\s+([A-Za-z]).*$/, "$1 $2")),
          toTitlecase(trim(v).replace(/\./, ""))
        )
      )
    )
  ),
  "; "
)
```

## Separate and Clean Author Affiliations
Add a column based on the `affiliations` column and name it "affiliations_key". Select "Edit cells," then "Transform." Add the following script:
```r
value
  .replace(/[^;(]*\(/, "(")
  .replace("(", "")
  .replace(")", "")
  .toLowercase()
  .replace(/\s*;\s*/, "; ")
  .replace(/\s+/, " ")
  .trim()
```

## Cluster and Edit Near-Duplicates
For the columns `AU_key` and `SO_key`, select "Column menu," then "Edit cells." Select "Cluster and Edit." Run key collison methods (e.g., fingerprint, n-gram fingerprint) and nearest neighbor (e.g., Levenshtein distance). Merge obvious variants (e.g., “J Med Libr Assoc” ↔ “Journal of the Medical Library Association”).

## Export Cleaned Dataset
Export the cleaned dataset as an Excel file. Use a standard naming convention: last name/department-combined-original (e.g., smith-combined-original; marketing-combined-original). 
