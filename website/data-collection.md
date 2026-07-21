# Data Collection

## Checklist

- [ ] Create original and working folders
- [ ] Export data from Scopus (csv)
- [ ] Export data from Web of Science (bibtex)
- [ ] Export data from Dimensions (csv)
- [ ] Convert Web of Science file to a csv
- [ ] Standardize column names

## Create Original and Working Folders
Create a folder titled "Exports," and then add subfolders titled "Original" and "Working".

## Export data from Scopus
Export Scopus data in a CSV format. Export citation information, bibliographical information, abstract & keywords, funding details, and other information. Save the file with a standard naming convention: last name/department-scopus-original (e.g., smith-scopus-original.csv; marketing-scopus-original.csv). Save the original export in the "Original" subfolder, and then add a copy to the "Working" folder and update the file name (e.g., smith-scopus-working.csv; marketing-scopus-working.csv).

## Convert Web of Science File to a CSV
Use the following script to convert the original Web of Science (bibtex) file into a csv. Update the file paths for `C:/path/to/your/wos_file.bib` and `C:/path/to/your/wos_converted.csv`.

```r
library(bibliometrix)
wos_file <- "C:/path/to/your/wos_file.bib" # update path name
W <- convert2df(wos_file,
                dbsource = "wos",
                format = "bibtex")
write.csv(W,
          "C:/path/to/your/wos_converted.csv", # update path name
          row.names = FALSE)
```
## Standardize Column Names
In the Scopus and Dimensions csv files, update the column names to the following:

- Author = AU
- Title = TI
- Source / Source Title = SO
- Cited by / Times Cited = TC
- Year / Publication Year = PY
- DOI = DI
- Affiliations / Author Affiliations = affiliations

Then, in all of the files, add the column "DB" and note the database name (i.e., Scopus, Dimensions, ISI).
