# Data Collection

## Checklist

- [ ] Create a project folder
- [ ] Export data from Scopus (csv)
- [ ] Export data from Web of Science (bibtex)
- [ ] Export data from Dimensions (csv)
- [ ] Convert Web of Science file to a csv
- [ ] Standardize column names

## Create a Project Folder
Create a folder for the project (e.g., Smith Report Fall 2025; Marketing Report Spring 2026). Create a subfolder titled "Exports," and then add subfolders titled "Original" and "Working" in the "Exports" folder.

## Export Data from Scopus
Export Scopus data in a CSV format. Export citation information, bibliographical information, abstract & keywords, funding details, and other information. Save the file with a standard naming convention: last name/department-scopus-original (e.g., smith-scopus-original; marketing-scopus-original). Save the original export in the "Original" subfolder, and then add a copy to the "Working" folder and update the file name (e.g., smith-scopus-working; marketing-scopus-working).

## Export Data from Web of Science
Export Web of Science data in a BibTeX format. Export the full record and cited referneces. Save the file with a standard naming convention: last name/department-isi-original (e.g., smith-isi-original.csv; marketing-isi-original.csv). Save the original export in the "Original" subfolder, and then add a copy to the "Working" folder and update the file name (e.g., smith-isi-working; marketing-isi-working).

## Export Data from Dimensions
Export Dimensions data in a CSV format. Export the full record. Save the file with a standard naming convention: last name/department-dimensions-original (e.g., smith-dimensions-original; marketing-dimensions-original). Save the original export in the "Original" subfolder, and then add a copy to the "Working" folder and update the file name (e.g., smith-dimensions-working; marketing-dimensions-working).

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
In the Scopus and Dimensions working csv files, update the column names to the following:

- Author = AU
- Title = TI
- Source / Source Title = SO
- Cited by / Times Cited = TC
- Year / Publication Year = PY
- DOI = DI
- Affiliations / Author Affiliations = affiliations
- Publication Type = DT
- Abstract = AB
  
Then, add the column "DB" and note the database name (i.e., Scopus, Dimensions). Retain only the following columns in the Scopus, Web of Science, and Dimensions working files: DB, DI, TI, SO, AB, PY, DT, AU, TC, affiliations, KW_Merged (Web of Science only), Author Keywords (Scopus only), Index Keywords (Scopus only), RCR (in Dimensions only), FCR (in Dimensions only), and Fields of Research (in Dimensions only).
