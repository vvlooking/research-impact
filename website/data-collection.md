# Data Collection

## Checklist

- [ ] Export data from Scopus (csv)
- [ ] Export data from Web of Science (bibtex)
- [ ] Export data from Dimensions (csv)
- [ ] Convert Web of Science file to a csv
- [ ] Standardize column names

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
