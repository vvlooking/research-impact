# Research Impact Reporting

## Step 1: Data Collection
1. Export Web of Science publications (bibtex)
2. Export Scopus publications (csv)
3. Export Dimensions publications (csv)

## Step 2: Data Cleaning
1. [Convert Web of Science file to a csv](https://github.com/vvlooking/research-impact/blob/d08bb6b21a763651fa52b0a3852a69f3c284f423/data-collection)
2. Edit column names in Scopus and Dimensions files
- Author = AU
- Title = TI
- Source / Source Title = SO
- Cited by / Times Cited = TC
- Year / Publication Year = PY
- DOI = DI
- Affiliations / Author Affiliations = affiliations
