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
- Add column DB with database name (Scopus / Dimensions)

3. [Clean data in OpenRefine](https://github.com/vvlooking/research-impact/blob/cca98146341568773f508002236f1a7dbf8b6fb7/open-refine)
   
4. De-duplicate data in Excel, retaining articles from the database with the most comprehensive indexing
- De-duplicate first by DOI, then by title
  
5. Manually review and edit data in the following columns:
- AU_key: Ensure author names are formatted as Last Name First Initial (e.g., Smith M; Doe J)
- Publication Type (e.g., Article; Review; Book; Book Chapter; Preprint; Conference Proceedings; Abstract; Editorial Materials; Patents; Guidelines)
- Countries (e.g., usa; canada; england)

## Step 3: Analyze Publication Data
# Quick Summary
