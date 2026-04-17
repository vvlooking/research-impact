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
### [Quick Summary](https://github.com/vvlooking/research-impact/blob/10dbc12bfb38b74a98a3c3292516f00c67f76cf1/quick-summary)
- Create figure: "Total Citations of Documents Published Each Year"
- Calculate sum of times cited
- Calculate the average number of citations per document
- List the top three most cited documents

### [Productivity](https://github.com/vvlooking/research-impact/blob/2694228e5cedf5afe8e8136dfae6bb00e666ff1f/productivity)
- Calculate the number of documents published per year
- Calculate the average number of documents published per year
- Create figure: "Average Productivity per Active Year"
- Calculate the number and percentage of document types
- Calculate the number of documents by authorship order (i.e., sole, first, and last author)

### [Dissemination](https://github.com/vvlooking/research-impact/blob/ee1e42d18d7eefe63f6d5d9a5b64e37054d0f8f3/dissemination)
- List journal impact factors
- Mark journals in the top 10% of their category by JIF percentile
- Calculate the number and percentage of journal quartiles (Excel pivot table)
- Create figure: "Percentage of Documents Published in Journals by JIF Quartile"
- Create figure: "Articles by Journal Citation Reports (JCR) Category"
- Calculate JCR categories (Excel pivot table)

### [Collaboration](https://github.com/vvlooking/research-impact/blob/abe4b402b604d631308f259bbf86e931527ef135/collaboration)
- Calculate number of unique co-authors
- Calculate average number of co-authors per document
- Create figure: Collaboration Network (Authors)
- Calculate number of unique co-author affiliations (outside of USC)
- Identify top institutional affiliations of co-authors
- Create figure: "Collaboration Network (Institutions)"
- Calculate number of unique co-author affiliations (within USC)
- Create figure: "Collaboration Network (USC)"
