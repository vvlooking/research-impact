# Manually Review and Edit Cleaned Data

## Checklist
- [ ] De-duplicate documents
- [ ] Review author and affiliations
- [ ] Add publication types

## De-deuplicate Documents
Create a working copy of the file exported from OpenRefine and use a standard naming convention (e.g., smith-combined-deduplicated; marketing-combined-deduplicated). Save the file in the "Working" subfolder. Then, delete duplicate documents, retaining the record from the database with the most comprehensive indexing, by sorting first by the DOI and then the title. For each document, keep the following unique columns, regardless of which record is retained: `FCR`, `RCR`, `Fields of Research`, `KW_Merged`, `Author Keywords`, and `Index Keywords`.

## Review Author and Affiliations
Ensure author names are formatted as Last Name First Intitial (e.g., Smith M; Doe J) in the `AU_key` column. Add any missing affiliations in the `affiliations_key` column.

## Add Publication Types
Verify the publication type for each document (e.g., Article; Review; Book; Book Chapter; Preprint; Conference Proceedings; Abstract; Editorial Materials; Patents; Guidelines).
