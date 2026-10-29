# Data Dictionary

SQLite database: `nrc_crm.sqlite`. Contains tables `reference_materials`, `analyte_tables`, `compounds`, and `spectral_data`.

Built by `populateDatabase.R` (harvest) and `buildDatabase.R` (build). Updated weekly (Mondays 06:00 UTC) by the `update-database.yml` workflow.
Use https://inloop.github.io/sqlite-viewer/ to view the contents of the tables.

**Sources:** NRC Digital Repository and PubChem.

## reference_materials

One row per reference material (CRM).

| Column | Type | Key | Notes |
|---|---|---|---|
| rmid | TEXT | PK | CRM identifier from the OAI record |
| title | TEXT | | Title of the CRM |
| name | TEXT | | The part of title before the colon (e.g. DORM-1) |
| affiliation | TEXT | | Comma-separated list of contributors |
| material_type | TEXT | | One of "Matrix material", "Calibration solution", "High-purity material" |
| summary | TEXT | | Abstract from the digital repository entry |
| doi | TEXT | | |
| date | TEXT | | |

## analyte_tables

Certified values for each analyte in a CRM.

| Column | Type | Key | Notes |
|---|---|---|---|
| rmid | TEXT | PK, FK → reference_materials | |
| name | TEXT | PK | Analyte name (e.g. domoic acid) |
| quantity | TEXT | PK | e.g. mass fraction, concentration |
| value | TEXT | | Type is TEXT because some values include "<" (e.g. <90) |
| uncertainty | REAL | | |
| unit | TEXT | | e.g. µg/kg |
| type | TEXT | | One of "certified", "information", "reference" |

## compounds

One row per compound, enriched from PubChem.

| Column | Type | Key | Notes |
|---|---|---|---|
| name | TEXT | PK | Feeds the app's searchable analyte list |
| inchikey | TEXT | | Not a PK because not every analyte has one |
| cid | INTEGER | | Unique identifier for pubchem compounds |
| molecular_formula | TEXT | | |
| molecular_weight | REAL | | |
| smiles | TEXT | | Represents chemical structure of a compound |
| pKow | REAL | | Calculated as XLogP * -1 |
| exact_mass | REAL | | |
| TPSA | REAL | | |
| crms | TEXT | | Comma-separated CRMs the compound appears in |

## spectral_data

Links to spectral CSV files attached to CRM records.

| Column | Type | Key | Notes |
|---|---|---|---|
| rmid | TEXT | PK, FK → reference_materials | |
| name | TEXT | PK | Spectral name; may not exactly match `compounds.name`, so there is no FK to compounds |
| datatype | TEXT | PK | "Type of Data" from the CSV header (e.g. 1H-NMR) |
| link | TEXT | | URL of the text/csv file in the OAI record |

## Known quirks

- The NRC repository atom search is still used to find CRMs for a compound, because some CRMs have analytes but no analyte table.
- Some analyte tables contain duplicate entries of the same analyte/quantity combination. Duplicates are deleted.


