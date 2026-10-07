library(DBI)
library(RSQLite)
library(dplyr)

harvest <- readRDS("harvest.rds")

# remove duplicate and null primary keys
harvest$analyte_tables <- harvest$analyte_tables |> 
  filter(!is.na(name), !is.na(quantity)) |>
  distinct(rmid, name, quantity, .keep_all = TRUE)
harvest$spectral_data <- harvest$spectral_data |> 
  filter(!is.na(name), !is.na(datatype)) |>
  distinct(rmid, name, datatype, .keep_all = TRUE)

list2env(harvest, environment()) # recreates the four data frames

if (file.exists("nrc_crm.sqlite")) file.remove("nrc_crm.sqlite") # clean rebuild

# create / connect to the database
con <- dbConnect(RSQLite::SQLite(), "nrc_crm.sqlite")
dbExecute(con, "PRAGMA foreign_keys = ON")

# create tables
dbExecute(con, "
CREATE TABLE IF NOT EXISTS reference_materials (
  rmid          TEXT NOT NULL PRIMARY KEY,
  name          TEXT,
  title         TEXT,
  affiliation   TEXT,
  material_type TEXT,
  summary       TEXT,
  doi           TEXT,
  date          TEXT
)")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS compounds (
  name              TEXT NOT NULL PRIMARY KEY,
  inchikey          TEXT,
  cid               INTEGER,
  molecular_formula TEXT,
  molecular_weight  REAL,
  smiles            TEXT,
  pKow              REAL,
  exact_mass        REAL,
  TPSA              REAL,
  synonyms          TEXT
)")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS analyte_tables (
  rmid        TEXT NOT NULL,
  name        TEXT NOT NULL,
  quantity    TEXT NOT NULL,
  value       TEXT,
  uncertainty REAL,
  unit        TEXT,
  type        TEXT,
  PRIMARY KEY (rmid, name, quantity),
  FOREIGN KEY (rmid) REFERENCES reference_materials(rmid),
  FOREIGN KEY (name) REFERENCES compounds(name)
)")

dbExecute(con, "
CREATE TABLE IF NOT EXISTS spectral_data (
  rmid     TEXT NOT NULL,
  name     TEXT NOT NULL,
  inchikey TEXT,
  datatype TEXT NOT NULL,
  link     TEXT,
  PRIMARY KEY (rmid, name, datatype),
  FOREIGN KEY (rmid) REFERENCES reference_materials(rmid)
)")

# insert (parents first), all in one transaction
dbWithTransaction(con, {
  dbAppendTable(con, "reference_materials", reference_materials)
  dbAppendTable(con, "compounds", compounds)
  dbAppendTable(con, "analyte_tables", analyte_tables)
  dbAppendTable(con, "spectral_data", spectral_data)
})

# verify
dbListTables(con)
dbGetQuery(con, "SELECT COUNT(*) AS n FROM compounds")
dbGetQuery(con, "SELECT * FROM compounds LIMIT 5")

dbDisconnect(con)
