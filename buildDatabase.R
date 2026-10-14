library(DBI)
library(RSQLite)

DB_PATH <- "nrc_crm.sqlite"

harvest <- readRDS("harvest.rds")
list2env(harvest, environment())  # reference_materials, analyte_tables, compounds, spectral_data, harvest_started

# The database is now the source of truth and is never deleted by this script.
# To force a full rebuild (e.g. after a schema change), delete nrc_crm.sqlite and re-run populateDatabase.R (with no database it harvests everything)
con <- dbConnect(RSQLite::SQLite(), DB_PATH)
on.exit(dbDisconnect(con), add = TRUE)
dbExecute(con, "PRAGMA foreign_keys = ON")

# create tables (no-ops if they already exist)
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
  crms              TEXT
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

# bookkeeping: remembers where the last successful harvest started
dbExecute(con, "
CREATE TABLE IF NOT EXISTS meta (
  key   TEXT NOT NULL PRIMARY KEY,
  value TEXT
)")

# Insert new rows and update existing ones (matched on key_cols).
# Uses ON CONFLICT ... DO UPDATE rather than INSERT OR REPLACE, so parent
# rows are updated in place and never deleted out from under child rows.
upsert <- function(con, table, df, key_cols, coalesce = FALSE) {
  if (nrow(df) == 0) return(invisible(0L)) # skip empty batches (no updates)
  cols <- names(df)
  upd  <- setdiff(cols, key_cols)
  set_clause <- if (length(upd) == 0) {
    "DO NOTHING"
  } else if (coalesce) {
    # coalesce = TRUE: an NA in the incoming data never overwrites a stored value
    # (used for compounds, so a failed PubChem lookup can't wipe good data).
    paste0("DO UPDATE SET ", paste0(upd, " = COALESCE(excluded.", upd, ", ", table, ".", upd, ")", collapse = ", "))
  } else {
    paste0("DO UPDATE SET ", paste0(upd, " = excluded.", upd, collapse = ", "))
  }
  sql <- sprintf(
    "INSERT INTO %s (%s) VALUES (%s) ON CONFLICT (%s) %s",
    table, paste(cols, collapse = ", "),
    paste(rep("?", length(cols)), collapse = ", "),
    paste(key_cols, collapse = ", "), set_clause
  )
  dbExecute(con, sql, params = unname(as.list(df))) # run statement once per row
  invisible(nrow(df))
}

dbWithTransaction(con, {
  # parents first
  upsert(con, "reference_materials", reference_materials, "rmid")
  upsert(con, "compounds", compounds, "name", coalesce = TRUE)
  
  # a re-harvested record comes with its complete analyte table, so replace
  # that record's rows (this also removes analytes deleted upstream)
  if (nrow(analyte_tables) > 0) {
    refreshed <- unique(analyte_tables$rmid) # refreshed only contains new/changed records
    dbExecute(con, "DELETE FROM analyte_tables WHERE rmid = ?", params = list(refreshed))
  }
  upsert(con, "analyte_tables", analyte_tables, c("rmid", "name", "quantity"))
  
  # a spectrum whose record isn't in the database yet would violate the FK;
  # skip it now, it is picked up again on the next run
  if (nrow(spectral_data) > 0) {
    known_rmids <- dbGetQuery(con, "SELECT rmid FROM reference_materials")$rmid
    ok <- spectral_data$rmid %in% known_rmids
    if (any(!ok)) message(sum(!ok), " spectral rows skipped: no matching reference material yet")
    spectral_data <- spectral_data[ok, , drop = FALSE]
  }
  upsert(con, "spectral_data", spectral_data, c("rmid", "name", "datatype"))
  
  # only advance the harvest marker if everything above succeeded
  dbExecute(con, "INSERT INTO meta (key, value) VALUES ('last_harvest', ?)
                  ON CONFLICT (key) DO UPDATE SET value = excluded.value",
            params = list(harvest_started))
})

# verify
for (t in c("reference_materials", "compounds", "analyte_tables", "spectral_data")) {
  cat(sprintf("%-20s %d rows\n", t, dbGetQuery(con, paste("SELECT COUNT(*) n FROM", t))$n))
}