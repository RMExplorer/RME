# Deployment and Operations

How rmexplorer is built, updated, and deployed.

- **Live app:** https://rmexplorer-rmev1.share.connect.posit.cloud/
- **Old URL:** the previous shinyapps.io address redirects to the live app, so the migrated Connect Cloud item must be kept
- **Workflow file:** [`.github/workflows/update-database.yml`](../.github/workflows/update-database.yml) a GitHub Actions 
configuration that tells GitHub to automatically rebuild the database, deploy the app, and commit the updated database to the repo every Monday.

## Workflow Overview

| Step | Script / action | What it does |
|---|---|---|
| 1 | `populateDatabase.R` | Harvests NRC CRM records (OAI-PMH) and enriches with PubChem; writes `harvest.rds` |
| 2 | `buildDatabase.R` | Builds `nrc_crm.sqlite` from `harvest.rds` |
| 3 | rsconnect deploy | Publishes the app and database to Connect Cloud |
| 4 | Git commit | Commits the updated `nrc_crm.sqlite` back to the repo |

## Hosting

| Item | Value |
|---|---|
| Platform | Posit Connect Cloud |
| Account | rmexplorer |
| Content ID | 01a0ce84-2e9e-8ef0-5e3a-d2155d16f244 |
| Package requirement | rsconnect 1.11.1 or later |

## Secrets and settings

Configured in GitHub under **Settings → Secrets and variables → Actions**. `CONNECT_CLOUD_CLIENT_ID` and `CONNECT_CLOUD_CLIENT_SECRET`
are used by the workflow to deploy. They can be regenerated at https://login.posit.cloud/identity/credentials.

## Running manually

- **From GitHub:** Actions tab → "Update NRC CRM database" → **Run workflow** (requires `workflow_dispatch` in the YAML file)
- **Locally (RStudio Console):**
  1. `source("populateDatabase.R"")`
  2. `source("buildDatabase.R")`
  3. `install.packages("rsconnect")` (if not already installed)
  4. `rsconnect::connectCloudUser()` (first time only)
  5. `rsconnect::deployApp()`

## Troubleshooting
TODO: fill in as issues arise

| Symptom | Likely cause | What to check |
|---|---|---|
| | | |

