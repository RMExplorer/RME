# RME
Reference Material Explorer
<br><br>
The RM Explorer is a Shiny web application that enables data visualization and analysis based on the NRC digital repository of reference material certificates and 
integrates it with other data sources such as PubChem and the BIPM Key Comparison Database.


The RM Explorer was developed by the National Research Council of Canada's (NRC) Biotoxin Metrology and Inorganic Metrology Teams.

Please cite as: Bruno Garrido, Tanishka Ghosh, Daniel Yang, Patricia LeBlanc, Marcin Paluch, Sophie Roy, Pearse McCarron, Zoltán Mester, and Juris Meija, RM Explorer version 1.0, 2025 https://rmexplorer.shinyapps.io/RMEv1/

## Getting Started

### 1. Clone the repository
Clone the repository using Git:
```bash
git clone https://github.com/RMExplorer/RME.git
```
Then move into the project directory:
```bash
cd RME
```

### 2. Open the project
Open the project in RStudio by opening the `.Rproj` file.

### 3. Restore the R environment
This project uses `renv` to keep track of the R packages and package versions required by the application.  
If `renv` is not already installed, install it with:
```bash
install.packages("renv")
```
Then restore the project's environment:
```bash
renv::restore()
```

### 4. Run the app
Once the environment has been restored, start the application with:
```bash
shiny::runApp()
```