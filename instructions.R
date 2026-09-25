output$instructions <- renderUI({
  
  # helper for a single instruction section
  instruction_card <- function(title, content) {
    card(
      style = "padding: 1.5rem; margin-bottom: 2rem;",
      h4(title, style = "font-weight: bold; margin-bottom: 1rem;"),
      content
    )
  }
  
  tagList(
    
    # Header
    div(
      img(src = 'Icon.png', style = "height: 12vh; margin-bottom: 1rem;"),
      h2("RM Explorer Instructions", style = "font-weight: bold;"),
      p(
        "A guide to searching, exploring, and viewing reference material information.",
        style = "color: #555; font-size: 1rem;"
      ),
      style = "display:flex; flex-direction:column; align-items:center; text-align:center; padding: 2rem 0 1rem 0;"
    ),
    
    # Main content
    div(
      style = "width: 90%; margin: 0 auto; padding: 0 1rem;",
      
      # Home
      instruction_card(
        "Home",
        tagList(
          p("The Home page is the main workspace for searching for compounds and CRMs and viewing information about selected compounds."),
          p(
            "The ", strong("Search"), 
            " sidebar contains two sections: ", strong("CRM Search"), 
            " and ", strong("Compound Search"), 
            ". The main area of the page displays the properties of the selected compound."
          )
        )
      ),
      
      # CRM Search
      instruction_card(
        "CRM Search",
        tagList(
          p(
            "The ", strong("CRM Search"), 
            " section of the sidebar allows you to search and select Certified Reference Materials (CRMs) from the NRC Repository."
          ),
          p(
            "The ", strong("'Search for a CRM'"), 
            " dropdown displays the available CRMs. Select a CRM to add it to the CRM table."
          ),
          p(
            "You can use ", strong("'Select an Affiliate'"), 
            " to view the CRMs associated with a specific group."
          ),
          p(
            "Clicking the name of a CRM in the table will display a popup containing information about that CRM."
          ),
          p(
            "The ", strong("'Add All CRMs to the Table'"), 
            " button adds all CRMs from the NRC Repository to the table."
          ),
          p(
            "To add the compounds referenced by one or more CRMs to the Compound Table, select the desired CRM(s) and click ",
            strong("'Add Chosen CRM(s) to the Compound Table'"), "."
          ),
          p(
            "The ", strong("'Unselect All Rows'"), 
            " button can be used to clear all selected CRMs."
          )
        )
      ),
      
      # Compound Search
      instruction_card(
        "Compound Search",
        tagList(
          p(
            "The ", strong("Compound Search"), 
            " section of the sidebar allows you to search for compounds using an InChIKey, Compound name, IUPAC name, or any Keyword."
          ),
          p(
            "There is a checkbox next to the search dropdown that lets you choose whether the selected compound should be ",
            strong("added"), 
            " to the Compound Table or whether it should ",
            strong("replace"), 
            " everything currently in the table."
          ),
          p(
            "You can select a compound from the Compound Table to display its properties in the main area of the Home page."
          ),
          p(
            "The ", strong("'Unselect All Rows'"), 
            " button can be used to clear all selected compounds from the table."
          ),
          p(
            "You can save the compounds currently loaded in the table by clicking the ",
            strong("'Save Table Compounds'"), 
            " button. This will download a .csv file."
          ),
          p(
            "To restore a previously saved table, click ",
            strong("'Load Saved Compounds'"), 
            " and upload the .csv file."
          )
        )
      ),
      
      # Properties
      instruction_card(
        "Properties",
        tagList(
          p(
            "The ", strong("Properties"), 
            " section is displayed in the main area of the Home page. When a single compound is selected from the Compound Table, 
            detailed information about that compound will be displayed here."
          ),
          p(
            "If the compound has spectral data, you can click the ",
            strong("'Go To Spectral Data'"), 
            " button or scroll down the page to see its spectral data. Once you reach the Spectral Data section, you can select the 
            available spectral data from the dropdown to view the corresponding spectral graphs. A download button is also available to 
            download the selected spectral data."
          ),
          p(
            "A ", strong("'Similar Compounds'"), 
            " dropdown is available in the Properties section. It displays compounds that are similar to the selected compound in the 
            NRC Repository."
          ),
          p(
            "To add a similar compound to the Compound Table, select it from the dropdown and click the ",
            strong("'Add Compound'"), 
            " button."
          )
        )
      ),
      
      # Polarity-MW Plot
      instruction_card(
        "Polarity-MW Plot",
        tagList(
          p(
            "The Polarity-MW Plot contains four dropdowns: ",
            strong("Filter polarity (pKow)"), 
            ", ", strong("Filter Molecular Weight"), 
            ", ", strong("Show Compound Name"), 
            ", and ", 
            strong("Show All Analytes in the Compound Table or Only Selected Analytes"), 
            ". These allow further filtering of the compounds in the Compound Table."
          ),
          p(
            "A list of compounds will be displayed at the bottom of the page. This list contains all the compounds that match the 
            selected filters."
          ),
          p(
            "You can click ", strong("'Download the Compound List'"), 
            " to download information solely about the filtered compounds."
          )
        )
      ),
      
      # CMC Information
      instruction_card(
        "CMC Information",
        tagList(
          p(
            "The CMC Information page displays information from the KCDB API. Each column uses either ",
            strong("Uncertainty Convention One"), 
            " or ", strong("Uncertainty Convention Two"), 
            ", as specified by the last column."
          ),
          p(
            strong("Convention One"), 
            " is used when the expanded uncertainty range spans from the smallest numerical value of the uncertainty to the 
            largest numerical value of the uncertainty found within the quantity range."
          ),
          p(
            strong("Convention Two"), 
            " is used when the expanded uncertainty range is expressed as the uncertainty of the smallest value of the quantity to 
            the uncertainty of the largest value of the quantity; i.e., there is a link between the ",
            strong("'from'"), 
            " entries and a link between the ", 
            strong("'to'"), 
            " entries for the dissemination range and the expanded uncertainty range."
          ),
          p(
            "You may hover over the ", 
            strong("Uncertainty Convention"), 
            " column header in order to view this description on the page."
          )
        )
      ),
      
      br(), br()
    )
  )
})