## about.R
## Renders output$about for use in a tabPanel("About", uiOutput("about"))

output$about <- renderUI({
  
  # helper for a single FAIR criterion (bold code/title + description)
  criterion <- function(label, text) {
    tagList(
      p(strong(label), style = "margin-bottom: 0.2rem;"),
      p(text, style = "color:#555; margin-bottom: 1rem;")
    )
  }
  
  tagList(
    div(
      img(src = 'Icon.png', style = "height: 12vh; margin-bottom: 1rem;"),
      h2("About the RM Explorer", style = "font-weight: bold;"),
      style = "display:flex; flex-direction:column; align-items:center; text-align:center; padding: 2rem 0 1rem 0;"
    ),
    
    div(
      style = "width: 90%; margin: 0 auto; padding: 0 1rem;",
      
      card(
        style = "padding: 1.5rem; margin-bottom: 2rem;",
        h4("What is the RM Explorer?", style = "font-weight: bold;"),
        p("The Reference Material Explorer is an application built upon the NRC Digital Repository external Application Programming Interfaces (APIs) 
         that allows users to visualise, analyse and display useful information about the Reference Materials produced by the National 
         Research Council of Canada. This application relies upon and complies with FAIR data principles and showcases multiple uses of 
         machine-readable information in digital CRM certificates.", style = "font-size: 1rem; line-height: 1.6;")
      ),
      
      card(
        style = "padding: 1.5rem; margin-bottom: 2rem;",
        p("The RM Explorer was developed by the National Research Council of Canada's (NRC) Biotoxin Metrology Team."),
        p(strong("Please cite as:"), " Bruno Garrido, Tanishka Ghosh, Patricia LeBlanc, Pearse McCarron, Juris Meija, RM Explorer version 1.0, 2025. ",
          a(href = "https://rmexplorer-rmev1.share.connect.posit.cloud/", "https://rmexplorer-rmev1.share.connect.posit.cloud/", target = "_blank")),
        p("Report any bugs/issues to: ", a(href = "mailto:bruno.garrido@nrc-cnrc.gc.ca", "bruno.garrido@nrc-cnrc.gc.ca"))
      ),
      
      h4("FAIR Compliance", style = "font-weight: bold; margin-bottom: 1rem;"),
      p("The RM Explorer uses data from the digital certificates of reference materials and open-source compound
         identifiers (InChI / InChIKeys) to calculate information and present it in a user-friendly way. It also creates
         an integrated data structure by fetching information from external sources such as PubChem and comparing the
         information presented in these external sources to its calculated values."),
      p("The RM Explorer was structured to present data in a way that complies with ",
        a(href = "https://www.go-fair.org/fair-principles/", "FAIR principles", target = "_blank"), strong(":"),
        style = "margin-bottom: 1.5rem;"),
      
      accordion(
        id = "fairAccordion",
        open = FALSE,
        
        accordion_panel(
          value = "findable",
          title = tagList(bs_icon("search"), " Findable"),
          criterion("F1. (Meta)data are assigned a globally unique and persistent identifier",
                    "Each RM in the explorer is identified by a DOI record in the digital repository. Data can be downloaded with rich metadata."),
          criterion("F2. Data are described with rich metadata (defined by R1 below)",
                    "All data has associated metadata that ensures its traceability and analytically relevant information."),
          criterion("F3. Metadata clearly and explicitly include the identifier of the data they describe",
                    "Metadata includes the chemical identifiers and DOI when downloaded."),
          criterion("F4. (Meta)data are registered or indexed in a searchable resource",
                    "All data is indexed in the DOI record.")
        ),
        
        accordion_panel(
          value = "accessible",
          title = tagList(bs_icon("unlock"), " Accessible"),
          criterion("A1. (Meta)data are retrievable by their identifier using a standardised communications protocol",
                    "Data and metadata present in the RM Explorer are retrievable by their identifiers through the app and the NRC digital repository and are downloadable in common (.csv) file formats."),
          criterion("A1.1 The protocol is open, free, and universally implementable",
                    "The website is open and downloaded data and metadata use non-proprietary (.csv) format."),
          criterion("A2. Metadata are accessible, even when the data are no longer available",
                    "Metadata are stored in the repository using DOIs, which are widely known persistent identifiers.")
        ),
        
        accordion_panel(
          value = "interoperable",
          title = tagList(bs_icon("diagram-3"), " Interoperable"),
          criterion("I1. (Meta)data use a formal, accessible, shared, and broadly applicable language for knowledge representation",
                    "Data use formats that are widely accepted (DOIs), open-source (InChI) and non-proprietary (.csv)."),
          criterion("I2. (Meta)data use vocabularies that follow FAIR principles",
                    "Vocabularies used are fully FAIR compliant (DOIs and InChIs)."),
          criterion("I3. (Meta)data include qualified references to other (meta)data",
                    "RM Explorer cross-checks data using PubChem and links each unique entry to its PubChem entry when available.")
        ),
        
        accordion_panel(
          value = "reusable",
          title = tagList(bs_icon("arrow-repeat"), " Reusable"),
          criterion("R1. (Meta)data are richly described with a plurality of accurate and relevant attributes",
                    "The RM Explorer integrates different data sources (APIs) in a user-friendly interface to machine-readable digital CRM certificates. Therefore the attributes of the (meta)data are shared with the data sources."),
          criterion("R1.1. (Meta)data are released with a clear and accessible data usage license",
                    "All data is provided openly for non-commercial usage."),
          criterion("R1.2. (Meta)data are associated with detailed provenance",
                    "Data and metadata are obtained from three different APIs: NRC digital repository, PubChem and KCDB."),
          criterion("R1.3. (Meta)data meet domain-relevant community standards",
                    "Data whose source is a Reference Material was peer-reviewed or is under the scope of its provider's accreditation according to ISO/IEC 17034 and related standards. Data collected from external sources comply with their own relevant standards.")
        )
      ),
      
      br(), br()
    )
  )
})