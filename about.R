output$about <-renderUI({
  HTML(paste(
    "The RM Explorer was developed by the National Research Council of Canada's (NRC) Biotoxin Metrology Team.<br>
        Please cite as: Bruno Garrido, Tanishka Ghosh, Pearse McCarron, Juris Meija, RM Explorer version 0.3, 2025 
        <a href=https://rmexplorer.shinyapps.io/RMEv1/>https://rmexplorer.shinyapps.io/RMEv1/ </a><br><br>
        Report any bugs/issues to: bruno.garrido@nrc-cnrc.gc.ca",
    "<br><br><br><hr><br>",
    "<h2>FAIR Compliance</h2><br>",
    "<strong>The RM Explorer uses data from the digital certificates of reference materials and open-source compound identifiers (InChI / InChIKeys) to calculate information and present it in a user-friendly way. 
    It also creates an integrated data structure by fetching information from external sources such as PubChem and comparing the information presented in these external sources to its calculated values.
    </strong> <br><br><strong>The RM Explorer was structured to present data in a way that complies with FAIR principles (https://www.go-fair.org/fair-principles/): </strong><br><br>",
    "<br><h3>Findable</h3><br>",
    "<strong>F1. (Meta)data are assigned a globally unique and persistent identifier </strong><br>",
    "Each RM in the explorer is assigned its unique ID and is retrievable using that ID. Data can be downloaded with rich metadata including this unique identification.<br><br>",
    "<strong>F2. Data are described with rich metadata (defined by R1 below) </strong><br>",
    "All data has associated metadata that ensures its traceability and analytically relevant information.<br><br>",
    "<strong>F3. Metadata clearly and explicitly include the identifier of the data they describe</strong><br>",
    "Metadata includes the identifiers as a part of the filename when downloaded.<br><br>",
    "<strong>F4. (Meta)data are registered or indexed in a searchable resource</strong><br>",
    "All data can be searched and accessed online at the RM Explorer website.<br><br>",
    "<br><h3>Accessible</h3><br>",
    "<strong>A1. (Meta)data are retrievable by their identifier using a standardised communications protocol </strong><br>",
    "Data and metadata present in the RM Explorer are retrievable by their identifiers through the website and are downloadable in common (.csv) file formats. <br><br>",
    "<strong>A1.1 The protocol is open, free, and universally implementable </strong><br>",
    "The website is open and downloaded data and metadata use non-proprietary (.csv) format. <br><br>",
    "<strong>A1.2 The protocol allows for an authentication and authorisation procedure, where necessary </strong><br>",
    "Authentication and authorization are required to add data. Newly added data are curated/approved by users with admin privileges. <br><br>",
    "<strong>A2. Metadata are accessible, even when the data are no longer available </strong><br>",
    "Metadata are stored in a repository that is independent of the application itself which ensures accessibility to the metadata even when not presented in the app. Unique IDs are not reused, ensuring maintenance of relevant metadata. <br><br>",
    "<br><h3>Interoperable</h3><br>",
    "<strong>I1. (Meta)data use a formal, accessible, shared, and broadly applicable language for knowledge representation. </strong><br>",
    "Data use formats that are widely accepted (DOIs), open-source (InChI) and non-proprietary (.csv). <br><br>",
    "<strong>I2. (Meta)data use vocabularies that follow FAIR principles </strong><br>",
    "Vocabularies used are fully fair compliant (DOIs and InChIs). <br><br>",
    "<strong>I3. (Meta)data include qualified references to other (meta)data </strong><br>",
    "RM Explorer cross-checks data using PubChem and links each unique entry to its PubChem and a NP-MRD entry when available. <br><br>",
    "<br><h3>Reusable</h3><br>",
    "<strong>R1. (Meta)data are richly described with a plurality of accurate and relevant attributes </strong><br>",
    "The data in RM Explorer were generated to provide a user-friendly interface to machine-readable digital CRM certificates. Other data are generated/collected and cross-checked with external sources. Data are also curated to ensure it is useful. Variable names presented in the metadata are self-explanatory and include reference to the original raw data. <br><br>",
    "<strong>R1.1. (Meta)data are released with a clear and accessible data usage license </strong><br>",
    "All data is provided openly for non-commercial usage. <br><br>",
    "<strong>R1.2. (Meta)data are associated with detailed provenance </strong><br>",
    "Data and metadata keep information on its contributors and traceability to raw data files where applicable. <br><br>",
    "<strong>R1.3. (Meta)data meet domain-relevant community standards </strong><br>",
    "Data whose source are Reference Materials was peer-reviewed or is under the scope of its provider’s accreditation according to ISO/IEC 17034 and related standards. Data collected from external sources comply with their own relevant standards. <br><br>"
    ))
})