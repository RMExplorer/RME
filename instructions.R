output$instructions <- renderUI({
  HTML(paste(
    "<h3>Compounds Page </h3>",
    "<p>Use the drop downs to search for an Inchikey, Compound, or Keyword. 
    You can then select a compound from the table shown in order to display its properties on the Properties page. 
    You can also clear all your selection by clicking the 'Unselect All Rows' button located at the bottom left of the page. 
    By loggining in, you will be given the option to save the table you have created for later use. 
    You are allowed to create multiple tables.</p>",
    "<br><hr><br>",
    
    "<h3>Properties Page </h3>",
    "<p>When you have selected a singular compound from the table in the Compounds Page, 
    this page will show detailed information about the compound. 
    There will be an option to select a certificate in the 'Additional Information' section. 
    If the selected certificate has has additional (machine readable) information, it will be displayed.</p>",
    "<br><hr><br>",
    "<h3>Physico-chemical Properties Page </h3>",
    "<p>Contains 4 dropdowns (pKow, Molecular Weight, Show Label, Show Selected or All Compounds) 
    which allow further filtering of the compounds selected from the Compounds Page.
    </p>",
    "<p>You will be able to see a list of compounds at the bottom. This list contains all the compounds that match the filters.
    You can also click the 'Download the compound list' in order to download information soley about the filtered compounds.</p>",
    "<br><hr><br>",
    "<h3>Spectral Data Page </h3>",
    "<p>Once you have selected a singular compound from the Compounds Page, you can view all the spectral graphs it has by selecting it from the dropdown.
    There is also a download button located to the top right so you can download the spectral data you have selected.</p>"
  ))
})