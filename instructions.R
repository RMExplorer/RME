output$instructions <- renderUI({
  HTML(paste(
    "<h3>General Search Page </h3>",
    "<p>Use the dropdown to search for an Inchikey, Compound, IUPAC or any Keyword. There is a check box next to the search dropdown
    that lets you pick whether you want to add the selected substance to the table or if you want the selected substance to replace 
    everything that is in the table.
    You can then select a compound from the table shown in order to display its properties on the Properties tab. 
    You can also select a substance from the table and see its spectral data (if it has any) on the Spectral Data tab.
    You can clear all your selection by clicking the 'Unselect All Rows' button located at the top right of the page. 
    You can save the substances you have loaded in the table by clicking the 'Save Table Substances' button. This will give you a .csv file.
    If you want to view the same table later on, you can load it by clicking the 'Load Saved Substances' button and uploading the .csv file.</p>",
    "<br><hr><br>",
    
    "<h3>CRM Search Page </h3>",
    "<p>The `Search for a CRM` dropdown displays all the CRMs in the NRC Repository. Select a CRM from that dropdown to add it to the table below.
    You can also use the `Select an Affiliate` in order to see all the CRMs from a certain group.
    If you click on the name of a CRM on the table, you can see a popup which displays some information about the CRM.
    If you click the `Add All CRMs to the Table` button, then all the CRMs from the NRC Repository will be added to the table.
    If you want to add the substances referenced in a specific CRM to the Substance table in the General Search Page, click on the CRM(s) and 
    then press the `Add Choosen CRM(s) to the Substance Table` button.</p>",
    "<br><hr><br>",
    
    "<h3>Properties Page </h3>",
    "<p>When you have selected a singular substance from the table in the General Search Page, 
    this page will show detailed information about the substance. 
    There will be a `Similar Compounds` dropdown on this page which will display all the substances that are similar 
    to the choosen one in the NRC Repository. If you want, you can select a compound and then click the `Add Compound`
    button next to it in order to add it to the Subtance table in the General Search Page.</p>",
    "<br><hr><br>",
    
    "<h3>Polarity-MW Plot Page </h3>",
    "<p>Contains 4 dropdowns (Filter polarity (pKow), Filter Molecular Weight, Show Compound Name, Show All Analytes in the Substances Table 
    or Only Selected Analytes) 
    which allow further filtering of the substances in the Substance Table.
    </p>",
    "<p>You will be able to see a list of compounds at the bottom. This list contains all the compounds that match the filters.
    You can also click the 'Download the Compound List' in order to download information solely about the filtered compounds.</p>",
    "<br><hr><br>",
    
    "<h3>Spectral Data Page </h3>",
    "<p>Once you have selected a singular substance from the Substance Table in the General Search Page, 
    you can view all the spectral graphs it has by selecting them from the dropdown.
    There is also a download button located to the top right so you can download the spectral data you have selected.</p>",
    "<br><hr><br>",
    
    
    "<h3>CMC Information Page </h3>",
    "<p>Displays information from the KCDB API. Each column uses either Uncertainy Convention One or Two, as specified by the last column.
    Convention one is used when the expanded uncertainty range spans from the smallest numerical 
    value of the uncertainty to the largest numerical value of the uncertainty found within the quantity range. 
    Convention 2 two is used when the expanded uncertainty range is expressed as the uncertainty of the smallest 
    value of the quantity to the uncertainty of the largest value of the quantity; I.e., there is a link between 
    the `from` entries and a link between the `to` entries for the dissemination range and the expanded uncertainty range.
    You may hover over the Uncertainty Uncertainty Convention column header in order to view this description on the page.</p>"
    
  ))
})