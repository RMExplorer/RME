library(httr)

#get one entry to find out the total number of elements in order to request that number
requestBody <-'{
  "page": 0,
  "pageSize": 1,
  "showTable": false,
  "metrologyAreaLabel": "QM",
  "countries": [
    "CA"
  ]
}'
data <- NULL

tryCatch({
  #get the total number of elements and make another request with it
  kcdb <- POST("https://www.bipm.org/api/kcdb/cmc/searchData/chemistryAndBiology", 
               body=requestBody, 
               httr::add_headers(`accept` = 'application/json'), 
               httr::content_type('application/json'))
  
  totalElements <- content(kcdb)$totalElements
  
  #request all the elements
  requestBody <-paste('{
    "page": 0,
    "pageSize":', totalElements, ',
    "showTable": false,
    "metrologyAreaLabel": "QM",
    "countries": [
      "CA"
    ]
  }', sep="")
  
  kcdb <- POST("https://www.bipm.org/api/kcdb/cmc/searchData/chemistryAndBiology", 
               body=requestBody, 
               httr::add_headers(`accept` = 'application/json'), 
               httr::content_type('application/json'))
  
  kcdbData <- content(kcdb)$data
  
  #initialize the columns
  kcdbId <- c()
  approvalDate <- c()
  cmc <- c()
  cmcUnits <- c()
  cmcUncert <- c()
  cmcUncertUnit <- c()
  category <- c()
  subcategory <- c()
  analyteMatrix <- c()
  name <- c()
  crm <- c()
  crmUnits <- c()
  crmUncert <- c()
  crmUncertUnit <- c()
  uncertConvention <- c()
  
  #going into each item and combining the value into a column
  for (i in 1:length(kcdbData)) {
    kcdbId <- c(kcdbId, kcdbData[[i]]$kcdbCode)
    approvalDate <- c(approvalDate, kcdbData[[i]]$approvalDate)
    cmc <- c(cmc, paste(kcdbData[[i]]$cmc$lowerLimit, "to", kcdbData[[i]]$cmc$upperLimit))
    cmcUnits <- c(cmcUnits, kcdbData[[i]]$cmc$unit)
    cmcUncert <- c(cmcUncert, paste(kcdbData[[i]]$cmcUncertainty$lowerLimit, "to", kcdbData[[i]]$cmcUncertainty$upperLimit))
    cmcUncertUnit <- c(cmcUncertUnit, kcdbData[[i]]$cmcUncertainty$unit)
    category <- c(category, kcdbData[[i]]$categoryValue)
    subcategory <- c(subcategory, kcdbData[[i]]$subCategoryValue)
    analyteMatrix <- c(analyteMatrix, kcdbData[[i]]$analyteMatrix)
    name <- c(name, kcdbData[[i]]$analyteValue)
    crm <- c(crm, ifelse(!is.null(kcdbData[[i]]$crm$lowerLimit), 
                         paste(kcdbData[[i]]$crm$lowerLimit, "to", kcdbData[[i]]$crm$upperLimit), ""))
    crmUnits <- c(crmUnits, kcdbData[[i]]$crm$unit)
    crmUncert <- c(crmUncert, ifelse(!is.null(kcdbData[[i]]$crmUncertainty$lowerLimit), 
                                     paste(kcdbData[[i]]$crmUncertainty$lowerLimit, "to", 
                                           kcdbData[[i]]$crmUncertainty$upperLimit), ""))
    crmUncertUnit <- c(crmUncertUnit, kcdbData[[i]]$crmUncertainty$unit)
    uncertConvention <- c(uncertConvention, kcdbData[[i]]$uncertaintyConvention)
  }
  
  #make a data frame with all the columns
  data <- data.frame(name, kcdbId, approvalDate, 
                     cmc, cmcUnits, cmcUncert,  cmcUncertUnit, 
                     category, subcategory, analyteMatrix, 
                     crm, crmUnits, crmUncert, crmUncertUnit, uncertConvention)
  
  colnames(data) <- c("Name", "KCDB ID", "Approval Date", "CMC", "CMC Units", 
                      "CMC Uncertainty", "CMC Uncertainty Units", "Category", "Subcategory",
                      "Matrix", "CRM", "CRM Units", "CRM Uncertainty", "CRM Uncertainty Units", "Uncertainty Convention")
},
error = function(cond) {
  message(conditionMessage(cond))
  NA
},
warning = function(cond) {
  message(conditionMessage(cond))
  NULL
})

#attaches the hover text the the header for uncertainty convention
headerCallback <- c(
  "function(thead, data, start, end, display){",
  "  var uncertaintyTooltips = 'Check uncertainty convention column. If convention one is used, it means that these uncertainty values are achievable for any quantity value in the measurement range. If convention two is used, there is a linear relation between the quantity values and the achievable uncertainty';",
  "  var uncertaintyConventionTooltips = 'Convention one is used when the expanded uncertainty range spans from the smallest numerical value of the uncertainty to the largest numerical value of the uncertainty found within the quantity range. Convention 2 two is used when the expanded uncertainty range is expressed as the uncertainty of the smallest value of the quantity to the uncertainty of the largest value of the quantity; I.e., there is a link between the <<from>> entries and a link between the <<to>> entries for the dissemination range and the expanded uncertainty range.';",
  "  for(var i=0; i<=end; i++){",
  "    if ($('th:eq('+i+')',thead).text() == 'CMC Uncertainty Units') {",
  "      $('th:eq('+i+')',thead).attr('title', uncertaintyTooltips);",
  "      $('th:eq('+i+')',thead).attr('style', 'text-decoration: underline dotted;');",
  "    }",
  "    if ($('th:eq('+i+')',thead).text() == 'CRM Uncertainty Units') {",
  "      $('th:eq('+i+')',thead).attr('title', uncertaintyTooltips);",
  "      $('th:eq('+i+')',thead).attr('style', 'text-decoration: underline dotted;');",
  "    }",
  "    if ($('th:eq('+i+')',thead).text() == 'Uncertainty Convention') {",
  "      $('th:eq('+i+')',thead).attr('title', uncertaintyConventionTooltips);",
  "      $('th:eq('+i+')',thead).attr('style', 'text-decoration: underline dotted;');",
  "    }",
  "  }",
  "}"
)

#render the datatable shown on the cmc information tab
output$kcdbTable <- renderDT({
  datatable(data,rownames = FALSE,
            options = list(pageLength = 15, responsive = FALSE, scrollX = TRUE, headerCallback = JS(headerCallback)), 
            filter= list(position='top', clear = FALSE), escape = FALSE, 
            )
})