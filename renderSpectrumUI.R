#spectral data dropdown options for types of spectra
output$spectrumUI <- renderUI({
  list(
    uiOutput("spectrumView"),
    div(uiOutput("spectrumNothingSelected"), style="display:flex;justify-content:center;font-size:2rem;")
  )
})

