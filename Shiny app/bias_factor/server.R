
function(input, output, session) {
    
  #### Compute the bias factor ####
  bias.factor <- reactive({
    # MM: reject missing or impossible inputs
    if ( is.na( input$effect.estimate.page2 ) | is.na( input$RR_UD ) | is.na( input$RR_EU ) ) stop("Provide all three inputs.")
    if ( any( c( input$RR_UD, input$RR_EU ) < 1 ) ) stop("RR_UD and RR_EU must be at least 1.")
    if ( input$effect.estimate.page2 < 0 ) stop("Point estimate cannot be negative.")
  
    input$RR_UD*input$RR_EU / (input$RR_UD + input$RR_EU - 1)  
  })
  
  adjusted.effect <- reactive({
    adjust.effect <- ifelse(input$effect.estimate.page2 > 1, 
                            input$effect.estimate.page2 / bias.factor(), 
                            input$effect.estimate.page2 * bias.factor()
    )
  })
  
  output$Bias_Factor <- renderUI({
    
    HTML(paste0("The bias factor is ", round(bias.factor(), 2), ". At most, this bias factor could shift the risk ratio to ",
                round(adjusted.effect(), 2), "."))
  })
}