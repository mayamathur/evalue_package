source("startup.R")

function(input, output, session) {
  
  # JUST FOR TESTING
  output$fakeplot = renderPlot({ plot( rnorm(100), rnorm(100) ) })
    
    evals <- reactive({
        if ( input$outcomeType == "RR" ) {
            
            if ( is.na( input$est.RR )) return("Enter your point estimate")
            if ( is.na( input$trueRR )) return("Enter a true value")
            
            
            evals = round( evalues.RR( est = input$est.RR, lo = input$lo.RR, hi = input$hi.RR, true = input$trueRR )[2,], 2 )
            
            #             # check for warning messages
            #             # https://stackoverflow.com/questions/44722408/r-shiny-output-warning-messages-to-ui/44722732#44722732
            #             x <- tryCatch( evalues.RR( est = input$est.RR, lo = input$lo.RR, hi = input$hi.RR, true = input$trueRR ),
            #                            warning=function(w) { w })
            #             #x <- tryCatch(1:3 * 1:2, warning=function(w) { w })
            #             if (inherits(x, "simpleWarning")) {
            #                 mess <- x$message
            #                 showNotification(mess)
            #             }
            
        }
        
        if ( input$outcomeType == "OR.rare" ) {
            if ( is.na( input$est.OR.rare )) return("Enter your point estimate")
            if ( is.na( input$trueORrare )) return("Enter a true value")
            
            evals = round( evalues.OR( est = input$est.OR.rare, lo = input$lo.OR.rare, hi = input$hi.OR.rare, rare = TRUE, true = input$trueORrare )[2,], 2 )
        }
        
        if ( input$outcomeType == "OR.com" ) {
            if ( is.na( input$est.OR.com )) return("Enter your point estimate")
            if ( is.na( input$trueORcom )) return("Enter a true value")
            
            evals = round( evalues.OR( est = input$est.OR.com, lo = input$lo.OR.com, hi = input$hi.OR.com, rare = FALSE, true = input$trueORcom )[2,], 2 )
        }
        
        
        if ( input$outcomeType == "HR.rare" ) {
            if ( is.na( input$est.HR.rare )) return("Enter your point estimate")
            if ( is.na( input$trueHRrare )) return("Enter a true value")
            
            evals = round( evalues.HR( est = input$est.HR.rare, lo = input$lo.HR.rare, hi = input$hi.HR.rare, rare = TRUE, true = input$trueHRrare )[2,], 2 )
        }
        
        if ( input$outcomeType == "HR.com" ) {
            if ( is.na( input$est.HR.com )) return("Enter your point estimate")
            if ( is.na( input$trueHRcom )) return("Enter a true value")
            
            evals = round( evalues.HR( est = input$est.HR.com, lo = input$lo.HR.com, hi = input$hi.HR.com, rare = FALSE, true = input$trueHRcom )[2,], 2 )
        }
        
        if ( input$outcomeType == "MD" ) {  
            if ( is.na( input$est.MD )) return("Enter your point estimate")
            if ( is.na( input$trueMD )) return("Enter a true value")
            
            evals = round( evalues.MD( est = input$est.MD, se = input$se.MD, true = input$trueMD )[2,], 2 )
        }  
        
        #         if ( input$outcomeType == "RG" ) {  
        #             if ( is.na( input$beta.RG ) & is.na( input$pval.RG )) return("Enter your point estimate or p-value")
        #             evals = round( evalues.RG( beta = input$beta.RG, se = input$se.RG,
        #                                        pval = input$pval.RG, n = input$n.RG,
        #                                        true = input$true.RG )[2,], 2 )
        #         }  
        
        if ( input$outcomeType == "RD" ) {  
            
            if ( any( is.na( c( input$n11, input$n10, input$n01, input$n00, input$trueRD ) ) ) ) {
                return("Enter all of the above information")
            }
            
            evals = round( as.numeric( evalues.RD( n11 = input$n11, n10 = input$n10, n01 = input$n01, n00 = input$n00,
                                                    true = input$trueRD, alpha = input$alpha, grid = input$grid ) ), 2 )
            
        }
        
        return( evals )
    })    
    
    output$result.text = renderText({
        
        ##### Create String for UI ##### 
        
         #if there is input for the CI (either lower or upper)
         if ( !is.na(evals()[2]) | !is.na(evals()[3])  ) {
        
             eval.CI = min(evals(), na.rm=TRUE)
        
             result.string = paste( "E-value for point estimate: ", evals()[1],
                                    " and for confidence interval: ", eval.CI,
                                    sep="" )

         #if user only gave point estimate
         } else {
             result.string = paste( "E-value for point estimate: ", evals()[1],
                                    sep="" )
         }
        
        return( result.string )
    
    })

    #### Make the plot ####
    effect.estimate <- reactive({
        
        est.effect <- input$est.RR
        if( input$outcomeType == "OR.rare" ){
            est.effect <- input$est.OR.rare
        }else if( input$outcomeType == "OR.com" ){
            est.effect <- sqrt(input$est.OR.com)
        }else if( input$outcomeType == "HR.rare" ){
            est.effect <- input$est.HR.rare
        }else if ( input$outcomeType == "HR.com" ){
            est.effect <- ( 1 - 0.5^sqrt(input$est.HR.com) )/( 1 - 0.5^sqrt(1/input$est.HR.com) )
        }else if ( input$outcomeType == "MD" ){
            est.effect <- exp(0.91*input$est.MD)
        }else if ( input$outcomeType == "RD" ){
            est.effect <- ( input$n11/(input$n11 + input$n10) )/( input$n01/(input$n01 + input$n00) )
        }
        
        return( est.effect )
    })
  
    
    
    output$curveOfExplainAway <- renderPlotly({
        
        rr.ud <- function(rr.eu) {
            
            if(effect.estimate() > 1){
                
                ( effect.estimate()*(1 - rr.eu) )/( effect.estimate() - rr.eu )
                
            }else{
                
                ( (1/effect.estimate())*(1 - rr.eu) )/( (1/effect.estimate()) - rr.eu )
            }
        }
        
        g <- ggplotly(
            ggplot(data.frame(rr.eu = c(0, 20)), aes(rr.eu)) + 
                stat_function(fun = rr.ud) + 
                scale_y_continuous(limits = c(1, evals()[1]*3)) + 
                scale_x_continuous(limits = c(1, evals()[1]*3)) +
                xlab("Risk ratio for exposure-confounder relationship") + ylab("Risk ratio for confounder-disease relationship") + 
                geom_point(dat = data.frame(rr.eu = evals()[1], rr.ud = evals()[1]), aes(rr.eu, rr.ud)) +
                geom_text(dat = data.frame(rr.eu = evals()[1], rr.ud = evals()[1]), 
                          aes(rr.eu, rr.ud), 
                          label = paste0("E-value:\n (", round(evals()[1], 2), ",", round(evals()[1], 2),")"),
                          nudge_x = evals()[1]*(3/5), size = 3) + 
                theme_minimal()
        )
        
        g$x$data[[2]]$text <- "E-value"
        g$x$data[[1]]$text <- gsub("y", "RR_UD", g$x$data[[1]]$text)
        g$x$data[[1]]$text <- gsub("rr.eu", "RR_EU", g$x$data[[1]]$text)
        
        g
    }) 
    
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
                                input$effect.estimate.page2/bias.factor(), 
                                input$effect.estimate.page2*bias.factor()
                                )
    })
    
    output$Bias_Factor <- renderUI({

         HTML(paste0("The bias factor is ", round(bias.factor(), 2), ". At most, this bias factor could shift the risk ratio to ",
                     round(adjusted.effect(), 2), "."))
    })
    
}





