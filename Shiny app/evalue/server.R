source("startup.R")

function(input, output, session) {
  

    evals <- reactive({
        if ( input$outcomeType == "RR" ) {
            
            if ( is.na( input$est.RR )) return("Enter your point estimate")
            if ( is.na( input$trueRR )) return("Enter a true value")
            
            
            evals = round( evalues.RR( est = input$est.RR, lo = input$lo.RR, hi = input$hi.RR, true = input$trueRR )[2,], 2 )
            
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
    bias.factor <- reactive({
        
        bf <- input$est.RR/input$trueRR
        if( input$outcomeType == "OR.rare" ){
          bf <- input$est.OR.rare/input$trueORrare
        }else if( input$outcomeType == "OR.com" ){
          bf <- sqrt(input$est.OR.com)/sqrt(input$trueORcom)
        }else if( input$outcomeType == "HR.rare" ){
          bf <- input$est.HR.rare/input$trueHRrare
        }else if ( input$outcomeType == "HR.com" ){
          bf <- (  (( 1 - 0.5^sqrt(input$est.HR.com) )/( 1 - 0.5^sqrt(1/input$est.HR.com) ))  )/(  (( 1 - 0.5^sqrt(input$trueHRcom) )/( 1 - 0.5^sqrt(1/input$trueHRcom) ))  )
        }else if ( input$outcomeType == "MD" ){
          bf <- exp(0.91*input$est.MD)/exp(0.91*input$trueMD)
        }else if ( input$outcomeType == "RD" ){
             N = input$n10 + input$n11 + input$n01 + input$n00
             N1 = input$n10 + input$n11
             N0 = input$n00 + input$n01
             f = N1/N
             p0 = input$n01/N0
             p1 = input$n11/N1
             
             # Ding 2016, page 376 expression
             bf <- (1/(2*p0*f) )*( sqrt( (input$trueRD + p0*(1-f) - p1*f)^2 + 
                                                   4*p1*p0*f*(1-f) ) -
                                             (input$trueRD + p0*(1-f) - p1*f) ) 
        }
        
        return( bf )
    })
  
    
    
    output$curveOfExplainAway <- renderPlotly({
        
      # MM: do not attempt to make plot unless we have the point estimate
        if( !is.na( bias.factor() ) ) {
      
        rr.ud <- function(rr.eu) {
            
            if(bias.factor() > 1){
              
                ( bias.factor()*(1 - rr.eu) )/( bias.factor() - rr.eu )
                
            }else{
                
                ( (1/bias.factor())*(1 - rr.eu) )/( (1/bias.factor()) - rr.eu )
            }
        }
        
        g <- ggplotly(
            ggplot(data.frame(rr.eu = c(0, 20)), aes(rr.eu)) + 
                stat_function(fun = rr.ud) + 
                scale_y_continuous(limits = c(1, evals()[1]*3)) + 
                scale_x_continuous(limits = c(1, evals()[1]*3)) +
                xlab("Risk ratio for exposure-confounder relationship") + ylab("Risk ratio for confounder-outcome relationship") + 
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
        
        return(g)
        
        } else {
          # if we don't have point estimate, 
          # then show blank placeholder graph
          df = data.frame()
          g = ggplotly( ggplot(df) +
                          geom_point() +
                          xlim(0, 10) +
                          ylim(0, 10) +
                          theme_minimal() +
                          xlab("Risk ratio for exposure-confounder relationship") + ylab("Risk ratio for confounder-outcome relationship") + 
                          annotate("text", x = 5, y = 5, label = "(Enter your point estimate)") )
          return(g)
        }
    }) 

}





