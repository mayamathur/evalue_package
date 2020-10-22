

source("startup.R")


# message to display if non-null true value
nonnull.mess = 'Note: You are calculating a "non-null" E-value, i.e., an E-value for the minimum
                amount of unmeasured confounding needed to move the estimate and confidence interval
                to your specified true value rather than to the null value.'


fluidPage( title = "E-value calculator",
            
                      mainPanel(
                        
                          wellPanel(  HTML(paste("<b>We've moved!</b>",
                        
                                      "The E-value calculator <a href='https://evalue.hmdc.harvard.edu'>has moved</a>.",
                                      sep="<br/><br/>")) ),

                          width=6
                          
                      )
)
                      
                     








