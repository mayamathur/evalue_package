

source("startup.R")


# message to display if non-null true value
nonnull.mess = 'Note: You are calculating a "non-null" E-value, i.e., an E-value for the minimum
                amount of unmeasured confounding needed to move the estimate and confidence interval
                to your specified true value rather than to the null value.'


navbarPage( "",
            
            tabPanel( "Instructions",
                      mainPanel(
                        
                        wellPanel(  HTML(paste("<b>Please use the following citations:</b>",
                                               "(1) Mathur MB, Ding P, Riddell CA, VanderWeele TJ. (2018). Website and R package for computing E-values. <i>Epidemiology</i>, in press.",
                                               "(2) VanderWeele TJ, & Ding P. (2017). Sensitivity analysis in observational research: introducing the E-value. <i>Annals of Internal Medicine</i>, 167(4), 268-274.",
                                               sep="<br/><br/>")) ),
    
                          wellPanel(  HTML(paste("<b>Computing an E-value</b>",
                            
                                      "The tab 'Compute an E-value' computes the E-value, defined as the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the exposure and the outcome, conditional on the measured covariates, to fully explain away a specific exposure-outcome association. Note that for outcome types other than relative risks, assumptions are involved with the approximate conversions used. See first citation below for details.",

                                      "Alternatively, you can consider the confounding strength capable of moving the observed association to any other value (e.g. attenuating the observed association to a true causal effect that is no longer scientifically important, or alternatively increasing a near-null observed association to a value that is of scientific importance). For this purpose, simply type a non-null effect size into the box 'True causal effect to which to shift estimate' when computing the E-value.",
                                        
                                      "<b>Computing a bias factor</b>",                     
                            
                                      "Additionally, if you have substantive knowledge on the strength of the relationships between the unmeasured confounder(s) and the exposure and outcome, you can use these numbers to <a href='https://mmathur.shinyapps.io/bias_factor/'>calculate the bias factor</a>.", sep="<br/><br/>")) ),
                        
                          
                          wellPanel(  HTML(paste("Submit any bug reports to: <i>mmathur [AT] stanford [DOT] edu</i> or open
                                                 an issue on <a href='https://github.com/mayamathur/evalue/issues'>Github</a>.", sep="<br/>")) ),
                          
                          width=6
                          
                      )
                      
                      # EXAMPLE OF JAVASCRIPT TROUBLE
                      # sidebarPanel(
                      #     
                      #     # BOOKMARK: MAKE REPRODUCIBLE EXAMPLE WITH THIS
                      #     # OR TRY THIS: https://stackoverflow.com/questions/47844015/how-to-trigger-js-script-when-shiny-app-loads
                      #     # only runs once, like the ads
                      #     # HTML('<script type="text/javascript"> alert("Hello world"); </script>')
                      #     
                      #     # # runs every time
                      #     HTML('<b> PLACEHOLDER FOR AMAZON AD </b>')
                      #     
                      #     
                      #      , width=6 )
                         
                     
                      # AMAZON ADS
                      
                   #    sidebarPanel(
                   #      
                   #      HTML('<script type="text/javascript">
                   # amzn_assoc_placement = "adunit0";
                   # amzn_assoc_search_bar = "true";
                   # amzn_assoc_tracking_id = "evalue-20";
                   # amzn_assoc_ad_mode = "manual";
                   # amzn_assoc_ad_type = "smart";
                   # amzn_assoc_marketplace = "amazon";
                   # amzn_assoc_region = "US";
                   # amzn_assoc_title = "Amazon ads supporting our server costs";
                   # amzn_assoc_linkid = "cd775ac2352c26998b1123d681b4a179";
                   # amzn_assoc_asins = "0062279319,0142196754,1610397673,1472930339,0691147825,0312313926,0008276099,0465053947";
                   # </script>
                   # <script src="//z-na.amazon-adsystem.com/widgets/onejs?MarketPlace=US"></script>'),
                   #      
                   #      width = 6 )
                      
            ),
            

           tabPanel( "Compute an E-value",

                        mainPanel(
                            selectInput( "outcomeType", label = "Outcome type",
                                         choices = c( "Relative risk / rate ratio" = "RR", 
                                                      "Odds ratio (outcome prevalence <15%)" = "OR.rare",
                                                      "Odds ratio (outcome prevalence >15%)" = "OR.com",
                                                      "Hazard ratio (outcome prevalence <15%)" = "HR.rare",
                                                      "Hazard ratio (outcome prevalence >15%)" = "HR.com",
                                                     # "Linear regression coefficient" = "RG",
                                                      "Standardized mean difference (d)" = "MD", 
                                                      "Risk difference" = "RD" ) ),
                              
                            
                            # conditional panels that appear depending on selected outcome type
                   
                            conditionalPanel(
                                condition = "input.outcomeType == 'RR' ",
                                
                                numericInput('est.RR', 'Point estimate', NA, min = 1, max = 9),
                                numericInput('lo.RR', 'Confidence interval lower limit', NA, min = 1, max = 9),
                                numericInput('hi.RR', 'Confidence interval upper limit', NA, min = 1, max = 9),
                                numericInput('trueRR', 'True causal effect to which to shift estimate (default: null)', 1, min = 1, max = 9)
                            ),
                            
                            conditionalPanel(
                                
                                condition = "input.outcomeType == 'OR.rare' ",
                                
                                numericInput('est.OR.rare', 'Point estimate', NA, min = 1, max = 9),
                                numericInput('lo.OR.rare', 'Confidence interval lower limit', NA, min = 1, max = 9),
                                numericInput('hi.OR.rare', 'Confidence interval upper limit', NA, min = 1, max = 9),
                                numericInput('trueORrare', 'True causal effect to which to shift estimate (default: null)', 1, min = 1, max = 9)
                            ),
                            
                            conditionalPanel(
                                condition = "input.outcomeType == 'OR.com' ",
                                
                                numericInput('est.OR.com', 'Point estimate', NA, min = 1, max = 9),
                                numericInput('lo.OR.com', 'Confidence interval lower limit', NA, min = 1, max = 9),
                                numericInput('hi.OR.com', 'Confidence interval upper limit', NA, min = 1, max = 9),
                                numericInput('trueORcom', 'True causal effect to which to shift estimate (default: null)', 1, min = 1, max = 9)
                            ),
                            
                            conditionalPanel(
                                condition = "input.outcomeType == 'HR.rare' ",
                                
                                numericInput('est.HR.rare', 'Point estimate', NA, min = 1, max = 9),
                                numericInput('lo.HR.rare', 'Confidence interval lower limit', NA, min = 1, max = 9),
                                numericInput('hi.HR.rare', 'Confidence interval upper limit', NA, min = 1, max = 9),
                                numericInput('trueHRrare', 'True causal effect to which to shift estimate (default: null)', 1, min = 1, max = 9)
                            ),
                            
                            conditionalPanel(
                                condition = "input.outcomeType == 'HR.com' ",
                                
                                numericInput('est.HR.com', 'Point estimate', NA, min = 1, max = 9),
                                numericInput('lo.HR.com', 'Confidence interval lower limit', NA, min = 1, max = 9),
                                numericInput('hi.HR.com', 'Confidence interval upper limit', NA, min = 1, max = 9),
                                numericInput('trueHRcom', 'True causal effect to which to shift estimate (default: null)', 1, min = 1, max = 9)
                            ),
            
#                             conditionalPanel(
#                                 condition = "input.outcomeType == 'RG' ",
#                                 
#                                 numericInput('beta.RG', 'Regression coefficient estimate', NA, min = 1, max = 9),
#                                 numericInput('se.RG', 'Standard error of coefficient', NA, min = 1, max = 9),
#                                 numericInput('pval.RG', 'P-value of regression coefficient', NA, min = 1, max = 9),
#                                 numericInput('n.RG', 'Total sample size in regression', NA, min = 1, max = 9),
#                                 numericInput('true.RG', 'True causal effect to which to shift estimate (on standard mean difference scale; default: null)', 0, min = 1, max = 9)
#                             ),
                            
                            conditionalPanel(
                                condition = "input.outcomeType == 'MD' ",
                                
                                numericInput('est.MD', 'Point estimate', 0, min = 1, max = 9),
                                numericInput('se.MD', 'Standard error', NA, min = 1, max = 9),
                                numericInput('trueMD', 'True causal effect to which to shift estimate (default: null)', 0, min = 1, max = 9)
                            ),
                            
                            conditionalPanel(
                                condition = "input.outcomeType == 'RD' ",
                                
                                numericInput('n11', 'Number of exposed, diseased individuals', NA, min = 1, max = 9),
                                numericInput('n10', 'Number of exposed, non-diseased individuals', NA, min = 1, max = 9),
                                numericInput('n01', 'Number of unexposed, diseased individuals', NA, min = 1, max = 9),
                                numericInput('n00', 'Number of unexposed, non-diseased individuals', NA, min = 1, max = 9),
                                numericInput('alpha', 'Alpha level for confidence interval', 0.05, min = 1, max = 9),
                                numericInput('grid', 'Spacing for grid search of E-value', 0.0001, min = 1, max = 9),
                                numericInput('trueRD', 'True causal effect to which to shift estimate (default: null)', 0, min = 1, max = 9)
                            ),

                          # display results
                           wellPanel(  span( textOutput("result.text") ) ), 
                            
                            # warnings if computing non-null E-value
                            # note: because the condition is in Javascript, have to use period instead of dollar sign to 
                            #  access arguments, so CANNOT have period in the variable names (e.g., "true.RR" doesn't work!)
                            conditionalPanel( condition = "input.outcomeType == 'RR' & input.trueRR != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'OR.rare' & input.trueORrare != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'OR.com' & input.trueORcom != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'HR.rare' & input.trueHRrare != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'HR.com' & input.trueHRcom != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'MD' & input.trueMD != 0", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'RD' & input.trueRD != 0", nonnull.mess),


                            #wellPanel( HTML('<b> PLACEHOLDER FOR AMAZON AD </b>') ),
                  
                            
                            width = 6
                            
                    ),  # ends mainPanel

                     # panel for contour plot
                     sidebarPanel(

                      checkboxInput( 'makeplot', 'Show plot', FALSE ),

                      conditionalPanel( condition = "input.makeplot == true",
                                        plotlyOutput("curveOfExplainAway", width = "400px", height = "400px") ),

                      conditionalPanel( condition = "input.makeplot == true",
                                        HTML(paste("<br><b>What is the E-value?</b><br>The E-value is the minimum strength required for both the exposure-confounder and exposure-disease relationships that is required to 'explain away' the estimated relationship between exposure and disease.",
                                                   " If one of the two parameters is smaller than the E-value, the other must be larger, as defined by the curve below.",
                                                   " All points along the curve define joint relationships that explain away the estimated effect, including points to the right of the curve."))
                                         ),

                      width = 6

                       ) # end contour plot panel
           )
)








