

source("startup.R")


# message to display if non-null true value
nonnull.mess = 'Note: You are calculating a "non-null" E-value, i.e., an E-value for the minimum
                amount of unmeasured confounding needed to move the estimate and confidence interval
                to your specified true value rather than to the null value.'

# message to display for OLS
OLS.mess = 'Note: Using the standard deviation of the outcome yields a conservative approximation
of the standardized mean difference. For a non-conservative estimate, you could instead use the estimated residual standard deviation from your linear
regression model. Regardless, the reported E-value for the confidence interval treats the 
standard deviation as known, not estimated.'



navbarPage( "E-value calculator", id = "navbar",
          
            
            theme = shinytheme("flatly"),
            
            tabPanel( "Instructions",
                    
                      
                      mainPanel(
                        
                        
                        # Google Analytics
                        # this JS is from the "tracking code" available on the Google Analytics website
                                    tags$head( HTML( '<script async src="https://www.googletagmanager.com/gtag/js?id=UA-125815848-1"></script>
                        <script>
                                                       window.dataLayer = window.dataLayer || [];
                                                       function gtag(){dataLayer.push(arguments);}
                                                       gtag("js", new Date());

                                                       gtag("config", "UA-125815848-1");
                                                       </script>' ) ),
                        # END Google Analytics
                        
                          wellPanel(  HTML(paste("<b>Computing an E-value</b>",
                            
                                      'The tab "Compute an E-value" computes the E-value, defined as the minimum strength of association
                                      on the risk ratio scale that an unmeasured confounder would need to have with both the exposure
                                      and the outcome, conditional on the measured covariates, to fully explain away a specific
                                      exposure-outcome association. Note that for outcome types other than relative risks, assumptions
                                      are involved with the approximate conversions used. See citation (2) for details.',

                                      'Alternatively, you can consider the confounding strength capable of moving the observed
                                      association to any other value (e.g. attenuating the observed association to a true causal
                                      effect that is no longer scientifically important, or alternatively increasing a near-null
                                      observed association to a value that is of scientific importance). For this purpose, simply
                                      type a non-null effect size into the box "True causal effect to which to shift estimate"
                                      when computing the E-value.',
                                        
                                      "<b>Computing a bias factor</b>",                     
                            
                                      "Additionally, if you have substantive knowledge on the strength of the relationships
                                      between the unmeasured confounder(s) and the exposure and outcome, you can use these
                                      numbers to <a href='https://bias-factor.hmdc.harvard.edu/app/'>calculate the bias factor</a>.",
                                      sep="<br/><br/>"))
                                      
                                    
                                      ),

                          width=6
                    
                          
                      ),
                      
                      sidebarPanel(
                        HTML(paste("<b>Please use the following citations:</b>",
                                   
                                               "<a href='https://journals.lww.com/epidem/Citation/publishahead/Website_and_R_Package_for_Computing_E_Values.98679.aspx'>(1) Mathur MB, Ding P, Riddell CA, VanderWeele TJ. (2018). Website and R package
                                               for computing E-values. <i>Epidemiology</i>, 29(5), e45-e47.",
                                   
                                               "<a href='http://annals.org/aim/article-abstract/2643434/sensitivity-analysis-observational-research-introducing-e-value?doi=10.7326%2fM16-2607'>(2) VanderWeele TJ,
                                               & Ding P. (2017). Sensitivity analysis in observational research: introducing the
                                               E-value. <i>Annals of Internal Medicine</i>, 167(4), 268-274.</a>",
                                   
  
                                                "<b>Bug reports</b>",
                                   
                                                "Submit any bug reports to: <i>mmathur [AT] stanford [DOT] edu</i> or open
                                                    an issue on <a href='https://github.com/mayamathur/evalue/issues'>Github</a>.",
                                  
                                               sep="<br/><br/>") )

                             )
                      ),
                     
            #)
            

           tabPanel( "Compute an E-value",

                        mainPanel(
                            selectInput( "outcomeType", label = "Outcome type",
                                         choices = c( "Relative risk / rate ratio" = "RR", 
                                                      "Odds ratio (outcome prevalence <15%)" = "OR.rare",
                                                      "Odds ratio (outcome prevalence >15%)" = "OR.com",
                                                      "Hazard ratio (outcome prevalence <15%)" = "HR.rare",
                                                      "Hazard ratio (outcome prevalence >15%)" = "HR.com",
                                                      "Linear regression coefficient" = "OLS",
                                                      "Standardized mean difference (d)" = "MD", 
                                                      "Risk difference" = "RD" ) ),
                              
                            
                            # conditional panels that appear depending on selected outcome type
                   
                            conditionalPanel(
                                condition = "input.outcomeType == 'RR' ",
                                
                                numericInput('est.RR', 'Point estimate', NA, min = 1, max = 9),
                                numericInput('lo.RR', 'Confidence interval lower limit', NA, min = 1, max = 9),
                                numericInput('hi.RR', 'Confidence interval upper limit', NA, min = 1, max = 9),
                                numericInput('trueRR', 'True causal effect to which to shift estimate (default: null)', 1, min = 1, max = 9) 
                            ) ,
                            
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
            
                            conditionalPanel(
                                condition = "input.outcomeType == 'OLS' ",
                                numericInput('estOLS', 'Regression coefficient estimate', NA, min = 1, max = 9),
                                numericInput('seOLS', 'Standard error of coefficient', NA, min = 1, max = 9),
                                numericInput('sdOLS', 'Standard deviation of outcome', NA, min = 1, max = 9),
                                numericInput('deltaOLS', 'Contrast of interest in exposure', 1, min = 1, max = 9),
                                numericInput('trueOLS', 'True causal effect to which to shift estimate (on standard mean difference scale; default: null)', 0, min = 1, max = 9)
                            ),
                            
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
                            conditionalPanel( condition = "input.outcomeType == 'OLS' & input.trueOLS != 0", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'MD' & input.trueMD != 0", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'RD' & input.trueRD != 0", nonnull.mess),
                          
                            # conservatism message for OLS      
                            conditionalPanel( condition = "input.outcomeType == 'OLS'", OLS.mess),
                          
                            width = 6
                            
                    ),  # ends mainPanel

                     # panel for contour plot
                     sidebarPanel(

                      checkboxInput( 'makeplot', 'Show plot', FALSE ),

                      conditionalPanel( condition = "input.makeplot == true",
                                        plotlyOutput("curveOfExplainAway", width = "400px", height = "400px") ),

                      conditionalPanel( condition = "input.makeplot == true",
                                        HTML(paste("<br>Each point along the curve defines a joint relationship between the two sensitivity parameters that could potentially explain away the estimated effect.",
                                                   " If one of the two parameters is smaller than the E-value, the other must be larger, as defined by the plotted curve."))
                                         ),

                      width = 6

                       ) # end contour plot panel
           ),

           tabPanel("More resources",
                    
                    mainPanel(      HTML(paste( "<b>Developers</b>",
                                                
                                                
                                                "<br><br>This website was created by <a href='https://profiles.stanford.edu/maya-mathur'>Maya Mathur</a>,
                                            <a href='https://sites.google.com/site/pengdingpku/'>Peng Ding</a>, <a href='https://sph.berkeley.edu/corinne-riddell-phd'>Corinne Riddell</a>, and <a href='https://www.hsph.harvard.edu/tyler-vanderweele/tools-and-tutorials/'>Tyler VanderWeele</a>.",
                                                
                                                "<br><br><b>Other software</b>",
                                                
                                                "<br><br>You can alternatively compute E-values
                                        using the R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a> or 
                                       the Stata module <a href='https://ideas.repec.org/c/boc/bocode/s458592.html'>EVALUE</a>.",
                                                
                                                "<br><br><b>Additional references</b>",
                                                
                                                "<br><br>For more on the technical details and the interpretation of the E-value, see:",
                                                
                                                "<br><br><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4820664/'>(1) Ding P & VanderWeele TJ. (2017). Sensitivity analysis without assumptions. <i>Epidemiology</i>, 27(3): 368–377.</a>",
                                                
                                                "<br><br><a href='https://biostats.bepress.com/harvardbiostat/paper215/'>(2) VanderWeele TJ, Ding P, Mathur MB. (2019). Technical considerations in the use of the E-value. Harvard University Technical Report.",
                                                
                                                "<br><br><a href='https://annals.org/aim/article-abstract/2719984/correcting-misinterpretations-e-value'>(3) VanderWeele TJ, Mathur MB, Ding P. (2019). Correcting misinterpretations of the E-value. <i>Annals of Internal Medicine</i>, 170(2):131-132."
               
                                                
                    ) ) )
           )
           


)


# tags$footer(title="Your footer here", align = "right", style = "
# position:absolute;
#             bottom:0;
#             width:100%;
#             height:50px; /* Height of the footer */
#             color: white;
#             padding: 10px;
#             background-color: black;
#             z-index: 1000;"
# )









