


source("startup.R")



library(shiny)
library(plotly)

# message to display if non-null true value
nonnull.mess = 'Note: You are calculating a "non-null" E-value, i.e., an E-value for the minimum
                amount of unmeasured confounding needed to move the estimate and confidence interval
                to your specified true value rather than to the null value.'


navbarPage( "",
            tabPanel( "Instructions",
                      mainPanel(
    
                          wellPanel(  HTML(paste("This website computes the E-value, defined as the minimum strength of association on the risk ratio scale that an unmeasured confounder would need to have with both the treatment and the outcome, conditional on the measured covariates, to fully explain away a specific treatment-outcome association.",

                                        "Alternatively, you can consider the confounding strength capable of moving the observed association to any other value (e.g. attenuating the observed association to a true causal effect that is no longer scientifically important, or alternatively increasing a near-null observed association to a value that is of scientific importance). For this purpose, simply type a non-null effect size into the box 'True causal effect to which to shift estimate' when computing the E-value.",
                                        
                                                 "Note that for outcome types other than relative risks, assumptions are involved with the approximate conversions used. See first citation below for details.", sep="<br/><br/>")) ),
                          
                          wellPanel(  HTML(paste("Please use the following citation:",
                                                "(1) VanderWeele TJ, & Ding P. (2017). Sensitivity analysis in observational research: introducing the E-value. <i>Annals of Internal Medicine</i>, 167(4), 268-274.",
                                                sep="<br/><br/>")) ),
                          
                          # INSERT CITATION FOR SOFTWARE LETTER WHEN PUBLISHED

                          wellPanel(  HTML(paste("Submit any bug reports to: <i>mmathur [AT] stanford [DOT] edu</i>.", sep="<br/>")) )
                          #wellPanel(  HTML(paste("hello", "world", sep="<br/>")) )
                      )
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
 
                           wellPanel(  span( textOutput("result.text") ) ), 
                            
                            # warnings if computing non-null E-value
                            # note: because the condition is in Javascript, have to use period instead of dollar sign to 
                            #  access arguments, so CANNOT have period in the variable names (e.g., "trueRR" doesn't work!)
                            conditionalPanel( condition = "input.outcomeType == 'RR' & input.trueRR != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'OR.rare' & input.trueORrare != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'OR.com' & input.trueORcom != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'HR.rare' & input.trueHRrare != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'HR.com' & input.trueHRcom != 1", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'MD' & input.trueMD != 0", nonnull.mess),
                            conditionalPanel( condition = "input.outcomeType == 'RD' & input.trueRD != 0", nonnull.mess),
                            hr(),
                            plotlyOutput("curveOfExplainAway", width = "400px", height = "400px")
                    )
           ),

           tabPanel("Compute a bias factor",
                    HTML("<br>If you have substantive knowledge on the strength of the relationships between the unmeasured confounder",
                         " and the exposure and outcome, you can use these numbers to calculate the bias factor. Per VanderWeele and Ding (2017),",
                         " let RR<sub>UD</sub> denote the maximum risk ratio for the outcome comparing any two categories of the unmeasured",
                         " confounders, within either treatment group, conditional on the observed covariates. Let RR<sub>EU</sub> denote",
                         " the maximum risk ratio for any specific level of the unmeasured confounders comparing those with and without treatment, with",
                         " adjustment already made for the measured covariates.<br><br>"),
                    fluidRow(
                        column(4, 
                               HTML("<b>Specify the effect estimate (relative risk scale):</b>"),
                               numericInput(inputId = "effect.estimate.page2", label = NULL, value = 3.9, min = 1.02, max = 999, width = '80px')),
                        column(4, 
                               HTML("<b>Specify RR<sub>EU</sub>:</b>"),
                               numericInput(inputId = "RR_EU", label = NULL, value = 2, min = 1.02, max = 999, width = '80px')),
                        column(4, 
                               HTML("<b>Specify RR<sub>UD</sub>:</b>"),
                               numericInput(inputId = "RR_UD", label = NULL, value = 4, min = 1.02, max = 999, width = '80px'))
                    ),
                    hr(),
                    uiOutput("Bias_Factor")
                    )

)








