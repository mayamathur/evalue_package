source("startup.R")

### CHANGE THIS ###
# message to display if non-null true value
nonnull.mess = 'Note: You are calculating a "non-null" E-value, i.e., an E-value for the minimum
amount of unmeasured confounding needed to move the estimate and confidence interval
to your specified true value rather than to the null value.'

### CHANGE THIS ###
# message to display for OLS
OLS.mess = 'Note: Using the standard deviation of the outcome yields a conservative approximation
of the standardized mean difference. For a non-conservative estimate, you could instead use the estimated residual standard deviation from your linear
regression model. Regardless, the reported E-value for the confidence interval treats the 
standard deviation as known, not estimated.'


navbarPage( "Sensitivity analysis for unmeasured confounding in meta-analyses", id = "navbar",
            
            
            theme = shinytheme("flatly"),
            
            tabPanel("Sensitivity of the point estimate",
                     wellPanel(  HTML(paste('This website implements the sensitivity analyses described in <a href="https://www.tandfonline.com/doi/full/10.1080/01621459.2018.1529598">Mathur & VanderWeele (2020a)</a>.
                                            For more information on how to interpret these sensitivity analyses, see that paper and other materials listed in the "More Resources" tab.',
                                            
                                            
                                            "<b>Sensitivity analysis for the pooled point estimate</b>",
                                            
                                            'This tab computes the E-value for the pooled point estimate of a meta-analysis (Section 7.2 of reference XX). This E-value represents the average severity of confounding in the meta-analyzed studies (i.e., the minimum strength of association
                                            on the risk ratio scale that unmeasured confounder(s) would need to have with both the exposure
                                            and the outcome, conditional on the measured covariates), to fully explain away the observed meta-analytic point estimate. Note that for outcome types other than relative risks, assumptions
                                            are involved with the approximate conversions used. See citation (XX) for details.',
                                            
                                            'Alternatively, you can consider the average confounding strength capable of moving the observed
                                            point estimate to any other value (e.g. attenuating the observed association to a true causal
                                            effect that is no longer scientifically important, or alternatively increasing a near-null
                                            observed association to a value that is of scientific importance). For this purpose, simply
                                            type a non-null effect size into the box "True causal effect to which to shift estimate"
                                            when computing the meta-analytic E-value.',
                                            
                                            "<b>Example of how to interpret the results</b>",
                                            
                                            "For example, if your meta-analytic point estimate on the relative risk scale is 1.5 (95% confidence interval: [1.4, 1.5]), you will obtain an E-value for the point estimate of 2.37 and an E-value for the lower confidence interval limit of 2.15. This means that if, hypothetically, the meta-analyzed studies were subject to confounding such that, on average across the studies, there were unmeasured confounder(s) that were associated with the studies' exposures and outcomes by relative risks of at least 2.37 each, this amount of average confounding could potentially explain away the point estimate of 1.5 (i.e., to have the true causal effect be a relative risk of 1), but weaker average confounding could not. Similarly, if this strength of average confounding were at 2.15 across studies, this amount of confounding could potentially shift the confidence interval to include the null, but weaker average confounding could not.",
                                            
                                            "<b>A caveat about the pooled point estimate</b>",
                                            
                                            'Note that this tab of the website conducts sensitivity analyses that describe evidence strength only in terms of the pooled point estimate, a measure that does not fully characterize effect heterogeneity in a meta-analysis. For example, consider two meta-analyses with the pooled point estimate of relative risk = 1.1. The first, Meta-Analysis A, has very little heterogeneity, such that all true population effects are very close to 1.1. In contrast, despite having the same point estimate, Meta-Analysis B could have substantial heterogeneity, such that a large proportion of the true population effects are large (e.g., >1.5). Thus, Meta-Analysis B provides stronger support for the presence of meaningfully large effects than does Meta-Analysis A, and furthermore Meta-Analysis B might also suggest that a non-negligible proportion of the effects are actually preventive rather than causative (i.e., with relative risks less than 1). For this reason, meta-analyses that have some heterogeneity should generally report not only the point estimate, but also the estimated percentage of meaningfully strong population effects (REF), and sensitivity analyses should consider this quantity as well (which you can do using the tab "Sensitivity of the percentage of meaningfully strong effects" in this website.',
                                            
                                            sep="<br/><br/>"))
                                 
                                 
                     ),
                     
                     width=6,
                     
                     mainPanel(
                       selectInput( "outcomeType", label = "Outcome type",
                                    choices = c( "Relative risk / rate ratio" = "RR", 
                                                 "Odds ratio (outcome prevalence <15%)" = "OR.rare",
                                                 "Odds ratio (outcome prevalence >15%)" = "OR.com",
                                                 "Hazard ratio (outcome prevalence <15%)" = "HR.rare",
                                                 "Hazard ratio (outcome prevalence >15%)" = "HR.com",
                                                 "Standardized mean difference (d)" = "MD", 
                                                 "Risk difference" = "RD",
                                                 "Linear regression coefficient" = "OLS" ) ),
                       
                       
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
            
            tabPanel("Sensitivity of the percentage of meaningfully strong effects",
                     shinyjs::useShinyjs(),
                     wellPanel(  HTML(paste(
                       'This website implements the sensitivity analyses described in <a href="https://www.tandfonline.com/doi/full/10.1080/01621459.2018.1529598">Mathur & VanderWeele (2020a)</a>.
                       For more information on how to interpret these sensitivity analyses, see that paper and other materials listed in the "More Resources" tab.
                       
                       <br><br>In this tab, you can choose a fixed set of sensitivity parameters (the mean and variance of the bias factor distribution)
                       and estimate (1) the proportion of meaningfully strong causal effect sizes
                       with different amounts of confounding bias, (2) the minimum bias factor required to "explain away" the effect;
                       and (3) the minimum confounding strength required to "explain away" the effect. This tab also plots the estimated observed (confounded)
                       and true causal (unconfounded) effect distributions under the specified bias parameters.'))
                     ),
                     width = 6,
                     tabsetPanel(
                       tabPanel("Robust estimation",
                                fluidRow(
                                  tags$style(type = "text/css",
                                             "label { font-size: 12px; }"
                                  ),
                                  ### hidden method input used in server.R
                                  column(width=8, shinyjs::hidden(selectInput('calibrated_method', 'Method (calibrated or parametric)', choices = c('calibrated'), selected = 'calibrated'))
                                  ),
                                  
                                  fluidRow(
                                    shinydashboard::box(width=6,
                                                        title= h4(strong("Estimates from confounded meta-analysis")),
                                                        column(width=10,
                                                               fileInput('calibrated_uploaddat', label = 'Upload csv data', accept=c('text/csv',
                                                                                                                                     'text/comma-separated-values,text/plain',
                                                                                                                                     '.csv')) %>%
                                                                 shinyInput_label_embed(
                                                                   shiny_iconlink() %>%
                                                                     bs_embed_popover(title = 'data file -- need to add more here')),
                                                               column(width=5,
                                                                      textInput('calibrated_yi.name', "Name of variable in data containing studies' point estimates (log-RR scale)", placeholder = 'yi.name') %>%
                                                                        shinyInput_label_embed(
                                                                          shiny_iconlink() %>%
                                                                            bs_embed_popover(title = 'For calibrated method, please supply variable name')),
                                                                      textInput('calibrated_vi.name', "Name of variable in data containing studies' variance estimates", placeholder = 'vi.name') %>%
                                                                        shinyInput_label_embed(
                                                                          shiny_iconlink() %>%
                                                                            bs_embed_popover(title = 'For calibrated method, please supply variable name'))
                                                               ),
                                                               column(width=5,
                                                                      selectInput('calibrated_tail', 'Tail', choices = c('above', 'below'), selectize = FALSE, size = 2, selected = 'above') %>%
                                                                        shinyInput_label_embed(
                                                                          shiny_iconlink() %>%
                                                                            bs_embed_popover(title = 'above for the proportion of effects above q; below for the proportion of effects below q')),
                                                                      numericInput('calibrated_R', label = 'Number of bootstrap iterates', 1000) %>%
                                                                        shinyInput_label_embed(
                                                                          shiny_iconlink() %>%
                                                                            bs_embed_popover(title = 'Number of bootstrap iterates'))
                                                               )
                                                        ),
                                                        column(width=10,
                                                               actionButton(inputId = 'calibrated_calculate', label='Analyze')
                                                        )
                                    ),
                                    shinydashboard::box(width=6,
                                                        title= h4(strong("Specify sensitivity parameters and thresholds")),
                                                        column(width=10,
                                                               selectInput('calibrated_scale', 'Scale (RR or log-RR)', choices = c('RR', 'Log-RR'), selected = 'RR') %>%
                                                                        shinyInput_label_embed(
                                                                          shiny_iconlink() %>%
                                                                            bs_embed_popover(title = 'The scale (relative risk [RR] or log-relative risk [log-RR] on which you will input the meta-analytic pooled estimate, threshold, and mean bias factor below.')
                                                                          ),
                                                               numericInput('calibrated_muB', 'Bias factor in each study (on scale you specified above)', 1.5, min = 0, max = Inf, step = 0.01) %>%
                                                                 shinyInput_label_embed(
                                                                   shiny_iconlink() %>%
                                                                     bs_embed_popover(title = 'Used to adjust proportion by the provided bias factor')),
                                                               numericInput('calibrated_q', 'Threshold (q) for meaningfully strong effect size \n(on scale you specified above)', 0.8, min = 0, max = Inf, step = 0.01) %>%
                                                                 shinyInput_label_embed(
                                                                   shiny_iconlink() %>%
                                                                     bs_embed_popover(title = 'Effect size that represents the minimum threshold for a meaningfully strong effect size')),
                                                               numericInput('calibrated_r', 'Proportion below which strong effects are to be reduced (r)', 0.1, min = 0, max = 1, step = 0.1) %>%
                                                                 shinyInput_label_embed(
                                                                   shiny_iconlink() %>%
                                                                     bs_embed_popover(title = 'For the second two metrics, the value to which the proportion of meaningfully strong effects is to be reduced'))
                                                        )
                                    )
                                  ),
                                  
                                  ### results text ###
                                  wellPanel( textOutput("calibrated_results_prop"), span( textOutput("calibrated_text1") ), shiny_iconlink() %>%
                                               bs_embed_popover(title = "MAYA TO HELP INTEREPRET OUTPUT")),
                                  wellPanel( textOutput("calibrated_results_minbias"), span( textOutput("calibrated_text2") ), shiny_iconlink() %>%
                                               bs_embed_popover(title = "MAYA TO HELP INTEREPRET OUTPUT")),
                                  wellPanel( textOutput("calibrated_results_minconf"), span( textOutput("calibrated_text3") ), shiny_iconlink() %>%
                                               bs_embed_popover(title = "MAYA TO HELP INTEREPRET OUTPUT")),
                                  mainPanel(
                                    span( htmlOutput("calibrated_cm_messages"), style="color:red"), width = 8
                                  ),
                                  
                                  ### used for plot only:
                                  shinydashboard::box(width=6,
                                                      title=h4(strong("Range of bias factors to search")),
                                                      numericInput('calibrated_Bmin', 'Lower limit of bias factor (Bmin)', 1, min=0, max=Inf, step=0.1) %>%
                                                        shinyInput_label_embed(
                                                          shiny_iconlink() %>%
                                                            bs_embed_popover(title = 'used for plot only')),
                                                      numericInput('calibrated_Bmax', 'Upper limit of bias factor (Bmax)', 4, min=0, max=Inf, step=0.1) %>%
                                                        shinyInput_label_embed(
                                                          shiny_iconlink() %>%
                                                            bs_embed_popover(title = 'used for plot only')),
                                                      actionButton(inputId = 'calibrated_plot', label='Generate plot')
                                  ),
                                  
                                  mainPanel(
                                    plotOutput('calibrated_plot1')
                                  ),
                                  ### plot warnings:
                                  mainPanel(
                                    span( htmlOutput("calibrated_sens_plot_messages"), style="color:red"), width = 8
                                  )
                                ) 
                       ), ### closes tabPanel "Calibrated"
                       
                       tabPanel("Parametric estimation (allows heterogeneous bias)",
                                fluidRow(
                                  tags$style(type = "text/css",
                                             "label { font-size: 12px; }"
                                  ),
                                  ### hidden method input used in server.R
                                  column(width=8, shinyjs::hidden(selectInput('parametric_method', 'Method (calibrated or parametric)', choices = c('parametric'), selected = 'parametric'))
                                  ),
                                  shinydashboard::box(width=5,
                                                      title= h4(strong("Estimates from confounded meta-analysis")),
                                                      column(width=6,
                                                             selectInput('parametric_scale', 'Scale (RR or log-RR)', choices = c('RR', 'Log-RR'), selected = 'RR') %>%
                                                                      shinyInput_label_embed(
                                                                        shiny_iconlink() %>%
                                                                          bs_embed_popover(title = 'The scale (relative risk [RR] or log-relative risk [log-RR] on which you will input the meta-analytic pooled estimate, threshold, and mean bias factor below.'))
                                                             ,
                                                             numericInput('parametric_yr', 'Pooled effect size', 1.2, min = 0, max = Inf, step = 0.1) %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = 'The usual estimate of the average effect size in the meta-analysis of the potentially confounded studies, prior to any correction for unmeasured confounding')),
                                                             numericInput('parametric_se_yr', 'Estimated standard error of pooled effect (optional)', 0.01, min = 0, max = Inf, step = 0.01) %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = 'The estimated standard error of the pooled point estimate from confounded meta-analysis. Since the meta-analysis should be conducted with the point estimates on the log scale, you should input the standard error as it is reported by your meta-analysis software without taking the log again. If not provided, you will not get confidence intervals for the sensitivity analyses.')),
                                                             numericInput('parametric_t2', paste0('Estimated heterogeneity (', '\u03c4\u00b2', ')'), 0.10, min = 0, max = Inf, step = 0.1) %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = paste0('The estimated heterogeneity (', '\u03c4\u00b2', ') from the confounded meta-analysis. Since the meta-analysis should be conducted with the point estimates on the log scale, you should input ', '\u03c4\u00b2', ' as it is reported by your meta-analysis software without taking the log again.'))),
                                                             numericInput('parametric_prop_t2', paste0('Proportion of heterogeneity due to variations in confounding bias ', '\u03c4\u00b2'), .35, min = 0, max = 1, step = .1) %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = paste0('The proportion of ', '\u03c4\u00b2', ' from confounded meta-analysis. If not provided, you will not get confidence intervals for the sensitivity analyses.')))
                                                      )
                                  ),
                                  shinydashboard::box(width=7,
                                                      title= h4(strong("Specify sensitivity parameters and thresholds")),
                                                      column(width=6,
                                                             numericInput('parametric_muB', 'Mean bias factor across studies (on scale you specified above)', 1.5, min = 0, max = Inf, step = 0.01) %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = 'Mean bias factor on the chosen scale (RR or log) across studies. To estimate the proportion of effects stronger than q without correction for unmeasured confounding, set to 1.')),
                                                             numericInput('parametric_sigB', 'Standard deviation of log bias factors', 0, min = 0, max = Inf, step = 0.1) %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = 'Standard deviation of log bias factor across studies. To estimate the proportion of effects stronger than q without correction for unmeasured confounding, set to 0.')),
                                                             
                                                             numericInput('parametric_r', 'Proportion below which strong effects are to be reduced (r)', 0.20, min = 0, max = 1, step = 0.1) %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = 'For the second two metrics, the value to which the proportion of meaningfully strong effects is to be reduced')),
                                                             numericInput('parametric_q', 'Threshold (q) for meaningfully strong effect size (on scale you specified above)', 1.1, min = 0, max = Inf, step = 0.01) %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = 'Effect size that represents the minimum threshold for a meaningfully strong effect
                                                                                    size (on scale you specified above)')),
                                                             selectInput('parametric_tail', 'Tail', choices = c('above', 'below'), selectize = FALSE, size = 2, selected = 'above') %>%
                                                               shinyInput_label_embed(
                                                                 shiny_iconlink() %>%
                                                                   bs_embed_popover(title = 'above for the proportion of effects above q; below for the proportion of effects below q')),
                                                             actionButton(inputId = 'parametric_calculate', label='Analyze')
                                                                   )
                                                      
                                  )
                                ),
                                
                                ### results text ###
                                wellPanel( textOutput("parametric_results_prop"), span( textOutput("parametric_text1") ), shiny_iconlink() %>%
                                             bs_embed_popover(title = "MAYA TO HELP INTEREPRET OUTPUT"),
                                           span( htmlOutput("parametric_phatwarn"), style="color:red")),
                                wellPanel( textOutput("parametric_results_minbias"), span( textOutput("parametric_text2") ), shiny_iconlink() %>%
                                             bs_embed_popover(title = "MAYA TO HELP INTEREPRET OUTPUT")),
                                wellPanel( textOutput("parametric_results_minconf"), span( textOutput("parametric_text3") ), shiny_iconlink() %>%
                                             bs_embed_popover(title = "MAYA TO HELP INTEREPRET OUTPUT")),
                                mainPanel(
                                  span( htmlOutput("parametric_cm_messages"), style="color:red"), width = 8
                                ),
                                
                                shinydashboard::box(width=6,
                                                    title=h4(strong("Bias factor range to include in plot")),
                                                    numericInput('parametric_Bmin', 'Lower limit of bias factor (Bmin)', 1, min=0.1, max=Inf, step=0.1) %>%
                                                      shinyInput_label_embed(
                                                        shiny_iconlink() %>%
                                                          bs_embed_popover(title = 'used for plot only')),
                                                    numericInput('parametric_Bmax', 'Upper limit of bias factor (Bmax)', 4, min=0, max=Inf, step=0.1) %>%
                                                      shinyInput_label_embed(
                                                        shiny_iconlink() %>%
                                                          bs_embed_popover(title = 'used for plot only')),
                                                    actionButton(inputId = 'parametric_plot', label='Generate plot')
                                ),
                                
                                
                                mainPanel(
                                  plotOutput('parametric_plot1')
                                ),
                                ### plot warnings:
                                ## jl: all warnings from sens_plot should be output with the below now:
                                mainPanel(
                                  span( htmlOutput("parametric_sens_plot_messages"), style="color:red"), width = 8
                                )
                       ) ### closes tabPanel "Parametric"
                     ) ### closes tabsetPanel
            ), ### closes tabPanel "Fixed sensitivity parameters"
            
            tabPanel("More resources",
                     
                     mainPanel(      HTML(paste( 
                       
                       
                       "<b>More resources for these sensitivity analyses</b>",
                       
                       "<br><br>In addition to using this website, you can alternatively conduct these sensitivity analyses
                       using the function <code>confounded_meta</code> in the R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a> (<a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6066405/'>Mathur et al., 2018</a>).",
                       
                       "<br><br>For more information on the interpretation of these sensitivity analyses and guidance on choosing the sensitivity parameters, see <a href='https://www.tandfonline.com/doi/full/10.1080/01621459.2018.1529598'>Mathur & VanderWeele (2020a)</a>,
                       and for a review of methods to choose a threshold representing a meaningfully strong effect size, see the Supplement of see <a href='https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8057'>Mathur & VanderWeele (2019).</a>
                       For robust versions of these methods that work even when the estimated proportion is <0.15 or >0.85 and that do not assume the true causal effects are normally distributed, see <a href='https://journals.lww.com/epidem/Fulltext/2020/05000/Robust_Metrics_and_Sensitivity_Analyses_for.7.aspx?casa_token=PELPmhG6P3wAAAAA:D2bYC3kXCtRmncY-ELNt4I8ub1ZUhwTQjsFq8vh05h_EhV4kOJQuR3L97TsSLtun4zQ26Ys26ayF3aleMKj-93Q'>Mathur & VanderWeele (2020b).</a></a>",
                       
                       
                       "<br><br><b>More resources for other biases and study designs</b>",
                       
                       "<br><br>Similar methods and tools are also available to conduct analogous sensitivity analyses for other types of biases as follows. </br></br>
                       
                       To assess other biases in meta-analyses:   
                       
                       <ul>
                       <li>Publication bias in meta-analyses (<a href='https://osf.io/s9dp6/'>Mathur & VanderWeele, 2020c</a>;
                       R package <a href='https://cran.r-project.org/web/packages/PublicationBias/index.html'>PublicationBias</a>)</li>
                       
                       </ul>
                       
                       To assess biases in individual studies:
                       
                       <ul>
                       <li>Unmeasured confounding (the E-value) (<a href='https://annals.org/aim/fullarticle/2643434/sensitivity-analysis-observational-research-introducing-e-value'>VanderWeele & Ding, 2017</a>; <a href='http://www.evalue-calculator.com'>website</a>, R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>, or Stata package <a href='https://journals.sagepub.com/doi/abs/10.1177/1536867X20909696'>evalue</a>)</li>
                       <li>Selection bias (<a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6553568/'>Smith & VanderWeele, 2019a</a>; <a href='https://selection-bias.herokuapp.com/'>website</a> or R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>)</li>
                       
                       <li>Measurement error (<a href='https://academic.oup.com/aje/article/188/10/1823/5506602?casa_token=5ZyiVJp9_5UAAAAA:40rpOH1mRz0IDeRJ35atRRk9x6MJgIHMNOxLCcsnfouzN3qWXrght0XVWNIHQcRwWP1Bhgl8vY9B'>VanderWeele & Li, 2019</a>; R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>)</li>
                       
                       <li>A combination of unmeasured confounding, selection bias, and measurement error simultaneously (<a href='http://arxiv.org/abs/2005.02908'>Smith et al, 2020</a>;
                       R package <a href='https://cran.r-project.org/web/packages/EValue/index.html'>EValue</a>) </li>
                       
                       </ul>
                       
                       An analog of the E-value is also available to address unmeasured mediator-outcome confounding when carrying out mediation analysis for direct and indirect effects (<a href='https://journals.lww.com/epidem/Fulltext/2019/11000/Mediational_E_values__Approximate_Sensitivity.9.aspx'>Smith & VanderWeele, 2019b</a>).
                       
                       
                       </br></br>
                       ",
                       
                       "<b>Developers</b>",
                       
                       
                       "<br><br>This website was created by <a href='http://www.mayamathur.com'>Maya Mathur</a>, <a href='https://med.stanford.edu/qsu/current-members/Justin_Lee.html'>Justin Lee</a>, and <a href='https://www.hsph.harvard.edu/tyler-vanderweele/tools-and-tutorials/'>Tyler VanderWeele</a>.",
                       
                       
                       
                       "<br><br><b>References</b><br>",
                       
                       
                       "<ul>
                       <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4820664/'>Ding P & VanderWeele TJ (2016)</a>. Sensitivity analysis without assumptions. <i>Epidemiology</i>, 27(3), 368–377.</li>
                       
                       <li><a href='https://jamanetwork.com/journals/jama/fullarticle/2723079?casa_token=vP0UXdEX4HAAAAAA:y0GoUYecb4QvGnn23FNxpnOsBu5Z70-DW1apD84XqPWNL0kXYDXlT5hQVweAUZVh6zJe9BU_sA'>Haneuse S, VanderWeele TJ, & Arterburn D (2019)</a>. Using the E-value to assess the potential effect of unmeasured confounding in observational studies. <i>Journal of the American Medical Association</i>, 321(6), 602-603.</li>
                       
                       
                       <li> <a href='https://journals.sagepub.com/doi/abs/10.1177/1536867X20909696'>Linden A, Mathur MB, & VanderWeele TJ (2020)</a>. Conducting sensitivity analysis for unmeasured confounding in observational studies using E-values: The evalue package. <i>The Stata Journal</i> (in press).</li>
                       
                       <li> <a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6066405/'>Mathur MB, Ding P, Riddell CA, & VanderWeele TJ (2018).</a> Website and R package for computing E-values. <i>Epidemiology</i> 29(5), e45.</li>
                       
                       <li> <a href='https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8057'>Mathur MB & VanderWeele TJ (2019).</a> New statistical metrics for meta-analyses of heterogeneous effects. <i>Statistics in Medicine</i> 38(8), 1336-1342.</li>
                       
                       
                       <li><a href='https://www.tandfonline.com/doi/full/10.1080/01621459.2018.1529598'>Mathur MB & VanderWeele TJ (2020a)</a>. Sensitivity analysis for unmeasured confounding in meta-analyses. <i>Journal of the American Statistical Association</i> 115(529), 163-170.</li>
                       
                       <li><a href='https://pubmed.ncbi.nlm.nih.gov/32141922/'>Mathur MB & VanderWeele TJ (2020b)</a>. Robust metrics and sensitivity analyses for meta-analyses of heterogeneous effects. <i>Epidemiology</i> 31(3), 356-358.</li>
                       
                       <li><a href='https://osf.io/s9dp6/'>Mathur MB & VanderWeele TJ (2020c)</a>. Sensitivity analysis for publication bias in meta-analyses. Under review.</li>
                       <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6553568/'>Smith LH & VanderWeele TJ (2019a)</a>. Bounding bias due to selection. <i>Epidemiology</i> 30(4), 509.</li>
                       <li><a href='https://journals.lww.com/epidem/Fulltext/2019/11000/Mediational_E_values__Approximate_Sensitivity.9.aspx'>Smith LH & VanderWeele TJ (2019b)</a>. Mediational E-values: Approximate sensitivity analysis for mediator-outcome confounding. <i>Epidemiology</i> 30(6), 835-837.</li>
                       
                       <li><a href='https://annals.org/aim/fullarticle/2643434/sensitivity-analysis-observational-research-introducing-e-value'>VanderWeele TJ & Ding P (2017)</a>. Sensitivity analysis in observational research: Introducing the E-value. <i>Annals of Internal Medicine</i>, 167(4), 268-274.</li>
                       
                       <li><a href='https://www.degruyter.com/view/journals/jci/7/2/article-20180007.xml'>VanderWeele TJ, Ding P, & Mathur MB (2019a)</a>. Technical considerations in the use of the E-value. <i>Journal of Causal Inference</i>, 7(2).</li>
                       <li><a href='https://annals.org/aim/article-abstract/2719984/correcting-misinterpretations-e-value'>VanderWeele TJ, Mathur MB, & Ding P (2019b)</a>. Correcting misinterpretations of the E-value. <i>Annals of Internal Medicine</i> 170(2), 131-132.</li>
                       
                       <li><a href='https://academic.oup.com/aje/article/188/10/1823/5506602?casa_token=5ZyiVJp9_5UAAAAA:40rpOH1mRz0IDeRJ35atRRk9x6MJgIHMNOxLCcsnfouzN3qWXrght0XVWNIHQcRwWP1Bhgl8vY9B'>VanderWeele TJ & Li Y (2019)</a>. Simple sensitivity analysis for differential measurement error. <i>American Journal of Epidemiology</i>, 188(10), 1823-1829.</li>
                       </ul>"
                       
                       
                     ) ) )
            )
            
) ## closes navbarPage


