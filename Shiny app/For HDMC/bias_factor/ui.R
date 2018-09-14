
fluidPage(
  
  theme = shinytheme("flatly"),

HTML( "<br><b>WARNING:</b> This page calculates the maximum bias as a function of confounding strength, which is <b>not</b> the E-value.<br>
      To compute an E-value, <a href='https://mmathur.shinyapps.io/evalue/'>click here.</a><br>",
      "<br>If you have substantive knowledge on the strength of the relationships between the unmeasured confounder(s)",
      " and the exposure and outcome, you can use these numbers to calculate the bias factor.",
      " Let RR<sub>UD</sub> denote the maximum risk ratio for the outcome, conditional on the observed covariates, comparing any two categories of the unmeasured",
      " confounders and taken within either exposure group. Let RR<sub>EU</sub> denote",
      " the maximum risk ratio for any specific level of the unmeasured confounders comparing those with and without exposure, with",
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