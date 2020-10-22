source("startup.R")

function(input, output, session) {
  
  ##### For Tab Panel E-value #####
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
    
    if ( input$outcomeType == "OLS" ) {
      if ( is.na( input$estOLS ) ) return("Enter your point estimate")
      if ( is.na( input$sdOLS ) ) return("Enter your standard deviation")
      if ( is.na( input$trueOLS )) return("Enter a true value")
      evals = round( evalues.OLS( est = input$estOLS,
                                  se = input$seOLS,
                                  sd = input$sdOLS,
                                  delta = input$deltaOLS,
                                  true = input$trueOLS )[2,], 2 )
      
    }
    
    
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
    } else if ( input$outcomeType == "OLS" ){
      bf <- exp( 0.91 * (input$deltaOLS * input$estOLS / input$sdOLS) ) / exp( 0.91*input$trueOLS )
    } else if ( input$outcomeType == "MD" ){
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
  
  
  ##### For Tab Panel Calibrated Fixed sensitivity parameters ##### 
  mydata <- reactive({
    inFile <- input$calibrated_uploaddat
    
    if(is.null(inFile))
      return(NULL)
    
    tbl <- read.csv(inFile$datapath, stringsAsFactors = FALSE)
  })
  
  ### jl testing if data is being read okay:
  # output$calibrated_tab1 = renderTable(mydata())
  
  calibrated_output <- observeEvent(input$calibrated_calculate, {
    if(input$calibrated_scale=="RR"){
      q = logHR_to_logRR(log(input$calibrated_q))
      r = input$calibrated_r
      tail = input$calibrated_tail
      
      method = input$calibrated_method
      Bmin = input$calibrated_Bmin
      Bmax = input$calibrated_Bmax
      calib = mydata()[[input$calibrated_calib.name]]
      R = input$calibrated_R
      dat = mydata()
      calib.name = input$calibrated_calib.name
      
    } else {
      if(input$calibrated_scale=="Log-RR"){
        q = logHR_to_logRR(input$calibrated_q)
        r = input$calibrated_r
        tail = input$calibrated_tail
        
        method = input$calibrated_method
        Bmin = input$calibrated_Bmin
        Bmax = input$calibrated_Bmax
        calib = mydata()[[input$calibrated_calib.name]]
        R = input$calibrated_R
        dat = mydata()
        calib.name = input$calibrated_calib.name
      }
    }
    output$calibrated_text1 = renderText({
      ## just for testing, can delete
      # print(c(q_2,r_2,tail_2,method_2,Bmin_2,Bmax_2,calib_2,R_2,calib.name_2))
      # 
      # d=as.data.frame(list(author = c("Teo 2010", "Da silva-gane 2012", "Hussain 2013",
      #                                 "Shih 2013", "Shum 2014", "Brown 2015", "Kwok 2016", "Verberne 2016",
      #                                 "Chandna 2016", "Reindl-schwaighofer 2017", "Raman 2018", "Tam-tham 2018"),
      #                      year = c(2010, 2012, 2013, 2013, 2014, 2015, 2016, 2016, 2016,2017, 2018, 2018),
      #                      hr = c(0.44, 0.44, 0.46, 1.16, 0.46, 0.31,0.22, 0.62, 0.53, 0.23, 0.61, 0.67),
      #                      lb = c(0.22, 0.22, 0.32,1.07, 0.31, 0.21, 0.17, 0.42, 0.39, 0.18, 0.41, 0.53),
      #                      ub = c(0.86, 0.92, 0.68, 1.25, 0.68, 0.47, 0.3, 0.92, 0.73, 0.29, 0.91, 0.83),
      #                      n = c(57, 154, 306, 8341, 199, 286, 558, 311, 250, 8796, 204,838),
      #                      nx = c(41, 124, 164, 6292, 157, 164, 126, 204, 92, 8622,123, 500),
      #                      n0 = c(16, 30, 142, 2049, 42, 122, 432, 107, 158,174, 81, 338),
      #                      yi = c(-0.82098055206983, -0.82098055206983, -0.776528789498996,0.148420005118273, -0.776528789498996, -1.17118298150295, -1.51412773262978,
      #                             -0.478035800943, -0.63487827243597, -1.46967597005894, -0.49429632181478, -0.400477566597125),
      #                      vyi = c(0.116911650846615, 0.141626456589046,0.0397704305571613, 0.00145351248489691, 0.0397704305571613,0.0450842985184214, 0.0250415490680121,
      #                              0.0405449956738431, 0.0266844577833026,0.0139873914540288, 0.0416478534714748, 0.0119380066476652),
      #                      calib = c(-0.815500241994327, -0.814528779625426, -0.776052752266121,0.147175232542529, -0.776052752266121, -1.15489254358239,-1.49702475156308,
      #                                -0.488331228832504, -0.637983041992715, -1.46055146962155, -0.504254067888611, -0.404485663510471),
      #                      calib.logRR = c(-0.560977462897841, -0.560319288814832,
      #                                      -0.534223240540279, 0.101988678750287, -0.534223240540279,
      #                                      -0.788465088962296, -1.01180042488262, -0.337559759120245,
      #                                      -0.440157063435869, -0.988320988323321, -0.3485033298636,
      #                                      -0.279841496330782)))
      # 
      # q = logHR_to_logRR(log(.8))
      # r = .1
      # tail = "below"
      # method = "calibrated"
      # Bmin = 1
      # Bmax = 4
      # calib = d$calib.logRR
      # R = 2000
      # dat = d
      # calib.name = "calib.logRR"
      
      cm = suppressWarnings(confounded_meta(method=method,q=q, r=r, Bmin=Bmin, Bmax=Bmax, .calib=calib, tail=tail, .give.CI=TRUE, .R=R, .dat=dat, .calib.name=calib.name))
      
      p = round( as.numeric(cm$Est[which(cm$Value=="Phat.t")]), 3 )
      p_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Phat.t")]), 3 )
      p_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Phat.t")]), 3 )
      
      
      ##### Create String for UI #####
      string_p = paste( p, " (95% CI: ", p_lo, ", ", p_hi, ")", sep="" )
      return( string_p )
      
    }) ## closes calibrated_text1
    
    output$calibrated_text2 = renderText({
      cm = suppressWarnings(confounded_meta(method=method,q=q, r=r, Bmin=Bmin, Bmax=Bmax, .calib=calib, tail=tail, .give.CI=TRUE, .R=R, .dat=dat, .calib.name=calib.name))
      
      p = round( as.numeric(cm$Est[which(cm$Value=="Phat.t" )]), 3 )
      Tmin = round( as.numeric(cm$Est[which(cm$Value=="That" )]), 3 )
      Tmin_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="That" )]), 3 )
      Tmin_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="That" )]), 3 )
      
      
      ##### Create String for UI ##### 
      string_Tmin = ifelse(p < r, "Not applicable. This is already the case, even with no bias, given your pooled effect size, threshold, and choice of tail.", paste( Tmin, " (95% CI: ", Tmin_lo, ", ", Tmin_hi, ")", sep="" ))
      return( string_Tmin )
      
    }) ## closes calibrated_text2
    
    output$calibrated_text3 = renderText({
      cm = suppressWarnings(confounded_meta(method=method,q=q, r=r, Bmin=Bmin, Bmax=Bmax, .calib=calib, tail=tail, .give.CI=TRUE, .R=R, .dat=dat, .calib.name=calib.name))
      
      p = round( as.numeric(cm$Est[ which(cm$Value=="Phat.t") ]), 3 )
      Gmin = round( as.numeric(cm$Est[ which(cm$Value=="Ghat") ]), 3 )
      Gmin_lo = round( as.numeric(cm$CI.lo[ which(cm$Value=="Ghat") ]), 3 )
      Gmin_hi = round( as.numeric(cm$CI.hi[ which(cm$Value=="Ghat") ]), 3 )
      
      
      ##### Create String for UI ##### 
      string_Gmin = ifelse(p < r, "Not applicable. This is already the case, even with no bias, given your pooled effect size, threshold, and choice of tail.", paste( Gmin, " (95% CI: ", Gmin_lo, ", ", Gmin_hi, ")", sep="" ))
      return( string_Gmin )
      
    }) ## closes calibrated_text3
    
    ### warnings:
    output$calibrated_kwarn <- reactive({
      numStudies <- input$calibrated_k
      ifelse(numStudies <=10,
             "WARNING: These methods may not work well for meta-analyses with fewer than 10 studies.",
             "")
    }) ## closes calibrated_kwarn
    
    output$calibrated_phatwarn <- reactive({
      cm = suppressWarnings(confounded_meta(method=method,q=q, r=r, Bmin=Bmin, Bmax=Bmax, .calib=calib, tail=tail, .give.CI=TRUE, .R=R, .dat=dat, .calib.name=calib.name))
      
      p = round( cm$Est[ cm$Value=="Phat.t" ], 3 )
      ifelse(p<0.15 | p>0.85,
             HTML(paste('WARNING: Extreme estimated proportion', 'The estimated proportion of meaningfully strong effects is <0.15 or >0.85. The methods implemented in this website do not always work well in these situations. We would recommend instead applying alternative methods that have the same interpretation (see the "More Resouces" tab).', sep = "<br/>")), "")
    }) ## closes calibrated_phatwarn_2
    
    
    # ### 10/21/20 TBD, get the rest working then ask Maya about these plots:
    # output$calibrated_plot1 <- renderPlot({
    #   suppressWarnings(sens_plot_addtail(method=method, type="line", q=q, r=r, Bmin=Bmin, Bmax=Bmax, .calib=calib, tail=tail, .give.CI=TRUE, .R=R, .dat=dat, .calib.name=calib.name ))
    # }) ## closes calibrated_plot1
  }) ## closes calibrated_output
  
  
  
  ### results text for calibrated Fixed sensitivity parameters tab
  output$calibrated_results_prop = renderText({
    paste("Proportion of studies with population causal effects", input$calibrated_tail, input$calibrated_q, ":")
  })
  output$calibrated_results_minbias = renderText({
    paste("Minimum bias factor (RR scale) to reduce to less than", input$calibrated_r, "the proportion of studies with population causal effects", input$calibrated_tail, input$calibrated_q, ":")
  })
  output$calibrated_results_minconf = renderText({
    paste("Minimum confounding strength (RR scale) to reduce to less than", input$calibrated_r, "the proportion of studies with population causal effects", input$calibrated_tail, input$calibrated_q, ":")
  })
  
  
  ##### For Tab Panel Parametric Fixed sensitivity parameters ##### 
  parametric_output <- observeEvent(input$parametric_calculate, {
    if(input$parametric_scale=="RR"){
      yr_2 = log(input$parametric_yr)
      t2_2 = input$parametric_t2
      q_2 = log(input$parametric_q)
      vyr_2 = input$parametric_se_yr^2
      vt2_2 = input$parametric_se_t2^2
      muB_2 = log(input$parametric_muB)
      sigB_2 = input$parametric_sigB
      r_2 = input$parametric_r
      tail_2 = input$parametric_tail
      
      method_2 = input$parametric_method
      
    } else {
      if(input$parametric_scale=="Log-RR"){
        yr_2 = input$parametric_yr
        t2_2 = input$parametric_t2
        q_2 = input$parametric_q
        vyr_2 = input$parametric_se_yr^2
        vt2_2 = input$parametric_se_t2^2
        muB_2 = input$parametric_muB
        sigB_2 = input$parametric_sigB
        r_2 = input$parametric_r
        tail_2 = input$parametric_tail
        
        method_2 = input$parametric_method
      }
    }
    
    
    
    ### for testing, can delete:
    # yr_2 = log(1.2)
    # t2_2 = 0.1
    # q_2 = 1.1
    # vyr_2 = 0.01
    # vt2_2 = 0.1
    # muB_2 = 1.5
    # sigB_2 = 0
    # r_2 = 0.2
    # tail_2 = "below"
    # method_2 = "parametric"
    
    output$parametric_text1 = renderText({
      cm = suppressWarnings(confounded_meta(method=method_2,q=q_2, r = r_2, muB = muB_2, sigB = sigB_2, yr = yr_2, vyr = vyr_2,
                                            t2 = t2_2, vt2 = vt2_2, CI.level = 0.95, tail = tail_2))
      
      p = round( as.numeric(cm$Est[which(cm$Value=="Prop")]), 3 )
      p_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Prop")]), 3 )
      p_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Prop")]), 3 )
      
      
      ##### Create String for UI #####
      string_p = paste( p, " (95% CI: ", p_lo, ", ", p_hi, ")", sep="" )
      return( string_p )
      
    }) ## closes parametric_text1
    
    output$parametric_text2 = renderText({
      cm = suppressWarnings(confounded_meta(method=method_2,q=q_2, r = r_2, muB = muB_2, sigB = sigB_2, yr = yr_2, vyr = vyr_2,
                                            t2 = t2_2, vt2 = vt2_2, CI.level = 0.95, tail = tail_2))
      
      p = round( as.numeric(cm$Est[which(cm$Value=="Prop" )]), 3 )
      Tmin = round( as.numeric(cm$Est[which(cm$Value=="Tmin" )]), 3 )
      Tmin_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Tmin" )]), 3 )
      Tmin_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Tmin" )]), 3 )
      
      
      ##### Create String for UI ##### 
      string_Tmin = ifelse(p < r_2, "Not applicable. This is already the case, even with no bias, given your pooled effect size, threshold, and choice of tail.", paste( Tmin, " (95% CI: ", Tmin_lo, ", ", Tmin_hi, ")", sep="" ))
      return( string_Tmin )
      
    }) ## closes parametric_text2
    
    output$parametric_text3 = renderText({
      cm = suppressWarnings(confounded_meta(method=method_2,q=q_2, r = r_2, muB = muB_2, sigB = sigB_2, yr = yr_2, vyr = vyr_2,
                                            t2 = t2_2, vt2 = vt2_2, CI.level = 0.95, tail = tail_2))
      
      p = round( as.numeric(cm$Est[ which(cm$Value=="Prop") ]), 3 )
      Gmin = round( as.numeric(cm$Est[ which(cm$Value=="Gmin") ]), 3 )
      Gmin_lo = round( as.numeric(cm$CI.lo[ which(cm$Value=="Gmin") ]), 3 )
      Gmin_hi = round( as.numeric(cm$CI.hi[ which(cm$Value=="Gmin") ]), 3 )
      
      
      ##### Create String for UI ##### 
      string_Gmin = ifelse(p < r_2, "Not applicable. This is already the case, even with no bias, given your pooled effect size, threshold, and choice of tail.", paste( Gmin, " (95% CI: ", Gmin_lo, ", ", Gmin_hi, ")", sep="" ))
      return( string_Gmin )
      
    }) ## closes parametric_text3
    
    ### warnings:
    output$parametric_kwarn <- reactive({
      numStudies <- input$parametric_k
      ifelse(numStudies <=10,
             "WARNING: These methods may not work well for meta-analyses with fewer than 10 studies.",
             "")
    }) ## closes parametric_kwarn_2
    
    output$parametric_phatwarn <- reactive({
      cm = suppressWarnings(confounded_meta(method=method_2,q=q_2, r = r_2, muB = muB_2, sigB = sigB_2, yr = yr_2, vyr = vyr_2,
                                            t2 = t2_2, vt2 = vt2_2, CI.level = 0.95, tail = tail_2))
      
      p = round( cm$Est[ cm$Value=="Prop" ], 3 )
      ifelse(p<0.15 | p>0.85,
             HTML(paste('WARNING: Extreme estimated proportion', 'The estimated proportion of meaningfully strong effects is <0.15 or >0.85. The methods implemented in this website do not always work well in these situations. We would recommend instead applying alternative methods that have the same interpretation (see the "More Resouces" tab).', sep = "<br/>")), "")
    }) ## closes parametric_phatwarn_2
    
    
    ### 10/21/20 TBD, get the rest working then ask Maya about these plots:
    output$parametric_plot <- renderPlot({
      suppressWarnings(sens_plot_addtail( type="dist", q=q_2, yr=yr_2, vyr=vyr_2, t2=t2_2, vt2=vt2_2,
                                          muB=muB_2, sigB=sigB_2, tail=tail_2 ))
    }) ## closes parametric_plot1
  }) ## closes parametric_output
  
  ### results text for parametric Fixed sensitivity parameters tab
  output$parametric_results_prop = renderText({
    paste("Proportion of studies with population causal effects", input$parametric_tail, input$parametric_q, ":")
  })
  output$parametric_results_minbias = renderText({
    paste("Minimum bias factor (RR scale) to reduce to less than", input$parametric_r, "the proportion of studies with population causal effects", input$parametric_tail, input$parametric_q, ":")
  })
  output$parametric_results_minconf = renderText({
    paste("Minimum confounding strength (RR scale) to reduce to less than", input$parametric_r, "the proportion of studies with population causal effects", input$parametric_tail, input$parametric_q, ":")
  })
  
  
  ##### For Tab Panel Range of sensitivity parameters #####
  output$plot_3 <- renderPlot({
    
    if(input$scale_3=="RR"){
      yr_3 = log(input$yr_3)
      t2_3 = input$t2_3
      q_3= log(input$q_3)
      vyr_3 = input$se_yr_3^2
      vt2_3 = input$se_t2_3^2
      sigB_3 = input$sigB_3
      Bmin_3 = log(input$Bmin_3)
      Bmax_3 = log(input$Bmax_3)
      tail_3 = input$tail_3
      
      method_3 = "parametric"
      
    } else {
      if(input$scale_3=="Log-RR"){
        yr_3 = input$yr_3
        t2_3 = input$t2_3
        q_3 = input$q_3
        vyr_3 = input$se_yr_3^2
        vt2_3 = input$se_t2_3^2
        sigB_3 = input$sigB_3
        Bmin_3 = input$Bmin_3
        Bmax_3 = input$Bmax_3
        tail_3 = input$tail_3
        
        method_3 = "parametric"
      }
    }
    
    suppressWarnings(sens_plot_addtail(method = method_3, type="line", q=q_3, yr=yr_3, vyr=vyr_3, t2=t2_3, vt2=vt2_3,
                                       Bmin=Bmin_3, Bmax=Bmax_3, sigB=sigB_3, tail=tail_3 ))
  })
  
  
  
} ## closes function











### EXTRA CODE BELOW, CAN PROBABLY DELETE BUT KEEP FOR NOW ###

# ##### For Tab Panel Fixed sensitivity parameters #####
# output$plot2 <- renderPlot({
#   
#   # observeEvent( input$make.plot, {
#   yr_2 = log(input$yr_2)
#   t2_2 = input$t2_2
#   q_2 = log(input$q_2)
#   vyr_2 = input$se_yr_2^2
#   vt2_2 = input$se_t2_2^2
#   muB_2 = log(input$muB_2)
#   sigB_2 = input$sigB_2
#   r_2 = input$r_2
#   
#   
#   suppressWarnings(sens_plot_addtail( type="dist", q=q_2, yr=yr_2, vyr=vyr_2, t2=t2_2, vt2=vt2_2,
#                                       muB=muB_2, sigB=sigB_2 ))
#   
#   
#   #   } )
#   
# })

##### For Tab Panel Range of sensitivity parameters #####   
# output$plot1 <- renderPlot({
#   
#   if(input$scale=="RR"){
#     yr = log(input$yr)
#     t2 = input$t2
#     q = log(input$q)
#     vyr = input$se_yr^2
#     vt2 = input$se_t2^2
#     sigB = input$sigB
#     Bmin = log(input$Bmin)
#     Bmax = log(input$Bmax)
#     tail = input$tail
#   } else {
#     if(input$scale=="Log-RR"){
#       yr = input$yr
#       t2 = input$t2
#       q = input$q
#       vyr = input$se_yr^2
#       vt2 = input$se_t2^2
#       sigB = input$sigB
#       Bmin = input$Bmin
#       Bmax = input$Bmax
#       tail = input$tail
#     }
#   }
#   
#   suppressWarnings(sens_plot_addtail( type="line", q=q, yr=yr, vyr=vyr, t2=t2, vt2=vt2,
#                                       Bmin=Bmin, Bmax=Bmax, sigB=sigB, tail=tail ))
# })

##### WARNINGS #####
##### For Tab Panel Range of sensitivity parameters ##### 
# output$kwarn <- reactive({
#   numStudies <- input$k
#   ifelse(numStudies <=10,
#          "WARNING: These methods may not work well for meta-analyses with fewer than 10 studies.",
#          "")
# })

##### For Tab Panel Fixed sensitivity parameters ##### 
# output$kwarn_2 <- reactive({
#   numStudies <- input$k_2
#   ifelse(numStudies <=10,
#          "WARNING: These methods may not work well for meta-analyses with fewer than 10 studies.",
#          "")
# })


# output$phatwarn_2 <- reactive({
#   yr_2 = log(input$yr_2)
#   t2_2 = input$t2_2
#   q_2 = log(input$q_2)
#   vyr_2 = input$se_yr_2^2
#   vt2_2 = input$se_t2_2^2
#   muB_2 = log(input$muB_2)
#   sigB_2 = input$sigB_2
#   r_2 = input$r_2
#   tail_2 = input$tail_2
#   
#   cm = suppressWarnings(confounded_meta(q=q_2, r = r_2, muB = muB_2, sigB = sigB_2, yr = yr_2, vyr = vyr_2,
#                                         t2 = t2_2, vt2 = vt2_2, CI.level = 0.95, tail = tail_2))
#   
#   p = round( cm$Est[ cm$Value=="Prop" ], 3 )
#   ifelse(p<0.15 | p>0.85,
#          HTML(paste('WARNING: Extreme estimated proportion', 'The estimated proportion of meaningfully strong effects is <0.15 or >0.85. The methods implemented in this website do not always work well in these situations. We would recommend instead applying alternative methods that have the same interpretation (see the "More Resouces" tab).', sep = "<br/>")), "")
# })




