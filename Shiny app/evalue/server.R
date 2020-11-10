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
    
    # do not attempt to make plot unless we have the point estimate
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

  calibrated_output <- observeEvent(input$calibrated_calculate, {
    ### isolate on parameters to not update until action button pressed again
    scale = isolate(input$calibrated_scale)
    if(scale=="RR"){
      q = isolate(log(input$calibrated_q))
      r = isolate(input$calibrated_r)
      tail = isolate(input$calibrated_tail)
      muB = isolate(input$calibrated_muB)
      yi.name = isolate(input$calibrated_yi.name)
      vi.name = isolate(input$calibrated_vi.name)

      method = isolate(input$calibrated_method)
      Bmin = isolate(log(input$calibrated_Bmin))
      Bmax = isolate(log(input$calibrated_Bmax))
      R = isolate(input$calibrated_R)
      dat = isolate(mydata())

    } else {
      if(scale=="Log-RR"){
        q = isolate(input$calibrated_q)
        r = isolate(input$calibrated_r)
        tail = isolate(input$calibrated_tail)
        muB = isolate(input$calibrated_muB)
        yi.name = isolate(input$calibrated_yi.name)
        vi.name = isolate(input$calibrated_vi.name)

        method = isolate(input$calibrated_method)
        Bmin = isolate(input$calibrated_Bmin)
        Bmax = isolate(input$calibrated_Bmax)
        R = isolate(input$calibrated_R)
        dat = isolate(mydata())
      }
    }

    calibrated_cm <- reactive({
      withProgress(message="calculating...", value=1,{
        withCallingHandlers({
          shinyjs::html("calibrated_cm_messages", "")
          confounded_meta(method=method, muB=muB,q=q, r=r, yi.name=yi.name, vi.name=vi.name,
                                           tail=tail, give.CI=TRUE, R=R, dat=dat)
        },
        message = function(m){
          shinyjs::html(id="calibrated_cm_messages", html=paste0(m$message, '<br>'), add=TRUE)
        },
        warning = function(m){
          shinyjs::html(id="calibrated_cm_messages", html=paste0(m$message, '<br>'), add=TRUE)
        }
        )
      }) ## closes withProgress
    }) ## closes calibrated_cm output

    output$calibrated_text1 = renderText({

        # MBM: Why call confounded_meta three times for each of Phat, Tmin, and Gmin?
        #  Can't we just call it once and save all its output?
        #  Then we wouldn't be doing a redudant round of bootstrapping for Gmin
        ## jl: fixed w/ reactive cm above; just need to req(calibrated_cm()) for each output
      cm <- req(calibrated_cm())

        p = round( as.numeric(cm$Est[which(cm$Value=="Prop")]), 3 )
        p_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Prop")]), 3 )
        p_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Prop")]), 3 )


        ##### Create String for UI #####
        string_p = paste( p, " (95% CI: ", p_lo, ", ", p_hi, ")", sep="" )
        return( string_p )

    }) ## closes calibrated_text1

    output$calibrated_text2 = renderText({
      cm <- req(calibrated_cm())

        p = round( as.numeric(cm$Est[which(cm$Value=="Prop" )]), 3 )
        Tmin = round( as.numeric(cm$Est[which(cm$Value=="Tmin" )]), 3 )
        Tmin_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Tmin" )]), 3 )
        Tmin_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Tmin" )]), 3 )


        ##### Create String for UI #####
        string_Tmin = ifelse(p < r, "The proportion of meaningfully strong effects is already less than or equal to r even with no confounding, so this metric does not apply. No confounding at all is required to make the specified shift.", paste( Tmin, " (95% CI: ", Tmin_lo, ", ", Tmin_hi, ")", sep="" ))
        string_Tmin = ifelse(is.na(string_Tmin), "Cannot compute Tmin or Gmin without r. Returning only prop.", string_Tmin)
        return( string_Tmin )

    }) ## closes calibrated_text2

    output$calibrated_text3 = renderText({
      cm <- req(calibrated_cm())

        p = round( as.numeric(cm$Est[ which(cm$Value=="Prop") ]), 3 )
        Gmin = round( as.numeric(cm$Est[ which(cm$Value=="Gmin") ]), 3 )
        Gmin_lo = round( as.numeric(cm$CI.lo[ which(cm$Value=="Gmin") ]), 3 )
        Gmin_hi = round( as.numeric(cm$CI.hi[ which(cm$Value=="Gmin") ]), 3 )


        ##### Create String for UI #####
        string_Gmin = ifelse(p < r, "Not applicable. This is already the case, even with no bias, given your pooled effect size, threshold, and choice of tail.", paste( Gmin, " (95% CI: ", Gmin_lo, ", ", Gmin_hi, ")", sep="" ))
        string_Gmin = ifelse(is.na(string_Gmin), "Cannot compute Tmin or Gmin without r. Returning only prop.", string_Gmin)
        return( string_Gmin )

    }) ## closes calibrated_text3
  }) ## closes calibrated_output

  calibrated_plot <- observeEvent(input$calibrated_plot, {
    output$calibrated_plot1 = renderPlot({
      withProgress(message="generating plot...", value=1,{
        ### isolate on parameters to not update until action button pressed again
        scale = isolate(input$calibrated_scale)
        if(scale=="RR"){
          q = isolate(log(input$calibrated_q))
          r = isolate(input$calibrated_r)
          tail = isolate(input$calibrated_tail)
          muB = isolate(input$calibrated_muB)
          yi.name = isolate(input$calibrated_yi.name)
          vi.name = isolate(input$calibrated_vi.name)

          method = isolate(input$calibrated_method)
          Bmin = isolate(log(input$calibrated_Bmin))
          Bmax = isolate(log(input$calibrated_Bmax))
          R = isolate(input$calibrated_R)
          dat = isolate(mydata())

        } else {
          if(scale=="Log-RR"){
            q = isolate(input$calibrated_q)
            r = isolate(input$calibrated_r)
            tail = isolate(input$calibrated_tail)
            muB = isolate(input$calibrated_muB)
            yi.name = isolate(input$calibrated_yi.name)
            vi.name = isolate(input$calibrated_vi.name)

            method = isolate(input$calibrated_method)
            Bmin = isolate(input$calibrated_Bmin)
            Bmax = isolate(input$calibrated_Bmax)
            R = isolate(input$calibrated_R)
            dat = isolate(mydata())
          }
        }

        withCallingHandlers({
          shinyjs::html("calibrated_sens_plot_messages", "")
        sens_plot(method=method, type="line", q=q, yi.name=yi.name, vi.name=vi.name, Bmin=Bmin, Bmax=Bmax, tail=tail, give.CI=TRUE, R=R, dat=dat )
        },
        message = function(m){
          shinyjs::html(id="calibrated_sens_plot_messages", html=paste0(m$message, '<br>'), add=TRUE)
        },
        warning = function(m){
          shinyjs::html(id="calibrated_sens_plot_messages", html=paste0(m$message, '<br>'), add=TRUE)
        }
        )

      }) ## closes withProgress

      ### output plot warnings:
      ## jl: warnings/messages should now be built into the plot outputs using withCallingHandlers to pull messages/warnings from sens_plot itself

    }) ## closes calibrated_plot1


  }) ## closes calibrated_plot


  ### results text for calibrated Fixed sensitivity parameters tab
  output$calibrated_results_prop = renderText({
    paste("Proportion of studies with population causal effects", input$calibrated_tail, input$calibrated_scale, "=", input$calibrated_q, ":")
  })
  output$calibrated_results_minbias = renderText({
    paste("Minimum bias factor (RR scale) to reduce to less than", input$calibrated_r, "the proportion of studies with population causal effects", input$calibrated_tail, input$calibrated_scale, "=", input$calibrated_q, ":")
  })
  output$calibrated_results_minconf = renderText({
    paste("Minimum confounding strength (RR scale) to reduce to less than", input$calibrated_r, "the proportion of studies with population causal effects", input$calibrated_tail, input$calibrated_scale, "=", input$calibrated_q, ":")
  })
  
  
  ##### For Tab Panel Parametric Fixed sensitivity parameters #####
  parametric_output <- observeEvent(input$parametric_calculate, {
    ### isolate on parameters to not update until action button pressed again
    scale_2 = isolate(input$parametric_scale)
    if(scale_2=="RR"){
      yr_2 = isolate(log(input$parametric_yr))
      t2_2 = isolate(input$parametric_t2)
      q_2 = isolate(log(input$parametric_q))
      vyr_2 = isolate(input$parametric_se_yr^2)
      vt2_2 = isolate((input$parametric_prop_t2*(input$parametric_t2^2)))
      muB_2 = isolate(log(input$parametric_muB))
      sigB_2 = isolate(input$parametric_sigB)
      r_2 = isolate(input$parametric_r)
      tail_2 = isolate(input$parametric_tail)

      method_2 = isolate(input$parametric_method)
      Bmin_2 = isolate(log(input$parametric_Bmin))
      Bmax_2 = isolate(log(input$parametric_Bmax))

    } else {
      if(scale_2=="Log-RR"){
        yr_2 = isolate(input$parametric_yr)
        t2_2 = isolate(input$parametric_t2)
        q_2 = isolate(input$parametric_q)
        vyr_2 = isolate(input$parametric_se_yr^2)
        vt2_2 = isolate((input$parametric_prop_t2*(input$parametric_t2^2)))
        muB_2 = isolate(input$parametric_muB)
        sigB_2 = isolate(input$parametric_sigB)
        r_2 = isolate(input$parametric_r)
        tail_2 = isolate(input$parametric_tail)

        method_2 = isolate(input$parametric_method)
        Bmin_2 = isolate(input$parametric_Bmin)
        Bmax_2 = isolate(input$parametric_Bmax)
      }
    }

    parametric_cm <- reactive({
        withCallingHandlers({
          shinyjs::html("parametric_cm_messages", "")
          confounded_meta(method=method_2,q=q_2, r=r_2, muB=muB_2, sigB=sigB_2, yr=yr_2, vyr=vyr_2,
                                           t2=t2_2, vt2=vt2_2, CI.level=0.95, tail=tail_2)
        },
        message = function(m){
          shinyjs::html(id="parametric_cm_messages", html=paste0(m$message, '<br>'), add=TRUE)
        },
        warning = function(m){
          shinyjs::html(id="parametric_cm_messages", html=paste0(m$message, '<br>'), add=TRUE)
        }
        )
    }) ## closes calibrated_cm output

    output$parametric_text1 = renderText({
      cm = req(parametric_cm())

      p = round( as.numeric(cm$Est[which(cm$Value=="Prop")]), 3 )
      p_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Prop")]), 3 )
      p_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Prop")]), 3 )


      ##### Create String for UI #####
      string_p = paste( p, " (95% CI: ", p_lo, ", ", p_hi, ")", sep="" )
      return( string_p )

    }) ## closes parametric_text1

    output$parametric_text2 = renderText({
      cm = req(parametric_cm())

      p = round( as.numeric(cm$Est[which(cm$Value=="Prop" )]), 3 )
      Tmin = round( as.numeric(cm$Est[which(cm$Value=="Tmin" )]), 3 )
      Tmin_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Tmin" )]), 3 )
      Tmin_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Tmin" )]), 3 )


      ##### Create String for UI #####
      string_Tmin = ifelse(p < r_2, "Not applicable. This is already the case, even with no bias, given your pooled effect size, threshold, and choice of tail.", paste( Tmin, " (95% CI: ", Tmin_lo, ", ", Tmin_hi, ")", sep="" ))
      return( string_Tmin )

    }) ## closes parametric_text2

    output$parametric_text3 = renderText({
      cm = req(parametric_cm())

      p = round( as.numeric(cm$Est[ which(cm$Value=="Prop") ]), 3 )
      Gmin = round( as.numeric(cm$Est[ which(cm$Value=="Gmin") ]), 3 )
      Gmin_lo = round( as.numeric(cm$CI.lo[ which(cm$Value=="Gmin") ]), 3 )
      Gmin_hi = round( as.numeric(cm$CI.hi[ which(cm$Value=="Gmin") ]), 3 )


      ##### Create String for UI #####
      string_Gmin = ifelse(p < r_2, "Not applicable. This is already the case, even with no bias, given your pooled effect size, threshold, and choice of tail.", paste( Gmin, " (95% CI: ", Gmin_lo, ", ", Gmin_hi, ")", sep="" ))
      return( string_Gmin )

    }) ## closes parametric_text3

    ### parametric_output warnings:
    output$parametric_kwarn <- reactive({
      numStudies <- input$parametric_k
      ifelse(numStudies <=10,
             "WARNING: These methods may not work well for meta-analyses with fewer than 10 studies.",
             "")
    }) ## closes parametric_kwarn_2

    output$parametric_phatwarn <- reactive({
      cm = req(parametric_cm())

      p = round( cm$Est[ cm$Value=="Prop" ], 3 )
      ifelse(p<0.15 | p>0.85,
             HTML(paste('WARNING: Extreme estimated proportion', 'The estimated proportion of meaningfully strong effects is <0.15 or >0.85. The methods implemented in this website do not always work well in these situations. We would recommend instead applying alternative methods that have the same interpretation (see the "More Resouces" tab).', sep = "<br/>")), "")
    }) ## closes parametric_phatwarn_2

  }) ## closes parametric_output

  parametric_plot <- observeEvent(input$parametric_plot, {
    output$parametric_plot1 = renderPlot({
      ### isolate on parameters to not update until action button pressed again
      scale_2 = isolate(input$parametric_scale)
      if(scale_2=="RR"){
        yr_2 = isolate(log(input$parametric_yr))
        t2_2 = isolate(input$parametric_t2)
        q_2 = isolate(log(input$parametric_q))
        vyr_2 = isolate(input$parametric_se_yr^2)
        vt2_2 = isolate((input$parametric_prop_t2*(input$parametric_t2^2)))
        muB_2 = isolate(log(input$parametric_muB))
        sigB_2 = isolate(input$parametric_sigB)
        r_2 = isolate(input$parametric_r)
        tail_2 = isolate(input$parametric_tail)

        method_2 = isolate(input$parametric_method)
        Bmin_2 = isolate(log(input$parametric_Bmin))
        Bmax_2 = isolate(log(input$parametric_Bmax))
        CI.level_2 = 0.95
        give.CI_2 = TRUE

      } else {
        if(scale_2=="Log-RR"){
          yr_2 = isolate(input$parametric_yr)
          t2_2 = isolate(input$parametric_t2)
          q_2 = isolate(input$parametric_q)
          vyr_2 = isolate(input$parametric_se_yr^2)
          vt2_2 = isolate((input$parametric_prop_t2*(input$parametric_t2^2)))
          muB_2 = isolate(input$parametric_muB)
          sigB_2 = isolate(input$parametric_sigB)
          r_2 = isolate(input$parametric_r)
          tail_2 = isolate(input$parametric_tail)

          method_2 = isolate(input$parametric_method)
          Bmin_2 = isolate(input$parametric_Bmin)
          Bmax_2 = isolate(input$parametric_Bmax)
          CI.level_2 = 0.95
          give.CI_2 = TRUE
        }
      }

      withCallingHandlers({
        shinyjs::html("parametric_sens_plot_messages", "")
        sens_plot(method = method_2, type="line", q=q_2, yr=yr_2, vyr=vyr_2, t2=t2_2, vt2=vt2_2,
                                   Bmin=Bmin_2, Bmax=Bmax_2, sigB=sigB_2, tail=tail_2 )
      },
      message = function(m){
        shinyjs::html(id="parametric_sens_plot_messages", html=paste0(m$message, '<br>'), add=TRUE)
      },
      warning = function(m){
        shinyjs::html(id="parametric_sens_plot_messages", html=paste0(m$message, '<br>'), add=TRUE)
      }
      )

      ### output plot warnings:
      ## jl: warnings/messages should now be built into the plot outputs using withCallingHandlers to pull messages/warnings from sens_plot itself

    }) ## closes parametric_plot1
  }) ## closes parametric_plot



  ### results text for parametric Fixed sensitivity parameters tab
  output$parametric_results_prop = renderText({
    paste("Proportion of studies with population causal effects", input$parametric_tail, input$parametric_scale, "=", input$parametric_q, ":")
  })
  output$parametric_results_minbias = renderText({
    paste("Minimum bias factor (RR scale) to reduce to less than", input$parametric_r, "the proportion of studies with population causal effects", input$parametric_tail, input$parametric_scale, "=", input$parametric_q, ":")
  })
  output$parametric_results_minconf = renderText({
    paste("Minimum confounding strength (RR scale) to reduce to less than", input$parametric_r, "the proportion of studies with population causal effects", input$parametric_tail, input$parametric_scale, "=", input$parametric_q, ":")
  })
  
  
  
} ## closes server.R function
