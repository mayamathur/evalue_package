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
      q = log(input$calibrated_q)
      r = input$calibrated_r
      tail = input$calibrated_tail
      muB = input$calibrated_muB
      yi.name = input$calibrated_yi.name
      vi.name = input$calibrated_vi.name
      
      method = input$calibrated_method
      Bmin = log(input$calibrated_Bmin)
      Bmax = log(input$calibrated_Bmax)
      R = input$calibrated_R
      dat = mydata()
      
    } else {
      if(input$calibrated_scale=="Log-RR"){
        q = input$calibrated_q
        r = input$calibrated_r
        tail = input$calibrated_tail
        muB = input$calibrated_muB
        yi.name = input$calibrated_yi.name
        vi.name = input$calibrated_vi.name
        
        method = input$calibrated_method
        Bmin = input$calibrated_Bmin
        Bmax = input$calibrated_Bmax
        R = input$calibrated_R
        dat = mydata()
      }
    }
    
    output$calibrated_text1 = renderText({
      ## just for testing, can delete
      # print(c(q_2,r_2,tail_2,method_2,Bmin_2,Bmax_2,calib_2,R_2,calib.name_2))
      # 
      # d = readxl::read_xlsx("~/Box Sync/jlee/Maya/meta/data/dat.xlsx")
      # es = MetaUtility::scrape_meta(type="RR",
      #                               est = d$hr,
      #                               hi=d$ub)
      # d=cbind(d,es)
      # 
      # q = 0
      # r = NA
      # tail = "below"
      # method = "calibrated"
      # Bmin = 1
      # Bmax = 4
      # yi.name="yi"
      # vi.name="vyi"
      # 
      # R = 2000
      # dat = d
      
      withProgress(message="calculating proportion...", value=1,{
        
        cm = suppressWarnings(confounded_meta(method=method, muB=muB,q=q, r=r, yi.name=yi.name, vi.name=vi.name,
                                              tail=tail, give.CI=TRUE, R=R, dat=dat))
        
        p = round( as.numeric(cm$Est[which(cm$Value=="Prop")]), 3 )
        p_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Prop")]), 3 )
        p_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Prop")]), 3 )
        
        
        ##### Create String for UI #####
        string_p = paste( p, " (95% CI: ", p_lo, ", ", p_hi, ")", sep="" )
        return( string_p )
        
      }) ## closes withProgress
      
    }) ## closes calibrated_text1
    
    output$calibrated_text2 = renderText({
      withProgress(message="calculating minimum bias factor...", value=1,{
        cm = suppressWarnings(confounded_meta(method=method, muB=muB,q=q, r=r, yi.name=yi.name, vi.name=vi.name,
                                              tail=tail, give.CI=TRUE, R=R, dat=dat))
        
        p = round( as.numeric(cm$Est[which(cm$Value=="Prop" )]), 3 )
        Tmin = round( as.numeric(cm$Est[which(cm$Value=="Tmin" )]), 3 )
        Tmin_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Tmin" )]), 3 )
        Tmin_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Tmin" )]), 3 )
        
        
        ##### Create String for UI ##### 
        string_Tmin = ifelse(p < r, "The proportion of meaningfully strong effects is already less than or equal to r even with no confounding, so this metric does not apply. No confounding at all is required to make the specified shift.", paste( Tmin, " (95% CI: ", Tmin_lo, ", ", Tmin_hi, ")", sep="" ))
        string_Tmin = ifelse(is.na(string_Tmin), "Cannot compute Tmin or Gmin without r. Returning only prop.", string_Tmin)
        return( string_Tmin )
        
      }) ## closes withProgress
      
    }) ## closes calibrated_text2
    
    output$calibrated_text3 = renderText({
      withProgress(message="calculating minimum confounding strength...", value=1,{
        cm = suppressWarnings(confounded_meta(method=method, muB=muB,q=q, r=r, yi.name=yi.name, vi.name=vi.name,
                                              tail=tail, give.CI=TRUE, R=R, dat=dat))
        
        p = round( as.numeric(cm$Est[ which(cm$Value=="Prop") ]), 3 )
        Gmin = round( as.numeric(cm$Est[ which(cm$Value=="Gmin") ]), 3 )
        Gmin_lo = round( as.numeric(cm$CI.lo[ which(cm$Value=="Gmin") ]), 3 )
        Gmin_hi = round( as.numeric(cm$CI.hi[ which(cm$Value=="Gmin") ]), 3 )
        
        
        ##### Create String for UI ##### 
        string_Gmin = ifelse(p < r, "Not applicable. This is already the case, even with no bias, given your pooled effect size, threshold, and choice of tail.", paste( Gmin, " (95% CI: ", Gmin_lo, ", ", Gmin_hi, ")", sep="" ))
        string_Gmin = ifelse(is.na(string_Gmin), "Cannot compute Tmin or Gmin without r. Returning only prop.", string_Gmin)
        return( string_Gmin )
        
      }) ## closes withProgress
      
    }) ## closes calibrated_text3
  }) ## closes calibrated_output
  
  calibrated_plot <- observeEvent(input$calibrated_plot, {
    output$calibrated_plot1 = renderPlot({
      withProgress(message="generating plot...", value=1,{
        ### hmm not finding these inputs from above, need to put it here too?
        ### isolate on tail to not update until action button pressed again
        if(input$calibrated_scale=="RR"){
          q = log(input$calibrated_q)
          r = input$calibrated_r
          tail = isolate(input$calibrated_tail)
          muB = input$calibrated_muB
          yi.name = input$calibrated_yi.name
          vi.name = input$calibrated_vi.name
          
          method = input$calibrated_method
          Bmin = log(input$calibrated_Bmin)
          Bmax = log(input$calibrated_Bmax)
          R = input$calibrated_R
          dat = mydata()
          
        } else {
          if(input$calibrated_scale=="Log-RR"){
            q = input$calibrated_q
            r = input$calibrated_r
            tail = isolate(input$calibrated_tail)
            muB = input$calibrated_muB
            yi.name = input$calibrated_yi.name
            vi.name = input$calibrated_vi.name
            
            method = input$calibrated_method
            Bmin = input$calibrated_Bmin
            Bmax = input$calibrated_Bmax
            R = input$calibrated_R
            dat = mydata()
          }
        }
        
        # hist(1:100, main="test Generate calibrated plot button")
        suppressWarnings(sens_plot(method=method, type="line", q=q, yi.name=yi.name, vi.name=vi.name, Bmin=Bmin, Bmax=Bmax, tail=tail, give.CI=TRUE, R=R, dat=dat ))
        
        # d = read.csv("~/Box Sync/jlee/Maya/unmeasured_confounding/metashiny/d_sens_plot.csv", stringsAsFactors = FALSE)
        # 
        # method="calibrated"
        # type = "line"
        # tail = NA
        # muB=0
        # r=0.1
        # q = log(1.1)
        # R = 250
        # CI.level = 0.95
        # 
        # give.CI=TRUE
        # dat = d
        # yi.name = "yi"
        # vi.name = "vi"
        # Bmin = log(1)
        # Bmax = log(4)
        # breaks.x1 = NA
        # breaks.x2 = NA
        
        
      }) ## closes withProgress
      
      ### output plot warnings:
      ### Shiny user will be forced to choose tail, so don't need this warning
      # output$calibrated_warning_tail = reactive({
      #   if ( !tail %in% c("above", "below") ) {
      #     calib = MetaUtility::calib_ests( yi = dat[[yi.name]], 
      #                                      sei = sqrt( dat[[vi.name]] ) )
      #     
      #     tail = ifelse( median(calib) > log(1), "above", "below" )
      #     HTML("WARNING: Assuming you want tail =", tail, "because it wasn't specified")
      #   } 
      # }) ## closes calibrated_warning_tail
      
      output$calibrated_warning_boot = reactive({
        res = data.frame( B = seq(Bmin, Bmax, .01) )
        
        # evaluate Phat causal at each value of B
        res = res %>% rowwise() %>%
          mutate( Phat = Phat_causal( q = q,
                                      B = B,
                                      tail = tail,
                                      dat = dat,
                                      yi.name = yi.name,
                                      vi.name = vi.name ) )
        
        if ( give.CI == TRUE ) {
          require(boot)
          # look at just the values of B at which Phat jumps
          #  this will not exceed the number of point estimates in the meta-analysis
          # first entry should definitely be bootstrapped, so artificially set its diff to nonzero value
          diffs = c( 1, diff(res$Phat) )
          res.short = res[ diffs != 0, ]
          
          require(dplyr)
          
          
          # bootstrap a CI for each entry in res.short
          res.short = res.short %>% rowwise() %>%
            mutate( Phat_CI_lims(.B = B,
                                 R = R,
                                 q = q,
                                 tail = tail,
                                 dat = dat,
                                 yi.name = yi.name,
                                 vi.name = vi.name,
                                 CI.level = CI.level) )
          
          # merge this with the full-length res dataframe, merging by Phat itself
          res = merge( res, res.short, by = "Phat", all.x = TRUE )
          
          res = res %>% rename( B = B.x )
          
          if ( any( res$lo > res$Phat ) | any( res$hi < res$Phat ) ) {
            HTML( paste( "Some of the pointwise confidence intervals do not contain the proportion estimate itself. This reflects instability in the bootstrapping process. See the other warnings for details." ) )
          }}
      }) ## closes calibrated_warning_boot
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
    if(input$parametric_scale=="RR"){
      yr_2 = log(input$parametric_yr)
      t2_2 = input$parametric_t2
      q_2 = log(input$parametric_q)
      vyr_2 = input$parametric_se_yr^2
      vt2_2 = (input$parametric_prop_t2*(input$parametric_t2^2))
      muB_2 = log(input$parametric_muB)
      sigB_2 = input$parametric_sigB
      r_2 = input$parametric_r
      tail_2 = input$parametric_tail
      
      method_2 = input$parametric_method
      Bmin_2 = log(input$parametric_Bmin)
      Bmax_2 = log(input$parametric_Bmax)
      
    } else {
      if(input$parametric_scale=="Log-RR"){
        yr_2 = input$parametric_yr
        t2_2 = input$parametric_t2
        q_2 = input$parametric_q
        vyr_2 = input$parametric_se_yr^2
        vt2_2 = (input$parametric_prop_t2*(input$parametric_t2^2))
        muB_2 = input$parametric_muB
        sigB_2 = input$parametric_sigB
        r_2 = input$parametric_r
        tail_2 = input$parametric_tail
        
        method_2 = input$parametric_method
        Bmin_2 = input$parametric_Bmin
        Bmax_2 = input$parametric_Bmax
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
      cm = suppressWarnings(confounded_meta(method=method_2,q=q_2, r=r_2, muB=muB_2, sigB=sigB_2, yr=yr_2, vyr=vyr_2,
                                            t2=t2_2, vt2=vt2_2, CI.level=0.95, tail=tail_2))
      
      p = round( as.numeric(cm$Est[which(cm$Value=="Prop")]), 3 )
      p_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Prop")]), 3 )
      p_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Prop")]), 3 )
      
      
      ##### Create String for UI #####
      string_p = paste( p, " (95% CI: ", p_lo, ", ", p_hi, ")", sep="" )
      return( string_p )
      
    }) ## closes parametric_text1
    
    output$parametric_text2 = renderText({
      cm = suppressWarnings(confounded_meta(method=method_2,q=q_2, r=r_2, muB=muB_2, sigB=sigB_2, yr=yr_2, vyr=vyr_2,
                                            t2=t2_2, vt2=vt2_2, CI.level=0.95, tail=tail_2))
      
      p = round( as.numeric(cm$Est[which(cm$Value=="Prop" )]), 3 )
      Tmin = round( as.numeric(cm$Est[which(cm$Value=="Tmin" )]), 3 )
      Tmin_lo = round( as.numeric(cm$CI.lo[which(cm$Value=="Tmin" )]), 3 )
      Tmin_hi = round( as.numeric(cm$CI.hi[which(cm$Value=="Tmin" )]), 3 )
      
      
      ##### Create String for UI ##### 
      string_Tmin = ifelse(p < r_2, "Not applicable. This is already the case, even with no bias, given your pooled effect size, threshold, and choice of tail.", paste( Tmin, " (95% CI: ", Tmin_lo, ", ", Tmin_hi, ")", sep="" ))
      return( string_Tmin )
      
    }) ## closes parametric_text2
    
    output$parametric_text3 = renderText({
      cm = suppressWarnings(confounded_meta(method=method_2,q=q_2, r=r_2, muB=muB_2, sigB=sigB_2, yr=yr_2, vyr=vyr_2,
                                            t2=t2_2, vt2=vt2_2, CI.level=0.95, tail=tail_2))
      
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
      cm = suppressWarnings(confounded_meta(method=method_2,q=q_2, r=r_2, muB=muB_2, sigB=sigB_2, yr=yr_2, vyr=vyr_2,
                                            t2=t2_2, vt2=vt2_2, CI.level=0.95, tail=tail_2))
      
      p = round( cm$Est[ cm$Value=="Prop" ], 3 )
      ifelse(p<0.15 | p>0.85,
             HTML(paste('WARNING: Extreme estimated proportion', 'The estimated proportion of meaningfully strong effects is <0.15 or >0.85. The methods implemented in this website do not always work well in these situations. We would recommend instead applying alternative methods that have the same interpretation (see the "More Resouces" tab).', sep = "<br/>")), "")
    }) ## closes parametric_phatwarn_2
    
  }) ## closes parametric_output
  
  parametric_plot <- observeEvent(input$parametric_plot, { 
    output$parametric_plot1 = renderPlot({
      ### hmm not finding these inputs from above, need to put it here too?
      ### isolate on tail to not update until action button pressed again
      if(input$parametric_scale=="RR"){
        yr_2 = log(input$parametric_yr)
        t2_2 = input$parametric_t2
        q_2 = log(input$parametric_q)
        vyr_2 = input$parametric_se_yr^2
        vt2_2 = (input$parametric_prop_t2*(input$parametric_t2^2))
        muB_2 = log(input$parametric_muB)
        sigB_2 = input$parametric_sigB
        r_2 = input$parametric_r
        tail_2 = isolate(input$parametric_tail)
        
        method_2 = input$parametric_method
        Bmin_2 = log(input$parametric_Bmin)
        Bmax_2 = log(input$parametric_Bmax)
        CI.level_2 = 0.95
        give.CI_2 = TRUE
        
      } else {
        if(input$parametric_scale=="Log-RR"){
          yr_2 = input$parametric_yr
          t2_2 = input$parametric_t2
          q_2 = input$parametric_q
          vyr_2 = input$parametric_se_yr^2
          vt2_2 = (input$parametric_prop_t2*(input$parametric_t2^2))
          muB_2 = input$parametric_muB
          sigB_2 = input$parametric_sigB
          r_2 = input$parametric_r
          tail_2 = isolate(input$parametric_tail)
          
          method_2 = input$parametric_method
          Bmin_2 = input$parametric_Bmin
          Bmax_2 = input$parametric_Bmax
          CI.level_2 = 0.95
          give.CI_2 = TRUE
        }
      }
      
      # hist(1:100, main="test Generate parametric plot button")
      suppressWarnings(sens_plot(method = method_2, type="line", q=q_2, yr=yr_2, vyr=vyr_2, t2=t2_2, vt2=vt2_2,
                                 Bmin=Bmin_2, Bmax=Bmax_2, sigB=sigB_2, tail=tail_2 ))
      
      ### output plot warnings:
      
      ### Shiny user will be forced to choose tail, so don't need this warning
      # output$parametric_warning_tail = reactive({
      #   if ( !tail_2 %in% c("above", "below") ) {
      #     tail_2 = ifelse( yr_2 > log(1), "above", "below" )
      #     HTML(paste("WARNING: Assuming you want tail =", tail_2, "because it wasn't specified"))
      #   }
      # }) ## closes parametric_warning_tail
      
      output$parametric_warning_ci = reactive({
        if ( is.na(vyr_2) | is.na(vt2_2) ) {
          HTML( "No confidence interval because vyr or vt2 is NULL")
        }
      }) ## closes parametric_warning_ci
      
      output$parametric_warning_phatci = reactive({
        # get mean bias factor values for a vector of B's from Bmin to Bmax
        t = data.frame( B = seq(Bmin_2, Bmax_2, .01), phat = NA, lo = NA, hi = NA )
        t$eB = exp(t$B)
        
        for ( i in 1:dim(t)[1] ) {
          # r is irrelevant here
          # suppress warnings about Phat being close to 0 or 1
          #browser()
          cm = suppressMessages( confounded_meta( method = method_2,
                                                  q = q_2,
                                                  r = NA,
                                                  muB=t$B[i],
                                                  sigB=sigB_2,
                                                  yr=yr_2,
                                                  vyr=vyr_2,
                                                  t2=t2_2,
                                                  vt2=vt2_2,
                                                  CI.level=CI.level_2,
                                                  tail=tail_2 ) )
          
          t$phat[i] = cm$Est[ cm$Value=="Prop" ]
          t$lo[i] = cm$CI.lo[ cm$Value=="Prop" ]
          t$hi[i] = cm$CI.hi[ cm$Value=="Prop" ]
        }
        
        # can't compute a CI if the bounds aren't there
        no.CI = any( is.na(t$lo) ) | any( is.na(t$hi) ) | (give.CI_2 == FALSE)
        
        if ( no.CI==FALSE ){
          HTML("Calculating parametric confidence intervals in the plot. For values of Phat that are less than 0.15 or greater than 0.85, these confidence intervals may not perform well.")
        }
      }) ## closes parametric_warning_phatci
      
      
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
  
  
  ### 10/29/20 jl think we will remove this tab, but keep the code here for now ###
  ##### For Tab Panel Range of sensitivity parameters #####
  # output$plot_3 <- renderPlot({
  #   
  #   if(input$scale_3=="RR"){
  #     yr_3 = log(input$yr_3)
  #     t2_3 = input$t2_3
  #     q_3= log(input$q_3)
  #     vyr_3 = input$se_yr_3^2
  #     vt2_3 = (input$prop_t2_3*(input$t2_3^2))
  #     sigB_3 = input$sigB_3
  #     Bmin_3 = log(input$Bmin_3)
  #     Bmax_3 = log(input$Bmax_3)
  #     tail_3 = input$tail_3
  #     
  #     method_3 = "parametric"
  #     
  #   } else {
  #     if(input$scale_3=="Log-RR"){
  #       yr_3 = input$yr_3
  #       t2_3 = input$t2_3
  #       q_3 = input$q_3
  #       vyr_3 = input$se_yr_3^2
  #       vt2_3 = (input$prop_t2_3*(input$t2_3^2))
  #       sigB_3 = input$sigB_3
  #       Bmin_3 = input$Bmin_3
  #       Bmax_3 = input$Bmax_3
  #       tail_3 = input$tail_3
  #       
  #       method_3 = "parametric"
  #     }
  #   }
  #   
  #   suppressWarnings(sens_plot(method = method_3, type="line", q=q_3, yr=yr_3, vyr=vyr_3, t2=t2_3, vt2=vt2_3,
  #                                      Bmin=Bmin_3, Bmax=Bmax_3, sigB=sigB_3, tail=tail_3 ))
  # })
  
  
  
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
#   suppressWarnings(sens_plot( type="dist", q=q_2, yr=yr_2, vyr=vyr_2, t2=t2_2, vt2=vt2_2,
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
#   suppressWarnings(sens_plot( type="line", q=q, yr=yr, vyr=vyr, t2=t2, vt2=vt2,
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


