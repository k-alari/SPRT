#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# This applet was designed by Krissi Alari as apart of her senior capstone at
# Cal State Monterey Bay. This applet was designed specifically for E. coli
# testing in lettuce for a particular food testing company. The re-paramterization
# option of risk levels is only applicable for their testing purposes. The rest of
# the applet (using "Average (μ)" as the parameter of interest) is suitable for 
# general use of the SPRT.
#
# The sprt_graph_horizontal() function contains the buildPoly() function by
# Joran Elias, @joran https://stackoverflow.com/a/6809174/1870254
# and modified by @jan-glx https://stackoverflow.com/users/1870254/jan-glx 
# to shade rejection and acceptance areas in the SPRT graph.

library(shiny)
library(ggplot2)
library(tidyr)
library(dplyr)

# Define server logic 
shinyServer(function(input, output) {
  
    ## Boxplot Output
    output$plot<-renderPlot({
      
      ## Take user's data and convert to a usable format
      data = input$data.receive
      data = as.character(input$data.receive)  #Make sure it is a character
      temp = gsub( " ", "", data )  #Remove spaces
      data = as.numeric( unlist( strsplit( data, split="," ) ) )  #Split by comma and list as numeric vector
      
      ## Convert to a data frame so ggplot will work
      id=rep(1, length(data))
      df=data.frame("id"=id, "APC"=data)
      
      ## ggplot boxplot with data points jittered
      ggplot(aes(x=APC), data=df) + 
        geom_boxplot(fill="lightgreen") + theme_bw() + 
        geom_jitter(aes(y=0), height=0.1, col="darkgreen", size=3) +
        labs(x="Measurements", y="") + 
        theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
        ggtitle("Observed Data") + theme(plot.title = element_text(size=20), text = element_text(size=15))
      
    })
    
    ## SPRT graph
    output$sprtPlot <- renderPlot({
      
      ## SPRT graph for horizontal accept/reject lines
      sprt_graph_horizontal <- function(data, alpha, beta, mu0, mu1, sigma){ 
        
        ## Function to shade areas in a ggplot by creating a polygon
        buildPoly <- function(slope, intercept, above, xr, yr){
          # By Joran Elias, @joran https://stackoverflow.com/a/6809174/1870254
          # and modified by @jan-glx https://stackoverflow.com/users/1870254/jan-glx 
          # Find where the line crosses the plot edges
          yCross <- (yr - intercept) / slope
          xCross <- (slope * xr) + intercept
          
          #Build polygon by cases
          if (above & (slope >= 0)){
            rs <- data.frame(x=-Inf,y=Inf)
            if (xCross[1] < yr[1]){
              rs <- rbind(rs,c(-Inf,-Inf),c(yCross[1],-Inf))
            }
            else{
              rs <- rbind(rs,c(-Inf,xCross[1]))
            }
            if (xCross[2] < yr[2]){
              rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,Inf))
            }
            else{
              rs <- rbind(rs,c(yCross[2],Inf))
            }
          }
          if (!above & (slope >= 0)){
            rs <- data.frame(x= Inf,y= -Inf)
            if (xCross[1] > yr[1]){
              rs <- rbind(rs,c(-Inf,-Inf),c(-Inf,xCross[1]))
            }
            else{
              rs <- rbind(rs,c(yCross[1],-Inf))
            }
            if (xCross[2] > yr[2]){
              rs <- rbind(rs,c(yCross[2],Inf),c(Inf,Inf))
            }
            else{
              rs <- rbind(rs,c(Inf,xCross[2]))
            }
          }
          if (above & (slope < 0)){
            rs <- data.frame(x=Inf,y=Inf)
            if (xCross[1] < yr[2]){
              rs <- rbind(rs,c(-Inf,Inf),c(-Inf,xCross[1]))
            }
            else{
              rs <- rbind(rs,c(yCross[2],Inf))
            }
            if (xCross[2] < yr[1]){
              rs <- rbind(rs,c(yCross[1],-Inf),c(Inf,-Inf))
            }
            else{
              rs <- rbind(rs,c(Inf,xCross[2]))
            }
          }
          if (!above & (slope < 0)){
            rs <- data.frame(x= -Inf,y= -Inf)
            if (xCross[1] > yr[2]){
              rs <- rbind(rs,c(-Inf,Inf),c(yCross[2],Inf))
            }
            else{
              rs <- rbind(rs,c(-Inf,xCross[1]))
            }
            if (xCross[2] > yr[1]){
              rs <- rbind(rs,c(Inf,xCross[2]),c(Inf,-Inf))
            }
            else{
              rs <- rbind(rs,c(yCross[1],-Inf))
            }
          }
          return(rs)
        }
        GeomSection <- ggproto("GeomSection", GeomPolygon, 
                               default_aes = list(fill="blue", size=0, alpha=0.2, colour=NA, linetype="dashed"), 
                               required_aes = c("slope", "intercept", "above"),
                               draw_panel = function(data, panel_params, coord) {
                                 ranges <- coord$backtransform_range(panel_params)
                                 data$group <- seq_len(nrow(data))
                                 data <- data %>% group_by_all %>% do(buildPoly(.$slope, .$intercept, .$above, ranges$x, ranges$y)) %>% unnest
                                 GeomPolygon$draw_panel(data, panel_params, coord)
                               }
        )
        
        geom_section <- function (mapping = NULL, data = NULL, ..., slope, intercept, above, 
                                  na.rm = FALSE, show.legend = NA) {
          if (missing(mapping) && missing(slope) && missing(intercept) && missing(above)) {
            slope <- 1
            intercept <- 0
            above <- TRUE
          }
          if (!missing(slope) || !missing(intercept)|| !missing(above)) {
            if (missing(slope)) 
              slope <- 1
            if (missing(intercept)) 
              intercept <- 0
            if (missing(above)) 
              above <- TRUE
            data <- data.frame(intercept = intercept, slope = slope, above=above)
            mapping <- aes(intercept = intercept, slope = slope, above=above)
            show.legend <- FALSE
          }
          layer(data = data, mapping = mapping, stat = StatIdentity, 
                geom = GeomSection, position = PositionIdentity, show.legend = show.legend, 
                inherit.aes = FALSE, params = list(na.rm = na.rm, ...))
        }
        
        ## Note: Data needs to be imported as a single vector
        
        n = length(data)    ## Get sample size
        obs.num = 1:n       ## Store observation numbers (from 1 to n)
        test.stat = rep(NA, n)   ## Storage for test stat (likelihood ratio)
        
        ## Calculate likelihood ratios after each observation (take log)
        for (i in 1:n){
          test.stat[i] = (sum( ((data[1:i]-mu1)^2 - (data[1:i]-mu0)^2) / (2*sigma^2) ))
        }
        
        ## Upper and Lower bounds (take log)
        lower =log(  rep( beta / (1 - alpha), n) )
        upper =log( rep( (1 - beta) / alpha, n ) )
        
        ## set lower bound for ylim in ggplot (where we want the yaxis to start)
        lower_y = lower[1] - (abs(lower[1]) + abs(upper[1])) / 2
        ## set upper bound for ylim in ggplot (where we want the yaxis to end)
        upper_y = upper[1] + (abs(lower[1]) + abs(upper[1])) / 2
        
        ## Convert data into a data frame structure for easier use in ggplot
        obs.data = data.frame("obs_num" = obs.num, "APC" = data, "test_stat" = test.stat, "upper"=upper, "lower"=lower)
        
        ggplot(data=obs.data, aes(x=obs.num, y=test.stat)) + geom_point() + 
          geom_line(aes(x=obs.num, y=upper), size=1.5, color="deeppink", alpha=0.5) + 
          geom_line(aes(x=obs.num, y=lower), color="darkcyan", size=1.5, alpha=0.5) +
          labs(x="Observation Number", y="Test Statistic") + theme_bw() +
          geom_section(data=data.frame(slope=c(0,0), above=c(TRUE, FALSE), selected=c("selected","selected 2")), 
                       aes(slope=slope, above=above, intercept=c(upper[1], lower[1]), fill=selected), size=1) +
          ggtitle("Visual Representation of the SPRT") + ylim(min(lower_y, min(test.stat)), max(upper_y, max(test.stat))) +
          scale_x_continuous(n.breaks = 3) + theme(plot.title = element_text(size = 15)) + scale_fill_discrete(name = "SPRT Conclusion", labels = c("Conclude H0", "Conclude H1"))
      }
      
      ## Get user's inputs
      alpha = input$alpha
      beta = input$beta
      sigma = input$sigma
      ## Take user's data and convert to a useable format
      data = input$data.receive
      data = as.character(input$data.receive)  #Make sure it is a character
      temp = gsub( " ", "", data )  #Remove spaces
      data = as.numeric( unlist( strsplit( data, split="," ) ) )  #Split by comma and list as numeric vector
      
      ## Use the appropriate parameter of interest 
      ## (directly take in mu_0 and mu_1) 
      if(input$muCheck=="Opt1"){
        mu0 = input$mu0
        mu1 = input$mu1
      }
      
      ## or re-parameterize via risk levels
      if(input$muCheck=="Opt2"){
        r0 = input$r0
        r1 = input$r1
        
        risklevel = function(r0, r1, sigma){
          mu.grid= seq(0, 10, 0.0001) ## grid for mu values
          p = pnorm(7, mu.grid, sigma) ## take pnorm( 7, mu.grid, sigma=1.5 )
          mu0 = mu.grid[which.min(abs(p-(1-r0)))]  ## For null risk level (Prob of exceeding 7) of inputted 'r0'
          mu1 = mu.grid[which.min(abs(p-(1-r1)))]  ## For alt risk level (Prob of exceeding 7) of inputted 'r1'
          
          out = list(mu0, mu1)
          names(out) = c("mu0", "mu1")
          out
        }
        
        risk = risklevel(r0, r1, sigma)
        mu0 = risk$mu0
        mu1 = risk$mu1
      }
      
      ## Create plot
      sprt_graph_horizontal(data=data, alpha=alpha, beta=beta, mu0=mu0, mu1=mu1, sigma=sigma) + 
        theme(plot.title = element_text(size=20), text = element_text(size=15)) + 
        geom_point(size=3, col="navy") + ggtitle("SPRT")
      
    })
    
    output$distributions<-renderPlot({
      
      ## Get the user's sigma input
      sigma = input$sigma
      
      ## Get null and alt mu values based on user's preference (via mu inputs or risk level)
      if(input$muCheck=="Opt1"){
        mu0 = input$mu0
        mu1 = input$mu1
      }
      
      if(input$muCheck=="Opt2"){
        r0 = input$r0
        r1 = input$r1
        
        risklevel = function(r0, r1, sigma){
          mu.grid= seq(0, 10, 0.0001) ## grid for mu values
          p = pnorm(7, mu.grid, sigma) ## take pnorm( 7, mu.grid, sigma=1.5 )
          mu0 = mu.grid[which.min(abs(p-(1-r0)))]  ## For null risk level (Prob of exceeding 7) of inputted 'r0'
          mu1 = mu.grid[which.min(abs(p-(1-r1)))]  ## For alt risk level (Prob of exceeding 7) of inputted 'r1'
          
          out = list(mu0, mu1)
          names(out) = c("mu0", "mu1")
          out
        }
        
        risk = risklevel(r0, r1, sigma)
        mu0 = risk$mu0
        mu1 = risk$mu1
      }
      
      ## Plot null and alt distributions
      ggplot(data.frame(x = c(0, 1)), aes(x = x)) +
        stat_function(fun = dnorm, args = list(mu1, sigma), aes(color = "Alternative"), size = 2) +
        stat_function(fun = dnorm, args = list(mu0, sigma), aes(color = "Null"), size = 2) +
        xlim(min(mu0,mu1) - 3.5*sigma, max(mu0,mu1) + 3.5*sigma) + theme_bw() + ylab("Density") +
        scale_colour_brewer(palette="Accent") + xlab("Measurements") + theme(legend.title = element_blank()) +
        theme(legend.position="bottom") + theme(plot.title = element_text(size=20), text = element_text(size=15)) +
        ggtitle("Null and Alternative Distributions")
     
    })

    output$sprtRslt <- renderText({

        ## Convert inputted data into a new variable to use in the server file
        data = input$data.receive
        data = as.character(input$data.receive)  #Make sure it is a character
        temp = gsub( " ", "", data )  #Remove spaces
        data = as.numeric( unlist( strsplit( data, split="," ) ) )  #Split by comma and list as numeric vector
    
        ## Function to run the SPRT
        sprt.func = function(alpha, beta, mu0, mu1, sigma, data){
            
            ## Calculate boundaries
            lower = beta / (1 - alpha)
            upper = (1 - beta) / alpha
            
            ## get sample size
            n = length(data)
            
            ## Calculate likelihood ratio after ith observation
            test.stat = prod( (dnorm(data[1:n], mean=mu1, sd=sigma) / dnorm(data[1:n], mean=mu0, sd=sigma) ) )
            
            concl=NA ## to initialize
            
            ## Determine the conclusion
            if( test.stat < lower){
                text.concl="Given your data, the SPRT has concluded H0."
            } else if (test.stat > upper){
                text.concl="Given your data, the SPRT has concluded H1."
            } else {
                text.concl="Given your data, the SPRT is not able to make a conclusion. Make another observation and run the applet again."
            }
            
        }
        
        if(input$muCheck=="Opt1"){
          # Get inputs from user
          alpha = input$alpha
          beta = input$beta
          sigma = input$sigma
          mu0 = input$mu0
          mu1 = input$mu1
          
          conclusion = sprt.func(alpha=alpha, beta=beta, mu0=mu0, mu1=mu1, sigma=sigma, data=data)

        }
        
        if(input$muCheck=="Opt2"){
          # Get inputs from user
          alpha = input$alpha
          beta = input$beta
          sigma = input$sigma
          r0 = input$r0
          r1 = input$r1
          
          
          risklevel = function(r0, r1, sigma){
            mu.grid= seq(0, 10, 0.0001) ## grid for mu values
            p = pnorm(7, mu.grid, sigma) ## take pnorm( 7, mu.grid, sigma=1.5 )
            mu0 = mu.grid[which.min(abs(p-(1-r0)))]  ## For null risk level (Prob of exceeding 7) of inputted 'r0'
            mu1 = mu.grid[which.min(abs(p-(1-r1)))]  ## For alt risk level (Prob of exceeding 7) of inputted 'r1'
            
            out = list(mu0, mu1)
            names(out) = c("mu0", "mu1")
            out
          }
          
          risk = risklevel(r0, r1, sigma)
          
          conclusion = sprt.func(alpha=alpha, beta=beta, mu0=risk$mu0, mu1=risk$mu1, sigma=sigma, data=data)
        }

        paste(conclusion)

    })
    
    ## Heading text format
    output$headingtext <- renderText({
      paste("<B>SPRT Conclusion:</B>")
    })

})
