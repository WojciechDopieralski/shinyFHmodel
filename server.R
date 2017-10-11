
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# To DO:
# - wykresy w precision
# - prosta y = x
# - KS test


library(shiny)
library(dplyr)
library(openxlsx)
library(sae)
library(plotly)
library(DT)

shinyServer(function(input, output, session) {

  # inputs ------------------------------------------------------------------
  
  filedata <- reactive({
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    read.xlsx(xlsxFile = inFile$datapath, 
              sheet = as.numeric(input$inSheet))
    
  })
  
  filedataOut <- reactive({
    
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    
    read.xlsx(xlsxFile = inFile$datapath, 
              sheet = as.numeric(input$outSheet))
    
  })
  
  output$chooseDom <- renderUI({
    
    dataIn <- filedata()
    
    if (is.null(dataIn)) 
      return(NULL)
    
    colNames <- names(dataIn)
    
    selectizeInput(inputId = 'dom', 
                   label = 'Domain', 
                   choices = colNames, 
                   selected = colNames[1])
  })
  
  output$chooseDep <- renderUI({
    
    dataIn <-filedata()
    
    if (is.null(dataIn)) 
      return(NULL)
    
    colNames <- names(dataIn)
    
    selectizeInput(inputId = 'y', 
                   label = 'Dependent variable (y)', 
                   choices = colNames, 
                   selected = colNames[2])
  })
  
  output$chooseVariance <- renderUI({
    
    dataIn <-filedata()
    
    if (is.null(dataIn)) 
      return(NULL)
    
    colNames <- names(dataIn)
    
    selectizeInput(inputId = 'var', 
                   label = 'Variance/standard error', 
                   choices = colNames, 
                   selected = colNames[3])
    
  })
  
  output$typeVariance <- renderUI({
    
    dataIn <- filedata()
    
    if (is.null(dataIn)) 
      return(NULL)
    
    radioButtons(inputId = 'radioVar', 
                 label =  NULL, 
                 choices = c("Variance" = "var",
                             "Standard error" = "se"), 
                 selected = "se", 
                 inline = T)
  })
  
  output$chooseIndep <- renderUI({
    
    dataIn <-filedata()
    
    if (is.null(dataIn)) 
      return(NULL)
    
    colNames <- names(dataIn)
    
    selectizeInput(inputId = 'x', 
                   label = 'Independent variables (x)', 
                   choices = colNames, 
                   selected = colNames[4], 
                   multiple = TRUE)
  })
  
  output$estMethod <- renderUI({
    
    dataIn <-filedata()
    
    if (is.null(dataIn)) 
      return(NULL)
    
    radioButtons(inputId = 'radioEst', 
                 label = 'Estimation method', 
                 choices = c("REML" = "REML",
                             "ML" = "ML",
                             "FH" = "FH"), 
                 selected = "REML", inline = T)
  })

  output$downloadFileName <- renderUI({
    
    dataIn <-filedata()
    
    if (is.null(dataIn)) 
      return(NULL)
    
    textInput(inputId = 'downloadFile', label = 'Name of file with results', value = "FH_results")
  })
  
    
  output$downloadResults <- renderUI({
    
    dataIn <-filedata()
    
    if (is.null(dataIn)) 
      return(NULL)
    
    downloadButton(outputId = 'download', 
                   label = 'Download results')
    
  })
  
  # observe({
  #   
  #   inChooseDom <- input$dom
  #   inChooseDep <- input$y
  #   
  #   updateSelectizeInput(session = session, inputId = "dom", choices = colNames[!(colNames %in% inChooseDep)])
  # })

# outputs -----------------------------------------------------------------
  
  lmModel <- reactive({
    
    dataIn <- filedata()
    
    if (is.null(dataIn))
      return(NULL)
    
    yName <- input$y
    xName <- input$x
    
    formula <- as.formula(paste(yName, " ~ ", paste(xName, collapse ="+")))
    
    linearModel <- lm(formula, data = dataIn)
    
    return(linearModel)
    
  })
  
  output$lmSummary <- renderPrint({
    
    linearModel <- lmModel()

    summary(linearModel)
      
  })
  
    eblup <- reactive({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      yName <- input$y
      xName <- input$x
      vName <- input$var
      
      yData <- dataIn[[yName]]
      xData <- as.matrix(dataIn[xName])
      
      if(input$radioVar=="var"){
        vData <- dataIn[[vName]]
      } else {
        vData <- dataIn[[vName]]^2
      }
      
      fhModel <- mseFH(yData ~ xData, vardir = vData, method = input$radioEst)
      
      return(fhModel)
    })
    
    eblupOut <- reactive({
      
      if(input$outSheet!=0){
        
        dataIn <- filedata()
        dataOut <- filedataOut()
        
        if (is.null(dataOut))
          return(NULL)
        
        xName <- input$x
        vName <- input$var
        
        xDataIn <- as.matrix(dataIn[xName])
        xDataOut <- as.matrix(dataOut[xName])
        
        constIn <- rep(1, nrow(xDataIn))
        constOut <- rep(1, nrow(xDataOut))
        
        xDataConstIn <- cbind(constIn, xDataIn)
        xDataConstOut <- cbind(constOut, xDataOut)
        
        if(input$radioVar=="var"){
          vData <- dataIn[[vName]]
        } else {
          vData <- dataIn[[vName]]^2
        }
        
        fh <- eblup()
        
        eblupOut <- xDataConstOut %*% fh$est$fit$estcoef$beta
        
        sigma <- fh$est$fit$refvar
        
        gamma <- sigma/(sigma + vData)
        
        gxx <- matrix(0,ncol(xDataConstOut),ncol(xDataConstOut))
        
        for(i in 1:length(gamma)){
          gxx_i <- gamma[i]*xDataConstIn[i,]%*%t(xDataConstIn[i,])
          gxx <- gxx + gxx_i
        }
        
        gxxInv <- solve(gxx)
        
        mseOut <- rep(0, length(eblupOut))
        
        for(j in 1:length(eblupOut)){
          mseOut[j] <- sigma * t(xDataConstOut[j,]) %*% gxxInv %*% xDataConstOut[j,] + sigma
        }
        
        resultOut <- data.frame(
          dom = dataOut[[input$dom]],
          fh = eblupOut,
          fh_mse = mseOut
        )
        
        names(resultOut) <- c(input$dom, "fh", "fh_mse")
        
        return(resultOut)
        
      }
      
    })
    
    output$fhSummary <- renderPrint({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      fhModel <- eblup()
      
      fhModel$est$fit
      
    })
    
    resultsEblup <- reactive({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      fhModel <- eblup()
      
      result <- data.frame(
        dom = dataIn[[input$dom]],
        fh = fhModel$est$eblup,
        fh_mse = fhModel$mse
      )
      
      names(result) <- c(input$dom, "fh", "fh_mse")
      
      return(result)
    })
    
    resultsEblupOut <- reactive({
      
      eblupIn <- resultsEblup()
      eblupOut <- eblupOut()
      
      result <- rbind(eblupIn, eblupOut)
      
      return(result)
      
    })
    
    diagnosticsEblup <- reactive({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      fhModel <- eblup()

      if(input$radioVar=="var"){
        vData <- dataIn[[input$var]]
      } else {
        vData <- dataIn[[input$var]]^2
      }
      
      
      #synth <- as.matrix(dataIn[input$x])
      #synth <- fhModel$est$fit$estcoef$beta
      
      xDataIn <- as.matrix(dataIn[input$x])
      constIn <- rep(1, nrow(xDataIn))
      xDataConstIn <- cbind(constIn, xDataIn)

      model_diag <- data.frame(
        dom = dataIn[[input$dom]],
        ht = dataIn[[input$y]],
        ht_mse = vData,
        fh = fhModel$est$eblup,
        fh_mse = fhModel$mse,
        fh_s = as.numeric(xDataConstIn%*%fhModel$est$fit$estcoef$beta)
      ) %>%
        mutate(ht_cv=sqrt(ht_mse)/ht*100,
               fh_cv=sqrt(fh_mse)/fh*100,
               r=ht-fh,
               g=fhModel$est$fit$refvar/(fhModel$est$fit$refvar+vData),
               rs=as.numeric(scale(r)),
               u=g*(ht-fh_s),
               us=as.numeric(scale(u)),
               qr=qnorm(ppoints(length(rs)))[order(order(rs))],
               qu=qnorm(ppoints(length(us)))[order(order(us))])
      
      return(model_diag)
      
    })
    
    # output$test <- renderPrint({
    #   
    #   dataIn <- filedata()
    #   
    #   if (is.null(dataIn))
    #     return()
    #   
    #   a <- "test"
    # 
    #   a
    #   
    #   
    # })
    
    output$download <- downloadHandler(
      filename = function() {paste0(input$downloadFile, '.csv')},
      content = function(file) {

        if(input$outSheet!=0){
          resultsAll <- resultsEblupOut()
        } else{
          resultsAll <- resultsEblup()
        }
        
        write.table(resultsAll, file, sep=";", dec=",", row.names = F)
      }
    )
    
    output$eblupTable <- renderDataTable({
      
      resultsEblup()
      
    })
    
    output$stats <- renderUI({
      
      lm <- lmModel()
      
      if(!is.null(lm)){
        
        fh <- eblup()
        
        r2 <- paste("Linear R-square:", round(summary(lm)$r.squared*100,2), "%")
        sigma <- paste("Random effects variance:", round(fh$est$fit$refvar,4))
        aic <- paste("AIC:", round(fh$est$fit$goodness[[2]],4))
        
        HTML(paste(r2, sigma, aic, sep = '<br/>')) 
      }
      
    })
    
    output$betaCoeff <- renderTable({
      
      lm <- lmModel()
      
      if(!is.null(lm)){
        
        fh <- eblup()
        
        fhBeta <- fh$est$fit$estcoef
        row.names(fhBeta) <- names(lm$coefficients)
        fhBeta
      }
    }, include.rownames=T)
    
    
    output$randomErrors <- renderPlotly({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      diag <- diagnosticsEblup()
      
      if(!is.null(diag)){
        
        plot_ly(data = diag, type = "scatter", x = ~qr, y = ~rs, mode = "markers",
                text = ~paste("Domain: ", dom,
                              "<br>HT: ", round(ht,2),
                              "<br>FH: ", round(fh,2))) %>%
          layout(xaxis = list(title = "Theoretical quantiles",
                              zeroline = F),
                 yaxis = list(title = "Standardized residuals",
                              zeroline = F),
                 title = "Residuals") %>%
          add_trace(x = ~qr, y = ~fitted(lm(rs ~ qr)), mode = "lines", showlegend = F)

      }
      
    })
    
    output$randomEffects <- renderPlotly({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      diag <- diagnosticsEblup()
      
      if(!is.null(diag)){
        
        plot_ly(data = diag, type = "scatter", x = ~qu, y = ~us, mode = "markers",
                text = ~paste("Domain: ", dom,
                              "<br>HT: ", round(ht,2),
                              "<br>FH: ", round(fh,2))) %>%
          layout(xaxis = list(title = "Theoretical quantiles",
                              zeroline = F),
                 yaxis = list(title = "Standardized effect",
                              zeroline = F),
                 title = "Random effects") %>%
          add_trace(x = ~qu, y = ~fitted(lm(us ~ qu)), mode = "lines", showlegend = F)
        
      }
      
    })
    
    output$randomErrorsNorm <- renderUI({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      diag <- diagnosticsEblup()
      
      if(!is.null(diag)){
        
        ks <- ks.test(diag$rs, diag$qr)
        
        d <- paste("D = ", round(as.numeric(ks$statistic),4))
        p <- paste("p-value = ", round(as.numeric(ks$p.value),4))
        
        HTML(paste(paste0(ks$method," of residuals"), d, p, sep = '<br/>'))
        
      }
      
    })
    
    output$randomEffectsNorm <- renderUI({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      diag <- diagnosticsEblup()
      
      if(!is.null(diag)){
        
        ks <- ks.test(diag$us, diag$qu)
        
        d <- paste("D = ", round(as.numeric(ks$statistic),4))
        p <- paste("p-value = ", round(as.numeric(ks$p.value),4))
        
        HTML(paste(paste0(ks$method," of random effects"), d, p, sep = '<br/>'))
        
      }
      
    })
    
    output$mseHtmseFh <- renderPlotly({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      diag <- diagnosticsEblup()
      
      if(!is.null(diag)){
        
        plot_ly(data = diag, type = "scatter", x = ~ht_cv, y = ~fh_cv, mode = "markers",
                text = ~paste("Domain: ", dom,
                              "<br>HT: ", round(ht,2),
                              "<br>FH: ", round(fh,2))) %>%
          layout(xaxis = list(title = "Direct CV (in %)",
                              zeroline = F),
                 yaxis = list(title = "EBLUP CV (in %)",
                              zeroline = F))
        
      }
      
    })
    
    output$gammaMseHt <- renderPlotly({
      
      dataIn <- filedata()
      
      if (is.null(dataIn))
        return(NULL)
      
      diag <- diagnosticsEblup()
      
      if(!is.null(diag)){
        
        plot_ly(data = diag, type = "scatter", x = ~ht_cv, y = ~g, mode = "markers",
                text = ~paste("Domain: ", dom,
                              "<br>HT: ", round(ht,2),
                              "<br>FH: ", round(fh,2))) %>%
          layout(xaxis = list(title = "Direct CV (in %)",
                              zeroline = F),
                 yaxis = list(title = "Gamma",
                              zeroline = F))
        
      }
      
    })
  
})
