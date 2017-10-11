
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(openxlsx)
library(plotly)
library(DT)

shinyUI(fluidPage(

  titlePanel("Fay-Herriot model"),
  sidebarLayout(
    sidebarPanel(
      helpText(a(href="https://github.com/lwawrowski/shinyFHmodel/raw/master/testData.xlsx", target="_blank", "Download test data")),
      fileInput('file', 'Choose XLSX file',
                accept=c('.xlsx')),
      selectInput('inSheet', 'Sheet number with in-sample domains',
                  c("1" = 1,
                    "2" = 2,
                    "3" = 3,
                    "4" = 4,
                    "5" = 5),
                  1),
      selectInput('outSheet', 'Sheet number with out-of-sample domains',
                  c("None" = 0,
                    "1" = 1,
                    "2" = 2,
                    "3" = 3,
                    "4" = 4,
                    "5" = 5),
                  0),
      tags$hr(),
      uiOutput("chooseDom"),
      uiOutput("chooseDep"),
      uiOutput("chooseIndep"),
      uiOutput("chooseVariance"),
      uiOutput("typeVariance"),
      uiOutput("estMethod"),
      uiOutput("downloadFileName"),
      uiOutput("downloadResults")
    ),
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel("Diagnostics",
                 fluidRow(
                   column(4, h4("Goodness of model"), htmlOutput("stats")),
                   column(8, h4("Beta coefficients"), tableOutput("betaCoeff"))
                 ),
                 # verbatimTextOutput("test"),
                 h4("Diagnostics"),
                 fluidRow(
                   column(6, plotlyOutput("randomErrors", width = 400, height = 400)),
                   column(6, plotlyOutput("randomEffects", width = 400, height = 400))
                 ),
                 fluidRow(
                   column(6, htmlOutput("randomErrorsNorm")),
                   column(6, htmlOutput("randomEffectsNorm"))
                 )
                 ),
        tabPanel("Precision",
                 fluidRow(
                   column(6, plotlyOutput("gammaMseHt", width = 400, height = 400)),
                   column(6, plotlyOutput("mseHtmseFh", width = 400, height = 400))
                 )
                 ),
        tabPanel("Results",
                 dataTableOutput('eblupTable')
                 ),
        tabPanel("Function's summaries",
                 h4("Linear regression summary"),
                 verbatimTextOutput("lmSummary"),
                 h4("Fay-Herriot model summary"),
                 verbatimTextOutput("fhSummary")
                 )
      )
    )
  )
))
