library(shiny)
library(plotly)
source('fig1.R')
source('fig1_p0.R')
source('fig1_p1.R')
source('fig2.R')
source('fig3.R')
source('fig3_p0.R')
source('fig3_p1.R')
source('fig1_export.R')

ids='station_test'
load(file=paste('data/FS.RData',sep=''))
load(file=paste('data/FD.RData',sep=''))

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  sliderValues <- reactive({
    data.frame(
      Name = c("t10",
               "t90",
               "cache"))
  })

  output$plot1_p0 <- renderPlotly({
    fig1_p0(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
  })
  
  output$plot1_p1 <- renderPlotly({
    fig1_p1(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
  })
  
  output$plot1 <- renderPlotly({
    fig1(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
    })
  
  output$plot3_p0 <- renderPlotly({
    fig3_p0(ids, FD, input$slider1, input$slider2, input$slider3, XL=40, Dso=NULL)
  })
  
  output$plot3_p1 <- renderPlotly({
    fig3_p1(ids, FD, input$slider1, input$slider2, input$slider3, XL=40, Dso=NULL)
  })
  
  output$plot3 <- renderPlotly({
    fig3(ids, FD, input$slider1, input$slider2, input$slider3, XL=40, Dso=NULL)
  })
  
  output$plot2_p0 <- renderPlotly({
    fig2_p0(ids, FS, input$slider1, input$slider2, input$slider3)
  })
  
  output$plot2_hm <- renderPlotly({
    fig2_hm(ids, FS, input$slider1, input$slider2, input$slider3, X1m=input$slider_X1m, XAdm=input$slider_XAdm, type='heatmap')
  })
  
  output$exportFigDataF1a <- downloadHandler(
    filename = function() {
      paste('dataFig1a-', Sys.Date(), '.csv', sep='')
    },
      content = function(con) {
        DFish <- fig1_export(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
        write.csv(DFish[,c("x0", "s0_025", "s0_25", "s0_50", "s0_75", "s0_975")], con, row.names = FALSE)
        }
    )
  
  output$exportFigDataF1b <- downloadHandler(
    filename = function() {
      paste('dataFig1b-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig1_export(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
      write.csv(DFish[,c("x1", "s1_025", "s1_25", "s1_50", "s1_75", "s1_975")], con, row.names = FALSE)
    }
  )
  
  output$exportFigDataF1c <- downloadHandler(
    filename = function() {
      paste('dataFig1c-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig1_export(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
      write.csv(DFish[,c("xAd", "sAd_025", "sAd_25", "sAd_50", "sAd_75", "sAd_975")], con, row.names = FALSE)
    }
  )
    
   # showModal(modalDialog(
  #    title = "Important message",
  #    "This is an important message!"))
  })
  
