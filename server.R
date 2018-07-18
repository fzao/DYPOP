library(shiny)
library(plotly)
source('fig1.R')
source('fig2.R')
source('fig3.R')
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
               "cache"),
      Value = c(input$slider1, input$slider2, input$slider3))
  })

  output$value <- renderPrint({ sliderValues() })

  output$plot1 <- renderPlotly({
    fig1(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
    })
  
  output$plot2 <- renderPlotly({
    fig2(ids, FD, input$slider1, input$slider2, input$slider3, XL=40, Dso=NULL)
  })
  
  output$plot3_p0 <- renderPlotly({
    fig3_p0(ids, FS, input$slider1, input$slider2, input$slider3, Dso=NULL)
  })
  output$plot3_hm <- renderPlotly({
    fig3_hm(ids, FS, input$slider1, input$slider2, input$slider3, X1m=input$slider_X1m, XAdm=input$slider_XAdm, Dso=NULL)
  })
  
  output$exportFigData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        DFish <- fig1_export(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
        if(input$dataset=="Figure 1.a"){
          write.csv(DFish[,c("x0", "s0_025", "s0_25", "s0_50", "s0_75", "s0_975")], file, row.names = FALSE)
        }else if(input$dataset=="Figure 1.b"){
          write.csv(DFish[,c("x1", "s1_025", "s1_25", "s1_50", "s1_75", "s1_975")], file, row.names = FALSE)
        }else if(input$dataset=="Figure 1.c"){
          write.csv(DFish[,c("xAd", "sAd_025", "sAd_25", "sAd_50", "sAd_75", "sAd_975")], file, row.names = FALSE)
        }
      }
    )
    
   # showModal(modalDialog(
  #    title = "Important message",
  #    "This is an important message!"))
  })
  
