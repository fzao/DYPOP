library(shiny)
library(plotly)
source('fig1.R')
source('fig2.R')
source('fig3.R')

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
    fig1(ids, FS, input$slider1, input$slider2, input$slider3)
    })
  
  output$plot2 <- renderPlotly({
    fig2(ids, FD, input$slider1, input$slider2, input$slider3, XL=40, Dso=NULL)
  })
  
  output$plot3_p0 <- renderPlotly({
    fig3_p0(ids, FS, input$slider1, input$slider2, input$slider3, X1m=13.3, XAdm=23, Dso=NULL)
  })
  output$plot3_hm <- renderPlotly({
    fig3_hm(ids, FS, input$slider1, input$slider2, input$slider3, X1m=13.3, XAdm=23, Dso=NULL)
  })
})
