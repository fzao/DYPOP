#
#  _______     _______   ____  _____
# |  __ \ \   / /  __ \ / __ \|  __ \
# | |  | \ \_/ /| |__) | |  | | |__) |
# | |  | |\   / |  ___/| |  | |  ___/
# | |__| | | |  | |    | |__| | |
# |_____/  |_|  |_|     \____/|_|
#
# Un outil d'aide au diagnostic de l'etat des populations de truite fario
#
# Copyright (c) EDF-IRSTEA 2018
#
# Auteurs : Fabrice Zaoui - Victor Bret
#
# Licence CeCILL v2.1
#
#

source('fig1.R')
source('fig1_p0.R')
source('fig1_p1.R')
source('fig2.R')
source('fig3.R')
source('fig3_p0.R')
source('fig3_p1.R')
source('fig1_export.R')
source('fig2_export.R')
source('fig3_export.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  ids='station_test'
  load(file=paste('data/FS.RData',sep=''))
  load(file=paste('data/FD.RData',sep=''))
  
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
    fig2_hm(ids, FS, input$slider1, input$slider2, input$slider3, X1m=input$slider_X1m, XAdm=input$slider_XAdm, type3D=input$checkbox)
  })
  
  output$exportFigDataF1a <- downloadHandler(
    filename = function() {
      paste('dataFig1a-', Sys.Date(), '.csv', sep='')
    },
      content = function(con) {
        DFish <- fig1_export(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
        write.table(DFish[,c("x0", "s0_025", "s0_25", "s0_50", "s0_75", "s0_975")], con, dec = ".", sep =";", row.names = FALSE)
        }
    )
  
  output$exportFigDataF1b <- downloadHandler(
    filename = function() {
      paste('dataFig1b-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig1_export(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
      write.table(DFish[,c("x1", "s1_025", "s1_25", "s1_50", "s1_75", "s1_975")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  output$exportFigDataF1c <- downloadHandler(
    filename = function() {
      paste('dataFig1c-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig1_export(ids, FS, input$slider1, input$slider2, input$slider3, val0=0.1)
      write.table(DFish[,c("xAd", "sAd_025", "sAd_25", "sAd_50", "sAd_75", "sAd_975")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
    
  output$exportFigDataF2a <- downloadHandler(
    filename = function() {
      paste('dataFig2a-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig2_p0_export(ids, FS, input$slider1, input$slider2, input$slider3)
      write.table(DFish[,c("x0", "s0_025", "s0_25", "s0_50", "s0_75", "s0_975")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  output$exportFigDataF2b <- downloadHandler(
    filename = function() {
      paste('dataFig2b-', Sys.Date(), '.zip', sep='')
    },
    content = function(con) {
      DFish <- fig2_hm_export(ids, FS, input$slider1, input$slider2, input$slider3, X1m=input$slider_X1m, XAdm=input$slider_XAdm, type3D=input$checkbox)
      fs <- c()
      tmpdir <- tempdir()
      workdir <- getwd()
      setwd(tmpdir)
      path <- "Densite_Sup_1_Plus.csv"
      write.table(DFish[,c("xAd", "sAd_025", "sAd_25", "sAd_50", "sAd_75", "sAd_975")], path, dec = ".", sep =";", row.names = FALSE)
      fs <- c(fs, path)
      path <- "Densite_1_Plus.csv"
      write.table(DFish[,c("x1", "s1_025", "s1_25", "s1_50", "s1_75", "s1_975")], path, dec = ".", sep =";", row.names = FALSE)
      fs <- c(fs, path)
      zip(zipfile = con, files = fs)
      setwd(workdir)
    }
  )
  
  output$exportFigDataF3a <- downloadHandler(
    filename = function() {
      paste('dataFig3a-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig3_export(ids, FD, input$slider1, input$slider2, input$slider3, XL=40, Dso=NULL)
      write.table(DFish[,c("x", "d0l", "d0m", "d0h")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  output$exportFigDataF3b <- downloadHandler(
    filename = function() {
      paste('dataFig3b-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig3_export(ids, FD, input$slider1, input$slider2, input$slider3, XL=40, Dso=NULL)
      write.table(DFish[,c("x", "d1l", "d1m", "d1h")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  output$exportFigDataF3c <- downloadHandler(
    filename = function() {
      paste('dataFig3c-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig3_export(ids, FD, input$slider1, input$slider2, input$slider3, XL=40, Dso=NULL)
      write.table(DFish[,c("x", "dAdl", "dAdm", "dAdh")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  modalTemp <- function(){
    showModal(modalDialog(
      h5("TEMPERATURES"), hr(),
      h5(strong("Des moyennes de percentiles annuels de la température de l’eau (T10 et T90) sont utilisées pour synthétiser la gamme extrême du régime thermique de la station d’étude.")),
      h5("T10 est un descripteur des températures chaudes et T90 des températures froides."),
      tags$ol(
        tags$li("T10 : moyenne interannuelle des températures dépassées 10% du temps (°C)"), 
        tags$li("T90 : moyenne interannuelle des températures dépassées 90% du temps (°C)"))
      , easyClose = TRUE, footer = NULL))
  }
  
  observeEvent(input$link1, {
    modalTemp()
  })
  
  observeEvent(input$link2, {
    modalTemp()
  })
  
  observeEvent(input$link3, {
    showModal(modalDialog(
      h5("ABRIS"), hr(),
      h5(strong("La disponibilité en abris est la somme des surfaces offrant un abri physique (berges et blocs), rapportée à la surface mouillée de la station.")),
      h5("Cette variable est issue d’une mesure de terrain qui consiste à mesurer chaque abris d’un volume minimum de 20*10*10 cm (L*l*H). On ne conserve ensuite que la surface de l'abris (hauteur non prise en compte), à 5 cm près en longueur et en largeur."),
      h5("Cette surface peut être mesurée à l’aide d’un bâton gradué légèrement flexible (utilisation d’une goulotte cache-câbles par exemple). Le décompte par type de cache (20*20, 20*30, 25*30…) permet de faciliter la prise de note et de travailler ensuite sur la distribution des abris, leur taille moyenne etc.")
      , easyClose = TRUE, footer = NULL))
    
  })
  
  observeEvent(input$redir1, {
    updateTabsetPanel(session, "tabs", selected = "apropos")
  })

  observeEvent(input$redir2, {
    updateTabsetPanel(session, "tabs", selected = "visu")
  })
  
  observeEvent(input$details, {
    showModal(modalDialog(
      img(src='Tab.jpg', width=580, align = "center"),
      easyClose = TRUE, footer = NULL))
  })
  
  observeEvent(input$gamme, {
    showModal(modalDialog(
      img(src='Densites.png', width=580, align = "center"),
      easyClose = TRUE, footer = NULL))
  })
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
  
  })
  
