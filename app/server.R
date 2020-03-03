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
# Copyright (c) EDF-INRAE 2018-2020
#
# Auteurs : Fabrice Zaoui - Victor Bret
#
# Licence CeCILL v2.1
#
#

source('src/fig1_en.R')
source('src/fig1_p0_en.R')
source('src/fig1_p1_en.R')
source('src/fig2_en.R')
source('src/fig3_en.R')
source('src/fig3_p0_en.R')
source('src/fig3_p1_en.R')
source('src/fig1.R')
source('src/fig1_p0.R')
source('src/fig1_p1.R')
source('src/fig2.R')
source('src/fig3.R')
source('src/fig3_p0.R')
source('src/fig3_p1.R')
source('src/fig1_export.R')
source('src/fig2_export.R')
source('src/fig3_export.R')


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  ids='station_test'
  if(Sys.info()["sysname"] == "Darwin"){
    load(file=paste('../data/FS.RData',sep=''))
    load(file=paste('../data/FD.RData',sep=''))
  }else{
    load(file=paste('/home/dypop/data/FS.RData',sep=''))
    load(file=paste('/home/dypop/data/FD.RData',sep=''))
  }

  sliderValues <- reactive({
    data.frame(
      Name = c("t10",
               "t90",
               "cache"))
  })
  
  # English part
  output$plot1_p0_en <- renderPlotly({
    fig1_p0_en(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en, val0=0.1)
  })
  
  output$plot1_p1_en <- renderPlotly({
    fig1_p1_en(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en, val0=0.1)
  })
  
  output$plot1_en <- renderPlotly({
    fig1_en(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en, val0=0.1)
    })
  
  output$plot3_p0_en <- renderPlotly({
    fig3_p0_en(ids, FD, input$slider1_en, input$slider2_en, input$slider3_en, XL=40, Dso=NULL)
  })
  
  output$plot3_p1_en <- renderPlotly({
    fig3_p1_en(ids, FD, input$slider1_en, input$slider2_en, input$slider3_en, XL=40, Dso=NULL)
  })
  
  output$plot3_en <- renderPlotly({
    fig3_en(ids, FD, input$slider1_en, input$slider2_en, input$slider3_en, XL=40, Dso=NULL)
  })
  
  output$plot2_p0_en <- renderPlotly({
    fig2_p0_en(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en)
  })
  
  output$plot2_hm_en <- renderPlotly({
    fig2_hm_en(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en, X1m=input$slider_X1m_en, XAdm=input$slider_XAdm_en, type3D=input$checkbox_en)
  })
  
  output$exportFigDataF1a_en <- downloadHandler(
    filename = function() {
      paste('dataFig1a-', Sys.Date(), '.csv', sep='')
    },
      content = function(con) {
        DFish <- fig1_export(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en, val0=0.1)
        write.table(DFish[,c("x0", "s0_025", "s0_25", "s0_50", "s0_75", "s0_975")], con, dec = ".", sep =";", row.names = FALSE)
        }
    )
  
  output$exportFigDataF1b_en <- downloadHandler(
    filename = function() {
      paste('dataFig1b-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig1_export(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en, val0=0.1)
      write.table(DFish[,c("x1", "s1_025", "s1_25", "s1_50", "s1_75", "s1_975")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  output$exportFigDataF1c_en <- downloadHandler(
    filename = function() {
      paste('dataFig1c-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig1_export(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en, val0=0.1)
      write.table(DFish[,c("xAd", "sAd_025", "sAd_25", "sAd_50", "sAd_75", "sAd_975")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
    
  output$exportFigDataF2a_en <- downloadHandler(
    filename = function() {
      paste('dataFig2a-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig2_p0_export(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en)
      write.table(DFish[,c("x0", "s0_025", "s0_25", "s0_50", "s0_75", "s0_975")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  output$exportFigDataF2b_en <- downloadHandler(
    filename = function() {
      paste('dataFig2b-', Sys.Date(), '.zip', sep='')
    },
    content = function(con) {
      DFish <- fig2_hm_export(ids, FS, input$slider1_en, input$slider2_en, input$slider3_en, X1m=input$slider_X1m_en, XAdm=input$slider_XAdm_en, type3D=input$checkbox_en)
      fs <- c()
      tmpdir <- tempdir()
      workdir <- getwd()
      setwd(tmpdir)
      path <- "Density_Sup_1_Plus.csv"
      write.table(DFish[,c("xAd", "sAd_025", "sAd_25", "sAd_50", "sAd_75", "sAd_975")], path, dec = ".", sep =";", row.names = FALSE)
      fs <- c(fs, path)
      path <- "Density_1_Plus.csv"
      write.table(DFish[,c("x1", "s1_025", "s1_25", "s1_50", "s1_75", "s1_975")], path, dec = ".", sep =";", row.names = FALSE)
      fs <- c(fs, path)
      zip(zipfile = con, files = fs)
      setwd(workdir)
    }
  )
  
  output$exportFigDataF3a_en <- downloadHandler(
    filename = function() {
      paste('dataFig3a-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig3_export(ids, FD, input$slider1_en, input$slider2_en, input$slider3_en, XL=40, Dso=NULL)
      write.table(DFish[,c("x", "d0l", "d0m", "d0h")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  output$exportFigDataF3b_en <- downloadHandler(
    filename = function() {
      paste('dataFig3b-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig3_export(ids, FD, input$slider1_en, input$slider2_en, input$slider3_en, XL=40, Dso=NULL)
      write.table(DFish[,c("x", "d1l", "d1m", "d1h")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  output$exportFigDataF3c_en <- downloadHandler(
    filename = function() {
      paste('dataFig3c-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      DFish <- fig3_export(ids, FD, input$slider1_en, input$slider2_en, input$slider3_en, XL=40, Dso=NULL)
      write.table(DFish[,c("x", "dAdl", "dAdm", "dAdh")], con, dec = ".", sep =";", row.names = FALSE)
    }
  )
  
  modalTemp_en <- function(){
    showModal(modalDialog(
      h5("TEMPERATURES"), hr(),
      h5(strong("Annual water temperature percentile means (T10 and T90) are used to synthesize the extreme range of the thermal regime of the study station.")),
      h5("T10 is a descriptor of hot temperatures and T90 of cold temperatures."),
      tags$ol(
        tags$li("T10: interannual average of temperatures exceeded 10% of the time (°C)"), 
        tags$li("T90: interannual average of temperatures exceeded 90% of the time (°C)"))
      , easyClose = TRUE, footer = NULL))
  }
  
  observeEvent(input$link1_en, {
    modalTemp_en()
  })
  
  observeEvent(input$link2_en, {
    modalTemp_en()
  })
  
  observeEvent(input$link3_en, {
    showModal(modalDialog(
      h5("SHELTERS"), hr(),
      h5(strong("Shelter availability is the sum of the areas offering physical shelter (banks and blocks), relative to the wet area of the station.")),
      h5("This variable comes from a field measurement which consists of measuring each shelter with a minimum volume of 20*10*10 cm3 (L*W*H). We then only keep the surface of the shelter (height not taken into account), to within 5 cm in length and width."),
      h5("This surface can be measured using a slightly flexible graduated stick (use of a cable cover chute, for example). The count by type of cache (20*20, 20*30, 25*30, etc.) makes it easier to take notes and then work on the distribution of shelters, their average size, etc.")
      , easyClose = TRUE, footer = NULL))
    
  })
  
  observeEvent(input$redir1_en, {
    updateTabsetPanel(session, "tabs_en", selected = "apropos_en")
  })

  observeEvent(input$redir2_en, {
    updateTabsetPanel(session, "tabs_en", selected = "visu_en")
  })
  
  observeEvent(input$details_en, {
    showModal(modalDialog(
      img(src='Tab_en.jpg', width=580, align = "center"),
      easyClose = TRUE, footer = NULL))
  })
  
  observeEvent(input$gamme_en, {
    showModal(modalDialog(
      img(src='Density.png', width=580, align = "center"),
      easyClose = TRUE, footer = NULL))
  })
  
  # French part
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
      path <- "Density_Sup_1_Plus.csv"
      write.table(DFish[,c("xAd", "sAd_025", "sAd_25", "sAd_50", "sAd_75", "sAd_975")], path, dec = ".", sep =";", row.names = FALSE)
      fs <- c(fs, path)
      path <- "Density_1_Plus.csv"
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
      h5(strong("Des moyennes de percentiles annuels de la température de l'eau (T10 et T90) sont utilisées pour sythétiser la gamme extrême du régime thermique de la station d'étude.")),
      h5("T10 est un descripteur des températures chaudes et T90 des tempérautres froides."),
      tags$ol(
        tags$li("T10: moyenne interannuelle des températures dépassées 10% du temps (°C)"), 
        tags$li("T90: moyenne interannuelle des températures dépassées 90% du temps (°C)"))
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
      h5("Cette variable est issue d'une mesure de terrain qui consiste à mesurer chaque abris d'un volume minimum 20*10*10 cm3 (L*l*H). On ne conserve ensuite que la surface de l'abris (hauteur non prise en compte), à 5 cm près en longueur et en largeur."),
      h5("Cette surface peut être mesurée à l'aide d'un bâton gradué légèrement flexible (utilisation d'une goulotte cache-câbles par exemple). Le décompte par type de cache (20*20, 20*30, 25*30...) permet de faciliter la prise de note et de travailler ensuite sur la distribution des abris, leur taille moyenne etc.")
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
  
