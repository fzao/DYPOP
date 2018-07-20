library(shiny)
library(plotly)

load(file=paste('data/FS.RData',sep=''))


shinyUI(
  fluidPage(title="DYPOP",
            fluidRow(
                     column(2, align="left",
                            HTML('<h1 style="color: #FFA02F; background-color: #FFFFFF;">DYPOP</h1>')),
                     column(10, align="right",
                            img(height=100, width=65, src="EDF_RetD.png"),
                            img(height=100, width=97, src="Irstea.png"))
            ),
            tabsetPanel(
                        # ACCUEIL ---------------------------------------------------
                        tabPanel(HTML('<h4 style="color: #005BBB; "><b>Accueil</b></h4>'),
                                 fluidRow(
                                 )
                        ),
                        # DATA ------------------------------------------------------
                        tabPanel(HTML('<h4 style="color: #005BBB; "><b>Visualisation</b></h4>'),
                                 fluidRow(
                                          column(2, align = "left",
                                                 br(),br(),
                                                  wellPanel(h4(strong("PARAMETRES")),sliderInput("slider1", label = h3("T 10"), min = 9., max = 17., value = 10, step = 1.),
                                                  sliderInput("slider2", label = h3("T 90"), min = 1., max = 7., value = 5., step = 1.),
                                                  sliderInput("slider3", label = h3("% Cache"), min = 0., max = 7., value = 3., step = 1.),
                                                  verbatimTextOutput("value"),
                                                  br(),
                                                  selectInput("dataset", h3("Choisir une figure:"), choices = c("Figure 1.a", "Figure 1.b", "Figure 1.c"), width="100%"),
                                                  downloadButton("exportFigData",h5(strong("Download")), width="200%"))),
                                          column(10, align = "center",
                                                 br(),br(),br(),
                                                 tabsetPanel(
                                                             # Fig. 1 ---------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #005BBB; "><b>Figure 1</b></h4>'),
                                                                      plotlyOutput('plot1')
                                                             ),
                                                             # Fig. 2 ------------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #005BBB; "><b>Figure 2</b></h4>'),
                                                                      plotlyOutput('plot2')
                                                             ),
                                                             # Fig. 3 ------------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #005BBB; "><b>Figure 3</b></h4>'),
                                                                      plotlyOutput('plot3_p0'),
                                                                      plotlyOutput('plot3_hm'),
                                                                      column(6, 
                                                                             sliderInput("slider_XAdm", label = h3("D[>1+, y-1]"), min = 0., max = max(FS[["XAd"]]), value = FS[["XAd"]][floor(length(FS[["XAd"]])/2)], step = FS[["XAd"]][3]-FS[["XAd"]][2])),
                                                                      column(6, 
                                                                             sliderInput("slider_X1m", label = h3("D[1+, y-1]"), min = 0, max = max(FS[["X1"]]), value = FS[["X1"]][floor(length(FS[["X1"]])/2)], step = FS[["X1"]][3]-FS[["X1"]][2]))
                                                             )
                                                 )
                                          )
                                  )
                        ),
                        # A PROPOS ----------------------------------------------------
                        tabPanel(HTML('<h4 style="color: #005BBB; "><b>A propos</b></h4>'),
                                 fluidRow(
                                         )
                        )
          )
    )
)
