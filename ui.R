library(shiny)
library(plotly)

load(file=paste('data/FS.RData',sep=''))


shinyUI(
  fluidPage(title="DYPOP",
            fluidRow(
                     column(4, align="left",
                            HTML('<h1 style="color: #FFA02F; background-color: #FFFFFF;">DYPOP</h1>'),
                            HTML('<h4 style="color: #A5C226; background-color: #FFFFFF;">Partage des résultats d\'un modèle de dynamique de population de truite</h5>')),
                     column(8, align="right",
                            img(height=100, width=65, src="EDF_RetD.png"),
                            img(height=100, width=97, src="Irstea.png"))
            ),
            tabsetPanel(
                        # ACCUEIL ---------------------------------------------------
                        tabPanel(HTML('<h4 style="color: #005BBB; "><b>Accueil</b></h4>'),
                                 br(),
                                 fluidRow(
                                       column(12, align="left",
                                                  includeMarkdown("accueil_1.md")
                                       )
                                 ), br(),
                                 fluidRow(
                                       column(6, align="center",
                                                  img(src='Diapositive1.jpg', width = 800, align = "center")
                                       ),
                                       column(6, align="center",
                                                  img(src='Diapositive2.jpg', width = 800, align = "center")
                                       )
                                   ), br(),
                                 fluidRow(
                                       column(12, align="left",
                                              includeMarkdown("accueil_2.md")
                                       )
                                 )
                        ),
                        # DATA ------------------------------------------------------
                        tabPanel(HTML('<h4 style="color: #005BBB; "><b>Visualisation</b></h4>'),
                                 fluidRow(
                                          column(2, align = "left",
                                                 br(),br(),
                                                  wellPanel(h4(strong("PARAMETRES")),
                                                            sliderInput("slider1", label = h3("T 10"), min = 9., max = 17., value = 10, step = 1.),
                                                            sliderInput("slider2", label = h3("T 90"), min = 1., max = 7., value = 5., step = 1.),
                                                            sliderInput("slider3", label = h3("% Cache"), min = 0., max = 7., value = 3., step = 1.)
                                                  )),
                                          column(10, align = "left",
                                                 br(),br(),br(),
                                                 tabsetPanel(
                                                             # Fig. 1 ---------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Survie</b></h4>'),
                                                                      tabsetPanel(
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">0+</h4>'), br(),
                                                                                  plotlyOutput('plot1_p0'),
                                                                                  downloadButton("exportFigDataF1a","Download")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                  plotlyOutput('plot1_p1'),
                                                                                  downloadButton("exportFigDataF1b","Download")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">Adults</h4>'), br(),
                                                                                  plotlyOutput('plot1'),
                                                                                  downloadButton("exportFigDataF1c","Download")
                                                                        )
                                                                      )
                                                             ),
                                                             # Fig. 2 ------------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Cohorte</b></h4>'),
                                                                      tabsetPanel(
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                  plotlyOutput('plot2_p0')
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                  checkboxInput("checkbox", label = "vue 3D", value = FALSE),
                                                                                  plotlyOutput('plot2_hm'),br(),
                                                                                  column(5, align="center",
                                                                                      sliderInput("slider_XAdm", label = h4('D[>1+,y-1] (right margin)'), min = 0., max = max(FS[["XAd"]]), value = FS[["XAd"]][floor(length(FS[["XAd"]])/2)], step = FS[["XAd"]][3]-FS[["XAd"]][2])),
                                                                                 column(2,
                                                                                        h4(helpText("MARGINAL VIEWS"))), 
                                                                                 column(5, align="center",
                                                                                      sliderInput("slider_X1m", label = h4('D[1+,y-1] (top margin)'), min = 0, max = max(FS[["X1"]]), value = FS[["X1"]][floor(length(FS[["X1"]])/2)], step = FS[["X1"]][3]-FS[["X1"]][2]))
                                                                        )
                                                                      )
                                                             ),
                                                             # Fig. 3 ------------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Densité</b></h4>'),
                                                                      tabsetPanel(
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">0+</h4>'), br(),
                                                                                  plotlyOutput('plot3_p0')
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                  plotlyOutput('plot3_p1')
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                  plotlyOutput('plot3')
                                                                        )
                                                                      )
                                                             )
                                                 )
                                          )
                                  )
                        ),
                        # A PROPOS ----------------------------------------------------
                        tabPanel(HTML('<h4 style="color: #005BBB; "><b>A propos</b></h4>'),
                                 br(),
                                 fluidRow(
                                   column(12, align="left",
                                          includeMarkdown("a_propos_1.md")
                                   )
                                 ), br(),
                                 fluidRow(
                                   column(12, align="center",
                                          img(src='Map.jpg', width=600, align = "center")
                                   )
                                 ), br(),
                                 fluidRow(
                                   column(12, align="left",
                                          includeMarkdown("a_propos_2.md")
                                   )
                                 ),
                                 fluidRow(
                                   column(12, align="center",
                                          img(src='Tab.jpg', width=800, align = "center")
                                   )
                                 ), br(),
                                 fluidRow(
                                   column(12, align="left",
                                          includeMarkdown("a_propos_3.md")
                                   )
                                 )
                              )
                        )
  )
)
