library(shiny)
library(plotly)

load(file=paste('data/FS.RData',sep=''))


shinyUI(
  fluidPage(title="DYPOP",
            fluidRow(
                     column(10, align="left",
                            HTML('<h1 style="color: #FFA02F; background-color: #FFFFFF;">DYPOP</h1>'),
                            HTML('<h4 style="color: #A5C226; background-color: #FFFFFF;">Partage des résultats d\'un modèle de dynamique de population de truite</h5>')),
                     column(2, align="right",
                            img(height=40, width=80, src="edf.jpg"),
                            img(height=80, width=77, src="Irstea.png"))
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
                                       column(12, align="center",
                                                  img(src='Diapositive1.jpg', align = "center")
                                       )
                                 ), br(),
                                 fluidRow(
                                   column(12, align="center",
                                          img(src='Diapositive2.jpg', align = "center")
                                   )
                                 ), br(),
                                 fluidRow(
                                       column(12, align="left",
                                              includeMarkdown("accueil_2.md")
                                       )
                                 )
                        ),
                        # VISUALISATION ------------------------------------------------------
                        tabPanel(HTML('<h4 style="color: #005BBB; "><b>Visualisation</b></h4>'),
                                 fluidRow(
                                     includeMarkdown("visu_text_1.md")
                                     #includeMarkdown("visu_text_2.md")
                                 ),
                                 fluidRow(
                                          column(2, align = "left",
                                                  br(),
                                                  wellPanel(h4(strong("DONNEES D'ENTREE")),hr(),
                                                            actionLink(inputId = "link1", label=h5("LES TEMPERATURES CHAUDES")),
                                                            sliderInput("slider1", label = h3("T10"), min = 9., max = 17., value = 10, step = 0.5),br(),hr(),
                                                            actionLink(inputId = "link2", label=h5("LES TEMPERATURES FROIDES")),
                                                            sliderInput("slider2", label = h3("T90"), min = 1., max = 7., value = 5., step = 0.5),br(),hr(),
                                                            actionLink(inputId = "link3", label=h5("LA DISPONIBILITE EN ABRIS")),
                                                            sliderInput("slider3", label = h3("% Cache"), min = 0., max = 7., value = 3., step = 0.5)
                                                  )),
                                          column(10, align = "left",
                                                 br(),
                                                 tabsetPanel(
                                                             # Fig. 1 ---------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Survie</b></h4>'),
                                                                      includeMarkdown("survie_1.md"),
                                                                      tabsetPanel(
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">0+</h4>'), br(),
                                                                                  plotlyOutput('plot1_p0'),
                                                                                  downloadButton("exportFigDataF1a","Télécharger les taux de survie")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                  plotlyOutput('plot1_p1'),
                                                                                  downloadButton("exportFigDataF1b","Télécharger les taux de survie")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                  plotlyOutput('plot1'),
                                                                                  downloadButton("exportFigDataF1c","Télécharger les taux de survie")
                                                                        )
                                                                      ), br(),
                                                                      includeMarkdown("survie_2.md")
                                                             ),
                                                             # Fig. 2 ------------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Cohorte</b></h4>'),
                                                                      includeMarkdown("cohorte_1.md"),
                                                                      tabsetPanel(
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                  includeMarkdown("cohorte_2.md"),
                                                                                  plotlyOutput('plot2_p0'), br(),
                                                                                  includeMarkdown("cohorte_3.md")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                  includeMarkdown("cohorte_4.md"),
                                                                                  checkboxInput("checkbox", label = "vue 3D", value = FALSE),
                                                                                  plotlyOutput('plot2_hm'),br(),
                                                                                  column(5, align="center",
                                                                                      sliderInput("slider_XAdm", label = h4('Densité >1+ année précédente (n-1)'), min = 0., max = max(FS[["XAd"]]), value = FS[["XAd"]][floor(length(FS[["XAd"]])/2)], step = FS[["XAd"]][3]-FS[["XAd"]][2])),
                                                                                 column(2,
                                                                                        h4(helpText("VUES MARGINALES"))), 
                                                                                 column(5, align="center",
                                                                                      sliderInput("slider_X1m", label = h4('Densité de 1+ année précédente (n-1)'), min = 0, max = max(FS[["X1"]]), value = FS[["X1"]][floor(length(FS[["X1"]])/2)], step = FS[["X1"]][3]-FS[["X1"]][2]))
                                                                        )
                                                                      )
                                                             ),
                                                             # Fig. 3 ------------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Densité</b></h4>'),
                                                                      includeMarkdown("densite.md"),
                                                                      tabsetPanel(
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">0+</h4>'), br(),
                                                                                  plotlyOutput('plot3_p0'),
                                                                                  downloadButton("exportFigDataF3a","Télécharger les densités")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                  plotlyOutput('plot3_p1'),
                                                                                 downloadButton("exportFigDataF3b","Télécharger les densités")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                  plotlyOutput('plot3'),
                                                                                 downloadButton("exportFigDataF3c","Télécharger les densités")
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
