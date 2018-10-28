library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(plotly)

# WARINING : Some constant values to change with new data !!!
#load(file=paste('data/FS.RData',sep=''))
#Amax <- max(FS[["XAd"]])
Amax <- 35.
#Avalue <- FS[["XAd"]][floor(length(FS[["XAd"]])/2)]
Avalue <- 17.4
#Astep <- FS[["XAd"]][3]-FS[["XAd"]][2]
Astep <- 0.2
#Bmax <- max(FS[["X1"]])
Bmax <- 70.
#Bvalue <- FS[["X1"]][floor(length(FS[["X1"]])/2)]
Bvalue <- 34.8
#Bstep <- FS[["X1"]][3]-FS[["X1"]][2]
sBstep <- 0.4


appCSS <- "
#loading-content {
  position: absolute;
  background: #FFFFFF;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFA02F;
}
"

shinyUI(
  fluidPage(title="DYPOP",
            useShinyjs(),
            inlineCSS(appCSS),
            # Loading message
            div(
              id = "loading-content",
              h1("Chargement DYPOP...") %>% withSpinner(type=8, color="#A5C226")

            ),
            
            hidden(
              div(
                id = "app-content",
            
            fluidRow(
                     column(10, align="left",
                            HTML('<h1 style="color: #FFA02F; background-color: #FFFFFF;">DYPOP</h1>'),
                            HTML('<h4 style="color: #A5C226; background-color: #FFFFFF;">Un outil d\'aide au diagnostic de l\'état des populations de truite fario</h5>')),
                     column(2, align="right",
                            img(height=40, width=80, src="edf.jpg"),
                            img(height=80, width=77, src="Irstea.png"))
            ),
            tabsetPanel(id="tabs",
                        # ACCUEIL ---------------------------------------------------
                        tabPanel(value="accueil", HTML('<h4 style="color: #005BBB; "><b>Accueil</b></h4>'),
                                 br(),
                                 fluidRow(
                                   column(4, align="left",
                                          includeMarkdown("accueil_1.md"),
                                          actionLink(inputId = "redir1", label=HTML('<h4 style="color: #005BBB; ">=> Pour en savoir plus, se rendre sur l\'onglet \"A propos\"</h4>')),
                                          includeMarkdown("accueil_2.md"),
                                          actionLink(inputId = "redir2", label=HTML('<h4 style="color: #005BBB; ">=> Pour utiliser DYPOP, se rendre sur l\'onglet \"Visualisation\"</h4>')),
                                          includeMarkdown("accueil_3.md")
                                          ),
                                   column(4, align="center",
                                          HTML('<a href="Diapositive1.jpg" target="new">
                                            <img src="Diapositive1.jpg" alt="Life cycle" width="600" height="450">
                                            </a>'),br(),br(),br(),
                                          img(width = 300, src='Diapositive2.jpg', align = "center")
                                          ))
                        ),
                        # VISUALISATION ------------------------------------------------------
                        tabPanel(value="visu", HTML('<h4 style="color: #005BBB; "><b>Visualisation</b></h4>'),
                                 fluidRow(
                                     includeMarkdown("visu_text_1.md")
                                     #includeMarkdown("visu_text_2.md")
                                 ),
                                 fluidRow(
                                          column(2, align = "left",
                                                  br(),
                                                  wellPanel(h4(strong("DONNEES D'ENTREE")),hr(),
                                                            actionLink(inputId = "link1", label=h5("LES TEMPERATURES CHAUDES")),
                                                            sliderInput("slider1", label = h3("T10"), min = 9., max = 17., value = 10, step = 1.),br(),hr(),
                                                            actionLink(inputId = "link2", label=h5("LES TEMPERATURES FROIDES")),
                                                            sliderInput("slider2", label = h3("T90"), min = 1., max = 7., value = 5., step = 1.),br(),hr(),
                                                            actionLink(inputId = "link3", label=h5("LA DISPONIBILITE EN ABRIS")),
                                                            sliderInput("slider3", label = h3("% Abris"), min = 0., max = 7., value = 3., step = 1.)
                                                  )),
                                          column(10, align = "left",
                                                 br(),
                                                 tabsetPanel(
                                                             # Fig. 1 ---------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Survie</b></h4>'),
                                                                      includeMarkdown("survie_1.md"),
                                                                      tabsetPanel(
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">0+</h4>'), br(),
                                                                                  plotlyOutput('plot1_p0') %>% withSpinner(type=8, color="#A5C226"),
                                                                                  downloadButton("exportFigDataF1a","Télécharger les taux de survie")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                  plotlyOutput('plot1_p1') %>% withSpinner(type=8, color="#A5C226"),
                                                                                  downloadButton("exportFigDataF1b","Télécharger les taux de survie")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                  plotlyOutput('plot1') %>% withSpinner(type=8, color="#A5C226"),
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
                                                                                  plotlyOutput('plot2_p0') %>% withSpinner(type=8, color="#A5C226"),
                                                                                  downloadButton("exportFigDataF2a","Télécharger les densités"), br(), br(),
                                                                                  includeMarkdown("cohorte_3.md")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                  includeMarkdown("cohorte_4.md"),
                                                                                  #checkboxInput("checkbox", label = "vue 3D", value = FALSE),
                                                                                  switchInput(inputId = "checkbox", label = "Vue 3D", value = FALSE, handleWidth = 50, size = 'mini'),
                                                                                  plotlyOutput('plot2_hm') %>% withSpinner(type=8, color="#A5C226"),
                                                                                  downloadButton("exportFigDataF2b","Télécharger les densités"), br(),
                                                                                  column(5, align="center",
                                                                                      sliderInput("slider_XAdm", label = h4('Densité >1+ année (n-1)'), min = 0., max = Amax, value = Avalue, step =Astep)),
                                                                                      #knobInput(inputId = "slider_XAdm", label = h4('Densité >1+ année (n-1)'),min = 0., max = Amax, value = Avalue, step =Astep, displayPrevious = TRUE, lineCap = "round",fgColor = "#428BCA",inputColor = "#428BCA")),
                                                                                  column(2,
                                                                                        h4(helpText("VUES MARGINALES"))), 
                                                                                  column(5, align="center",
                                                                                      sliderInput("slider_X1m", label = h4('Densité 1+ année (n-1)'), min = 0, max = Bmax, value = Bvalue, step = Bstep))
                                                                                      #knobInput(inputId = "slider_X1m", label = h4('Densité 1+ année (n-1)'),min = 0., max = Bmax, value = Bvalue, step =Bstep, displayPrevious = TRUE, lineCap = "round",fgColor = "#428BCA",inputColor = "#428BCA"))

                                                                        )
                                                                      )
                                                             ),
                                                             # Fig. 3 ------------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Densité</b></h4>'),
                                                                      includeMarkdown("densite.md"),
                                                                      tabsetPanel(
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">0+</h4>'), br(),
                                                                                  plotlyOutput('plot3_p0') %>% withSpinner(type=8, color="#A5C226"),
                                                                                  downloadButton("exportFigDataF3a","Télécharger les densités")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                  plotlyOutput('plot3_p1') %>% withSpinner(type=8, color="#A5C226"),
                                                                                 downloadButton("exportFigDataF3b","Télécharger les densités")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                  plotlyOutput('plot3') %>% withSpinner(type=8, color="#A5C226"),
                                                                                 downloadButton("exportFigDataF3c","Télécharger les densités")
                                                                        )
                                                                      )
                                                             )
                                                 )
                                          )
                                  )
                        ),
                        # A PROPOS ----------------------------------------------------
                        tabPanel(value="apropos", HTML('<h4 style="color: #005BBB; "><b>A propos</b></h4>'),
                                 br(),
                                 fluidRow(
                                   column(4, align="left",
                                          includeMarkdown("a_propos_1.md")
                                   ),
                                   column(2, align="center",
                                          br(), br(),
                                          HTML('<a href="Map.jpg" target="new">
                                            <img src="Map.jpg" alt="Map" width="300" height="281">
                                            </a>')
                                   )
                                 ), 
                                 hr(),
                                 fluidRow(
                                   column(12, align="left",
                                          includeMarkdown("a_propos_2.md"),
                                          actionLink(inputId = "details", label=HTML('<h4 style="color: #005BBB; ">Voir les détails</h4>'))
                                   )
                                 ),
                                 fluidRow(
                                   column(6, align="left",
                                          includeMarkdown("a_propos_3.md"),
                                          hr(),
                                          includeMarkdown("a_propos_3bis.md"),
                                          actionLink(inputId = "gamme", label=HTML('<h4 style="color: #005BBB; ">Détails de la gamme de densités du jeu de données</h4>'))
                                   )
                                 ),
                                 fluidRow(
                                   column(6, align="left",
                                          includeMarkdown("a_propos_4.md")
                                   )
                                 )
                              )
                        )
              ))
  )
)
