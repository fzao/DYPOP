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

library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(plotly)


# WARINING : Some constant values to change with new data !!!
tstep = 0.25
#load(file=paste('/home/dypop/data/FS.RData',sep=''))
#Amax <- max(FS[["XAd"]])
Amax <- 35.
#Avalue <- FS[["XAd"]][floor(length(FS[["XAd"]])/2)]
Avalue <- 17.
#Astep <- FS[["XAd"]][3]-FS[["XAd"]][2]
Astep <- 1.
#Bmax <- max(FS[["X1"]])
Bmax <- 70.
#Bvalue <- FS[["X1"]][floor(length(FS[["X1"]])/2)]
Bvalue <- 34.
#Bstep <- FS[["X1"]][3]-FS[["X1"]][2]
Bstep <- 2.


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
            tags$head(tags$link(rel="shortcut icon", href="Dypop_icon.png")),
            useShinyjs(),
            inlineCSS(appCSS),
            # Loading message
            div(
              id = "loading-content",
              h1("Loading DYPOP...") %>% withSpinner(type=8, color="#A5C226")
            ),

            hidden(
              div(
                id = "app-content",
                navbarPage(title = "VERSION",
                    tabPanel("English",
                        fluidRow(
                                 column(10, align="left",
                                        HTML('<h1 style="color: #FFA02F; background-color: #FFFFFF; font-size:400%"><b>DYPOP</b></h1>'),
                                        HTML('<h2 style="color: #A5C226; background-color: #FFFFFF;">A tool to estimate brown trout population status</h2>')),
                                 column(2, align="right",
                                        img(height=40, width=80, src="edf.jpg"), HTML('<h4> </h4>'),
                                        img(height=30, width=114, src="Inrae.jpg"))
                        ), br(),
                        tabsetPanel(id="tabs_en",
                                    # ACCUEIL ---------------------------------------------------
                                    tabPanel(value="accueil_en", HTML('<h4 style="color: #005BBB; "><b>Welcome</b></h4>'),
                                             br(),
                                             fluidRow(
                                               column(4, align="left",
                                                      includeMarkdown("md/accueil_1_en.md"),
                                                      actionLink(inputId = "redir1_en", label=HTML('<h4 style="color: #005BBB; ">=> To find out more, go to the \"About\" tab</h4>')),
                                                      includeMarkdown("md/accueil_2_en.md"),
                                                      actionLink(inputId = "redir2_en", label=HTML('<h4 style="color: #005BBB; ">=> To use DYPOP, go to the \"Analysis\" tab</h4>')),
                                                      includeMarkdown("md/accueil_3_en.md")
                                                      ),
                                               column(4, align="center",
                                                      HTML('<a href="Diapositive1_en.jpg" target="new">
                                                        <img src="Diapositive1_en.jpg" alt="Life cycle" width="600" height="450">
                                                        </a>'),br(),br(),br(),
                                                      img(width = 300, src='Diapositive2_en.jpg', align = "center")
                                                      ))
                                    ),
                                    # ANALYSIS ------------------------------------------------------
                                    tabPanel(value="visu_en", HTML('<h4 style="color: #005BBB; "><b>Analysis</b></h4>'),
                                             fluidRow(
                                                 includeMarkdown("md/visu_text_1_en.md")
                                                 #includeMarkdown("md/visu_text_2.md")
                                             ),
                                             fluidRow(
                                                      column(2, align = "left",
                                                              br(),
                                                              wellPanel(h4(strong("INPUT DATA")),hr(),
                                                                        actionLink(inputId = "link1_en", label=h5("HOT TEMPERATURES")),
                                                                        sliderInput("slider1_en", label = h3("T10"), min = 9., max = 17., value = 10, step = tstep),br(),hr(),
                                                                        actionLink(inputId = "link2_en", label=h5("COLD TEMPERATURES")),
                                                                        sliderInput("slider2_en", label = h3("T90"), min = 1., max = 8., value = 5., step = tstep),br(),hr(),
                                                                        actionLink(inputId = "link3_en", label=h5("SHELTER AVAILABILITY")),
                                                                        sliderInput("slider3_en", label = h3("% Shelter"), min = 0., max = 7., value = 3., step = tstep)
                                                              )),
                                                      column(10, align = "left",
                                                             br(),
                                                             tabsetPanel(
                                                                         # Fig. 1 ---------------------------------------------------
                                                                         tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Survival</b></h4>'),
                                                                                  includeMarkdown("md/survie_1_en.md"),
                                                                                  tabsetPanel(
                                                                                    tabPanel(HTML('<h4 style="color: #A5C226; ">0+</h4>'), br(),
                                                                                              plotlyOutput('plot1_p0_en') %>% withSpinner(type=8, color="#A5C226"),
                                                                                              downloadButton("exportFigDataF1a_en","Download survival rates")
                                                                                    ),
                                                                                    tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                              plotlyOutput('plot1_p1_en') %>% withSpinner(type=8, color="#A5C226"),
                                                                                              downloadButton("exportFigDataF1b_en","Download survival rates")
                                                                                    ),
                                                                                    tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                              plotlyOutput('plot1_en') %>% withSpinner(type=8, color="#A5C226"),
                                                                                              downloadButton("exportFigDataF1c_en","Download survival rates")
                                                                                    )
                                                                                  ), br(),
                                                                                  includeMarkdown("md/survie_2_en.md")
                                                                         ),
                                                                         # Fig. 2 ------------------------------------------------------
                                                                         tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Cohort</b></h4>'),
                                                                                  includeMarkdown("md/cohorte_1_en.md"),
                                                                                  tabsetPanel(
                                                                                    tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                              includeMarkdown("md/cohorte_2_en.md"),
                                                                                              plotlyOutput('plot2_p0_en') %>% withSpinner(type=8, color="#A5C226"),
                                                                                              downloadButton("exportFigDataF2a_en","Download densities"), br(), br(),
                                                                                              includeMarkdown("md/cohorte_3_en.md")
                                                                                    ),
                                                                                    tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                              includeMarkdown("md/cohorte_4_en.md"),
                                                                                              #checkboxInput("checkbox", label = "vue 3D", value = FALSE),
                                                                                              switchInput(inputId = "checkbox_en", label = "3D view", value = FALSE, handleWidth = 50, size = 'mini'),
                                                                                              plotlyOutput('plot2_hm_en') %>% withSpinner(type=8, color="#A5C226"),
                                                                                              downloadButton("exportFigDataF2b_en","Download densities"), br(),
                                                                                              column(5, align="center",
                                                                                                  sliderInput("slider_XAdm_en", label = h4('>1+ density year (n-1)'), min = 0., max = Amax, value = Avalue, step =Astep)),
                                                                                                  #knobInput(inputId = "slider_XAdm", label = h4('Densité >1+ année (n-1)'),min = 0., max = Amax, value = Avalue, step =Astep, displayPrevious = TRUE, lineCap = "round",fgColor = "#428BCA",inputColor = "#428BCA")),
                                                                                              column(2,
                                                                                                    h4(helpText("MARGINAL VIEWS"))),
                                                                                              column(5, align="center",
                                                                                                  sliderInput("slider_X1m_en", label = h4('1+ density year (n-1)'), min = 0, max = Bmax, value = Bvalue, step = Bstep))
                                                                                                  #knobInput(inputId = "slider_X1m", label = h4('Densité 1+ année (n-1)'),min = 0., max = Bmax, value = Bvalue, step =Bstep, displayPrevious = TRUE, lineCap = "round",fgColor = "#428BCA",inputColor = "#428BCA"))
            
                                                                                    )
                                                                                  )
                                                                         ),
                                                                         # Fig. 3 ------------------------------------------------------
                                                                         tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Density</b></h4>'),
                                                                                  includeMarkdown("md/densite_en.md"),
                                                                                  tabsetPanel(
                                                                                    tabPanel(HTML('<h4 style="color: #A5C226; ">0+</h4>'), br(),
                                                                                              plotlyOutput('plot3_p0_en') %>% withSpinner(type=8, color="#A5C226"),
                                                                                              downloadButton("exportFigDataF3a_en","Download densities")
                                                                                    ),
                                                                                    tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                              plotlyOutput('plot3_p1_en') %>% withSpinner(type=8, color="#A5C226"),
                                                                                             downloadButton("exportFigDataF3b_en","Dowload densities")
                                                                                    ),
                                                                                    tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                              plotlyOutput('plot3_en') %>% withSpinner(type=8, color="#A5C226"),
                                                                                             downloadButton("exportFigDataF3c_en","Dowload densities")
                                                                                    )
                                                                                  )
                                                                         )
                                                             )
                                                      )
                                              )
                                    ),
                                    # A PROPOS ----------------------------------------------------
                                    tabPanel(value="apropos_en", HTML('<h4 style="color: #005BBB; "><b>About</b></h4>'),
                                             br(),
                                             fluidRow(
                                               column(4, align="left",
                                                      includeMarkdown("md/a_propos_1_en.md")
                                               ),
                                               column(2, align="center",
                                                      br(), br(),
                                                      HTML('<a href="Map_en.jpg" target="new">
                                                        <img src="Map_en.jpg" alt="Map" width="300" height="281">
                                                        </a>')
                                               )
                                             ),
                                             hr(),
                                             fluidRow(
                                               column(12, align="left",
                                                      includeMarkdown("md/a_propos_2_en.md"),
                                                      actionLink(inputId = "details_en", label=HTML('<h4 style="color: #005BBB; ">See the details</h4>'))
                                               )
                                             ),
                                             fluidRow(
                                               column(6, align="left",
                                                      includeMarkdown("md/a_propos_3_en.md"),
                                                      hr(),
                                                      includeMarkdown("md/a_propos_3bis_en.md"),
                                                      actionLink(inputId = "gamme_en", label=HTML('<h4 style="color: #005BBB; ">Details of the dataset density range</h4>'))
                                               )
                                             ),
                                             fluidRow(
                                               column(6, align="left",
                                                      includeMarkdown("md/a_propos_4_en.md")
                                               )
                                             )
                                          )
                                    )
                    ),
                    tabPanel("French",
                             fluidRow(
                               column(10, align="left",
                                      HTML('<h1 style="color: #FFA02F; background-color: #FFFFFF; font-size:400%"><b>DYPOP</b></h1>'),
                                      HTML('<h2 style="color: #A5C226; background-color: #FFFFFF;">Un outil d\'aide au diagnostic de l\'état des populations de truite fario</h2>')),
                               column(2, align="right",
                                      img(height=40, width=80, src="edf.jpg"), HTML('<h4> </h4>'),
                                      img(height=30, width=114, src="Inrae.jpg"))
                             ), br(),
                             tabsetPanel(id="tabs",
                                         # ACCUEIL ---------------------------------------------------
                                         tabPanel(value="accueil", HTML('<h4 style="color: #005BBB; "><b>Accueil</b></h4>'),
                                                  br(),
                                                  fluidRow(
                                                    column(4, align="left",
                                                           includeMarkdown("md/accueil_1.md"),
                                                           actionLink(inputId = "redir1", label=HTML('<h4 style="color: #005BBB; ">=> Pour en savoir plus, se rendre sur l\'onglet \"A propos\"</h4>')),
                                                           includeMarkdown("md/accueil_2.md"),
                                                           actionLink(inputId = "redir2", label=HTML('<h4 style="color: #005BBB; ">=> Pour utiliser DYPOP, se rendre sur l\'onglet\"Analyse\"</h4>')),
                                                           includeMarkdown("md/accueil_3.md")
                                                    ),
                                                    column(4, align="center",
                                                           HTML('<a href="Diapositive1.jpg" target="new">
                                                        <img src="Diapositive1.jpg" alt="Life cycle" width="600" height="450">
                                                        </a>'),br(),br(),br(),
                                                           img(width = 300, src='Diapositive2.jpg', align = "center")
                                                    ))
                                         ),
                                         # ANALYSE ------------------------------------------------------
                                         tabPanel(value="visu", HTML('<h4 style="color: #005BBB; "><b>Analyse</b></h4>'),
                                                   fluidRow(
                                                     includeMarkdown("md/visu_text_1.md")
                                                     #includeMarkdown("md/visu_text_2.md")
                                                   ),
                                                  fluidRow(
                                                    column(2, align = "left",
                                                           br(),
                                                           wellPanel(h4(strong("DONNEES D'ENTREE")),hr(),
                                                                     actionLink(inputId = "link1", label=h5("LES TEMPERATURES CHAUDES")),
                                                                     sliderInput("slider1", label = h3("T10"), min = 9., max = 17., value = 10, step = tstep),br(),hr(),
                                                                     actionLink(inputId = "link2", label=h5("LES TEMPERATURES FROIDES")),
                                                                     sliderInput("slider2", label = h3("T90"), min = 1., max = 8., value = 5., step = tstep),br(),hr(),
                                                                     actionLink(inputId = "link3", label=h5("LA DISPONIBILITE EN ABRIS")),
                                                                     sliderInput("slider3", label = h3("% Abris"), min = 0., max = 7., value = 3., step = tstep)
                                                           )),
                                                    column(10, align = "left",
                                                           br(),
                                                           tabsetPanel(
                                                             # Fig. 1 ---------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Survie</b></h4>'),
                                                                      includeMarkdown("md/survie_1.md"),
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
                                                                      includeMarkdown("md/survie_2.md")
                                                             ),
                                                             # Fig. 2 ------------------------------------------------------
                                                             tabPanel(HTML('<h4 style="color: #FFA02F; "><b>Cohorte</b></h4>'),
                                                                      includeMarkdown("md/cohorte_1.md"),
                                                                      tabsetPanel(
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">1+</h4>'), br(),
                                                                                 includeMarkdown("md/cohorte_2.md"),
                                                                                 plotlyOutput('plot2_p0') %>% withSpinner(type=8, color="#A5C226"),
                                                                                 downloadButton("exportFigDataF2a","Télécharger les densités"), br(), br(),
                                                                                 includeMarkdown("md/cohorte_3.md")
                                                                        ),
                                                                        tabPanel(HTML('<h4 style="color: #A5C226; ">>1+</h4>'), br(),
                                                                                 includeMarkdown("md/cohorte_4.md"),
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
                                                                      includeMarkdown("md/densite.md"),
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
                                                           includeMarkdown("md/a_propos_1.md")
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
                                                           includeMarkdown("md/a_propos_2.md"),
                                                           actionLink(inputId = "details", label=HTML('<h4 style="color: #005BBB; ">Voir les détails</h4>'))
                                                    )
                                                  ),
                                                  fluidRow(
                                                    column(6, align="left",
                                                           includeMarkdown("md/a_propos_3.md"),
                                                           hr(),
                                                           includeMarkdown("md/a_propos_3bis.md"),
                                                           actionLink(inputId = "gamme", label=HTML('<h4 style="color: #005BBB; ">Détails sur la gamme de densités du jeu de données</h4>'))
                                                    )
                                                  ),
                                                  fluidRow(
                                                    column(6, align="left",
                                                           includeMarkdown("md/a_propos_4.md")
                                                    )
                                                  )
                                         )
                             )
                    )
              )))
  )
)
