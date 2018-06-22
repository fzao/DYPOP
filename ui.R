library(shiny)
library(plotly)

# Define UI for application that draws a histogram
pageWithSidebar(
  headerPanel('Dynamique Population Truite'),
  sidebarPanel(
    sliderInput("slider1", label = h3("T 10"), min = 9., 
                max = 17., value = 10, step = 1.),
    sliderInput("slider2", label = h3("T 90"), min = 1., 
                max = 7., value = 5., step = 1.),
    sliderInput("slider3", label = h3("% Cache"), min = 0., 
                max = 7., value = 3., step = 1.),
    verbatimTextOutput("value")
  ),
  mainPanel(
    tabsetPanel(
      # Fig. 1 ---------------------------------------------------
      tabPanel(HTML('<h4 style="color: #005BBB; "><b>Figure 1</b></h4>'),
               plotlyOutput('plot1', height = 'auto', width = 'auto')
      ),
      # Fig. 2 ------------------------------------------------------
      tabPanel(HTML('<h4 style="color: #005BBB; "><b>Figure 2</b></h4>'),
               plotlyOutput('plot2')
      ),
      # Fig. 3 ------------------------------------------------------
      tabPanel(HTML('<h4 style="color: #005BBB; "><b>Figure 3</b></h4>'),
               plotlyOutput('plot3')
      )
  )
)
)
