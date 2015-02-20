library(shiny)

shinyUI(fluidPage(
        titlePanel("Geometric Mean in DWI"),
        
        sidebarLayout(
                sidebarPanel( 
                        tabsetPanel(
                                tabPanel("Mono",
                                         h4("Sim-Parameters"),
                                         sliderInput("ADC", label = "ADC [mm²/10³s]",
                                                     min = 0, max = 10,
                                                     value = 5, step = 0.01),
                                         actionButton("CalcMono", label = "Action")),
                                tabPanel("IVIM Segmented"),
                                tabPanel("IVIM Full"),
                                tabPanel("Kurtosis"))),
                mainPanel(
                        plotOutput("Plot"))
        ),
        
        fluidRow("Results")
))

