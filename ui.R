
ui =   navbarPage(
  #fluidPage(

    title = fluidRow(column(12, h4(tags$a(href="https://www.sciencedirect.com/science/article/pii/S2590113320300079?via%3Dihub", 
                                              "Global Epidemiology - Available online 23 March 2020"))
                )),
    tabPanel('Exponential growth model', 
    wellPanel(style = "background: LightSteelBlue", 
              fluidRow(column(1, shinyWidgets::dropdown(div(style = "background: lightblue; font-size: 10px;",
                                                            DTOutput('dat')),
                                                        label =  'Report/Date',
                                                        style = "gradient", size = 'xs', block=T, animate=T)),
                column(2, numericInput('m','Number of days to predict', 13, 2,13,1)),
                       # column(2, numericInput('cutttL','Report day - Lower bond', 47, 1, 100, 1)),
                       # column(2, numericInput('cutttU','Report day - Upper bond', tail(dfcovv$report, 1), 100, 1))
                       column(2, dateInput("cutttL", "cut - Lower bond", value = "2020-03-03", 
                              min  =  head(dfcovv$d, 1), max = tail(dfcovv$d, 1))),
                       column(2, dateInput("cutttU", "cut - Upper bond", value = "2020-03-22",
                              min  =  head(dfcovv$d, 1), max = tail(dfcovv$d, 1)))
                       #column(2, numericInput('start','start Value in NLS', 100, 1, 200, 1))
                       )),
    fluidRow(column(4, wellPanel(p(strong('Exponential growth')), verbatimTextOutput('model'),p(strong('Su - Residual standard error'))),
                    wellPanel(p('Predicted values'), DTOutput('dfP')), hr(), 
                    wellPanel(p('Actual values'), DTOutput('dfF'))), 
             column(8, wellPanel(checkboxInput(inputId = "redline", label = "Add red line 1,000,000",  value = TRUE),
                                 withSpinner(plotOutput("plot1who"))), 
                       wellPanel(checkboxInput(inputId = "resid", label = "Add line and ares",  value = TRUE), 
                                 plotOutput("diffplotwho"))
                  )
               )
    ),
    tabPanel('WHO Data', fluidRow(column(6, wellPanel(DTOutput('dfcov'))))
             )
 
)
     
