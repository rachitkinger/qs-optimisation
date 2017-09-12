library(shiny)


ui <- fluidPage(
    
    # App title ----
    titlePanel("QS Optimisation - beta"),
    
    # Sidebar layout with input and output definitions ----
    fluidRow(
        column(4,
               p("The project is currently in beta, so we apologise in advance for any unexpected
                 behaviour that you experience. If you come across any bugs please ",
                 a(href="mailto:rachit.kinger@jpress.co.uk", "email us.")),
               p("For usage documentation please visit ", 
                 a(href="https://rachitkinger.github.io/qs-optimisation/", "the project page "),
                 "or visit our open source project repository on ", 
                 a(href="https://github.com/rachitkinger/qs-optimisation", "github."))
        ),
        column(4,
               fileInput(inputId ="file1", 
                         label = "Choose CSV File",
                         multiple = FALSE,
                         accept = c("text/csv",
                                    "text/comma-separated-values,text/plain",
                                    ".csv"))
               ),
        column(4,
               sliderInput(inputId = "budget.range",
                           label = "Select 30-Day Spend Range",
                           min = 100,
                           max = 5000,
                           value = c(300,2000),
                           step = 100,
                           round = TRUE,
                           ticks = TRUE,
                           pre = "£",
                           post ="")
               
               )
    ),
    
    fluidRow(
        column(8),
        
        column(4,
               downloadButton(outputId = "qs_download",
                              label = "Export Analysis")
        )
    ),
    
    fluidRow(
        tableOutput(outputId = "p1.table")
    ),
    
    fluidRow(
        tableOutput(outputId = "p2.table")
    ),
    
    
    fluidRow(
        DT::dataTableOutput(outputId = "final.table")
    )
)
 