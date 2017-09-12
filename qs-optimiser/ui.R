library(shiny)


ui <- fluidPage(
    
    # App title ----
    titlePanel("QS Optimisation (beta)"),
    
    # Sidebar layout with input and output definitions ----
    fluidRow(
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
        
        h2("Priority 1 Campaigns"),
        tableOutput(outputId = "p1.table")
    ),
    
    fluidRow(
        h2("Priority 2 Campaigns"),
        tableOutput(outputId = "p2.table")
    ),
    
    
    fluidRow(
        column(8),
        
        column(4,
               downloadButton(outputId = "qs_download",
                              label = "Export Analysis")
        )
    ),
    
    fluidRow(
        DT::dataTableOutput(outputId = "final_table")
    )
)
   
#     
#     sidebarLayout(
#         
#         # Sidebar panel for inputs ----
#         sidebarPanel(
#             
#             # Input: Select a file ----
#             fileInput(inputId ="file1", 
#                       label = "Choose CSV File",
#                       multiple = FALSE,
#                       accept = c("text/csv",
#                                  "text/comma-separated-values,text/plain",
#                                  ".csv")),
#             
#             # Horizontal line ----
#             tags$hr(),
#             
#             
#         # Input: slider for budget range
#             sliderInput(inputId = "budget.range",
#                         label = "Select 30-Day Spend Range",
#                         min = 100,
#                         max = 5000,
#                         value = c(300,2000),
#                         step = 50,
#                         round = TRUE,
#                         ticks = TRUE,
#                         pre = "£",
#                         post =".00"
#                         ),
#         
#         h3("Upload .csv files only")
#         ),
#         
#         # Main panel for displaying outputs ----
#         mainPanel(
#             
#             
#                     # Output: Data file ----
#                     h2("Priority 1 Campaigns"),
#                     tableOutput(outputId = "p1.table"),
#                     br(),
#                     h2("Priority 2 Campaigns"),
#                     tableOutput(outputId = "p2.table"),
#                     
#                     # fluidRow(
#                     #     column(2,
#                     #            selectInput(inputId = "priority",
#                     #                        label = "Priority Level:",
#                     #                        c("All", "Priority 1", "Priority 2"),
#                     #                        multiple = TRUE)
#                     #     ),
#                     #     column(2,
#                     #            selectInput(inputId = "campaign",
#                     #                        label = "Campaign:",
#                     #                        c("All")
#                     #                        multiple = TRUE)
#                     #     )
#                     # ),
#                     # 
#                     # Create a new row for the table.
#                     
#                     fluidRow(
#                         DT::dataTableOutput(outputId = "final.table")
#                     )
#            
#         )
#         
#     )
# )