library(shiny)
# Define server logic to read selected file ----
# server <- function(input, output) {
#     
#     
#         
#         # input$file1 will be NULL initially. After the user selects
#         # and uploads a file, head of that data file by default,
#         # or all rows if selected, will be shown.
#         
#         req(input$file1)
#         
#         df <- read.csv(input$file1$datapath,
#                        header = TRUE,
#                        skip = 5)
#         
#         test <- df$Clicks[2]
#         output$test <- renderText(test)
#     
#     
# }
server <- function(input, output) {
    
    options(shiny.maxRequestSize=40*1024^2) 
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = TRUE)
                       
        return(head(df))
 
        
    })
    
}