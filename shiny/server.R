library(shiny)
library(dplyr)
library(tidyr)
library(scales)
library(data.table)
library(DT)

##utility functions
global_data <- NULL
global_data_with_budget <- NULL
global_final_list <- NULL
global_final_dt <- NULL

# process_data is called by process_upload (inside server) and it updates global_data
# and returns its value inside the server function
process_data <- function(df) {
    #colClasses mapped to the column names as the report was created on 06/09/2017 by rachit.kinger@jpress.co.uk
    colClasses <- c("character", #Account
                    "character", #Campaign
                    "character", #Keyword
                    "character", #Customer.ID
                    "factor",    #Keyword.State
                    "factor",    #Match.type
                    "numeric",   #Max..CPC
                    "numeric",   #Clicks
                    "numeric",   #Impressions
                    "character", #CTR
                    "numeric",   #Avg..CPC
                    "numeric",   #Avg..CPM
                    "numeric",   #Cost
                    "numeric",   #Avg..position
                    "integer",   #Quality.score
                    "factor",    #Ad.group.state
                    "factor",    #Campaign.state
                    "factor",    #Status
                    "factor",    #Expected.click.through.rate
                    "factor",    #Ad.relevance
                    "factor",    #Landing.page.experience
                    "integer",   #Qual..score..hist..
                    "factor",    #Expected.click.through.rate..hist..
                    "factor",    #Ad.relevance..hist..
                    "factor",    #Landing.page.experience..hist..
                    "numeric",   #First.page.CPC
                    "numeric",   #First.position.CPC
                    "numeric",   #FTop.of.page.CPC
                    "numeric",   #Max..CPM
                    "character", #Destination.URL
                    "character", #Final.URL
                    "character", #Mobile.final.URL
                    "character", #Tracking.template
                    "character", #Custom.parameter
                    "factor",    #Time.zone
                    "factor",    #Currency
                    "factor",    #Lables
                    "integer",   #Est..add..clicks.wk...300..bid.
                    "integer",   #Est..add..clicks.wk...50..bid.
                    "integer",   #Est..add..clicks.wk...50..bid..1
                    "integer",   #Est..add..clicks.wk..top.page.bid
                    "integer",   #Est..add..clicks.wk..first.position.bid.
                    "numeric",   #Est..add..cost.wk...300..bid.
                    "numeric",   #Est..add..cost.wk...50..bid.
                    "numeric",   #Est..add..cost.wk...50..bid..1
                    "numeric",   #Est..add..cost.wk..top.page.bid.
                    "numeric",   #Est..add..cost.wk..first.position.bid.
                    "numeric",   #Base.max..CPC
                    "factor",    #Campaign.Type
                    "factor",    #Campaign.subtype
                    "character", #Bounce.rate
                    "numeric",   #Pages...session
                    "numeric",   #Avg..session.duration..seconds.
                    "character", #X..new.sessions
                    "character", #Search.Exact.match.IS
                    "character", #Search.Lost.IS..rank.
                    "character" #Search.Impr..share
    )
    #converting each columns class as desired (area of improv - how to do this at the time of loading data?)
    for (i in 1:length(colClasses)) {
        if(colClasses[i] == "character") {df[,i] <- as.character(df[,i])}
        else if(colClasses[i] == "integer") {df[,i] <- as.integer(df[,i])}
        else if(colClasses[i] == "numeric") {df[,i] <- as.numeric(df[,i])}
        else if(colClasses[i] == "factor") {df[,i] <- as.factor(df[,i])}
    }
    
    # extract relevant columns into another dataset  
    cols <- c(1,2,3,4,5,8,11,13,14,15,16,17,18,19,20,21) # relevant columns 
    df <- df[,cols]
    
    # data wrangling
    
    df <- tbl_df(df)
    
    # filter out the 0 cost and NA quality scores on which no analysis can be done
    df <- df %>% 
        filter(!is.na(Quality.score)) %>% 
        filter(Cost != 0) 
    
    
    # define QS-buckets  #could be user defined
    LQS <- c(1,2,3)    #low quality score
    MQS <- c(4,5,6)    #medium quality score
    HQS <- c(7,8,9,10) #high quality score
    
    # create new variable qs-bucket based on above definitions
    qs_included <- df %>% 
        mutate(qs.bucket = ifelse(Quality.score %in% LQS,"Low",
                                  ifelse(Quality.score %in% MQS, "Medium",
                                         ifelse(Quality.score %in% HQS, "High", NA))))
    global_data <- qs_included
    return(global_data)
}

# budget is called by budget_range (inside servert). It updates global_data_with_budget
# and returns its value inside the server function
budget <- function(lower = 300, upper = 5000, df) {
    with_budget <- df %>% 
        group_by(Campaign) %>% 
        filter(sum(Cost) > lower & sum(Cost) < upper)    
   
        global_data_with_budget <- with_budget
    return(global_data_with_budget)
    
}

#p1p2data contains rules for p1 p2 and processes data as per that
# it is called from inside the server and updates global_final_list
p1p2data <- function(df) { 
    campaigns <- distinct(df, Campaign) #unique Campaigns only
    LQS <- NULL
    for(i in 1:nrow(campaigns)) {
        x <- df %>% filter(Campaign == campaigns[i,]) %>% filter(qs.bucket == "Low") 
        LQS[i] <- sum(x$Cost, na.rm = TRUE)
    }
    
    MQS <- NULL
    for(i in 1:nrow(campaigns)) {
        x <- df %>% filter(Campaign == campaigns[i,]) %>% filter(qs.bucket == "Medium") 
        MQS[i] <- sum(x$Cost, na.rm = TRUE)
    }
    
    HQS <- NULL
    for(i in 1:nrow(campaigns)) {
        x <- df %>% filter(Campaign == campaigns[i,]) %>% filter(qs.bucket == "High") 
        HQS[i] <- sum(x$Cost, na.rm = TRUE)
    }
    
    tot_spend <- NULL
    for(i in 1:nrow(campaigns)) {
        x <- df %>% filter(Campaign == campaigns[i,]) 
        tot_spend[i] <- sum(x$Cost, na.rm = TRUE)
    }
    
    
    spend.compare.raw <- data.frame(
        Campaign = campaigns[,1],
        LQS.Spend = LQS/tot_spend,
        MQS.Spend = MQS/tot_spend,
        HQS.Spend = HQS/tot_spend,
        Total.Spend = tot_spend
    )
    
    
    spend.compare.format <- function(x) {
        with(x[order(-x$LQS.Spend, -x$Total.Spend, -x$MQS.Spend),], #first sort in order of LQS spend levels & Total.Spend & MQS Spend
             data.frame(
                 Campaign = Campaign,
                 LQS.Spend = scales::percent(LQS.Spend),
                 MQS.Spend = scales::percent(MQS.Spend),
                 HQS.Spend = scales::percent(HQS.Spend),
                 Total.Spend = scales::dollar_format(prefix = "£")(Total.Spend)
             ))
        
    }
    # Rules
    ## Rule 1 = LQS + MQS > 30% if yes, check
    ## Rule 1.a = LQS > 10%, if yes then perform LQS analysis, if no then perform MQS analysis
    
    
    r1.p1 <- spend.compare.raw %>%
        filter(LQS.Spend + MQS.Spend > .3) %>% 
        filter(LQS.Spend > .1 ) %>% 
        arrange(desc(LQS.Spend))
    
    r1.p2 <-  spend.compare.raw %>%
        filter(LQS.Spend + MQS.Spend > .3) %>% 
        filter(LQS.Spend <= .1 )  %>% 
        arrange(desc(MQS.Spend))
    
    r1.p1.formatted <- spend.compare.format(r1.p1)
    r1.p2.formatted <- spend.compare.format(r1.p2)
    
    
    
    
    
    #       }           #closing braces #for progress bar
    # })               #closing braces #for progress bar
    return(list(p1.formated=r1.p1.formatted, 
                p2.formated=r1.p2.formatted,
                p1= r1.p1,
                p2= r1.p2,
                alldata = df))
}

#create_final_data creates the final datatable that contains keywords
#and performance indicators. It is run from inside the server and it
#updates the global_final_dt
create_final_data <- function(list) { 
    if(is.null(list)) { return()}
                         rule1.p1 <- list$p1
                         rule1.p2 <- list$p2
                         with_budget <- list$alldata
                         
                         no.of.p1.campaigns.to.optimise <- nrow(rule1.p1)
                         no.of.p2.campaigns.to.optimise <- nrow(rule1.p2)
                         
                         p1.low <- list(NULL)
                         p1.med <- list(NULL)
                         library(scales)
                         for(i in 1:no.of.p1.campaigns.to.optimise) {
                             
                             p1.low[[i]] <-  with_budget %>% 
                                 filter(Campaign == rule1.p1$Campaign[i]) %>% 
                                 filter(qs.bucket == "Low") %>% 
                                 mutate(Cost.Prop = (Cost / sum(Cost))) %>% 
                                 arrange(desc(Cost.Prop)) %>% 
                                 mutate(Proportion.Of.Campaign.Cost = percent(Cost / rule1.p1$Total.Spend[i])) %>% 
                                 top_n(5, Cost.Prop) %>% 
                                 mutate(Proportion.Of.QS.Bucket = percent(Cost.Prop)) %>%
                                 mutate(Priority = "Priority1") %>% 
                                 mutate(Campaign.Spend = dollar_format(prefix = "£")(rule1.p1$Total.Spend[i]) ) %>% 
                                 select(Priority, Campaign, Keyword, qs.bucket, Expected.click.through.rate, Ad.relevance, Landing.page.experience, Proportion.Of.QS.Bucket, Proportion.Of.Campaign.Cost, Campaign.Spend)
                             
                             
                             p1.med[[i]] <-  with_budget %>% 
                                 filter(Campaign == rule1.p1$Campaign[i]) %>% 
                                 filter(qs.bucket == "Medium") %>% 
                                 mutate(Cost.Prop = (Cost / sum(Cost))) %>% 
                                 arrange(desc(Cost.Prop)) %>% 
                                 mutate(Proportion.Of.Campaign.Cost = percent(Cost / rule1.p1$Total.Spend[i])) %>% 
                                 top_n(5, Cost.Prop) %>% 
                                 mutate(Proportion.Of.QS.Bucket = percent(Cost.Prop)) %>%
                                 mutate(Priority = "Priority1") %>% 
                                 mutate(Campaign.Spend = dollar_format(prefix = "£")(rule1.p1$Total.Spend[i]) ) %>% 
                                 select(Priority, Campaign, Keyword, qs.bucket, Expected.click.through.rate, Ad.relevance, Landing.page.experience, Proportion.Of.QS.Bucket, Proportion.Of.Campaign.Cost, Campaign.Spend)
                             
                         }
                         
                         p1_df <- NULL
                         for(z in 1:length(p1.low)) {
                             p1_df <- rbind(p1_df, p1.low[[z]])
                         }
                         
                         for(y in 1:length(p1.med)) {
                             p1_df <- rbind(p1_df, p1.med[[y]])
                         }
                         
                         p2.low <- list(NULL)
                         p2.med <- list(NULL)
                         for(i in 1:no.of.p2.campaigns.to.optimise) {
                             
                             p2.low[[i]] <-  with_budget %>% 
                                 filter(Campaign == rule1.p2$Campaign[i]) %>% 
                                 filter(qs.bucket == "Low") %>% 
                                 mutate(Cost.Prop = (Cost / sum(Cost))) %>% 
                                 arrange(desc(Cost.Prop)) %>% 
                                 mutate(Proportion.Of.Campaign.Cost = percent(Cost / rule1.p2$Total.Spend[i])) %>% 
                                 top_n(5, Cost.Prop) %>% 
                                 mutate(Proportion.Of.QS.Bucket = percent(Cost.Prop)) %>%
                                 mutate(Priority = "Priority2") %>% 
                                 mutate(Campaign.Spend = dollar_format(prefix = "£")(rule1.p2$Total.Spend[i]) ) %>% 
                                 select(Priority, Campaign, Keyword, qs.bucket, Expected.click.through.rate, Ad.relevance, Landing.page.experience, Proportion.Of.QS.Bucket, Proportion.Of.Campaign.Cost, Campaign.Spend)
                             
                             
                             p2.med[[i]] <-  with_budget %>% 
                                 filter(Campaign == rule1.p2$Campaign[i]) %>% 
                                 filter(qs.bucket == "Medium") %>% 
                                 mutate(Cost.Prop = (Cost / sum(Cost))) %>% 
                                 arrange(desc(Cost.Prop)) %>% 
                                 mutate(Proportion.Of.Campaign.Cost = percent(Cost / rule1.p2$Total.Spend[i])) %>% 
                                 top_n(5, Cost.Prop) %>% 
                                 mutate(Proportion.Of.QS.Bucket = percent(Cost.Prop)) %>%
                                 mutate(Priority = "Priority2") %>% 
                                 mutate(Campaign.Spend = dollar_format(prefix = "£")(rule1.p2$Total.Spend[i]) ) %>% 
                                 select(Priority, Campaign, Keyword, qs.bucket, Expected.click.through.rate, Ad.relevance, Landing.page.experience, Proportion.Of.QS.Bucket, Proportion.Of.Campaign.Cost, Campaign.Spend)
                             
                         }
                         
                         p2_df <- NULL
                         for(z in 1:length(p2.low)) {
                             p2_df <- rbind(p2_df, p2.low[[z]])
                         }
                         
                         for(y in 1:length(p2.med)) {
                             p2_df <- rbind(p2_df, p2.med[[y]])
                         }
                         
                         final_df <- rbind(p1_df,p2_df)
                         
                     return(final_df)
    
}


 server <- function(input, output, session) {
   
     options(shiny.maxRequestSize=200*1024^2) #allow 200MB files to be uploaded
    
# process_upload is a reactive function that calls process_data (global function)
     process_upload <- reactive({
        
        
         progress <- Progress$new(session, min = 1, max = 4)
         on.exit(progress$close())
         req(input$file1)
         progress$set(message = "Cleaning data",
                      value = 2)
        user_upload <- read.csv(input$file1$datapath,
                                 sep = ",",
                                 skip = 5,
                                 colClasses = rep("character", 57))
        user_upload <- user_upload[-424909,] # remove last row, called "Total"
        
        return(process_data(user_upload))
   })
   
# budget_range is a reactive function that calls budget (global function)
      budget_range <- reactive({
          data_for_budget <- process_upload()
          progress <- Progress$new(session, min = 1, max = 4)
          on.exit(progress$close())
          progress$set(message = "Processing data",
                       detail = "...mapping budgets",
                       value = 2)
              return(budget(lower = input$budget.range[1], upper = input$budget.range[2], df = data_for_budget))
   })
   
# this call updates global_final_list
    update_global_final_list <- function() {
           
            data <- budget_range()
           progress <- Progress$new(session, min = 1, max = 4)
           on.exit(progress$close())
           progress$set(message = "Processing data",
                        detail = "...determinig priorities",
                        value = 3)
            global_final_list <<- p1p2data(data)
            return(global_final_list)
          
    }
   
# update global_final_dt
   update_global_final_dt <- function() {
       
    my_data <- update_global_final_list()
    progress <- Progress$new(session, min = 1, max = 4)
    on.exit(progress$close())
    progress$set(message = "Done Processing!",
                 detail = "...creating final output",
                 value = 3)
    global_final_dt <<- create_final_data(my_data)
    return(global_final_dt)
    }    
    
      
     output$p1.table <- renderTable({
        #my_p1 <- update_global_final_list()
         my_p1 <- global_final_list
         if(is.null(my_p1)) {
             my_p1 <- update_global_final_list()
             } 
         return(my_p1$p1.formated)
        
     })
      
     
     output$p2.table <- renderTable({
         #my_p2 <- update_global_final_list()
         my_p2 <- global_final_list
         if(is.null(my_p2)) {
             my_p2 <- update_global_final_list()
           } 
         return(my_p2$p2.formated)
    })
     
     output$final.table <- DT::renderDataTable(filter = "top", DT::datatable({
        my_dt <- global_final_dt
        if(is.null(my_dt)) {
            my_dt <- update_global_final_dt()
        } 
         return(my_dt)
     }))
     
     
  output$qs_download <- downloadHandler(
         filename = function() {
             paste("qs-optimisation-", Sys.Date(), ".csv", sep="")
         },
         content = function(file) {
             my_dt <- global_final_dt
             if(is.null(my_dt)) {
                 my_dt <- update_global_final_dt()
             } 
             write.csv(my_dt, file)
         }
     )
    
 }
 
 