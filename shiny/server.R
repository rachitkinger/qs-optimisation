library(shiny)
library(dplyr)
library(tidyr)
library(scales)
library(data.table)
library(DT)

 server <- function(input, output) {
    options(shiny.maxRequestSize=200*1024^2) #allow 200MB files to be uploaded
    
   final_list <- reactive({
        
        
        withProgress(message = "Processing data..please wait  ", #for progress bar
                     detail = "...this can take a while",        #for progress bar               
                     value = 0, {                               #for progress bar
            for (q in 1:3) {                                   #for progress bar
            incProgress(1/4)                                   #for progress bar
        req(input$file1)     
        program_data <- read.csv(input$file1$datapath,
                                 sep = ",",
                                 skip = 5,
                                 colClasses = rep("character", 57))
        program_data <- program_data[-424909,] # remove last row, called "Total"
        
        
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
            if(colClasses[i] == "character") {program_data[,i] <- as.character(program_data[,i])}
            else if(colClasses[i] == "integer") {program_data[,i] <- as.integer(program_data[,i])}
            else if(colClasses[i] == "numeric") {program_data[,i] <- as.numeric(program_data[,i])}
            else if(colClasses[i] == "factor") {program_data[,i] <- as.factor(program_data[,i])}
        }
        
        # extract relevant columns into another dataset  
        cols <- c(1,2,3,4,5,8,11,13,14,15,16,17,18,19,20,21) # relevant columns 
        qs_data <- program_data[,cols]
        
        # data wrangling

        qs_data <- tbl_df(qs_data)
        
        # filter out the 0 cost and NA quality scores on which no analysis can be done
        qs_data <- qs_data %>% 
            filter(!is.na(Quality.score)) %>% 
            filter(Cost != 0) 
        
        
        # define QS-buckets  #could be user defined
        LQS <- c(1,2,3)    #low quality score
        MQS <- c(4,5,6)    #medium quality score
        HQS <- c(7,8,9,10) #high quality score
        
        # create new variable qs-bucket based on above definitions
        qs_data <- qs_data %>% 
            mutate(qs.bucket = ifelse(Quality.score %in% LQS,"Low",
                                      ifelse(Quality.score %in% MQS, "Medium",
                                             ifelse(Quality.score %in% HQS, "High", NA))))
        
        # separate campaigns with monthly spend > £500.
        budget.lower.limit <- 300   #default value, can be changed by user
        budget.upper.limit <- 10000 #default value, can be changed by user
        final <- qs_data %>% 
            group_by(Campaign) %>% 
            filter(sum(Cost) > budget.lower.limit & sum(Cost) < budget.upper.limit)                
        
        # # sort the data by Account level spend > Campaign level spend > Keyword level spend
        # t <- summarise(final, Account.Spend = sum(Cost)) #Account.Spend is a new variable
        # t <- arrange(t, desc(Spend))
        
        # get data on each Campaign's spend on LQS, MQS & HQS and Total Spend
        campaigns <- distinct(final, Campaign) #unique Campaigns only
        LQS <- NULL
        for(i in 1:nrow(campaigns)) {
            x <- final %>% filter(Campaign == campaigns[i,]) %>% filter(qs.bucket == "Low") 
            LQS[i] <- sum(x$Cost, na.rm = TRUE)
        }
        
        MQS <- NULL
        for(i in 1:nrow(campaigns)) {
            x <- final %>% filter(Campaign == campaigns[i,]) %>% filter(qs.bucket == "Medium") 
            MQS[i] <- sum(x$Cost, na.rm = TRUE)
        }
        
        HQS <- NULL
        for(i in 1:nrow(campaigns)) {
            x <- final %>% filter(Campaign == campaigns[i,]) %>% filter(qs.bucket == "High") 
            HQS[i] <- sum(x$Cost, na.rm = TRUE)
        }
        
        tot_spend <- NULL
        for(i in 1:nrow(campaigns)) {
            x <- final %>% filter(Campaign == campaigns[i,]) 
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
        
        
        
        

             }           #closing braces #for progress bar
       })               #closing braces #for progress bar
        return(list(p1.formated=r1.p1.formatted, 
                    p2.formated=r1.p2.formatted,
                    p1= r1.p1,
                    p2= r1.p2))
    })     
         
     output$p1.table <- renderTable({
         my.data <- final_list()
         return(my.data$p1.formated)
        
    })
     
     output$p2.table <- renderTable({
         my.data <- final_list()
         return(my.data$p2.formated)
         
     })
     
     output$final.table <- DT::renderDataTable(filter = "top", DT::datatable({
         my.data <- final_data()
         return(my.data)
     }))
     
     
     final_data <- reactive({
     
         my.data <- final_list()
         
         withProgress(message = "Preparing Output..nearly done  ", #for progress bar
                      value = 0, {                               #for progress bar
                          for (q in 1:2) {                                   #for progress bar
                              incProgress(1/2)                                   #for progress bar
                              
         rule1.p1 <- my.data$p1
         rule1.p2 <- my.data$p2
         
         no.of.p1.campaigns.to.optimise <- nrow(rule1.p1)
         no.of.p2.campaigns.to.optimise <- nrow(rule1.p2)
         
         p1.low <- list(NULL)
         p1.med <- list(NULL)
         library(scales)
         for(i in 1:no.of.p1.campaigns.to.optimise) {
             
             p1.low[[i]] <-  final %>% 
                 filter(Campaign == rule1.p1$Campaign[i]) %>% 
                 filter(qs.bucket == "Low") %>% 
                 mutate(Cost.Prop = (Cost / sum(Cost))) %>% 
                 arrange(desc(Cost.Prop)) %>% 
                 mutate(Proportion.Of.Campaign.Cost = percent(Cost / rule1.p1$Total.Spend[i])) %>% 
                 top_n(5, Cost.Prop) %>% 
                 mutate(Proportion.Of.QS.Bucket = percent(Cost.Prop)) %>%
                 mutate(Priority = "Prority1") %>% 
                 mutate(Campaign.Spend = dollar_format(prefix = "£")(rule1.p1$Total.Spend[i]) ) %>% 
                 select(Priority, Campaign, Keyword, qs.bucket, Expected.click.through.rate, Ad.relevance, Landing.page.experience, Proportion.Of.QS.Bucket, Proportion.Of.Campaign.Cost, Campaign.Spend)
             
             
             p1.med[[i]] <-  final %>% 
                 filter(Campaign == rule1.p1$Campaign[i]) %>% 
                 filter(qs.bucket == "Medium") %>% 
                 mutate(Cost.Prop = (Cost / sum(Cost))) %>% 
                 arrange(desc(Cost.Prop)) %>% 
                 mutate(Proportion.Of.Campaign.Cost = percent(Cost / rule1.p1$Total.Spend[i])) %>% 
                 top_n(5, Cost.Prop) %>% 
                 mutate(Proportion.Of.QS.Bucket = percent(Cost.Prop)) %>%
                 mutate(Priority = "Prority1") %>% 
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
             
             p2.low[[i]] <-  final %>% 
                 filter(Campaign == rule1.p2$Campaign[i]) %>% 
                 filter(qs.bucket == "Low") %>% 
                 mutate(Cost.Prop = (Cost / sum(Cost))) %>% 
                 arrange(desc(Cost.Prop)) %>% 
                 mutate(Proportion.Of.Campaign.Cost = percent(Cost / rule1.p2$Total.Spend[i])) %>% 
                 top_n(5, Cost.Prop) %>% 
                 mutate(Proportion.Of.QS.Bucket = percent(Cost.Prop)) %>%
                 mutate(Priority = "Prority2") %>% 
                 mutate(Campaign.Spend = dollar_format(prefix = "£")(rule1.p2$Total.Spend[i]) ) %>% 
                 select(Priority, Campaign, Keyword, qs.bucket, Expected.click.through.rate, Ad.relevance, Landing.page.experience, Proportion.Of.QS.Bucket, Proportion.Of.Campaign.Cost, Campaign.Spend)
             
             
             p2.med[[i]] <-  final %>% 
                 filter(Campaign == rule1.p2$Campaign[i]) %>% 
                 filter(qs.bucket == "Medium") %>% 
                 mutate(Cost.Prop = (Cost / sum(Cost))) %>% 
                 arrange(desc(Cost.Prop)) %>% 
                 mutate(Proportion.Of.Campaign.Cost = percent(Cost / rule1.p2$Total.Spend[i])) %>% 
                 top_n(5, Cost.Prop) %>% 
                 mutate(Proportion.Of.QS.Bucket = percent(Cost.Prop)) %>%
                 mutate(Priority = "Prority2") %>% 
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
                          
                          }}) #closing braces for progress bar
         return(final_df)
         
     })
     
     
     
     output$qs_download <- downloadHandler(
         
         filename = function() {
             paste("qs-optimisation-", Sys.Date(), ".csv", sep="")
         },
         content = function(file) {
             data.download <- final_data()
             write.csv(data.download, file)
         }
     )
     
     
 }
 
 