library(shiny)
library(dplyr)
library(tidyr)
library(scales)
library(data.table)
library(DT)

# process_data is called by process_upload (inside server) and it 
# creates a clean dataframe, subsets the relevant columns
# and returns its value inside the server function
process_data <- function(df) {
    #colClasses mapped to the column names as the report was created on 12/09/2017 by rachit.kinger@jpress.co.uk
    colClasses <- c("character", #Account
                    "character", #Campaign
                    "character", #Keyword
                    "character", #Customer.ID
                    "factor",    #Keyword.State
                    "numeric",   #Clicks
                    "numeric",   #Cost
                    "integer",   #Quality.score
                    "factor",    #Ad.group.state
                    "factor",    #Campaign.state
                    "factor",    #Status
                    "factor",    #Expected.click.through.rate
                    "factor",    #Ad.relevance
                    "factor"     #Landing.page.experience
                    
    )
    #converting each columns class as desired (area of improv - how to do this at the time of loading data?)
    for (i in 1:length(colClasses)) {
        if(colClasses[i] == "character") {df[,i] <- as.character(df[,i])}
        else if(colClasses[i] == "integer") {df[,i] <- as.integer(df[,i])}
        else if(colClasses[i] == "numeric") {df[,i] <- as.numeric(df[,i])}
        else if(colClasses[i] == "factor") {df[,i] <- as.factor(df[,i])}
    }
    
    # data wrangling
    
    df <- tbl_df(df)
    
    # define QS-buckets  #could be user defined
    LQS <- c(1,2,3)    #low quality score
    MQS <- c(4,5,6)    #medium quality score
    HQS <- c(7,8,9,10) #high quality score
    
    # create new variable qs-bucket based on above definitions
    qs_included <- df %>% 
        mutate(qs.bucket = ifelse(Quality.score %in% LQS,"Low",
                                  ifelse(Quality.score %in% MQS, "Medium",
                                         ifelse(Quality.score %in% HQS, "High", NA))))
    global_data <<- qs_included
    return(global_data)
}

# budget is called by budget_range (inside server). It applies
# the budget.slider options on the dataframe returned by 
# process_data and sends it back for further use
budget <- function(slider = c(300,2000), df) {
    with_budget <- df %>% 
        group_by(Campaign) %>% 
        filter(sum(Cost) > slider[1] & sum(Cost) < slider[2])    
    
    global_data_with_budget <<- with_budget
    return(global_data_with_budget)
    
}

#p1p2data contains rules for priority1 priority2 and processes data as per that
# it is called from inside the server and subsets the budget data into relevant
# priorities and orders them
p1p2data <- function(df) { 
    campaigns <- distinct(df, Campaign) #unique Campaigns only
    LQS <- NULL
    for(i in 1:nrow(campaigns)) {
        x <- df %>% filter(Campaign == campaigns[i,]) %>% filter(qs.bucket == "Low") 
        LQS[i] <- if(nrow(x) > 0) {sum(x$Cost, na.rm = TRUE)} else {0}
    }
    
    MQS <- NULL
    for(i in 1:nrow(campaigns)) {
        x <- df %>% filter(Campaign == campaigns[i,]) %>% filter(qs.bucket == "Medium") 
        MQS[i] <- if(nrow(x) > 0) {sum(x$Cost, na.rm = TRUE)} else {0}
    }
    
    HQS <- NULL
    for(i in 1:nrow(campaigns)) {
        x <- df %>% filter(Campaign == campaigns[i,]) %>% filter(qs.bucket == "High") 
        HQS[i] <- if(nrow(x) > 0) {sum(x$Cost, na.rm = TRUE)} else {0}
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
    
    names(r1.p1.formatted)[1] <- "Priority 1 Campaigns"
    names(r1.p2.formatted)[1] <- "Priority 2 Campaigns"
    
    global_final_list <<- list(p1.formated=r1.p1.formatted, 
                               p2.formated=r1.p2.formatted,
                               p1= r1.p1,
                               p2= r1.p2,
                               alldata = df)
    
    #       }           #closing braces #for progress bar
    # })               #closing braces #for progress bar
    return(global_final_list)
}

#create_final_data creates the final datatable that contains keywords
#and performance indicators. It is run from inside the server and it
#updates the global_final_dt. This is also the table that is downloaded when
# user clicks on Export Analysis button
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
    
    global_final_dt <<- rbind(p1_df,p2_df)
    return(global_final_dt)
    
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
                                colClasses = rep("character", 14))
        user_upload <- user_upload[-nrow(user_upload),] # remove last row, called "Total"
        
        return(process_data(user_upload))
    })
    
    # budget_range is a reactive function that calls budget (global function)
    
        budget_range <- reactive({
            data_for_budget <- process_upload()
            progress <- Progress$new(session, min = 1, max = 4)
            on.exit(progress$close())
            progress$set(message = "Processing data",
                         detail = "...applying budgets",
                         value = 2)
            return(budget(slider = input$budget.range, df = data_for_budget))
        })
        
        
       output$p1.table <- renderTable({
                progress <- Progress$new(session, min = 1, max = 4)
                on.exit(progress$close())
                progress$set(message = "Processing data",
                             detail = "...determinig priorities",
                             value = 3)
            return(p1p2data(budget_range())$p1.formated)
            
        })
        
        
        output$p2.table <- renderTable({
            progress <- Progress$new(session, min = 1, max = 4)
            on.exit(progress$close())
            progress$set(message = "Processing data",
                         detail = "...implementing priorities",
                         value = 3) 
            return(p1p2data(budget_range())$p2.formated)
        })
        
        output$final.table <- DT::renderDataTable(filter = "top", DT::datatable({
            progress <- Progress$new(session, min = 1, max = 4)
            on.exit(progress$close())
            progress$set(message = "Almost Done!",
                         detail = "...creating final output",
                         value = 3)
            return(create_final_data(p1p2data(budget_range())))
        }))
        
        
        output$qs_download <- downloadHandler(
            filename = function() {
                paste("qs-optimisation-", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
                my_dt <- global_final_dt
                if(is.null(my_dt)) {
                    progress <- Progress$new(session, min = 1, max = 4)
                    on.exit(progress$close())
                    progress$set(message = "Done!",
                                 detail = "...creating final output",
                                 value = 3)
                     my_dt <-  global_final_dt
                } 
                write.csv(my_dt, file)
            }
        )
        
       
}

