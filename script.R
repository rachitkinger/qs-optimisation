#load the keyword data into R
program_data <- read.csv("../data/all-campaigns-keyword-data.csv", sep = ",", 
                         skip = 5, colClasses = rep("character", 57))
program_data <- program_data[-424909,]

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
library(dplyr)
library(tidyr)
qs_data <- tbl_df(qs_data)


# define QS-buckets  #could be user defined
LQS <- c(1,2,3)    #low quality score
MQS <- c(4,5,6)    #medium quality score
HQS <- c(7,8,9,10) #high quality score

# create new variable qs-bucket based on above definitions
qs_data <- qs_data %>% 
    mutate(qs.bucket = ifelse(Quality.score %in% LQS,"Low",
                              ifelse(Quality.score %in% MQS, "Medium",
                                     ifelse(Quality.score %in% HQS, "High", NA))))
           


# separate accounts with monthly spend > £500.
budget <- 500
final <- qs_data %>% 
                    group_by(Account) %>% 
                        filter(sum(Cost) > budget)                

# # sort the data by Account level spend > Campaign level spend > Keyword level spend
# t <- summarise(final, Account.Spend = sum(Cost)) #Account.Spend is a new variable
# t <- arrange(t, desc(Spend))

# get data on each Account's spend on LQS, MQS & HQS and Total Spend
Accounts <- distinct(final, Account) #unique accounts only - this can be changed to distinct campaigns as well
LQS <- NULL
for(i in 1:nrow(Accounts)) {
    x <- final %>% filter(Account == Accounts[i,]) %>% filter(qs.bucket == "Low") 
    LQS[i] <- sum(x$Cost, na.rm = TRUE)
}

MQS <- NULL
for(i in 1:nrow(Accounts)) {
    x <- final %>% filter(Account == Accounts[i,]) %>% filter(qs.bucket == "Medium") 
    MQS[i] <- sum(x$Cost, na.rm = TRUE)
}

HQS <- NULL
for(i in 1:nrow(Accounts)) {
    x <- final %>% filter(Account == Accounts[i,]) %>% filter(qs.bucket == "High") 
    HQS[i] <- sum(x$Cost, na.rm = TRUE)
}

tot_spend <- NULL
for(i in 1:nrow(Accounts)) {
    x <- final %>% filter(Account == Accounts[i,]) 
    tot_spend[i] <- sum(x$Cost, na.rm = TRUE)
}


spend.compare.raw <- data.frame(
    Account = Accounts[,1],
    LQS.Spend = LQS/tot_spend,
    MQS.Spend = MQS/tot_spend,
    HQS.Spend = HQS/tot_spend,
    Total.Spend = tot_spend
)

library(scales) # to convert into % format and £ format, don't forget to use as.numeric when you want to run comparisons
spend.compare.formatted <- with(spend.compare.raw, data.frame(
    Account = Accounts,
    LQS.Spend = percent(LQS.Spend),
    MQS.Spend = percent(MQS.Spend),
    HQS.Spend = percent(HQS.Spend),
    Total.Spend = dollar_format(prefix = "£")(Total.Spend)
))


