#load the keyword data into R
keyword-data <- read.csv("../data/all-campaigns-keyword-data.csv", sep = ",", skip = 5)
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
    if(colClasses[i] == "character") {t7[,i] <- as.character(t7[,i])}
    else if(colClasses[i] == "integer") {t7[,i] <- as.integer(t7[,i])}
    else if(colClasses[i] == "numeric") {t7[,i] <- as.numeric(t7[,i])}
    else if(colClasses[i] == "factor") {t7[,i] <- as.factor(t7[,i])}
}
