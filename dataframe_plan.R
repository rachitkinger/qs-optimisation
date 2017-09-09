# return entire p1 in a dataframe
# plan is to return the entire p1 & p2 in a dataframe and then
# use datatable layout like this - 
# http://shiny.rstudio.com/gallery/basic-datatable.html

p1.low <- list(NULL)
p1.med <- list(NULL)
for(i in 1:no.of.p1.campaigns.to.optimise) {
    
    p1.low[[i]] <-  final %>% 
        filter(Campaign == rule1.p1$Campaign[i]) %>% 
        filter(qs.bucket == "Low") %>% 
        mutate(Cost.Prop = Cost / sum(Cost)) %>% 
        arrange(desc(Cost.Prop)) %>% 
        mutate(Cumulative.Cost = cumsum(Cost.Prop)) %>% 
        top_n(5, Cost.Prop) %>% 
        mutate(Priority = "Prority1") %>% 
        select(Priority, Campaign, Keyword, qs.bucket, Expected.click.through.rate, Ad.relevance, Landing.page.experience)
    
    p1.med[[i]] <- final %>% 
        filter(Campaign == rule1.p1$Campaign[i]) %>% 
        filter(qs.bucket == "Medium") %>% 
        mutate(Cost.Prop = Cost / sum(Cost)) %>% 
        arrange(desc(Cost.Prop)) %>% 
        mutate(Cumulative.Cost = cumsum(Cost.Prop)) %>% 
        top_n(5, Cost.Prop) %>% 
        mutate(Priority = "Prority1") %>% 
        select(Priority, Campaign, Keyword, qs.bucket, Expected.click.through.rate, Ad.relevance, Landing.page.experience)
    
}

p1_df <- NULL
for(z in 1:length(p1.low)) {
    p1_df <- rbind(p1_df, p1.low[[z]])
}

for(y in 1:length(p1.med)) {
    p1_df <- rbind(p1_df, p1.med[[y]])
}

## old version

p1 <- list()
for(i in 1:no.of.p1.campaigns.to.optimise) {
    low <-  final %>% 
        filter(Campaign == rule1.p1$Campaign[i]) %>% 
        filter(qs.bucket == "Low") %>% 
        select(Account, Campaign, Keyword, Cost, Quality.score, qs.bucket,Expected.click.through.rate, Ad.relevance, Landing.page.experience) %>% 
        mutate(Cost.Prop = Cost / sum(Cost)) %>% 
        arrange(desc(Cost.Prop)) %>% 
        mutate(Cumulative.Cost = cumsum(Cost.Prop)) %>% 
        top_n(5, Cost.Prop)
    
    med <- final %>% 
        filter(Campaign == rule1.p1$Campaign[i]) %>% 
        filter(qs.bucket == "Medium") %>% 
        select(Account, Campaign, Keyword, Cost, Quality.score, qs.bucket,Expected.click.through.rate, Ad.relevance, Landing.page.experience) %>% 
        mutate(Cost.Prop = Cost / sum(Cost)) %>% 
        arrange(desc(Cost.Prop)) %>% 
        mutate(Cumulative.Cost = cumsum(Cost.Prop)) %>% 
        top_n(5, Cost.Prop)
    
    p1[[i]] <- list(CampaignID = rule1.p1$Campaign[i],
                    LQS = low,
                    MQS = med)
}

p2 <- list()
for(i in 1:no.of.p2.campaigns.to.optimise) {
    low <-  final %>% 
        filter(Campaign == rule1.p2$Campaign[i]) %>% 
        filter(qs.bucket == "Low") %>% 
        select(Account, Campaign, Keyword, Cost, Quality.score, qs.bucket,Expected.click.through.rate, Ad.relevance, Landing.page.experience) %>% 
        mutate(Cost.Prop = Cost / sum(Cost)) %>% 
        arrange(desc(Cost.Prop)) %>% 
        mutate(Cumulative.Cost = cumsum(Cost.Prop)) %>% 
        top_n(5, Cost.Prop)
    
    med <- final %>% 
        filter(Campaign == rule1.p2$Campaign[i]) %>% 
        filter(qs.bucket == "Medium") %>% 
        select(Account, Campaign, Keyword, Cost, Quality.score, qs.bucket,Expected.click.through.rate, Ad.relevance, Landing.page.experience) %>% 
        mutate(Cost.Prop = Cost / sum(Cost)) %>% 
        arrange(desc(Cost.Prop)) %>% 
        mutate(Cumulative.Cost = cumsum(Cost.Prop)) %>% 
        top_n(5, Cost.Prop)
    
    p2[[i]] <- list(CampaignID = rule1.p2$Campaign[i],
                    LQS = low,
                    MQS = med)
}

#naming the entire list
#to find the campaign ID of relevant campaign use t.all$Campaign1$CampaignID
names(p1) <- paste0("Campaign", seq_along(p1))
names(p2) <- paste0("Campaign", seq_along(p2))
final_list <- list(p1 = p1, p2 = p2)
