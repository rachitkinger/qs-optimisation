## FAQs for QS Optimisation  

If you want to know how to use the app, please see below but if you have any specific questions on the algorithm please write to the product team.  

### How to use the app  
Currently, this app only works with a specific file type. The file type is available in the reports section of the MCC. Details of the file are:  
Name: `all-campaigns-keyword-data`  
Type: `Keyword performance`  
Format: `.csv`  
Creation date: `6 Sep 2017`  
Created by: `rachit.kinger@jpress.co.uk`  

**Step 1** Download the file and save it to your desktop  
**Step 2** Open the app by going to this URL (yet to finalised)  
**Step 3** Upload the file as it is  
Wait for some time. Processing the file can take upto a minute. (we are working on how to reduce that time, please bear with us)  

#### Interpreting the output  
The analysis is available on the screen. As soon as the output is ready, the screen will be updated with three tables.  

**Table 1** contains the names of Accounts and Campaigns that are top priority for optimisation.  
**Table 2** contains the names of Accounts and Campaigns that are top priority for optimisation.  
**Table 3** contains full details on keywords and what the problem areas are. (this table can be downloaded as a `.csv` file. Just click on the *Export Analysis* button)  

##### What do the different columns mean?  
`Priority`  This is self-evident. Priority is decided upon the proportion of campaign spend that is going towards low quality keywords. The higher the proportion, the higher the priority  
`Campaign`  Campaign name as available in the MCC  
`Keyword`   Keyword along with match type  
`qs.bucket` Stands for Quality Score Bucket. There are 3 buckets - Low, Medium & High based on the quality score of the keyword on the date of download of the report  
`Expected.click.through.rate`, `Ad.relevance`, and, `Landing.page.experience`   There are 3 states that each of these variables can take - Below Average, Average & Above Average. We need to improve these indicators to improve the overall Quality Score, and hence reduce the amount of money spent on low quality keywords. The table at the end of this section will help you understand what to do when these indicators are Below Average.  
`Proportion.Of.QS.Bucket`   The proportion of money that this keyword attracts as compared to the entire QS Bucket. For example: if a campaign spends £100 and 50% of that money is going in Low QS Bucket. And £20 is being spent on this keyword, the Proportion of QS Bucket spend that this keyword represents is £20/£50 = 40%  
`Proportion.Of.Campaign.Cost`   The proportion of money that this keyword attracts as compared to the entire campaign's spend. Taking the previous example, the proportion that this keyword represents is £20/£100 = 20%.  
`Campaign.Spend`    The money spent by this campaign in the last 30 days.  

 
 Indicator variables and what actions we can take if the indicator is Below Average or Average:  
 
       Indicator variable     |  Parameter                 
       -----------------------|----------------------------
       Expected CTR           |  Keyword optimisation      
       Ad copy relevance      |  Ad copy optimisation      
       Landing page relevance |  Landing page optimisation  

##### What to do when you have identified a parameter
**Keyword optimisation**: Look at search terms and see if we can find more suitable keywords to capture these search terms  
**Ad copy optimisation**: See if you can include the keyword in the ad copy  
**Landing page optimisation**: See if you can find a more suitable landing page for it, or suggest to the client to include this and other similar keywords in the landing page  


You can go back to the [main page](https://rachitkinger.github.io/qs-optimisation/). 