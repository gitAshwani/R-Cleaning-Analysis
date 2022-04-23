# --------------------------------------------------------------
# Business Analytics Tools - Open Source - Group 12
#     FAN Fangda
#     NITHARWAL Ashwani 
#     RAJI Hind
# --------------------------------------------------------------

# --------------------------------------------------------------
# Load libraries
# --------------------------------------------------------------
# install.packages("readxl")
# rm(list=ls())
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library (ggplot2)
load("DataGroupAssignment.Rdata")

products <- read_excel("Appendices Group Assignment.xlsx",sheet=1)

countries <- read_excel("Appendices Group Assignment.xlsx",sheet=2)
languages <- read_excel("Appendices Group Assignment.xlsx",sheet=3)
applications <- read_excel("Appendices Group Assignment.xlsx",sheet=4)
#########################################################

########   Demographics  ########

# check size of the table 
dim(Demographics) 
# check length of unique values per variable 
sapply(Demographics, function(x) length(unique(x))) 

# check 'NULL' character variables
sapply(Demographics,function(x) sum(x=="NULL"))
# check NA values 
sapply(Demographics,function(x) sum(is.na(x))) 

# convert dates datatypes 
Demographics$FirstPay <- as.Date(Demographics$FirstPay, "%Y%m%d") 
Demographics$FirstAct <- as.Date(Demographics$FirstAct, "%Y%m%d") 
Demographics$FirstSp <- as.Date(Demographics$FirstSp, "%Y%m%d") 
Demographics$FirstCa <- as.Date(Demographics$FirstCa, "%Y%m%d") 
Demographics$FirstGa <- as.Date(Demographics$FirstGa, "%Y%m%d")
Demographics$FirstPo <- as.Date(Demographics$FirstPo, "%Y%m%d")

# check row with missing gender 
Demographics[is.na(Demographics$Gender),] 
# check gender distribution in language == 4 
Demographics %>%filter(Language==4) %>%count(Gender) 
# assign gender 1 to missing value 
Demographics[is.na(Demographics$Gender),"Gender"] <- 1 
# check row with missing gender again
Demographics[is.na(Demographics$Gender),]
# rename the Gender column 
Demographics <- Demographics %>% mutate(Gender = ifelse(Gender == 0,"Female","Male")) 

# merge with countries 
Demographics <- merge(x=Demographics,y=countries,by="Country",all.x=TRUE) 
# merge with languages 
Demographics <- merge(x=Demographics,y=languages,by="Language",all.x=TRUE) 
# merge with applications 
Demographics <- merge(x = Demographics, y = applications, by = "ApplicationID", all.x=TRUE, all.y = FALSE)

# remove index columns 
Demographics <- Demographics %>%select(-c("ApplicationID","Language","Country")) 
# rename new columns after join 
Demographics <- Demographics %>%rename(Country = "Country Name", Language = "Language Description", Application = "Application Description") 
# validate min and max of RegDate to be within timeline
min(Demographics$RegDate)
max(Demographics$RegDate)

# check NA values 
sapply(Demographics,function(x) sum(is.na(x))) 
# check Demographics table size with 42649 rows
dim(Demographics) 
# check row with missing FirstAct
Demographics[is.na(Demographics$FirstAct),] 
# remove the 2 rows with missing FirstAct
# data[data$x1 != 2 & data$x2 != "e", ]  
Demographics <- Demographics[Demographics$UserID != 1371535 & Demographics$UserID != 1365903, ] 
# check NA values again
sapply(Demographics,function(x) sum(is.na(x))) 
# check Demographics table size with 42647 rows
dim(Demographics) 

# create variable for time difference between first active date and registration date 
Demographics$RegDate_FirstAct <- as.Date(Demographics$FirstAct) - as.Date(Demographics$RegDate)

# create variable for whether customer plays a category
Demographics$PlaysSportsbook = ifelse(is.na(Demographics$FirstSp),"No","Yes")
Demographics$PlaysCasino = ifelse(is.na(Demographics$FirstCa),"No","Yes")
Demographics$PlaysGames = ifelse(is.na(Demographics$FirstGa),"No","Yes")
Demographics$PlaysPoker = ifelse(is.na(Demographics$FirstPo),"No","Yes")

# check the minimul of FirstPay variable for DailyAgg table
min(Demographics$FirstPay)




###DaillyAgg#####################


#created UserDailyAggregation2 to avoid directly manipulating the original table UserDailyAggregation 
UserDailyAggregation2 <- UserDailyAggregation

#reading sheet 1 for product ID values

productList <- as.list(products$`Product Description`)

#adding product description to user aggregate table 
UserDailyAggregation2 <- left_join(UserDailyAggregation2,products,by="ProductID")

#changing data type of date column
UserDailyAggregation2$Date <- as.Date(UserDailyAggregation2$Date, "%Y%m%d") 
#add column balance
UserDailyAggregation2 <- UserDailyAggregation2 %>% mutate(Balance = Winnings - Stakes)

#creating new subset dataframe having only userid and firstpay
useridfirstpay <- data.frame(Demographics$UserID,Demographics$FirstPay)
#converting datatype of firstpay
useridfirstpay$Demographics.FirstPay <- as.Date(useridfirstpay$Demographics.FirstPay, "%Y%m%d") 
#renaming columns
useridfirstpay <- useridfirstpay %>% rename(UserID = Demographics.UserID, FirstPay = Demographics.FirstPay)
#adding the firstpay column to user aggregate table
UserDailyAggregation2 <- left_join(UserDailyAggregation2,useridfirstpay,by='UserID')
#creating a Dummy variable to know if 1 then include it in next step
UserDailyAggregation2$Dummy <- ifelse(UserDailyAggregation2$Date >= UserDailyAggregation2$FirstPay, 1, 0)
#subsetting the data and keeping only the values with dummy variable value 1
UserDailyAggregation2 <- subset(UserDailyAggregation2, Dummy==1)  

#checking na values
sapply(UserDailyAggregation2,function(x) sum(is.na(x))) 

#creating aggreagation dataset by grouping by UserID and knowing the insights from it
aggregation <- UserDailyAggregation2 %>% group_by(UserID) %>%
    summarise(total_win_amount = sum(Winnings),
              total_stake_amount = sum(Stakes),
              final_balance = sum(Balance),
              total_bets = sum(Bets),
              total_count_of_presence = n(),
              bets_per_presence = total_bets/ total_count_of_presence,
              #final_win_vs_stake variable have 11 na values and so the status too have 11 na values. Check.
              final_win_vs_stake = total_win_amount/ total_stake_amount,
              status = ifelse(final_win_vs_stake>1,'Profit','Loss')) 

#creating monthwise dataset to get insights monthwise
monthwise <- UserDailyAggregation2 %>% group_by(format(Date, "%m")) %>%
    summarise(total_win_amount = sum(Winnings),
              total_stake_amount = sum(Stakes),
              final_balance = sum(Balance),
              total_bets = sum(Bets),
              count = n())

#creating productwise dataset to get insights productwise
productwise <- UserDailyAggregation2 %>% group_by(ProductID) %>%
    summarise(total_win_amount = sum(Winnings),
              total_stake_amount = sum(Stakes),
              final_balance = sum(Balance),
              total_bets = sum(Bets),
              count = n())

#creating month column with only month number
UserDailyAggregation2$Month <- as.numeric(format(UserDailyAggregation2$Date, "%m"))

#creating productID columns for every userID
useridproductwise <- as.data.frame(unique(UserDailyAggregation2$UserID))
colnames(useridproductwise) <- 'UserID'
for (i in 1:8) {
    prod <- UserDailyAggregation2 %>% filter(ProductID==i) %>% 
        group_by(UserID) %>%
        dplyr::summarise(avgStakes = mean(Stakes),
                         avgWins = mean(Winnings),
                         totalBets = sum(Bets),
                         balance = sum(Balance),
                         count=n()) %>% 
        mutate(win_vs_stake = avgWins/avgStakes) %>%
        transmute(UserID,
                  !!paste0("win_vs_stake_product_",productList[i]):=win_vs_stake,
                  !!paste0("count_product_",productList[i]):=count,
                  !!paste0("total_bets_product_",productList[i]):=totalBets,
                  !!paste0("balance_product_",productList[i]):=balance)
    
    useridproductwise <- left_join(useridproductwise,prod,by='UserID')
}

#creating monthwise columns for every userId

for (i in 2:9) {
    prod1 <- UserDailyAggregation2 %>% filter(Month==i) %>% 
        group_by(UserID) %>%
        dplyr::summarise(avgStakes = mean(Stakes),
                         avgWins = mean(Winnings),
                         totalBets = sum(Bets),
                         balance = sum(Balance),
                         count=n()) %>% 
        mutate(win_vs_stake = avgWins/avgStakes) %>%
        transmute(UserID,
                  !!paste0("win_vs_stake_month_",i):=win_vs_stake,
                  !!paste0("count_month_",i):=count,
                  !!paste0("total_bets_month_",i):=totalBets,
                  !!paste0("balance_month_",i):=balance)
    
    useridproductwise <- left_join(useridproductwise,prod1,by='UserID')
}
#replacing NA values with 0
useridproductwise[is.na(useridproductwise)] = 0

#assigning to a new table
useraggbasetable <- useridproductwise

#left join aggreagation table for full aggregated values of a userID 
#useraggbasetable is the final table for useraggregation table
useraggbasetable <- left_join(useraggbasetable,aggregation,by='UserID')

sapply(aggregation,function(x) sum(is.na(x))) 

# 11 missing values for "status" column and "final_win_vs_stake" column
sapply(useraggbasetable,function(x) sum(is.na(x))) 






########   POKERCHIPS  ########

# split the TransDateTime column in PokerChipConversions
Poker_a <- separate(PokerChipConversions,TransDateTime, c("date", "time"),sep =" ")
Poker_a$date <- as.Date(Poker_a$date)
Poker_a$time <- as.numeric(substr(Poker_a$time,1,2))

# create new Poker2 table with transformed variables
Poker2  <- Poker_a %>% group_by(UserID) %>%  
    summarise(MaxPokerTranDate = max(date),
              MinPokerTranDate = min(date),
              TotalPokerTranSell = sum(TransAmount[TransType==24]),
              TotalPokerTranBuy = sum(TransAmount[TransType==124]),
              MaxPokerTranSell = max(TransAmount[TransType==24]),
              MaxPokerTranBuy = max(TransAmount[TransType==124]),
              MinPokerTranSell = min(TransAmount[TransType==24]),
              MinPokerTranBuy = min(TransAmount[TransType==124]),
              AvgPokerTranSell = mean(TransAmount[TransType==24]),
              AvgPokerTranBuy = mean(TransAmount[TransType==124]),
              CountPokerTranSell = sum(TransType==24),
              CountPokerTranBuy = sum(TransType==124)
    )

# check NA values 
sapply(Poker2,function(x) sum(is.na(x))) 

#create NA marker for AvgPokerTranBuy
#O when value is missing and 1 when it exists
Poker2$MarkerAvgBuy<-ifelse(is.na(Poker2$AvgPokerTranBuy), 0, 1)
# mean(AvgPokerTranBuy) = 34.8 
Poker2 %>% filter(MarkerAvgBuy == 1) %>% summarize(Avg = mean(AvgPokerTranBuy))
#create NA marker for AvgPokerTranSell
Poker2$MarkerAvgSell<-ifelse(is.na(Poker2$AvgPokerTranSell), 0, 1)
# mean(AvgPokerTranSell) = 38.4 
Poker2 %>% filter(MarkerAvgSell == 1) %>% summarize(Avg = mean(AvgPokerTranSell))
# fix missing values
Poker2$AvgPokerTranBuy[is.nan(Poker2$AvgPokerTranBuy)] <- 34.8
Poker2$AvgPokerTranSell[is.nan(Poker2$AvgPokerTranSell)] <- 38.4
# check NA values again
sapply(Poker2,function(x) sum(is.na(x))) 
# check Inf values
sapply(Poker2,function(x) sum(x==Inf))
#create NA marker for MinPokerTranBuy
Poker2$MarkerMinBuy<-ifelse(Poker2$MinPokerTranBuy == Inf, 0, 1)
# max(MinPokerTranBuy) = 2274 
Poker2 %>% filter(MarkerMinBuy == 1) %>% summarize(max = max(MinPokerTranBuy))

Poker2$MinPokerTranBuy <- ifelse(Poker2$MinPokerTranBuy == Inf , 2274, Poker2$MinPokerTranBuy)

#create NA marker for MinPokerTranSell
Poker2$MarkerMinSell<-ifelse(Poker2$MinPokerTranSell == Inf, 0, 1)
# max(MinPokerTranSell) = 980 
Poker2 %>% filter(MarkerMinSell == 1) %>% summarize(max = max(MinPokerTranSell))

Poker2$MinPokerTranSell <- ifelse(Poker2$MinPokerTranSell == Inf , 980, Poker2$MinPokerTranSell)

# check Inf values again
sapply(Poker2,function(x) sum(x==Inf))

# check -Inf values
sapply(Poker2,function(x) sum(x==-Inf))
#create NA marker for MaxPokerTranBuy
Poker2$MarkerMaxBuy<-ifelse(Poker2$MaxPokerTranBuy == -Inf, 0, 1)
# min(MaxPokerTranBuy) = 0.0005 
Poker2 %>% filter(MarkerMaxBuy == 1) %>% summarize(min = min(MaxPokerTranBuy))

Poker2$MaxPokerTranBuy <- ifelse(Poker2$MaxPokerTranBuy == -Inf , 0.0005, Poker2$MaxPokerTranBuy)

#create NA marker for MaxPokerTranSell
Poker2$MarkerMaxSell<-ifelse(Poker2$MaxPokerTranSell == -Inf, 0, 1)
# min(MaxPokerTranSell) = 0.00612 
Poker2 %>% filter(MarkerMaxSell == 1) %>% summarize(min = min(MaxPokerTranSell))

Poker2$MaxPokerTranSell <- ifelse(Poker2$MaxPokerTranSell == -Inf , 0.00612, Poker2$MaxPokerTranSell)
# check -Inf values again
sapply(Poker2,function(x) sum(x==-Inf))

# total number poker transaction days per user
Poker2$PokerTranDays<- as.numeric(difftime(Poker2$MaxPokerTranDate,Poker2$MinPokerTranDate, units = "days"))

# sell frequency 
Poker2$FrequencyPokerSell <- ifelse(Poker2$PokerTranDays == 0,0,
                                    Poker2$CountPokerTranSell / Poker2$PokerTranDays)
# buy frequency 
Poker2$FrequencyPokerBuy <- ifelse(Poker2$PokerTranDays == 0,0,
                                   Poker2$CountPokerTranBuy / Poker2$PokerTranDays)
# poker balance
Poker2$PokerBalance <- Poker2$TotalPokerTranSell - Poker2$TotalPokerTranBuy

# remove marker columns 
Poker2 <- Poker2 %>%select(-c("MarkerAvgBuy","MarkerAvgSell","MarkerMinBuy","MarkerMinSell","MarkerMaxBuy","MarkerMaxSell")) 

# merge Poker final table and Demographics final table
DemoPoker <- merge(Demographics, Poker2, by = 'UserID', all.x = TRUE, all.y = TRUE)
# subset MinPokerTranDate > FirstPay 
DemoPoker2 <- DemoPoker[DemoPoker$MinPokerTranDate > DemoPoker$FirstPay, ] 
# check NA values in DemoPoker2, rows where UserID=NA means MinPokerTranDate < FirstPay
sapply(DemoPoker2,function(x) sum(is.na(x))) 
# remove rows where UserID is NA in DemoPoker2 
DemoPoker2 <- DemoPoker2[which(!is.na(DemoPoker2$UserID)),]
# check NA values in DemoPoker2 again, before had 2387 entries in Poker2, now DataMart2 has 1164 valid entries
sapply(DemoPoker2,function(x) sum(is.na(x))) 
# remove Demographics columns 
DemoPoker2 <- DemoPoker2 %>%select(-c("RegDate","FirstPay","FirstAct","FirstSp","FirstCa","FirstGa","FirstPo","Gender","Country","Language","Application","RegDate_FirstAct","PlaysSportsbook","PlaysCasino","PlaysGames","PlaysPoker")) 

sapply(DemoPoker2,function(x) sum(is.na(x))) 


### CREATING THE DATAMART ###


DataMart <- merge(Demographics, useraggbasetable, by = 'UserID', all.x = TRUE, all.y = TRUE)
DataMart <- merge(DataMart, DemoPoker2, by ='UserID',all.x = TRUE, all.y = TRUE)

sapply(DataMart,function(x) sum(is.na(x))) 

# rm(list=c("aggregation" , "applications", "countries" , "DemoPoker","languages","monthwise","Poker_a","Poker2","prod","prod1","productList","products","productwise","UserDailyAggregation2","useridfirstpay","useridproductwise"))

#replacing NA values with 0
library(lubridate)
DataMart <- replace_na(DataMart,list(value=0))

DataMart$FirstAct <- as.Date(DataMart$FirstAct, "%Y%m%d") 
DataMart$month_FirstAct <- month(DataMart$FirstAct)

### Analysis ###

# plot1: country
user_by_country_plot <- DataMart %>% 
  group_by(Country,month_FirstAct) %>% summarize(count = n()) %>% 
  arrange(desc(count)) %>% 
  mutate(percent = round(count/sum(count) * 100,2)) %>% 
  filter(percent > 0.1)  %>%
    ungroup()

#ggplot(user_by_country_plot, aes(x=count, y=reorder(Country,count))) + 
#   geom_bar(stat = "identity", fill = "light blue") +
#   geom_text(aes(label=paste(percent,"%",sep="")),hjust=1, col="white", size=2.5) +
#    labs(title= "COUNT OF USERS PER COUNTRY",
#         x = "Number of users") 

# plot2: gender
gender_plot <- DataMart %>%
    group_by(Gender,month_FirstAct) %>% summarize(count=n()) %>%
    ungroup()

#ggplot(gender_plot, aes(x=count, y=Gender)) + 
#   geom_bar(stat = "identity", fill = "light blue") +
#    labs(title= "COUNT OF USERS PER GENDER",
#         x = "Number of users") 

# plot3: users by country by gender 
user_by_country_gender_plot <- DataMart %>% 
    group_by(Country,Gender) %>% 
    summarize(count = n()) %>% 
    arrange(Country, Gender) %>%
    filter(count > 200) %>%
    ungroup()

#ggplot(user_by_country_gender_plot, aes(x=count, y=Country)) + 
#    geom_bar(position = "dodge",stat = "identity", aes(fill=Gender)) +
#    labs(title= "COUNT OF USERS BY COUNTRY AND GENDER",
#         x = "Number of users",
#         y = "Country and Gender") 

# plot4: RegDate_FirstAct by gender 
RegDate_FirstAct_gender_plot <- DataMart %>% 
    group_by(Gender,month_FirstAct) %>% 
    summarize(mean = mean(RegDate_FirstAct)) %>%
    ungroup()

#ggplot(RegDate_FirstAct_gender_plot, aes(x=mean, y=Gender)) + 
#    geom_bar(stat = "identity", fill = "light blue") +
#    labs(title= "TIME FROM REGISTRATION TO FIRST ACTIVE DATE PER GENDER",
#         x = "Days") 

# plot5: RegDate_FirstAct by country
# lowest 26 countries, filtered by days < 10
RegDate_FirstAct_country_plot <- DataMart %>% 
    group_by(Country) %>% 
    summarize(mean = mean(RegDate_FirstAct)) %>%
    arrange (mean)%>%
    mutate(mean=round(mean,2))%>%
    filter(mean<10) %>%
    ungroup()

#ggplot(RegDate_FirstAct_country_plot, aes(x=mean, y=reorder(Country,mean))) + 
#    geom_bar(stat = "identity", fill = "light blue") +
#    geom_text(aes(label=mean,hjust=1, col="white", size=2.5)) +
#    labs(title= "TIME FROM REGISTRATION TO FIRST ACTIVE DATE PER COUNTRY",
#         x = "Days") 

##### additional calculations for plot 6 to plot 9 #####

# Poker Count
# poker count is 85.908
pokercount <- DemoPoker2 %>%
    select(c(1,12,13)) %>%
    mutate(pokercount = CountPokerTranSell + CountPokerTranBuy)%>%
    summarize(pokercount=mean(pokercount))
# poker mean buy count (mean total bets) is 49.11082
DemoPoker2 %>%
    select(c(1,12,13)) %>%
    summarize(pokerbuycount=mean(CountPokerTranBuy))

# poker balance
# mean poker balance is -326.6411
DemoPoker2 %>%
    select(c(1,17))%>%
    summarize(pokerbal=mean(PokerBalance))

# poker win/stake ratio is 1.887767
pokerwinvsstake <- DemoPoker2 %>%
    select(c(1,10,11))%>%
    mutate(pokerwin_stake=AvgPokerTranSell / AvgPokerTranBuy)%>%
    summarize(mean=mean(pokerwin_stake))

###########################

# plot6: win_vs_stake per product
# poker win/stake ratio is 1.887767

win_vs_stake <- useraggbasetable %>%
    select(c(1,2,6,10,14,18,22,26,30)) 

win_vs_stake1 <- pivot_longer(win_vs_stake,-c(UserID),names_to = "product_category",values_to = "win_vs_stake_ratio")
win_vs_stake1$product_category <- substring(win_vs_stake1$product_category,22)

win_vs_stake1 <- win_vs_stake1 %>%
    group_by(product_category) %>%
    summarize(mean = mean(win_vs_stake_ratio)) %>%
    mutate(n = 1:8)

# add poker win_vs_stake
win_vs_stake1[which(win_vs_stake1$n==5),'mean'] = 1.887767
# substitute Inf value with max value for Casino BossMedia product group
win_vs_stake1[which(win_vs_stake1$n==1),'mean'] = 5

#ggplot(win_vs_stake1, aes(x=mean, y=product_category)) + 
#    geom_bar(stat = "identity", fill = "light blue") +
#    labs(title= "WIN VS STAKE RATIO PER PRODUCT",
#         x = "RATIO", y = "PRODUCT") 



# plot7: totalbets per product
# poker mean buy count (mean total bets) is 49.11082
totalbets <- useraggbasetable %>%
    select(c(1,4,8,12,16,20,24,28,32)) 

totalbets1 <- pivot_longer(totalbets,-c(UserID),names_to = "product_category",values_to = "totalbets")
totalbets1$product_category <- substring(totalbets1$product_category,20)

totalbets1 <- totalbets1 %>%
    group_by(product_category) %>%
    summarize(mean = mean(totalbets))  %>%
    mutate(n = 1:8)

# add poker totalbets
totalbets1[which(totalbets1$n==5),'mean'] = 49.11082

#ggplot(totalbets1, aes(x=mean, y=product_category)) + 
#    geom_bar(stat = "identity", fill = "light blue") +
#   labs(title= "TOTAL BETS PER PRODUCT",
#        x = "TOTALBETS", y = "PRODUCT") 

# plot8: balance per product
# mean poker balance is 326.6411
balance <- useraggbasetable %>%
    select(c(1,5,9,13,17,21,25,29,33)) 

balance1 <- pivot_longer(balance,-c(UserID),names_to = "product_category",values_to = "balance")
balance1$product_category <- substring(balance1$product_category,17)

balance1 <- balance1 %>%
    group_by(product_category) %>%
    summarize(mean = mean(balance)) %>%
    mutate(n = 1:8)

# add poker balance
balance1[which(balance1$n==5),'mean'] = -326.6411

#ggplot(balance1, aes(x=mean, y=product_category)) + 
#    geom_bar(stat = "identity", fill = "light blue") +
#   labs(title= "BALANCE PER PRODUCT",
#        x = "BALANCE", y = "PRODUCT") 

# plot9: balance per month

balancemonth <- useraggbasetable %>%
    select(c(1,37,41,45,49,53,57,61,65)) 

balancemonth1 <- pivot_longer(balancemonth,-c(UserID),names_to = "month",values_to = "balance")
# balancemonth1$product_category <- substring(balancemonth1$product_category,17)

balancemonth1 <- balancemonth1 %>%
    group_by(month) %>%
    summarize(mean = mean(balance)) 

# ggplot(balancemonth1, aes(x=mean, y=month)) + 
#    geom_bar(stat = "identity", fill = "light blue") +
#    labs(title= "BALANCE PER MONTH (EXCEPT POKER)",
#        x = "BALANCE", y = "MONTH") 


# Number of Poker Players that Used "Free Promotional Money" = 2387 - 1164 = 1223
# Max total win per user: 1093423 (UserID 1340382)
# Max total stake per user: 1127196 (UserID 1340382)
# Max balance per user: 37037.76 (UserID 1324607)
# Min balance per user: -76165.513 (UserID 1371058)
# Month with highest number of bets: March (3395828)
# Month with lowest number of bets: July (1140469)


# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# START OF CODE FOR THE SHINY APP
# ------------------------------------------------------------------------
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Sidebar 
# ------------------------------------------------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(id="mainmenu",
                menuItem("Summary", tabName = "tabSummary"),
                conditionalPanel(
                  condition = "input.mainmenu == 'tabSummary'",
                  sliderInput( "month", label = "Select the month ...", value = 5, min = 2, max = 10,step =1),
                  actionButton("update1", "Update Plot B - Users"),
                  actionButton("update2", "Update Plot C - Users"),
                  actionButton("update3", "Update Plot E - Reaction Time")
                ),
                menuItem("Betting Details", tabName =  "tabdetail")))



# ------------------------------------------------------------------------
# Dashboard body
# ------------------------------------------------------------------------
body <- dashboardBody(
    tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;}'
    ))),
    tabItems(
       
        tabItem(tabName = "tabSummary",
                h2("Users Information Summary"),
                hr(),
                fluidRow(
                   
                    valueBox(42647, "Total Number of Users",  color = "black"),
                    valueBox("March", "Month with Highest Number of Bets",  color = "black"),
                    valueBox("July", "Month with Lowest Number of Bets",  color = "black"),
                    valueBox(1093423, "Maximum Total Win Per User", color = "black"),
                    valueBox(1127196, "Maximum Total Stake Per User",  color = "black"),
                    valueBox(37037.76, "Maximum Balance Per User",  color = "black"),
                    valueBox(-76165.513, "Minimum Balance Per User",   color = "black"),
                    
                    
                ),
                hr(),
                
               
                tabsetPanel(type = "tabs", selected = "Users - Demographics Information", 
                            tabPanel("Users - Demographics Information",
                                     hr(),
                                     fluidRow(
                                         column(6,
                                                plotOutput("summ_user_by_country_gender_plot")
                                         ),
                                         column(6,
                                                plotOutput("summ_user_by_country_plot")
                                         ),
                                         column(6,plotOutput("summ_gender_plot")
                                                )
                                     )),
                            tabPanel("Reaction Time - Time From Registration To Active",
                                     hr(),
                                     fluidRow(
                                         column(6,plotOutput("summ_RegDate_FirstAct_country_plot")
                                                ),
                                         column(6,plotOutput("summ_RegDate_FirstAct_gender_plot")
                                                )
                                     )))),
        tabItem(tabName =  "tabdetail",
                h2("Betting Details"),
                hr(),
                
                
                tabsetPanel(type = "tabs", selected = "Win VS Stake Productwise", 
                            
                            tabPanel("Win VS Stake Productwise",
                                     hr(),
                                     fluidRow(
                                         column(6,
                                                plotOutput("summ_win_vs_stake1")
                                         ))),
                            tabPanel("Totalbets Productwise",
                                     hr(),
                                     fluidRow(
                                         column(6,
                                                plotOutput("summ_totalbets1")
                                         ))),
                            tabPanel("Balance Productwise",
                                     hr(),
                                     fluidRow(
                                         column(6,
                                                plotOutput("summ_balance1")
                                         ))),
                            tabPanel("Balance Monthwise",
                                     hr(),
                                     fluidRow(
                                         column(6,
                                                plotOutput("summ_balancemonth1")
                                         )))))))
                                    
    
                           


# ------------------------------------------------------------------------------
# Dashboard function
# ------------------------------------------------------------------------------
ui <- dashboardPage(
    dashboardHeader(title = "GROUP 12"),
    sidebar,
    body
)

# --------------------------------------------------------------
# SERVER 
# --------------------------------------------------------------
server <- function(input, output) {
    buttonplot1 <- eventReactive(input$update1, {
      user_by_country_plot %>% filter(month_FirstAct == input$month)
    })
 
    output$summ_user_by_country_plot <- renderPlot({
        ggplot(buttonplot1(), aes(x=count, y=reorder(Country,count))) + 
            geom_bar(stat = "identity", fill = "light blue") +
            geom_text(aes(label=paste(percent,"%",sep="")),hjust=1, col="white", size=2.5) +
            labs(title= "PLOT B: COUNT OF USERS PER COUNTRY",
                 x = "Number of users", y = "Country") 
    })
    
    buttonplot2 <- eventReactive(input$update2, {
      gender_plot %>% filter(month_FirstAct == input$month)
    })
    
    output$summ_gender_plot <- renderPlot({
        ggplot(buttonplot2(), aes(x=count, y=Gender)) + 
            geom_bar(stat = "identity", fill = "light blue") +
            labs(title= "PLOT C: COUNT OF USERS PER GENDER",
                 x = "Number of users", y = "Gender") 
    })
    
   
    
    output$summ_user_by_country_gender_plot <- renderPlot({
        ggplot(user_by_country_gender_plot, aes(x=count, y=Country)) + 
            geom_bar(position = "dodge",stat = "identity", aes(fill=Gender)) +
            labs(title= "PLOT A: COUNT OF USERS BY COUNTRY AND GENDER",
                 x = "Number of users",
                 y = "Country") 
    })
    
    buttonplot3 <- eventReactive(input$update3, {
      RegDate_FirstAct_gender_plot %>% filter(month_FirstAct == input$month)
    })
    
    output$summ_RegDate_FirstAct_gender_plot <- renderPlot({
        ggplot(buttonplot3(), aes(x=mean, y=Gender)) + 
            geom_bar(stat = "identity", fill = "light blue") +
            labs(title= "PLOT E: Reaction Time BY GENDER",
                 x = "Days") 
    })
    
    
    output$summ_RegDate_FirstAct_country_plot <- renderPlot({
        ggplot(RegDate_FirstAct_country_plot, aes(x=mean, y=reorder(Country,mean))) + 
            geom_bar(stat = "identity", fill = "light blue") +
            geom_text(aes(label=mean,hjust=1, col="white", size=2.5)) +
            labs(title= "PLOT D: Reaction Time BY COUNTRY",
                 x = "Days", y = "Country") 
    })
    
    output$summ_win_vs_stake1 <- renderPlot({
        ggplot(win_vs_stake1, aes(x=mean, y=product_category)) + 
            geom_bar(stat = "identity", fill = "light blue") +
            labs(title= "WIN VS STAKE RATIO PER PRODUCT",
                 x = "RATIO", y = "PRODUCT") 
    })
    
    

    output$summ_totalbets1 <- renderPlot({
        ggplot(totalbets1, aes(x=mean, y=product_category)) + 
            geom_bar(stat = "identity", fill = "light blue") +
            labs(title= "TOTAL BETS PER PRODUCT",
                 x = "TOTALBETS", y = "PRODUCT") 
    })
    
    output$summ_balance1 <- renderPlot({
        ggplot(balance1, aes(x=mean, y=product_category)) + 
            geom_bar(stat = "identity", fill = "light blue") +
            labs(title= "BALANCE PER PRODUCT",
                 x = "BALANCE", y = "PRODUCT") 
    })
    
    output$summ_balancemonth1 <- renderPlot({
        ggplot(balancemonth1, aes(x=mean, y=month)) + 
            geom_bar(stat = "identity", fill = "light blue") +
            labs(title= "BALANCE PER MONTH (EXCEPT POKER)",
                 x = "BALANCE", y = "MONTH") 
    })
    
  
    
    
}

shinyApp(ui = ui, server = server)








