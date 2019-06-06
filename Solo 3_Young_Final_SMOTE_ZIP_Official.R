#Brent Young
#MSDS 450, Winter 2019
#Solo 3 Assignment

library(Hmisc)
library(VIM) #Missingness Map
library (ggplot2) #Data Visualization

# Multiple plot function

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


###### Load Data #####

setwd("~/R/MSDS 450/Solo 3")
load("XYZ_complete_customer_data_frame.RData")
ls()
mydata <- complete.customer.data.frame #30779 observations and 554 variables
names(mydata) #column names

#write.csv(mydata, file="BDY.csv") # use your initials for the file name

###################################### Exploratory Data Analysis #####################################

##### High Level Descriptive Statistics #####
str(mydata)
summary(mydata)
#describe(mydata)
dim(mydata) #30779 customers, 554 variables

#Table by data type
type_table <- sapply(mydata, typeof)
table(type_table)
#type_table
#character    double   integer 
#345       161        48 

#Buyer Status
table(mydata$BUYER_STATUS) #Active, Inactive, and Lapsed

#16th Campaign - Response Variable (1= Responded, 0 = Did Not Respond)
table(mydata$RESPONSE16)
#    0     1 
#28187  2592

#Customers Targeted in 16th Campaign (1=Received Mailer, 0=Did Not Receive Mailer)
table(mydata$ANY_MAIL_16)
#    0     1 
#15857 14922 

#16th Campaign - Response vs. Targeted
xtabs(~RESPONSE16 + ANY_MAIL_16, data = mydata) 
#14705 people did not respond and weren't targeted
#13483 people did not respond and were targeted
#1152 people did respond and weren't targeted
#1440 people did respond and were targeted

xtabs(~RESPONSE16 + ANY_MAIL_16, data = mydata) 

##### Feature Engineering #####

#cum15TOTAMT
#Sum of Total Sales $ By Customer for Previous Campaigns
mydata$cum15TOTAMT <- mydata$TOTAMT0 + mydata$TOTAMT1 + mydata$TOTAMT2 + mydata$TOTAMT3 +
  mydata$TOTAMT4 + mydata$TOTAMT5 + mydata$TOTAMT6 + mydata$TOTAMT7 + mydata$TOTAMT8 +
  mydata$TOTAMT9 + mydata$TOTAMT10 + mydata$TOTAMT11 + mydata$TOTAMT12 + mydata$TOTAMT13 +
  mydata$TOTAMT14 + mydata$TOTAMT15

#REVPERCUST
mydata$REVPERCUST = mydata$cum15TOTAMT*1.10 - (mydata$TOTAL_MAIL_15*3.00)

#PRE2009SALES
#Total Sales Before 2009 
mydata$PRE2009SALES = mydata$LTD_SALES - mydata$YTD_SALES_2009

#PRE2009TRANSACTIONS
#Total # of Transactions Before 2009 
mydata$PRE2009TRANSACTIONS = mydata$LTD_TRANSACTIONS - 
  mydata$YTD_TRANSACTIONS_2009

#cum15QTY
#Sum of # of Items Ordered for Previous Campaigns
mydata$cum15QTY <- mydata$QTY0 + mydata$QTY1 + mydata$QTY2 + mydata$QTY3 +
  mydata$QTY4 + mydata$QTY5 + mydata$QTY6 + mydata$QTY7 + mydata$QTY8 +
  mydata$QTY9 + mydata$QTY10 + mydata$QTY11 + mydata$QTY12 + mydata$QTY13 +
  mydata$QTY14 + mydata$QTY15

#salepertrans
#Sales Per Transaction
mydata$salepertrans <- mydata$PRE2009SALES/mydata$PRE2009TRANSACTIONS

#salepercamp
#Sum of Total Sales for 15th Campaign
mydata$salepercamp <- mydata$cum15TOTAMT/mydata$TOTAL_MAIL_15

#Descriptive Statistics of Created Variables
meansalepercust <- mean(mydata$PRE2009SALES) #Average Dollars Spent Per Customer #979.2211
meansalepercust
minsalepercust <- min(mydata$PRE2009SALES) #0
minsalepercust
maxsalepercust <- max(mydata$PRE2009SALES) # 94350
maxsalepercust

mean(mydata$REVPERCUST) #Average Revenue Per Customer for Previous Campaigns #192.6719
mean(mydata$PRE2009TRANSACTIONS) #Average Transactions Per Customer for Previous Campaigns #3.800546
mean(mydata$cum15TOTAMT) #Average Total Sales Per Customer for Previous Campaignsis #187.1452
mean(mydata$cum15QTY) #Average Quantity Purchased for Previous Campaigns #1.941389

#Trimmed and Remove Outliers and NA's
mean(mydata$salepertrans, trim = 0.01, na.rm = TRUE) #Average of sale of $233 per transaction
mean(mydata$salepercamp, trim = 0.05, na.rm = TRUE) #Average sales for cumulative 15th campaign #$36

#Load Libraries
require(rpart)
require(rpart.plot)
require(tree)
require(rattle)
require(caTools)
require(ROCR)
require(ResourceSelection) #hoslem.test
library(PRROC) #ROC and Precision Recall Curve

library(corrgram)
library(MASS)
library(randomForest)
library(inTrees)
library(pROC)
library(caret)
library(dplyr)

###############################################################################################
###Step 1 - Pretend you are a domain expert and pick upto 50 'important' predictors
###################################################################################################

subdat <- subset(mydata, select=c("ZIP","REVPERCUST", "salepercamp", "PRE2009SALES","PRE2009TRANSACTIONS","cum15QTY","QTY15",
                                  "cum15TOTAMT","TOTAMT15","SUM_MAIL_15","TOTAL_MAIL_15","RESPONSE16","salepertrans",
                                  "CHANNEL_ACQUISITION","ANY_MAIL_16",
                                  "DEBIT_CC","MAJOR_CC","COMPUTER_ELECTRONIC","INC_SCS_AMT_V4","INC_WIOUTSCS_V4",
                                  "INC_WITHSCS_V4","INC_WOUTSCS_AMT_4","FIPSCNTY","MED_INC","MED_FAMINCOM","P_CAPITA_INCOM",
                                  "AVG_COMMUTETIM","MED_HOME","CUR_EST_MED_INC","STATE_INC_INDEX","STATE_INC_DECILES",
                                  "EXAGE","ADULT1_G","MARRIED","ETHNIC_MATCH","HOMEOWNR","ADD_TYPE",
                                  "LOR1","DUS","NUM_CHILD","NUMBADLT","IND_DMR","OCCUPATION_GROUP",
                                  "IND_ED","PRESCHLD","CNTY_INC","MAILPREF","CHANNEL_DOMINANCE","SALES",
                                  "ZONLINE","ZMOB","ZPRCHPHN","ZMOBMULT","ZHMDECOR","ZHOMEENT",
                                  "ZKITCHEN","ZPRCHONL"))

                                  
str(subdat)

################################################################################################
############### Step 2 - Pick only the people who were sent mailers in Campaign 16 ##########
###############################################################################################

subdat2 <- subset(subdat, ANY_MAIL_16 > 0)
str(subdat2)

##### Change Variable Types #####
#Factors
subdat2$ZIP <- as.factor(subdat2$ZIP)
subdat2$RESPONSE16 <- as.factor(subdat2$RESPONSE16)
subdat2$CHANNEL_ACQUISITION <- as.factor(subdat2$CHANNEL_ACQUISITION)
subdat2$DEBIT_CC <- as.factor(subdat2$DEBIT_CC)
subdat2$MAJOR_CC <- as.factor(subdat2$MAJOR_CC)
subdat2$COMPUTER_ELECTRONIC <- as.factor(subdat2$COMPUTER_ELECTRONIC)
subdat2$INC_WIOUTSCS_V4 <- as.factor(subdat2$INC_WIOUTSCS_V4)
subdat2$INC_WITHSCS_V4 <- as.factor(subdat2$INC_WITHSCS_V4)
subdat2$FIPSCNTY <- as.factor(subdat2$FIPSCNTY)
subdat2$ADULT1_G <- as.factor(subdat2$ADULT1_G)
subdat2$MARRIED <- as.factor(subdat2$MARRIED)
subdat2$ETHNIC_MATCH <- as.factor(subdat2$ETHNIC_MATCH)
subdat2$HOMEOWNR <- as.factor(subdat2$HOMEOWNR)
subdat2$ADD_TYPE <- as.factor(subdat2$ADD_TYPE)
subdat2$DUS <- as.factor(subdat2$DUS)
subdat2$IND_DMR <- as.factor(subdat2$IND_DMR)
subdat2$OCCUPATION_GROUP <- as.factor(subdat2$OCCUPATION_GROUP)
subdat2$IND_ED <- as.factor(subdat2$IND_ED)
subdat2$PRESCHLD <- as.factor(subdat2$PRESCHLD)
subdat2$MAILPREF <- as.factor(subdat2$MAILPREF)
subdat2$CHANNEL_DOMINANCE <- as.factor(subdat2$CHANNEL_DOMINANCE)
subdat2$SALES <- as.factor(subdat2$SALES)
subdat2$ZONLINE <- as.factor(subdat2$ZONLINE)
subdat2$ZMOB <- as.factor(subdat2$ZMOB)
subdat2$ZPRCHPHN <- as.factor(subdat2$ZPRCHPHN)
subdat2$ZMOBMULT <- as.factor(subdat2$ZMOBMULT)
subdat2$ZHMDECOR <- as.factor(subdat2$ZHMDECOR)
subdat2$ZHOMEENT <- as.factor(subdat2$ZHOMEENT)
subdat2$ZKITCHEN <- as.factor(subdat2$ZKITCHEN)
subdat2$ZPRCHONL <- as.factor(subdat2$ZPRCHONL)

#Numeric
subdat2$EXAGE <- as.numeric(subdat2$EXAGE)
subdat2$LOR1 <- as.numeric(subdat2$LOR1)
subdat2$NUM_CHILD <- as.numeric(subdat2$NUM_CHILD)
subdat2$SUM_MAIL_15 <- as.numeric(subdat2$SUM_MAIL_15)
subdat2$TOTAL_MAIL_15 <- as.numeric(subdat2$TOTAL_MAIL_15)
subdat2$REVPERCUST <- as.numeric(subdat2$REVPERCUST)
subdat2$salepercamp <- as.numeric(subdat2$salepercamp)

str(subdat2)
#Remove Variables
subdat2$ANY_MAIL_16 <- NULL 

#subdat2 <- subset(subdat, EXAGE > 0)
##unitamtt <- mean(subdat2$TOTAMT)

#write.csv(subdat2, file="BDY2.csv") # use your initials for the file name

##################################################################################
################### Step 3 - EDA - Cleaning up of the data  #####################
#################################################################################

str(subdat2)

# Convert Blanks to NA
subdat2[subdat2 == ""] <- NA

#Check for Missingness
sum(is.na(subdat2)) 
sapply(subdat2, function(x) sum(is.na(x)))
aggr_plot <- aggr(subdat2, col=c('#9ecae1','#de2d26'), numbers=TRUE,prop=FALSE, sortVars=TRUE, labels=names(subdat2), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(subdat2,2,pMiss)

#Impute Factor Variables with Majority Class (except ETHNIC_MATCH)
subdat2$DEBIT_CC[is.na(subdat2$DEBIT_CC)] <- "U" #Majority
subdat2$MAJOR_CC[is.na(subdat2$MAJOR_CC)] <- "U" #Majority
subdat2$COMPUTER_ELECTRONIC[is.na(subdat2$COMPUTER_ELECTRONIC)] <- "U" #Majority
subdat2$CHANNEL_DOMINANCE[is.na(subdat2$CHANNEL_DOMINANCE)] <- "C" #Majority

levels(subdat2$ETHNIC_MATCH)<-c(levels(subdat2$ETHNIC_MATCH),"N")
subdat2$ETHNIC_MATCH[is.na(subdat2$ETHNIC_MATCH)] <- "N" #Blanks = No

subdat2$HOMEOWNR[is.na(subdat2$HOMEOWNR)] <- "Y" #Majority
subdat2$ADD_TYPE[is.na(subdat2$ADD_TYPE)] <- "S" #Majority
subdat2$IND_DMR[is.na(subdat2$IND_DMR)] <- "U" #Majority
subdat2$MAILPREF[is.na(subdat2$MAILPREF)] <- "N" #Majority

subdat2$ZONLINE[is.na(subdat2$ZONLINE)] <- "Y" #Majority
subdat2$ZMOB[is.na(subdat2$ZMOB)] <- "Y" #Majority
subdat2$ZPRCHPHN[is.na(subdat2$ZPRCHPHN)] <- "U" #Majority
subdat2$ZMOBMULT[is.na(subdat2$ZMOBMULT)] <- "Y" #Majority
subdat2$ZHMDECOR[is.na(subdat2$ZHMDECOR)] <- "Y" #Majority
subdat2$ZHOMEENT[is.na(subdat2$ZHOMEENT)] <- "U" #Majority
subdat2$ZKITCHEN[is.na(subdat2$ZKITCHEN)] <- "U" #Majority
subdat2$ZPRCHONL[is.na(subdat2$ZPRCHONL)] <- "Y" #Majority
subdat2$SALES[is.na(subdat2$SALES)] <- "U" #Unknown

subdat2$ZIP[is.na(subdat2$ZIP)] <- "60091" #Majority

#Impute Numeric Variables with Median
subdat2$EXAGE[subdat2$EXAGE=="U"] <- NA
subdat2$EXAGE[is.na(subdat2$EXAGE)] = median(subdat2$EXAGE, na.rm = TRUE)

#Impute Numeric Variables with 0
subdat2$salepercamp[is.na(subdat2$salepercamp)] <- 0
subdat2$salepercamp[is.infinite(subdat2$salepercamp)] <- 0
subdat2$salepertrans[is.na(subdat2$salepertrans)] <- 0
subdat2$NUM_CHILD[is.na(subdat2$NUM_CHILD)] <- 0

#Check for Missingness
sum(is.na(subdat2)) 
sapply(subdat2, function(x) sum(is.na(x)))
aggr_plot <- aggr(subdat2, col=c('#9ecae1','#de2d26'), numbers=TRUE,prop=FALSE, sortVars=TRUE, labels=names(subdat2), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(subdat2,2,pMiss)

str(subdat2)

#subdat2$logPRE2009TRANSACTIONS <- log(subdat2$PRE2009TRANSACTIONS) #Possible log transformation

####################################### Split Datasets ######################################
#Split Data into Train/Test
#train <- subset(subdat2, LEARNING_TEST=="LEARNING") #splits data into training dataset
#test <- subset(subdat2, LEARNING_TEST=="TEST") #splits data into test dataset

set.seed(123)
validationIndex <- createDataPartition(subdat2$RESPONSE16, p=0.70, list=FALSE)
train <- subdat2[validationIndex,]
validation <- subdat2[-validationIndex,]

str(train)
str(validation)

sum(is.na(train)) 
sum(is.na(validation)) 

summary(train)

############################## EDA for Classification Models - Training Data ##############################

##################### EDA Numeric ################################

subdatnumcor <- subset(train, select=c("salepercamp","REVPERCUST","PRE2009SALES","PRE2009TRANSACTIONS","cum15QTY", "QTY15",
                                       "cum15TOTAMT", "TOTAMT15","SUM_MAIL_15","TOTAL_MAIL_15",
                                       "salepertrans","INC_SCS_AMT_V4","INC_WOUTSCS_AMT_4","MED_INC","MED_FAMINCOM",
                                       "P_CAPITA_INCOM","AVG_COMMUTETIM","MED_HOME","CUR_EST_MED_INC","STATE_INC_INDEX","STATE_INC_DECILES",
                                       "EXAGE","LOR1","NUM_CHILD","NUMBADLT","CNTY_INC"))

str(subdatnumcor)
summary(subdatnumcor)

par(mfrow=c(3,3))
hist(subdatnumcor$salepercamp, col = "#ece7f2", xlab = "salepercamp", main = "Histogram of salepercamp")
hist(subdatnumcor$salepertrans, col = "#ece7f2", xlab = "salepertrans", main = "Histogram of salepertrans")
hist(subdatnumcor$MED_HOME, col = "#ece7f2", xlab = "MED_HOME", main = "Histogram of MED_HOME")
boxplot(subdatnumcor$salepercamp, col = "#ece7f2", main = "Boxplot of salepercamp")
boxplot(subdatnumcor$salepertrans, col = "#ece7f2", main = "Boxplot of salepertrans")
boxplot(subdatnumcor$MED_HOME, col = "#ece7f2", main = "Boxplot of MED_HOME")
par(mfrow=c(1,1))

par(mfrow=c(3,3))
hist(subdatnumcor$PRE2009SALES, col = "#ece7f2", xlab = "PRE2009SALES", main = "Histogram of PRE2009SALES")
hist(subdatnumcor$PRE2009TRANSACTIONS, col = "#ece7f2", xlab = "PRE2009TRANSACTIONS", main = "Histogram of PRE2009TRANSACTIONS")
hist(subdatnumcor$cum15QTY, col = "#ece7f2", xlab = "cum15QTY", main = "Histogram of cum15QTY")
boxplot(subdatnumcor$PRE2009SALES, col = "#ece7f2", main = "Boxplot of PRE2009SALES")
boxplot(subdatnumcor$PRE2009TRANSACTIONS, col = "#ece7f2", main = "Boxplot of PRE2009TRANSACTIONS")
boxplot(subdatnumcor$cum15QTY, col = "#ece7f2", main = "Boxplot of cum15QTY")
par(mfrow=c(1,1))

par(mfrow=c(3,3))
hist(subdatnumcor$QTY15, col = "#ece7f2", xlab = "QTY15", main = "Histogram of QTY15")
hist(subdatnumcor$cum15TOTAMT, col = "#ece7f2", xlab = "cum15TOTAMT", main = "Histogram of cum15TOTAMT")
hist(subdatnumcor$TOTAMT15, col = "#ece7f2", xlab = "TOTAMT15", main = "Histogram of TOTAMT15")
boxplot(subdatnumcor$QTY15, col = "#ece7f2", main = "Boxplot of QTY15")
boxplot(subdatnumcor$cum15TOTAMT, col = "#ece7f2", main = "Boxplot of cum15TOTAMT")
boxplot(subdatnumcor$TOTAMT15, col = "#ece7f2", main = "Boxplot of TOTAMT15")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(subdatnumcor$TOTAL_MAIL_15, col = "#ece7f2", xlab = "TOTAL_MAIL_15", main = "Histogram of TOTAL_MAIL_15")
hist(subdatnumcor$REVPERCUST, col = "#ece7f2", xlab = "REVPERCUST", main = "Histogram of REVPERCUST")
boxplot(subdatnumcor$TOTAL_MAIL_15, col = "#ece7f2", main = "Boxplot of TOTAL_MAIL_15")
boxplot(subdatnumcor$REVPERCUST, col = "#ece7f2", main = "Boxplot of REVPERCUST")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(subdatnumcor$SUM_MAIL_15, col = "#ece7f2", xlab = "SUM_MAIL_15", main = "Histogram of SUM_MAIL_15")
hist(subdatnumcor$INC_SCS_AMT_V4, col = "#ece7f2", xlab = "INC_SCS_AMT_V4", main = "Histogram of INC_SCS_AMT_V4")
boxplot(subdatnumcor$SUM_MAIL_15, col = "#ece7f2", main = "Boxplot of SUM_MAIL_15")
boxplot(subdatnumcor$INC_SCS_AMT_V4, col = "#ece7f2", main = "Boxplot of INC_SCS_AMT_V4")
par(mfrow=c(1,1))

par(mfrow=c(3,3))
hist(subdatnumcor$INC_WOUTSCS_AMT_4, col = "#ece7f2", xlab = "INC_WOUTSCS_AMT_4", main = "Histogram of INC_WOUTSCS_AMT_4")
hist(subdatnumcor$MED_INC, col = "#ece7f2", xlab = "MED_INC", main = "Histogram of MED_INC")
hist(subdatnumcor$MED_FAMINCOM, col = "#ece7f2", xlab = "MED_FAMINCOM", main = "Histogram of MED_FAMINCOM")
boxplot(subdatnumcor$INC_WOUTSCS_AMT_4, col = "#ece7f2", main = "Boxplot of INC_WOUTSCS_AMT_4")
boxplot(subdatnumcor$MED_INC, col = "#ece7f2", main = "Boxplot of MED_INC")
boxplot(subdatnumcor$MED_FAMINCOM, col = "#ece7f2", main = "Boxplot of MED_FAMINCOM")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(subdatnumcor$P_CAPITA_INCOM, col = "#ece7f2", xlab = "P_CAPITA_INCOM", main = "Histogram of P_CAPITA_INCOM")
hist(subdatnumcor$AVG_COMMUTETIM, col = "#ece7f2", xlab = "AVG_COMMUTETIM", main = "Histogram of AVG_COMMUTETIM")
boxplot(subdatnumcor$P_CAPITA_INCOM, col = "#ece7f2", main = "Boxplot of P_CAPITA_INCOM")
boxplot(subdatnumcor$AVG_COMMUTETIM, col = "#ece7f2", main = "Boxplot of AVG_COMMUTETIM")
par(mfrow=c(1,1))

par(mfrow=c(3,3))
hist(subdatnumcor$CUR_EST_MED_INC, col = "#ece7f2", xlab = "CUR_EST_MED_INC", main = "Histogram of CUR_EST_MED_INC")
hist(subdatnumcor$STATE_INC_INDEX, col = "#ece7f2", xlab = "STATE_INC_INDEX", main = "Histogram of STATE_INC_INDEX")
hist(subdatnumcor$STATE_INC_DECILES, col = "#ece7f2", xlab = "STATE_INC_DECILES", main = "Histogram of STATE_INC_DECILES")
boxplot(subdatnumcor$CUR_EST_MED_INC, col = "#ece7f2", main = "Boxplot of CUR_EST_MED_INC")
boxplot(subdatnumcor$STATE_INC_INDEX, col = "#ece7f2", main = "Boxplot of STATE_INC_INDEX")
boxplot(subdatnumcor$STATE_INC_DECILES, col = "#ece7f2", main = "Boxplot of STATE_INC_DECILES")
par(mfrow=c(1,1))

par(mfrow=c(3,3))
hist(subdatnumcor$EXAGE, col = "#ece7f2", xlab = "EXAGE", main = "Histogram of EXAGE")
hist(subdatnumcor$LOR1, col = "#ece7f2", xlab = "LOR1", main = "Histogram of LOR1")
hist(subdatnumcor$NUM_CHILD, col = "#ece7f2", xlab = "NUM_CHILD", main = "Histogram of NUM_CHILD")
boxplot(subdatnumcor$EXAGE, col = "#ece7f2", main = "Boxplot of EXAGE")
boxplot(subdatnumcor$LOR1, col = "#ece7f2", main = "Boxplot of LOR1")
boxplot(subdatnumcor$NUM_CHILD, col = "#ece7f2", main = "Boxplot of NUM_CHILD")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(subdatnumcor$NUMBADLT, col = "#ece7f2", xlab = "NUMBADLT", main = "Histogram of NUMBADLT")
hist(subdatnumcor$CNTY_INC, col = "#ece7f2", xlab = "CNTY_INC", main = "Histogram of CNTY_INC")
boxplot(subdatnumcor$NUMBADLT, col = "#ece7f2", main = "Boxplot of NUMBADLT")
boxplot(subdatnumcor$CNTY_INC, col = "#ece7f2", main = "Boxplot of CNTY_INC")
par(mfrow=c(1,1))

#Outlier Analysis

quantile(train$salepercamp, c(.01, .05, .95, .99))
quantile(train$REVPERCUST, c(.01, .05, .95, .99))
quantile(train$PRE2009SALES, c(.01, .05, .95, .99))
quantile(train$PRE2009TRANSACTIONS, c(.01, .05, .95, .99))
quantile(train$cum15QTY, c(.01, .05, .95, .99))
quantile(train$QTY15, c(.01, .05, .95, .99))
quantile(train$cum15TOTAMT, c(.01, .05, .95, .99))
quantile(train$TOTAMT15, c(.01, .05, .95, .99))
quantile(train$SUM_MAIL_15, c(.01, .05, .95, .99))
quantile(train$TOTAL_MAIL_15, c(.01, .05, .95, .99))
quantile(train$salepertrans, c(.01, .05, .95, .99))
quantile(train$INC_SCS_AMT_V4, c(.01, .05, .95, .99))
quantile(train$INC_WOUTSCS_AMT_4, c(.01, .05, .95, .99))
quantile(train$MED_INC, c(.01, .05, .95, .99))
quantile(train$MED_FAMINCOM, c(.01, .05, .95, .99))
quantile(train$P_CAPITA_INCOM, c(.01, .05, .95, .99))
quantile(train$AVG_COMMUTETIM, c(.01, .05, .95, .99))
quantile(train$MED_HOME, c(.01, .05, .95, .99))
quantile(train$CUR_EST_MED_INC, c(.01, .05, .95, .99))
quantile(train$STATE_INC_INDEX, c(.01, .05, .95, .99))
quantile(train$STATE_INC_DECILES, c(.01, .05, .95, .99))
quantile(train$EXAGE, c(.01, .05, .95, .99))
quantile(train$LOR1, c(.01, .05, .95, .99))
quantile(train$NUM_CHILD, c(.01, .05, .95, .99))
quantile(train$NUMBADLT, c(.01, .05, .95, .99))
quantile(train$CNTY_INC, c(.01, .05, .95, .99))

summary(train)

#Boxplots for Numeric Variables by RESPONSE16

A1<-ggplot(train, aes(x=RESPONSE16, y= salepercamp)) + 
  geom_boxplot(fill="#045a8d",notch=FALSE) +
  labs(title="Distribution of salepercamp") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
A1

ggplot(train, aes(x=RESPONSE16, y= REVPERCUST)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of REVPERCUST") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

A2<-ggplot(train, aes(x=RESPONSE16, y= PRE2009SALES)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of PRE2009SALES") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
A2

A3<-ggplot(train, aes(x=RESPONSE16, y= PRE2009TRANSACTIONS)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of PRE2009TRANSACTIONS") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
A3

A4<-ggplot(train, aes(x=RESPONSE16, y= cum15QTY)) + 
  geom_boxplot(fill="#045a8d",notch=FALSE) +
  labs(title="Distribution of cum15QTY") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
A4

ggplot(train, aes(x=RESPONSE16, y= QTY15)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of QTY15") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= cum15TOTAMT)) + 
  geom_boxplot(fill="#045a8d",notch=FALSE) +
  labs(title="Distribution of cum15TOTAMT") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= TOTAMT15)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of TOTAMT15") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= SUM_MAIL_15)) + 
  geom_boxplot(fill="#045a8d",notch=FALSE) +
  labs(title="Distribution of SUM_MAIL_15") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

A5<-ggplot(train, aes(x=RESPONSE16, y= TOTAL_MAIL_15)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of TOTAL_MAIL_15") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
A5

ggplot(train, aes(x=RESPONSE16, y= salepertrans)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of salepertrans") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= INC_SCS_AMT_V4)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of INC_SCS_AMT_V4") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

A6<-ggplot(train, aes(x=RESPONSE16, y= INC_WOUTSCS_AMT_4)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of INC_WOUTSCS_AMT_4") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
A6

ggplot(train, aes(x=RESPONSE16, y= MED_INC)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of MED_INC") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= MED_FAMINCOM)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of MED_FAMINCOM") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= P_CAPITA_INCOM)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of P_CAPITA_INCOM") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= AVG_COMMUTETIM)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of AVG_COMMUTETIM") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

A7<-ggplot(train, aes(x=RESPONSE16, y= MED_HOME)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of MED_HOME") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))
A7

ggplot(train, aes(x=RESPONSE16, y= CUR_EST_MED_INC)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of CUR_EST_MED_INC") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= STATE_INC_INDEX)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of STATE_INC_INDEX") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= STATE_INC_DECILES)) + 
  geom_boxplot(fill="#045a8d",notch=FALSE) +
  labs(title="Distribution of STATE_INC_DECILES") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= EXAGE)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of EXAGE") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= LOR1)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of LOR1") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= NUM_CHILD)) + 
  geom_boxplot(fill="#045a8d",notch=FALSE) +
  labs(title="Distribution of NUM_CHILD") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= NUMBADLT)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of NUMBADLT") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train, aes(x=RESPONSE16, y= CNTY_INC)) + 
  geom_boxplot(fill="#045a8d",notch=TRUE) +
  labs(title="Distribution of CNTY_INC") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

multiplot(A5,A7, cols=2)

#Correlation Matrix
library(GGally) #Data Visualization
require(lattice)
library(ggcorrplot) #Data Visualization
library(corrplot) #Data Visualization
par(mfrow=c(1,1))  
corr <- round(cor(subdatnumcor),2)
ggcorrplot(corr, outline.col = "white", ggtheme = ggplot2::theme_gray, 
           colors = c("#E46726", "white", "#6D9EC1"),lab = TRUE)
par(mfrow=c(1,1))  

##################### EDA Categorical ################################
library(ggplot2)
ggplot(train) +
  geom_bar( aes(RESPONSE16) ) +
  ggtitle("RESPONSE16") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

library(ggplot2)
ggplot(train) +
  geom_bar( aes(ZIP) ) +
  ggtitle("ZIP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(CHANNEL_ACQUISITION) ) +
  ggtitle("CHANNEL_ACQUISITION") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(DEBIT_CC) ) +
  ggtitle("DEBIT_CC") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(MAJOR_CC) ) +
  ggtitle("MAJOR_CC") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(COMPUTER_ELECTRONIC) ) +
  ggtitle("COMPUTER_ELECTRONIC") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(INC_WIOUTSCS_V4) ) +
  ggtitle("INC_WIOUTSCS_V4") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(INC_WITHSCS_V4) ) +
  ggtitle("INC_WITHSCS_V4") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(FIPSCNTY) ) +
  ggtitle("FIPSCNTY") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ADULT1_G) ) +
  ggtitle("ADULT1_G") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(MARRIED) ) +
  ggtitle("MARRIED") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ETHNIC_MATCH) ) +
  ggtitle("ETHNIC_MATCH") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(HOMEOWNR) ) +
  ggtitle("HOMEOWNR") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ADD_TYPE) ) +
  ggtitle("ADD_TYPE") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(DUS) ) +
  ggtitle("DUS") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(IND_DMR) ) +
  ggtitle("IND_DMR") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(OCCUPATION_GROUP) ) +
  ggtitle("OCCUPATION_GROUP") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(IND_ED) ) +
  ggtitle("IND_ED") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(PRESCHLD) ) +
  ggtitle("PRESCHLD") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(MAILPREF) ) +
  ggtitle("MAILPREF") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(CHANNEL_DOMINANCE) ) +
  ggtitle("CHANNEL_DOMINANCE") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(SALES) ) +
  ggtitle("SALES") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ZONLINE) ) +
  ggtitle("ZONLINE") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ZMOB) ) +
  ggtitle("ZMOB") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ZPRCHPHN) ) +
  ggtitle("ZPRCHPHN") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ZMOBMULT) ) +
  ggtitle("ZMOBMULT") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ZHMDECOR) ) +
  ggtitle("ZHMDECOR") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ZHOMEENT) ) +
  ggtitle("ZHOMEENT") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

ggplot(train) +
  geom_bar( aes(ZKITCHEN) ) +
  ggtitle("ZKITCHEN") +
  theme(plot.title=element_text(lineheight=0.8, face="bold", hjust=0.5))

###Crosstabs
library(gmodels)
library(gridExtra)
attach(train)

#ZIP 
CrossTable(RESPONSE16,ZIP, prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZIP ,fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ZIP,mean)

#ZIP 
CrossTable(RESPONSE16,ZIP, prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZIP ,fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ZIP,mean)

A1 <- ggplot(train, aes(x = ZIP, fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal() + coord_flip()
grid.arrange(l,A1, nrow = 2, top = textGrob("ZIP",gp=gpar(fontsize=15,font=2)))

#INC_WIOUTSCS_V4 
CrossTable(RESPONSE16, INC_WIOUTSCS_V4 , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(INC_WIOUTSCS_V4 ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ INC_WIOUTSCS_V4 ,mean)

A10 <- ggplot(train, aes(x = INC_WIOUTSCS_V4 , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+theme_minimal() + coord_flip()
grid.arrange(l,A10, nrow = 2,top = textGrob("INC_WIOUTSCS_V4",gp=gpar(fontsize=15,font=2)))

multiplot(A2, A10, cols=1)
#DEBIT_CC 
CrossTable(RESPONSE16,DEBIT_CC, prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(DEBIT_CC ,fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$DEBIT_CC,mean)

A1 <- ggplot(train, aes(x = DEBIT_CC, fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal() + coord_flip()
grid.arrange(l,A1, nrow = 2, top = textGrob("DEBIT_CC",gp=gpar(fontsize=15,font=2)))

#MAJOR_CC 
CrossTable(RESPONSE16,MAJOR_CC, prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(MAJOR_CC ,fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$MAJOR_CC,mean)

A1 <- ggplot(train, aes(x = MAJOR_CC, fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal() + coord_flip()
grid.arrange(l,A1, nrow = 2, top = textGrob("MAJOR_CC",gp=gpar(fontsize=15,font=2)))

#COMPUTER_ELECTRONIC 
CrossTable(RESPONSE16,COMPUTER_ELECTRONIC, prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(COMPUTER_ELECTRONIC ,fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$COMPUTER_ELECTRONIC,mean)

A1 <- ggplot(train, aes(x = COMPUTER_ELECTRONIC, fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal() + coord_flip()
grid.arrange(l,A1, nrow = 2, top = textGrob("COMPUTER_ELECTRONIC",gp=gpar(fontsize=15,font=2)))

#INC_WITHSCS_V4 
CrossTable(RESPONSE16, INC_WITHSCS_V4 , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(INC_WITHSCS_V4 ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ INC_WITHSCS_V4 ,mean)

A3 <- ggplot(train, aes(x = INC_WITHSCS_V4 , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+theme_minimal() + coord_flip()
grid.arrange(l,A3, nrow = 2,top = textGrob("INC_WITHSCS_V4",gp=gpar(fontsize=15,font=2)))

#FIPSCNTY 
CrossTable(RESPONSE16, FIPSCNTY , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(FIPSCNTY ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ FIPSCNTY ,mean)

A4 <- ggplot(train, aes(x = FIPSCNTY , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+theme_minimal() + coord_flip()
grid.arrange(l,A4, nrow = 2,top = textGrob("FIPSCNTY ",gp=gpar(fontsize=15,font=2)))

#ADULT1_G 
CrossTable(RESPONSE16, ADULT1_G , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ADULT1_G ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ADULT1_G ,mean)

A4 <- ggplot(train, aes(x = ADULT1_G , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+theme_minimal() + coord_flip()
grid.arrange(l,A4, nrow = 2,top = textGrob("ADULT1_G ",gp=gpar(fontsize=15,font=2)))

#MARRIED 
CrossTable(RESPONSE16,MARRIED, prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(MARRIED ,fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$MARRIED,mean)

B1 <- ggplot(train, aes(x = MARRIED, fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal() + coord_flip()
grid.arrange(l,B1, nrow = 2, top = textGrob("MARRIED",gp=gpar(fontsize=15,font=2)))

#ETHNIC_MATCH 
CrossTable(RESPONSE16,ETHNIC_MATCH, prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ETHNIC_MATCH ,fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ETHNIC_MATCH,mean)

B1 <- ggplot(train, aes(x = ETHNIC_MATCH, fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal() + coord_flip()
grid.arrange(l,B1, nrow = 2, top = textGrob("ETHNIC_MATCH",gp=gpar(fontsize=15,font=2)))

#HOMEOWNR 
CrossTable(RESPONSE16, HOMEOWNR , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(HOMEOWNR ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ HOMEOWNR ,mean)

B2 <- ggplot(train, aes(x = HOMEOWNR , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+theme_minimal() + coord_flip()
grid.arrange(l,B2, nrow = 2,top = textGrob("HOMEOWNR",gp=gpar(fontsize=15,font=2)))

#ADD_TYPE 
CrossTable(RESPONSE16, ADD_TYPE , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ADD_TYPE ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ADD_TYPE ,mean)

B2 <- ggplot(train, aes(x = ADD_TYPE , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+theme_minimal() + coord_flip()
grid.arrange(l,B2, nrow = 2,top = textGrob("ADD_TYPE",gp=gpar(fontsize=15,font=2)))

#IND_DMR 
CrossTable(RESPONSE16, IND_DMR , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(IND_DMR ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ IND_DMR ,mean)

B2 <- ggplot(train, aes(x = IND_DMR , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+theme_minimal() + coord_flip()
grid.arrange(l,B2, nrow = 2,top = textGrob("IND_DMR",gp=gpar(fontsize=15,font=2)))

#DUS 
CrossTable(RESPONSE16, DUS , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(DUS ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ DUS ,mean)

B2 <- ggplot(train, aes(x = DUS , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+theme_minimal() + coord_flip()
grid.arrange(l,B2, nrow = 2,top = textGrob("DUS",gp=gpar(fontsize=15,font=2)))

#OCCUPATION_GROUP 
CrossTable(RESPONSE16, OCCUPATION_GROUP , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(OCCUPATION_GROUP ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ OCCUPATION_GROUP ,mean)

B2 <- ggplot(train, aes(x = OCCUPATION_GROUP , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+theme_minimal() + coord_flip()
grid.arrange(l,B2, nrow = 2,top = textGrob("OCCUPATION_GROUP",gp=gpar(fontsize=15,font=2)))

#IND_ED 
CrossTable(RESPONSE16, IND_ED , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(DUS ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ IND_ED ,mean)

B2 <- ggplot(train, aes(x = IND_ED , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#3182bd"))+theme_minimal() + coord_flip()
grid.arrange(l,B2, nrow = 2,top = textGrob("IND_ED",gp=gpar(fontsize=15,font=2)))

#MAILPREF 
CrossTable(RESPONSE16, MAILPREF , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(MAILPREF ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ MAILPREF ,mean)

B3 <- ggplot(train, aes(x = MAILPREF , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+theme_minimal() + coord_flip()
grid.arrange(l,B3, nrow = 2,top = textGrob("MAILPREF ",gp=gpar(fontsize=15,font=2)))

#PRESCHLD 
CrossTable(RESPONSE16, PRESCHLD , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(PRESCHLD ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ PRESCHLD ,mean)

B3 <- ggplot(train, aes(x = PRESCHLD , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+theme_minimal() + coord_flip()
grid.arrange(l,B3, nrow = 2,top = textGrob("PRESCHLD ",gp=gpar(fontsize=15,font=2)))

#CHANNEL_DOMINANCE 
CrossTable(RESPONSE16, CHANNEL_DOMINANCE , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(CHANNEL_DOMINANCE ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ CHANNEL_DOMINANCE ,mean)

B3 <- ggplot(train, aes(x = CHANNEL_DOMINANCE , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#9ecae1", "#08306b"))+theme_minimal() + coord_flip()
grid.arrange(l,B3, nrow = 2,top = textGrob("CHANNEL_DOMINANCE ",gp=gpar(fontsize=15,font=2)))

#SALES 
CrossTable(RESPONSE16, SALES , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(SALES ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ SALES ,mean)

A2 <- ggplot(train, aes(x = SALES , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+theme_minimal() + coord_flip()
grid.arrange(l,A2, nrow = 2,top = textGrob("SALES",gp=gpar(fontsize=15,font=2)))

#ZONLINE 
CrossTable(RESPONSE16, ZONLINE , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZONLINE ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ZONLINE ,mean)

A2 <- ggplot(train, aes(x = ZONLINE , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+theme_minimal() + coord_flip()
grid.arrange(l,A2, nrow = 2,top = textGrob("ZONLINE",gp=gpar(fontsize=15,font=2)))

#ZMOB 
CrossTable(RESPONSE16, ZMOB , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZMOB ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ZMOB ,mean)

A2 <- ggplot(train, aes(x = ZMOB , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+theme_minimal() + coord_flip()
grid.arrange(l,A2, nrow = 2,top = textGrob("ZMOB",gp=gpar(fontsize=15,font=2)))

#ZPRCHPHN 
CrossTable(RESPONSE16, ZPRCHPHN , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZPRCHPHN ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ZPRCHPHN ,mean)

A2 <- ggplot(train, aes(x = ZPRCHPHN , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+theme_minimal() + coord_flip()
grid.arrange(l,A2, nrow = 2,top = textGrob("ZPRCHPHN",gp=gpar(fontsize=15,font=2)))

#ZMOBMULT 
CrossTable(RESPONSE16, ZMOBMULT , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZMOBMULT ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ZMOBMULT ,mean)

A2 <- ggplot(train, aes(x = ZMOBMULT , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+theme_minimal() + coord_flip()
grid.arrange(l,A2, nrow = 2,top = textGrob("ZMOBMULT",gp=gpar(fontsize=15,font=2)))

#ZHMDECOR 
CrossTable(RESPONSE16, ZHMDECOR , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZHMDECOR ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ZHMDECOR ,mean)

A2 <- ggplot(train, aes(x = ZHMDECOR , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+theme_minimal() + coord_flip()
grid.arrange(l,A2, nrow = 2,top = textGrob("ZHMDECOR",gp=gpar(fontsize=15,font=2)))

#ZHOMEENT 
CrossTable(RESPONSE16, ZHOMEENT , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZHOMEENT ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ZHOMEENT ,mean)

A2 <- ggplot(train, aes(x = ZHOMEENT , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+theme_minimal() + coord_flip()
grid.arrange(l,A2, nrow = 2,top = textGrob("ZHOMEENT",gp=gpar(fontsize=15,font=2)))

#ZKITCHEN 
CrossTable(RESPONSE16, ZKITCHEN , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZKITCHEN ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ZKITCHEN ,mean)

A2 <- ggplot(train, aes(x = ZKITCHEN , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+theme_minimal() + coord_flip()
grid.arrange(l,A2, nrow = 2,top = textGrob("ZKITCHEN",gp=gpar(fontsize=15,font=2)))

#ZPRCHONL 
CrossTable(RESPONSE16, ZPRCHONL , prop.r=FALSE, prop.c=TRUE, prop.t=TRUE, prop.chisq=FALSE)
l <- ggplot(train, aes(ZPRCHONL ,fill = RESPONSE16))+ geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+
  theme_minimal()
l <- l + geom_histogram(stat="count")
tapply(as.numeric(train$RESPONSE16) - 1 , train$ ZPRCHONL ,mean)

A2 <- ggplot(train, aes(x = ZPRCHONL , fill = RESPONSE16)) + geom_bar(position = 'fill')+
  scale_fill_manual(values=c("#a6bddb", "#2b8cbe"))+theme_minimal() + coord_flip()
grid.arrange(l,A2, nrow = 2,top = textGrob("ZPRCHONL",gp=gpar(fontsize=15,font=2)))

#Prevalance Statistic for Responses16
#Train
table(train$RESPONSE16) #1008 bought, 9438 did not buy
1008/(9438 + 1008) #0.09649627 bought
9438/(9438 + 1008) #0.9035037 did not buy

#validation
table(validation$RESPONSE16) #432 bought, 4044 did not buy
432/(4044 + 432) #0.09651475 bought
4044/(4044 + 432) #0.9034853 did not buy

str(train)

####################################### Build Models ######################################

#########################################################################
######## Step 4 build models - Logistic Regression Model #############
##########################################################################

library(glm2)
library(car)
detach(package:ModelMetrics)

#Full Model for Variable Selection & Baseline
model.logfull <- glm(RESPONSE16 ~ ., data = train, family=binomial("logit"))
varImp(model.logfull)

#Stepwise Regression for Variable Selection
model.lower = glm(RESPONSE16 ~ 1, train, family = binomial(link="logit"))
model.logfull <- glm(RESPONSE16 ~ ., train, family=binomial("logit"))
stepAIC(model.lower, scope = list(upper=model.logfull), direction="both", validation="Chisq", data=balanced.data)

#Original
#PRE2009TRANSACTIONS + TOTAL_MAIL_15 + 
 # QTY15 + CHANNEL_ACQUISITION + salepercamp + ADULT1_G + PRE2009SALES + 
  #MED_HOME + ZPRCHONL + cum15QTY + CHANNEL_DOMINANCE + INC_WOUTSCS_AMT_4 + 
  #HOMEOWNR + AVG_COMMUTETIM + ZHMDECOR

model.log2 <- glm(RESPONSE16 ~ PRE2009TRANSACTIONS + TOTAL_MAIL_15 + 
                    QTY15 + CHANNEL_ACQUISITION + salepercamp + ADULT1_G + PRE2009SALES + 
                    MED_HOME + ZPRCHONL + cum15QTY + CHANNEL_DOMINANCE + INC_WOUTSCS_AMT_4 + 
                    HOMEOWNR + AVG_COMMUTETIM + ZHMDECOR,data = train, family=binomial("logit")) 

summary(model.log2)
hoslem.test(train$RESPONSE16, fitted(model.log2)) #check p-value and is small, reject null hypothesis and statistically significant model
Anova(model.log2, type="II", validation="Wald")
varImp(model.log2)
#nagelkerke(model.log2)
vif(model.log2)

#Performance Metrics
AIC(model.log2) #6100.637
BIC(model.log2) #6267.478

##Performance on validation Set
glm.pred <- predict(model.log2, validation, type="response")
hist(glm.pred)#Histogram of predicted probabilities

#Confusion Matrix
glm.pred  <- ifelse(glm.pred  > 0.5,1,0)
xtab.log1=table(glm.pred, validation$RESPONSE16)
confusionMatrix(xtab.log1, positive = "1") #0.9015

#Plot Curves
require(PRROC)
prob <- predict(model.log2, newdata=validation, type="response")

fg <- prob[validation$RESPONSE16 == 1]
bg <- prob[validation$RESPONSE16 == 0]

#ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

#Precision Recall Curve
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)

#Compute area under ROC curve
roc <- roc.curve( fg, bg );
print( roc ) #0.7151599

#Compute area under PR curve
pr <- pr.curve( fg, bg )
print( pr ) #0.2142101

#ROC Curve (Alternative)
#library(ROCR)
#prob <- predict(model.log2, newdata=validation, type="response")
#pred <- prediction(prob, validation$RESPONSE16)
#perf <- performance(pred, measure = "tpr", x.measure = "fpr")
#plot(perf,lwd=2, col="#3182bd", main="ROC Curve", colorize=TRUE)
#abline(a=0, b=1)

#AUC
#perf_auc <- performance(pred, measure = "auc")
#perf_auc <- perf_auc@y.values[[1]]
#perf_auc #0.7151599

## Evaluate on All data in subset
glm.pred_all<- predict(model.log2, subdat2, type = "response")
hist(glm.pred_all)

pred2df <- as.data.frame(glm.pred_all)
data_all <- cbind(subdat2,glm.pred_all)
str(data_all)

#Average Sales Per Transactions
mean(data_all$salepertrans) #Average subset is $254.8435
mean(data_all$salepertrans [data_all$glm.pred_all>0.70]) #70% probability is 318.3577

#Average Revenue per Customer for All
mean(data_all$REVPERCUST) #Average subset is $271.1046

#Targeted Customers with New Model
sum(data_all$glm.pred_all>0.65) #53
sum(data_all$glm.pred_all>0.5) #100
sum(data_all$glm.pred_all>0.4) #189
sum(data_all$glm.pred_all>0.3) #390
sum(data_all$glm.pred_all>0.2) #1083
sum(data_all$glm.pred_all>0.15) #2113
sum(data_all$glm.pred_all>0.10) #4558
sum(data_all$glm.pred_all>0.05) #11441

#Average Revenue per Customer for New Model
mean(data_all$REVPERCUST [data_all$glm.pred_all>0.65]) #3093.568
mean(data_all$REVPERCUST [data_all$glm.pred_all>0.5]) #2690.291
mean(data_all$REVPERCUST [data_all$glm.pred_all>0.4]) #2449.486
mean(data_all$REVPERCUST [data_all$glm.pred_all>0.3]) #1953.931
mean(data_all$REVPERCUST [data_all$glm.pred_all>0.2]) #1265.134
mean(data_all$REVPERCUST [data_all$glm.pred_all>0.15]) #963.7352
mean(data_all$REVPERCUST [data_all$glm.pred_all>0.10]) #634.8024
mean(data_all$REVPERCUST [data_all$glm.pred_all>0.05]) #333.3991

str(glm.pred_all)

####################################################################
######## Step 5 LDA model ###############################
##################################################################
detach(package:ModelMetrics)

###LDA###
model.lda2 <- lda(RESPONSE16 ~ PRE2009TRANSACTIONS + TOTAL_MAIL_15 + 
                    QTY15 + CHANNEL_ACQUISITION + salepercamp + ADULT1_G + PRE2009SALES + 
                    MED_HOME + ZPRCHONL + cum15QTY + CHANNEL_DOMINANCE + INC_WOUTSCS_AMT_4 + 
                    HOMEOWNR + AVG_COMMUTETIM + ZHMDECOR,train, family=binomial("logit"))

##Performance on validation Set
lda.pred <- predict(model.lda2, validation)$posterior[,2]
hist(lda.pred)

#Confusion Matrix
lda.pred_cm  <- ifelse(lda.pred  > 0.5,1,0)
xtab.lda1=table(lda.pred_cm, validation$RESPONSE16)
confusionMatrix(xtab.lda1, positive = "1") #0.8975

#Plot Curves
require(PRROC)
prob <- predict(model.lda2, validation)$posterior[,2]

fg <- prob[validation$RESPONSE16 == 1]
bg <- prob[validation$RESPONSE16 == 0]

#ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

#Precision Recall Curve
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)

#Compute area under ROC curve
roc <- roc.curve( fg, bg );
print( roc ) #0.7193693

#Compute area under PR curve
pr <- pr.curve( fg, bg )
print( pr ) #0.2198539

#ROC Curve (Alternative)
#library(ROCR)

#validation.lda <-  predict(model.lda2, validation)$posterior
#pred <- prediction(validation.lda[,2], validation$RESPONSE16)
#perf <- performance(pred, "tpr", "fpr")
#plot(perf,lwd=2, col="#3182bd", main="ROC Curve", colorize=TRUE)
#abline(a=0, b=1)

#AUC
#perf_auc <- performance(pred, measure = "auc")
#perf_auc <- perf_auc@y.values[[1]]
#perf_auc 

## Evaluate on All data in subset
lda.pred_all <- predict(model.lda2, subdat2)$posterior[,2]
hist(lda.pred_all)

pred2df <- as.data.frame(lda.pred_all)
data_all2 <- cbind(subdat2,lda.pred_all)
str(data_all2)

#Average Revenue per Customer for All
mean(data_all2$REVPERCUST) #Average subset is $271.1046

#Targeted Customers with New Model
sum(data_all2$lda.pred_all>0.65) #142
sum(data_all2$lda.pred_all>0.5) #268
sum(data_all2$lda.pred_all>0.4) #408
sum(data_all2$lda.pred_all>0.3) #678
sum(data_all2$lda.pred_all>0.2) #1283
sum(data_all2$lda.pred_all>0.15) #2065
sum(data_all2$lda.pred_all>0.10) #3708
sum(data_all2$lda.pred_all>0.05) #8771

#Average Revenue per Customer for New Model
mean(data_all2$REVPERCUST[data_all2$lda.pred_all>0.65]) #2845.481
mean(data_all2$REVPERCUST[data_all2$lda.pred_all>0.5]) #2416.554
mean(data_all2$REVPERCUST[data_all2$lda.pred_all>0.4]) #1990.168
mean(data_all2$REVPERCUST[data_all2$lda.pred_all>0.3]) #1673.998
mean(data_all2$REVPERCUST[data_all2$lda.pred_all>0.2]) #1287.399
mean(data_all2$REVPERCUST[data_all2$lda.pred_all>0.15]) #1032.211
mean(data_all2$REVPERCUST[data_all2$lda.pred_all>0.10]) #741.666
mean(data_all2$REVPERCUST[data_all2$lda.pred_all>0.05]) #411.6367

####################################################################
######## Step 6 Random Forest model ###############################
##################################################################

#Balance Imbalanced Data using SMOTE (Synthetic Minority Over-sampling Technique)
library(DMwR)
set.seed(1)
balanced.data <- SMOTE(RESPONSE16 ~., train, perc.over = 100, k = 5, perc.under = 200)
as.data.frame(table(balanced.data$RESPONSE16))
prop.table(table(balanced.data$RESPONSE16))

###Random Forest Model###
library(caret)
detach(package:ModelMetrics)

#Use RF to obtain important variables from subset
set.seed(12)
rf1 <- randomForest(RESPONSE16 ~ .,data=balanced.data,importance=TRUE,ntree=100)
summary(rf1)

##getTree(rf1,1,labelVar=TRUE)
##?getTree
print(rf1)
plot(rf1) #100
importance(rf1)
importance(rf1)
varImpPlot(rf1) 

#Create Random Forest Model with Top 20 Important Variables
set.seed(1)
detach(package:ModelMetrics)
#rf.model <- randomForest(RESPONSE16 ~ PRE2009TRANSACTIONS + ZIP + PRE2009SALES + SALES + INC_WIOUTSCS_V4 + DUS
               #          + INC_WIOUTSCS_V4 + TOTAL_MAIL_15 + cum15QTY + salepertrans + INC_WOUTSCS_AMT_4 + CNTY_INC
                #         + cum15TOTAMT + STATE_INC_INDEX + EXAGE + CUR_EST_MED_INC + MED_INC + MED_HOME + REVPERCUST + LOR1
                 #        = balanced.data,importance=TRUE,ntree=100)


#Choose Predictors that Overlapped (stepAIC vs. RF Top 20 Important Variables)
set.seed(1)
detach(package:ModelMetrics)

rf.model <- randomForest(RESPONSE16 ~ PRE2009TRANSACTIONS + TOTAL_MAIL_15 + PRE2009SALES + MED_HOME
                         + cum15QTY + INC_WIOUTSCS_V4, data = balanced.data,importance=TRUE,ntree=100)

summary(rf.model)

##getTree(rf1,1,labelVar=TRUE)
##?getTree
print(rf.model)
plot(rf.model) #100
importance(rf.model)
varImpPlot(rf.model) 

##Performance on validation Set
set.seed(1)
prob <- predict(rf.model, newdata = validation, type = "prob")[,2]
hist(prob)#Histogram of predicted probabilities

#Confusion Matrix for Probabilities
library(caret)

glm.pred  <- ifelse(prob  > 0.50,1,0)
xtab.RF1=table(glm.pred, validation$RESPONSE16)
confusionMatrix(xtab.RF1, positive = "1") #0.7339

#Confusion Matrix Detail
confusion.matrix <- table(prob, validation$RESPONSE16)
confusion.matrix

#Plot Curves
require(PRROC)
set.seed(1)

fg <- prob[validation$RESPONSE16 == 1]
bg <- prob[validation$RESPONSE16 == 0]

#ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

#Precision Recall Curve
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)

#Compute area under ROC curve
roc <- roc.curve( fg, bg );
print( roc ) #0.6850298

#Compute area under PR curve
pr <- pr.curve( fg, bg )
print( pr ) #0.1906895

#AUC
#library(ModelMetrics)
#set.seed(1)
#prob <- predict(rf.model, newdata=validation, type='prob')
#auc<-auc(validation$RESPONSE16,prob[,2])
#auc #0.6850298 

#ROC Curve
#rf1pdf <- as.data.frame(prob)
#rf1.pred = prediction(prob, validation$RESPONSE16)
#rf1.perf = performance(rf1.pred,"tpr","fpr")
#plot(rf1.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
#abline(a=0,b=1,lwd=2,lty=2,col="gray")

## Evaluate on All data in subset
set.seed(1)
prediction_rf_all<- predict(rf.model, newdata = subdat2, type = "prob")[,2]
hist(prediction_rf_all)

pred2df <- as.data.frame(prediction_rf_all)
data_alldf <- cbind(subdat2,prediction_rf_all)
str(data_alldf)

#Average Sales Per Transactions
mean(data_alldf$salepertrans) #Average subset is $254.8435
mean(data_alldf$salepertrans[data_alldf$prediction_rf_all>0.70]) #70% probability is 296.3083

#Average Revenue per Customer for All
mean(data_alldf$REVPERCUST) #Average subset is $271.1046

#Targeted Customers with New Model
sum(data_alldf$prediction_rf_all>0.65) #2332
sum(data_alldf$prediction_rf_all>0.5) #4003
sum(data_alldf$prediction_rf_all>0.4) #5353
sum(data_alldf$prediction_rf_all>0.3) #7090
sum(data_alldf$prediction_rf_all>0.2) #9416
sum(data_alldf$prediction_rf_all>0.15) #10706
sum(data_alldf$prediction_rf_all>0.10) #12120
sum(data_alldf$prediction_rf_all>0.05) #13660

#Average Revenue per Customer for New Model
mean(data_alldf$REVPERCUST[data_alldf$prediction_rf_all>0.65]) #699.7214
mean(data_alldf$REVPERCUST[data_alldf$prediction_rf_all>0.5]) #615.9711
mean(data_alldf$REVPERCUST[data_alldf$prediction_rf_all>0.4]) #537.7053
mean(data_alldf$REVPERCUST[data_alldf$prediction_rf_all>0.3]) #454.294
mean(data_alldf$REVPERCUST[data_alldf$prediction_rf_all>0.2]) #380.1277
mean(data_alldf$REVPERCUST[data_alldf$prediction_rf_all>0.15]) #350.9264
mean(data_alldf$REVPERCUST[data_alldf$prediction_rf_all>0.10]) #321.3439
mean(data_alldf$REVPERCUST[data_alldf$prediction_rf_all>0.05]) #292.8959

str(prediction_rf_all)

########################################### XYZ Didn't Target #########################################################
subdat3 <- subset(subdat, ANY_MAIL_16 == 0)
str(subdat3)

##### Change Variable Types #####
#Factors
subdat2$ZIP <- as.factor(subdat2$ZIP)
subdat3$RESPONSE16 <- as.factor(subdat3$RESPONSE16)
subdat3$CHANNEL_ACQUISITION <- as.factor(subdat3$CHANNEL_ACQUISITION)
subdat3$DEBIT_CC <- as.factor(subdat3$DEBIT_CC)
subdat3$MAJOR_CC <- as.factor(subdat3$MAJOR_CC)
subdat3$COMPUTER_ELECTRONIC <- as.factor(subdat3$COMPUTER_ELECTRONIC)
subdat3$INC_WIOUTSCS_V4 <- as.factor(subdat3$INC_WIOUTSCS_V4)
subdat3$INC_WITHSCS_V4 <- as.factor(subdat3$INC_WITHSCS_V4)
subdat3$FIPSCNTY <- as.factor(subdat3$FIPSCNTY)
subdat3$ADULT1_G <- as.factor(subdat3$ADULT1_G)
subdat3$MARRIED <- as.factor(subdat3$MARRIED)
subdat3$ETHNIC_MATCH <- as.factor(subdat3$ETHNIC_MATCH)
subdat3$HOMEOWNR <- as.factor(subdat3$HOMEOWNR)
subdat3$ADD_TYPE <- as.factor(subdat3$ADD_TYPE)
subdat3$DUS <- as.factor(subdat3$DUS)
subdat3$IND_DMR <- as.factor(subdat3$IND_DMR)
subdat3$OCCUPATION_GROUP <- as.factor(subdat3$OCCUPATION_GROUP)
subdat3$IND_ED <- as.factor(subdat3$IND_ED)
subdat3$PRESCHLD <- as.factor(subdat3$PRESCHLD)
subdat3$MAILPREF <- as.factor(subdat3$MAILPREF)
subdat3$CHANNEL_DOMINANCE <- as.factor(subdat3$CHANNEL_DOMINANCE)
subdat3$SALES <- as.factor(subdat3$SALES)
subdat3$ZONLINE <- as.factor(subdat3$ZONLINE)
subdat3$ZMOB <- as.factor(subdat3$ZMOB)
subdat3$ZPRCHPHN <- as.factor(subdat3$ZPRCHPHN)
subdat3$ZMOBMULT <- as.factor(subdat3$ZMOBMULT)
subdat3$ZHMDECOR <- as.factor(subdat3$ZHMDECOR)
subdat3$ZHOMEENT <- as.factor(subdat3$ZHOMEENT)
subdat3$ZKITCHEN <- as.factor(subdat3$ZKITCHEN)
subdat3$ZPRCHONL <- as.factor(subdat3$ZPRCHONL)

#Numeric
subdat3$EXAGE <- as.numeric(subdat3$EXAGE)
subdat3$LOR1 <- as.numeric(subdat3$LOR1)
subdat3$NUM_CHILD <- as.numeric(subdat3$NUM_CHILD)
subdat3$SUM_MAIL_15 <- as.numeric(subdat3$SUM_MAIL_15)
subdat3$TOTAL_MAIL_15 <- as.numeric(subdat3$TOTAL_MAIL_15)
subdat3$REVPERCUST <- as.numeric(subdat3$REVPERCUST)
subdat3$salepercamp <- as.numeric(subdat3$salepercamp)

str(subdat3)
#Remove Variables
subdat3$ANY_MAIL_16 <- NULL 

##################################################################################
####### Step 3 - EDA - Cleaning up of the data for XYZ Didn't Target  ############
#################################################################################

str(subdat3)

#Check for Missingness
sum(is.na(subdat3)) 
sapply(subdat3, function(x) sum(is.na(x)))
aggr_plot <- aggr(subdat3, col=c('#9ecae1','#de2d26'), numbers=TRUE,prop=FALSE, sortVars=TRUE, labels=names(subdat3), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(subdat3,2,pMiss)

# Convert Blanks to NA
subdat3[subdat3 == ""] <- NA

#Impute Factor Variables with Majority Class (except ETHNIC_MATCH)
subdat3$DEBIT_CC[is.na(subdat3$DEBIT_CC)] <- "U" #Majority
subdat3$MAJOR_CC[is.na(subdat3$MAJOR_CC)] <- "U" #Majority
subdat3$COMPUTER_ELECTRONIC[is.na(subdat3$COMPUTER_ELECTRONIC)] <- "U" #Majority
subdat3$CHANNEL_DOMINANCE[is.na(subdat3$CHANNEL_DOMINANCE)] <- "C" #Majority

levels(subdat3$ETHNIC_MATCH)<-c(levels(subdat3$ETHNIC_MATCH),"N")
subdat3$ETHNIC_MATCH[is.na(subdat3$ETHNIC_MATCH)] <- "N" #Blanks = No

subdat3$HOMEOWNR[is.na(subdat3$HOMEOWNR)] <- "Y" #Majority
subdat3$ADD_TYPE[is.na(subdat3$ADD_TYPE)] <- "S" #Majority
subdat3$IND_DMR[is.na(subdat3$IND_DMR)] <- "U" #Majority
subdat3$MAILPREF[is.na(subdat3$MAILPREF)] <- "N" #Majority

subdat3$ZONLINE[is.na(subdat3$ZONLINE)] <- "Y" #Majority
subdat3$ZMOB[is.na(subdat3$ZMOB)] <- "Y" #Majority
subdat3$ZPRCHPHN[is.na(subdat3$ZPRCHPHN)] <- "U" #Majority
subdat3$ZMOBMULT[is.na(subdat3$ZMOBMULT)] <- "Y" #Majority
subdat3$ZHMDECOR[is.na(subdat3$ZHMDECOR)] <- "Y" #Majority
subdat3$ZHOMEENT[is.na(subdat3$ZHOMEENT)] <- "U" #Majority
subdat3$ZKITCHEN[is.na(subdat3$ZKITCHEN)] <- "U" #Majority
subdat3$ZPRCHONL[is.na(subdat3$ZPRCHONL)] <- "Y" #Majority
subdat3$SALES[is.na(subdat3$SALES)] <- "U" #Unknown

subdat2$ZIP[is.na(subdat2$ZIP)] <- "60091" #Majority

#Impute Numeric Variables with Median
subdat3$EXAGE[subdat3$EXAGE=="U"] <- NA
subdat3$EXAGE[is.na(subdat3$EXAGE)] = median(subdat3$EXAGE, na.rm = TRUE)

#Impute Numeric Variables with 0
subdat3$salepercamp[is.na(subdat3$salepercamp)] <- 0
subdat3$salepercamp[is.infinite(subdat3$salepercamp)] <- 0
subdat3$salepertrans[is.na(subdat3$salepertrans)] <- 0
subdat3$NUM_CHILD[is.na(subdat3$NUM_CHILD)] <- 0

#Check for Missingness
sum(is.na(subdat3)) 
sapply(subdat3, function(x) sum(is.na(x)))
aggr_plot <- aggr(subdat3, col=c('#9ecae1','#de2d26'), numbers=TRUE,prop=FALSE, sortVars=TRUE, labels=names(subdat3), cex.axis=.5, gap=2, ylab=c("Histogram of missing data","Pattern"))

#Check missing data percentage
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(subdat3,2,pMiss)

str(subdat3)

####################################################################
###########  Random Forest model for XYZ Didn't Target#############
##################################################################

###Random Forest Model###
library(caret)
detach(package:ModelMetrics)

prob <- predict(rf.model, newdata = subdat3, type = "prob")[,2]
hist(prob)

#Confusion Matrix for Probabilities
library(caret)

glm.pred  <- ifelse(prob  > 0.50,1,0)
xtab.RF1=table(glm.pred, subdat3$RESPONSE16)
confusionMatrix(xtab.RF1, positive = "1") #Accuracy: 0.7925;  Sensitivity : 0.42535;  Specificity : 0.82129 

#Confusion Matrix Detail
confusion.matrix <- table(prob, subdat3$RESPONSE16)
confusion.matrix

#Plot Curves
require(PRROC)
set.seed(1)

fg <- prob[subdat3$RESPONSE16 == 1]
bg <- prob[subdat3$RESPONSE16 == 0]

#ROC Curve    
roc <- roc.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(roc)

#Precision Recall Curve
pr <- pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
plot(pr)

#Compute area under ROC curve
roc <- roc.curve( fg, bg );
print( roc ) #0.67757

#Compute area under PR curve
pr <- pr.curve( fg, bg )
print( pr ) #0.1790455

#AUC
#library(ModelMetrics)
#prob <- predict(rf.model, newdata=subdat3, type='prob')
#auc<-auc(subdat3$RESPONSE16,prob[,2])
#auc #0.67757 

## Evaluate on All data in subset
prediction_rf_all2<- predict(rf.model, newdata = subdat3, type = "prob")[,2]

pred2df2 <- as.data.frame(prediction_rf_all2)
data_alldf2 <- cbind(subdat3,prediction_rf_all2)
str(data_alldf2)

#Average Sales Per Transactions
mean(data_alldf2$salepertrans) #Average subset is $204.4749
mean(data_alldf2$salepertrans[data_alldf2$prediction_rf_all2>0.70]) #70% probability is 212.1587

#Average Revenue per Customer for All
mean(data_alldf2$REVPERCUST) #Average subset is $118.8639

#Targeted Customers with New Model
sum(data_alldf2$prediction_rf_all2>0.65) #1387
sum(data_alldf2$prediction_rf_all2>0.5) #3118
sum(data_alldf2$prediction_rf_all2>0.4) #5002
sum(data_alldf2$prediction_rf_all2>0.3) #7931
sum(data_alldf2$prediction_rf_all2>0.2) #11536
sum(data_alldf2$prediction_rf_all2>0.15) #13335
sum(data_alldf2$prediction_rf_all2>0.10) #14717
sum(data_alldf2$prediction_rf_all2>0.05) #15598

#Average Revenue per Customer for New Model
mean(data_alldf2$REVPERCUST[data_alldf2$prediction_rf_all2>0.65]) #466.9753
mean(data_alldf2$REVPERCUST[data_alldf2$prediction_rf_all2>0.5]) #339.6213
mean(data_alldf2$REVPERCUST[data_alldf2$prediction_rf_all2>0.4]) #263.3613
mean(data_alldf2$REVPERCUST[data_alldf2$prediction_rf_all2>0.3]) #191.4004
mean(data_alldf2$REVPERCUST[data_alldf2$prediction_rf_all2>0.2]) #143.3182
mean(data_alldf2$REVPERCUST[data_alldf2$prediction_rf_all2>0.15]) #129.8518
mean(data_alldf2$REVPERCUST[data_alldf2$prediction_rf_all2>0.10]) #122.6703
mean(data_alldf2$REVPERCUST[data_alldf2$prediction_rf_all2>0.05]) #119.5404

str(prediction_rf_all2)

###############################################################
##########################Map ################################
###############################################################
library(ggmap)
library(gridExtra)
library(ggplot2)

# lets make it a little easier to type
cc <- mydata
ccMap <- subset(cc,select=c(LONG,LAT,RESPONSE16))
ccMap$lon <- -(as.numeric(substr(ccMap$LONG,2,9))/1000000)
ccMap$lat <- (as.numeric(substr(ccMap$LAT,2,9))/1000000)
m1 <- qmplot(lon, lat, data =ccMap[ccMap$RESPONSE16 == 0,], color=I('red'),size = I(0.5), darken = .0) + ggtitle("Response16 = 0")
m2 <- qmplot(lon, lat, data =ccMap[ccMap$RESPONSE16 == 1,], color=I('green'),size = I(0.5), darken = .0) + ggtitle("Response16 = 1")
grid.arrange(m1, m2, nrow=2)
