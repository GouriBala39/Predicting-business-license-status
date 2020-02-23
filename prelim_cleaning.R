### This dataset is insanely large and I would personally recommend working with 
### some external server. The Data cleaning and EDA might not really bother you
### but while using various classifiers and algorithms, your PC might not be able 
### to handle it(I hope it does though)


### Hopefully this could prove as a step-by-step guide as to how I solved this problem

##############################################################################

###  *************-----  Importing dataset -----****************

train_dataset <- read.csv("C:/Users/****/****/train_file.csv")

### This dataset has some 85000+ rows, let us not comprise on data
options(max.print = 90000)

##### Let us look at the training dataset given and understand few basic of the dataset
head(train_dataset)
nrow(train_dataset)  #85895
ncol(train_dataset)  #32
##############################################################################

### *************----   EDA   ----*****************

### Now according to problem statement, we need to predict License status
### So let's be intuitive about the same
table(train_dataset$LICENSE.STATUS)
####################################
#  AAC   AAI   INQ   REA   REV 
#30200 55400     2     3   290
####################################

### Let us check for uniques and missing values
for(i in 1:32)
{
  print(i)
  print(length(unique(train_dataset[,i]))) ## to find unique values in that column
  print(sum(is.na(train_dataset[,i]))) ## to check for NA values
}

####################
[1] 1
[1] 85895
[1] 0
[1] 2
[1] 85895
[1] 0
[1] 3
[1] 38897
[1] 0
[1] 4
[1] 177
[1] 0
[1] 5
[1] 38744
[1] 0
[1] 6
[1] 41687
[1] 0
[1] 7
[1] 42146
[1] 0
[1] 8
[1] 1223
[1] 0
[1] 9
[1] 54
[1] 0
[1] 10
[1] 1998
[1] 0
[1] 11
[1] 51
[1] 49701
[1] 12
[1] 77
[1] 56701
[1] 13
[1] 2384
[1] 0
[1] 14
[1] 29
[1] 54012
[1] 15
[1] 106
[1] 0
[1] 16
[1] 106
[1] 0
[1] 17
[1] 49769
[1] 1
[1] 18
[1] 5
[1] 0
[1] 19
[1] 2900
[1] 0
[1] 20
[1] 3079
[1] 0
[1] 21
[1] 4357
[1] 0
[1] 22
[1] 2
[1] 0
[1] 23
[1] 2826
[1] 0
[1] 24
[1] 298
[1] 0
[1] 25
[1] 4224
[1] 0
[1] 26
[1] 3498
[1] 0
[1] 27
[1] 3153
[1] 0
[1] 28
[1] 54
[1] 76446
[1] 29
[1] 21387
[1] 47246
[1] 30
[1] 21389
[1] 47246
[1] 31
[1] 21389
[1] 0
[1] 32
[1] 5
[1] 0

##################

### That is some awfully large number of NA's in some column
### Along with some disturbing unique values for some columns

##################

### Now let us draw some other conclusions:

## According to the data description :
## Columns like legal name and doing-business-as-name are the legal-wise and personal
##  names given to the record respectively
### so let us see if there are records with commonality
length(intersect(unique(train_dataset$LEGAL.NAME),unique(train_dataset$DOING.BUSINESS.AS.NAME)))
##### - 16012
### Again according to the data description : 
## Columns like license ID and license number are internal db id and number known to
## general public respectively.
################  --------- Note -------------
## Each license has a single license number that stays consistent throughout the
## lifetime of a license

## So let us see if there are records with commonality
length(intersect(unique(train_dataset$LICENSE.ID),unique(train_dataset$LICENSE.NUMBER)))
##### - 24708


## There are several types of dates present in the records and also a lot of detail 
## has been provided of the location of the record

## So we move to variable selection and feature engineering
########################################################################

########### --------- VARIABLE SELECTION --------- ############

### Hypothesis testing

fisher.test(train_dataset$APPLICATION.TYPE,train_dataset$LICENSE.STATUS)
### will not work as fisher test works well only on small samples

for (i in 4:31) 
{
  print(i)
  print(chisq.test(train_dataset[,i],train_dataset$LICENSE.STATUS
                   ,simulate.p.value = TRUE))
}
### We are seeing that missing values in the dataframe are hampering our results
### So let us deal with that
######################  ----------------------------
###### dealing with missing values
#### we will be replacing the NA values with the mode of the column
getmode <- function(x) {
  na_rowids <- na.omit(unique(x) )
  tab <- tabulate(match(x, na_rowids))
  na_rowids[tab == max(tab) ]
}
### let us first identify these columns
colindex <- c(11,12,14,17,28,29,31)
### -------------- let us consider a new dataframe so that our original does not 
### ------------------------------- get disturbed
df <- train
for (i in colindex)
{
  #print(i)
  df[,i] <- ifelse(is.na(df[,i]),getmode(df[,i]),df[,i]) 
}
###########################
### Checking
df[is.na.data.frame(df) == TRUE]

### This still shows missing values, this is because columns with datatype : date
### are diffiult to comprehend when it comes to calculating mode.

################ ---- so time for feature engineering


###### let us merge the ward  and the precinct column as according to the 
############ data description :
###  1. Same precinct numbers exist in multiple wards
###  2. Ward-Precinct column can be used to filter precinct across multiple wards
###  3. Ward shows the ward of the business office
for (i in 1:nrow(df)) {
  
  df$WARD.PRECINCT1[i] <- as.character(paste(df$WARD[i],df$PRECINCT[i],sep = "-"))
  
}
##################################

#### Now as mentioned earlier there are a number of dates present in the dataset
#### Some have missing values too
#### Datatype has time component questionable

######## So let's add features like : 
### 1. Does the number of days between when the payment is made and the application
###         requirements are completed, affects our target variable?
###   If not individually, maybe the delay is causing the license status of the record
###       to change.

########
### Next,
### 2. Note that columns such as License start date, issue date, and expiration dates
###    are not continuos or rythmic in sense there are rows with discrerpancy in data.

###############################################
### Introducing new dataframe 
df1 <-  df
### Let us check these out
strsplit(df1$APPLICATION.REQUIREMENTS.COMPLETE,split='T', fixed=TRUE) -> df1$APPLICATION.REQUIREMENTS.COMPLETE
sapply(df1$APPLICATION.REQUIREMENTS.COMPLETE, "[", 1) -> df1$APPLICATION.REQUIREMENTS.COMPLETE

strsplit(df1$PAYMENT.DATE,split = "T",fixed = TRUE) -> df1$PAYMENT.DATE
sapply(df1$PAYMENT.DATE, "[", 1) -> df1$PAYMENT.DATE

as.Date(df1$PAYMENT.DATE) - as.Date(df1$APPLICATION.REQUIREMENTS.COMPLETE) -> Days_in_bw
df1$Days_in_bw <- Days_in_bw

strsplit(df1$LICENSE.TERM.START.DATE,split = "T",fixed = TRUE) -> df1$LICENSE.TERM.START.DATE
sapply(df1$LICENSE.TERM.START.DATE, "[", 1) -> df1$LICENSE.TERM.START.DATE


############### Let us further assert our assumptions and conclusions with some 
#######             visualizations

##### Are there any remaining missing values present in our dataframe?
install.packages("DataExplorer")
library(DataExplorer)
plot_missing(
  df,
  group = list(Good = 0.05, OK = 0.4, Bad = 0.8, Remove = 1),
  missing_only = FALSE,
  geom_label_args = list(),
  title = NULL,
  ggtheme = theme_gray(),
  theme_config = list(legend.position = c("bottom"))
)
########## Let us explore plots 
library(ggplot2)

ggplot(df,aes(y = LICENSE.STATUS))+
  geom_point(aes(x = SITE.NUMBER,color = factor(APPLICATION.TYPE))
             ,size = 2)

ggplot(df,aes(y = STATE))+
  geom_point(aes(x = LICENSE.STATUS,color = factor(CONDITIONAL.APPROVAL))
             ,size = 2)
############### Note certain findings  :  
### out of 30200 AAC : 18 Y : conditional approved
### State codes with VT,ON,NM,NH,ME,DE do not have AAc
### State codes with GB,CN,WY do not have AAI
ggplot(df,aes(y = SSA))+
  geom_point(aes(x = LICENSE.STATUS,color = factor(APPLICATION.TYPE))
             ,size = 2)

ggplot(train_dataset,aes(y = SSA))+
  geom_point(aes(x = LICENSE.STATUS,color = factor(APPLICATION.TYPE))
             ,size = 2)
########## Note how missing data is affecting the plots
##### Majority of the dataset have renew application type 
#####    + its distribution also is not ideal for one-hit prediction
### More no of renew's in na.omit graph and a missing license status


############ Let us explore our feature engineered dataframes
ggplot(df1,aes(y = LICENSE.DESCRIPTION))+
  geom_point(aes(x = LICENSE.STATUS)
             ,size = 2)
### Note how no desciption is common to all 5 statuses
ggplot(df1,aes(y = Days_in_bw))+
  geom_point(aes(x = LICENSE.STATUS)
             ,size = 2)
### With several days in between, still some licenses were issued
### Very few revoked applications correspond to less days in between 

ggplot(df,aes(y = WARD))+
  geom_point(aes(x = PRECINCT)
             ,size = 2)
### This graph was plotted to find out what exactly the data description meant 
###      for precinct and ward columns
### Note the 4 outliers
ggplot(df,aes(y = PRECINCT))+
  geom_point(aes(x = WARD.PRECINCT1)
             ,size = 2)
### Observe the relationship b/w ward.precinct and precinct and 4 outliers

##### Let us remove columns based on above trials

df1$WARD.PRECINCT <- NULL
df1$APPLICATION.REQUIREMENTS.COMPLETE <- NULL
df1$LOCATION <- NULL
df1$LATITUDE <- NULL
df1$LONGITUDE <- NULL
df1$SITE.NUMBER <- NULL
df1$WARD <- NULL
df1$PRECINCT <- NULL
df1$LEGAL.NAME <- NULL
df1$DOING.BUSINESS.AS.NAME <- NULL
df1$ADDRESS <- NULL
df1$PAYMENT.DATE <- NULL
df1$DATE.ISSUED <- NULL
df1$LICENSE.STATUS.CHANGE.DATE <- NULL
df1$LICENSE.TERM.START.DATE <- NULL
df1$LICENSE.TERM.EXPIRATION.DATE <- NULL
df1$LICENSE.APPROVED.FOR.ISSUANCE <- NULL

############


