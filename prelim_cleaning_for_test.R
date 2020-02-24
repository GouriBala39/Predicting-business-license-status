#### Let us modify test dataset

test_dataset <- read.csv("test_file.csv")

test_df <- test_dataset
#### Similarly checking for NA's and uniques
for(i in 1:31)
{
  print(i)
  print(length(unique(test_dataset[,i]))) ## to find unique values in that column
  print(sum(is.na(test_dataset[,i]))) ## to check for NA values
}

colindex1 <- c(10,11,12,14,28,29,30)  ## vector containing NA values of column indices

### Trying to modify the NA's
for (i in colindex)
{
  test_df[,i] <- ifelse(is.na(test_df[,i]),getmode(test_df[,i]),test_df[,i]) 
}

##### Feature engineering
df2 <- test_df

for (i in 1:nrow(df2)) {
  
  df2$WARD.PRECINCT1[i] <- as.character(paste(df2$WARD[i],df2$PRECINCT[i],sep = "-"))
  
}

df2$WARD.PRECINCT <- NULL
df2$APPLICATION.REQUIREMENTS.COMPLETE <- NULL
df2$LOCATION <- NULL
df1$LATITUDE <- NULL
df1$LONGITUDE <- NULL
df1$SITE.NUMBER <- NULL
df1$WARD <- NULL
df1$PRECINCT <- NULL
df2$LEGAL.NAME <- NULL
df2$DOING.BUSINESS.AS.NAME <- NULL
df2$ADDRESS <- NULL
df2$PAYMENT.DATE <- NULL
df2$DATE.ISSUED <- NULL
df2$LICENSE.STATUS.CHANGE.DATE <- NULL
df2$LICENSE.TERM.START.DATE <- NULL
df2$LICENSE.TERM.EXPIRATION.DATE <- NULL
df2$LICENSE.APPROVED.FOR.ISSUANCE <- NULL
df2$APPLICATION.CREATED.DATE <- NULL
#############


##### Lets save the file and move to python!!!
write.csv(df2, "pythontest.csv")















