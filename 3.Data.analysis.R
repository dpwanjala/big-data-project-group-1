# read cleaned csv files
csv.SP500 <- read.csv(paste(p.data.clean, "csv.SP500.clean.csv", sep = ""))
csv.NASDAQ <- read.csv(paste(p.data.clean, "csv.NASDAQ.clean.csv", sep = ""))
csv.DJI <- read.csv(paste(p.data.clean, "csv.DJI.clean.csv",sep = ""))
csv.TSX <- read.csv(paste(p.data.clean,"csv.TSX.clean.csv", sep = ""))
csv.DAX <- read.csv(paste(p.data.clean, "csv.DAX.clean.csv", sep = ""))

csv.CVD.CAN <- read.csv(paste(p.data.clean,
                              "csv.CVD.CAN.clean.csv", sep = ""))
csv.CVD.USA <- read.csv(paste(p.data.clean,
                              "csv.CVD.USA.clean.csv", sep = ""))
csv.CVD.DE <- read.csv(paste(p.data.clean,
                             "csv.CVD.DE.clean.csv", sep = ""))

# create country data frames 
# Canada
CAN.CVD.df <- cbind.data.frame(csv.CVD.CAN$CAN.CVD.data.date,
                               csv.CVD.CAN$CAN.CVD.data.numtoday)
colnames(CAN.CVD.df) <- c("Date", "Daily cases")


# USA
USA.CVD.df <- cbind.data.frame(csv.CVD.USA$submission_date,
                               csv.CVD.USA$new_case)
colnames(USA.CVD.df) <- c("Date", "Daily cases")

# Germany 
DE.CVD.df <- cbind.data.frame(csv.CVD.DE$csv.CVD.DE.time_iso8601,
                              csv.CVD.DE$DE.daily.cases)
colnames(DE.CVD.df) <- c("Date", "Daily cases")

# create function to find start and stop dates for data 
find.dates <- function(date) {
  max.date <- max(date)
  min.date <- min(date)
  min.max <- c(min.date, max.date)
  return(min.max)
}

# Finding start and stop dates for covid data
# Canada
CAN.CVD.dates.min.max <- find.dates(csv.CVD.CAN$CAN.CVD.data.date)

# USA
USA.CVD.dates.min.max <- find.dates(csv.CVD.USA$submission_date)

# Germany
DE.CVD.dates.min.max <- find.dates(csv.CVD.DE$csv.CVD.DE.time_iso8601)

# create function to create vectors from dataframe sorted by column 
fmt.by.date <- function(x,y,min.val,max.val){
  df.vector.values <- c(x[y >= min.val & y <= max.val])
  return(df.vector.values)
}


# Create data frames to run correlation between 
# daily covid cases and index closing prices

# create vectors for dates from each index by USA covid dates
# create vectors for daily closing values from each index by USA covid dates
# SP500
SP500.dates <- fmt.by.date(csv.SP500$Date, csv.SP500$Date,
                           USA.CVD.dates.min.max[1], USA.CVD.dates.min.max[2])
SP500.close <- fmt.by.date(csv.SP500$Close.Last, csv.SP500$Date,
                           USA.CVD.dates.min.max[1], USA.CVD.dates.min.max[2])

# NASDAQ
NASDAQ.dates <- fmt.by.date(csv.NASDAQ$Date, csv.NASDAQ$Date,
                            USA.CVD.dates.min.max[1], USA.CVD.dates.min.max[2])
NASDAQ.close <- fmt.by.date(csv.NASDAQ$Close, csv.NASDAQ$Date,
                            USA.CVD.dates.min.max[1], USA.CVD.dates.min.max[2])

# DJI
DJI.dates <- fmt.by.date(csv.DJI$Date, csv.DJI$Date,
                         USA.CVD.dates.min.max[1], USA.CVD.dates.min.max[2])
DJI.close <- fmt.by.date(csv.DJI$Price, csv.DJI$Date,
                         USA.CVD.dates.min.max[1], USA.CVD.dates.min.max[2])

# create vectors for dates from each index by Canada covid dates
# create vectors for daily closing values from each index by Canada covid dates
# TSX
TSX.dates <- fmt.by.date(csv.TSX$Date, csv.TSX$Date,
                         CAN.CVD.dates.min.max[1], CAN.CVD.dates.min.max[2])
TSX.close <- fmt.by.date(csv.TSX$Close, csv.TSX$Date,
                         CAN.CVD.dates.min.max[1], CAN.CVD.dates.min.max[2])

# create vectors for dates from each index by German covid dates
# create vectors for daily closing values from each index by German covid dates
# DAX
DAX.dates <- fmt.by.date(csv.DAX$Date, csv.DAX$Date,
                         DE.CVD.dates.min.max[1], DE.CVD.dates.min.max[2])
DAX.close <- fmt.by.date(csv.DAX$Price, csv.DAX$Date,
                         DE.CVD.dates.min.max[1], DE.CVD.dates.min.max[2])

# create data frame for daily closing values from each index 
# by associated covid data
# SP500
SP500.df <- cbind.data.frame(SP500.dates, SP500.close)

# NASDAQ
NASDAQ.df <- cbind.data.frame(NASDAQ.dates, NASDAQ.close)

# DJI
DJI.df <- cbind.data.frame(DJI.dates, DJI.close)

# TSX
TSX.df <- cbind.data.frame(TSX.dates, TSX.close)

# DAX
DAX.df <- cbind.data.frame(DAX.dates, DAX.close)

# rename column names for index data frame dates to "Date"
# indices 
names(SP500.df)[names(SP500.df) == "SP500.dates"] <- "Date"
names(NASDAQ.df)[names(NASDAQ.df) == "NASDAQ.dates"] <- "Date"
names(DJI.df)[names(DJI.df) == "DJI.dates"] <- "Date"
names(TSX.df)[names(TSX.df) == "TSX.dates"] <- "Date"
names(DAX.df)[names(DAX.df) == "DAX.dates"] <- "Date"

# rename column names for index data frames to "Closing price"
names(SP500.df)[names(SP500.df) == "SP500.close"] <- "Closing price"
names(NASDAQ.df)[names(NASDAQ.df) == "NASDAQ.close"] <- "Closing price"
names(DJI.df)[names(DJI.df) == "DJI.close"] <- "Closing price"
names(TSX.df)[names(TSX.df) == "TSX.close"] <- "Closing price"
names(DAX.df)[names(DAX.df) == "DAX.close"] <- "Closing price"

# cast values in Date columns as Dates 
# countries 
CAN.CVD.df$Date <- as.Date(CAN.CVD.df$Date)
USA.CVD.df$Date <- as.Date(USA.CVD.df$Date)
DE.CVD.df$Date <- as.Date(DE.CVD.df$Date)
# indices
SP500.df$Date <- as.Date(SP500.df$Date)
NASDAQ.df$Date <- as.Date(NASDAQ.df$Date)
DJI.df$Date <- as.Date(DJI.df$Date)
TSX.df$Date <- as.Date(TSX.df$Date)
DAX.df$Date <- as.Date(DAX.df$Date)


# inner join merge of data frames for each country 
# such that equal dates with values match
# USA
USA.SP500.close.merged <- merge(x= USA.CVD.df, y=SP500.df, by = 'Date')
USA.NASDAQ.close.merged <- merge(x= USA.CVD.df, y=NASDAQ.df, by = 'Date')
USA.DJI.close.merged <- merge(x= USA.CVD.df, y=DJI.df, by = 'Date')
# Canada
CAN.TSX.close.merged <- merge(x= CAN.CVD.df, y=TSX.df, by = 'Date')
# Germany
DE.DAX.close.merged <- merge(x= DE.CVD.df, y=DAX.df, by = 'Date')


# test correlation 
# USA
cor(USA.SP500.close.merged$`Closing price`, USA.SP500.close.merged$`Daily cases`)
cor(USA.NASDAQ.close.merged$`Closing price`, USA.NASDAQ.close.merged$`Daily cases`)
cor(USA.DJI.close.merged$`Closing price`, USA.NASDAQ.close.merged$`Daily cases`)
# Canada
cor(CAN.TSX.close.merged$`Closing price`, CAN.TSX.close.merged$`Daily cases`)
# Germany
cor(DE.DAX.close.merged$`Closing price`, DE.DAX.close.merged$`Daily cases`)

# write csv files for each country covid data merged with 
# country index data
#SP500 & USA
write.csv(USA.SP500.close.merged, paste(p.output,
          "USA.SP500.close.merged.csv", sep = ""), row.names = FALSE)

# NASDAQ & USA
write.csv(USA.NASDAQ.close.merged, paste(p.output,
          "USA.NASDAQ.close.merged.csv", sep = ""), row.names = FALSE)

#DJI & USA
write.csv(USA.DJI.close.merged, paste(p.output,
          "USA.DJI.close.merged.csv", sep = ""), row.names = FALSE)

# TSX & Canada
write.csv(CAN.TSX.close.merged, paste(p.output,
          "CAN.TSX.close.merged.csv", sep = ""), row.names = FALSE)

# DAX & Germany
write.csv(DE.DAX.close.merged, paste(p.output,
          "DE.DAX.close.merged.csv", sep = ""), row.names = FALSE)


###### NEED TO WRITE ABOUT NEXT PART OF FUNCTION #########

# function to calculate percent difference between two values 
percent.dif <- function(x,y){
            top <- x-y
            bottom <- (x+y)/2
            final <- top/bottom
            return(final)
}


# Create data frames to run correlation between 
# daily covid cases and percent difference between index stock price high and low

# create vectors for dates from each index by USA covid dates
# create vectors for daily percent differences in stock price
# for each index by USA covid dates
# SP500
SP500.dates <- csv.SP500$Date[csv.SP500$Date >= USA.CVD.dates.min.max[1] 
                              & csv.SP500$Date <= USA.CVD.dates.min.max[2]]

SP500.diff <- abs(percent.dif((csv.SP500$Low[csv.SP500$Date >= USA.CVD.dates.min.max[1] 
                                          & csv.SP500$Date <= USA.CVD.dates.min.max[2]]),
  (csv.SP500$High[csv.SP500$Date >= USA.CVD.dates.min.max[1] 
                 & csv.SP500$Date <= USA.CVD.dates.min.max[2]]))*100)


# NASDAQ
NASDAQ.dates <- csv.NASDAQ$Date[csv.NASDAQ$Date >= USA.CVD.dates.min.max[1] 
                                & csv.NASDAQ$Date <= USA.CVD.dates.min.max[2]]
NASDAQ.diff <- abs(percent.dif((csv.NASDAQ$Low[csv.NASDAQ$Date >= USA.CVD.dates.min.max[1] 
                                              & csv.NASDAQ$Date <= USA.CVD.dates.min.max[2]]),
                               (csv.NASDAQ$High[csv.NASDAQ$Date >= USA.CVD.dates.min.max[1] 
                                               & csv.NASDAQ$Date <= USA.CVD.dates.min.max[2]]))*100)

# DJI
DJI.dates <- csv.DJI$Date[csv.DJI$Date >= USA.CVD.dates.min.max[1] 
                          & csv.DJI$Date <= USA.CVD.dates.min.max[2]]
DJI.diff <- abs(percent.dif((csv.DJI$Low[csv.DJI$Date >= USA.CVD.dates.min.max[1] 
                                               & csv.DJI$Date <= USA.CVD.dates.min.max[2]]),
                               (csv.DJI$High[csv.DJI$Date >= USA.CVD.dates.min.max[1] 
                                                & csv.DJI$Date <= USA.CVD.dates.min.max[2]]))*100)

# create vectors for dates from each index by Canada covid dates
# create vectors for daily percent differences in stock price
# TSX
TSX.dates <- csv.TSX$Date[csv.TSX$Date >= CAN.CVD.dates.min.max[1] 
                          & csv.TSX$Date <= CAN.CVD.dates.min.max[2]]
TSX.diff <- abs(percent.dif((csv.TSX$Low[csv.TSX$Date >= CAN.CVD.dates.min.max[1] 
                                         & csv.TSX$Date <= CAN.CVD.dates.min.max[2]]),
                            (csv.TSX$High[csv.TSX$Date >= CAN.CVD.dates.min.max[1] 
                                          & csv.TSX$Date <= CAN.CVD.dates.min.max[2]]))*100)

# create vectors for dates from each index by German covid dates
# create vectors for daily percent differences in stock price
# DAX
DAX.dates <- csv.DAX$Date[csv.DAX$Date >= DE.CVD.dates.min.max[1] 
                          & csv.DAX$Date <= DE.CVD.dates.min.max[2]]
DAX.diff <- abs(percent.dif((csv.DAX$Low[csv.DAX$Date >= DE.CVD.dates.min.max[1] 
                                          & csv.DAX$Date <= DE.CVD.dates.min.max[2]]),
                             (csv.DAX$High[csv.DAX$Date >= DE.CVD.dates.min.max[1] 
                                           & csv.DAX$Date <= DE.CVD.dates.min.max[2]]))*100)


# create data frame for daily percent differences in stock price from each index 
# by associated covid data
# SP500
SP500.df.diff <- cbind.data.frame(SP500.dates, SP500.diff)

# NASDAQ
NASDAQ.df.diff <- cbind.data.frame(NASDAQ.dates, NASDAQ.diff)

# DJI
DJI.df.diff <- cbind.data.frame(DJI.dates, DJI.diff)

# TSX
TSX.df.diff <- cbind.data.frame(TSX.dates, TSX.diff)

# DAX
DAX.df.diff <- cbind.data.frame(DAX.dates, DAX.diff)

# rename column names for all data frame dates to "Date"
# countries 
names(CAN.CVD.df)[names(CAN.CVD.df) == "CAN.CVD.data$date"] <- "Date"
names(USA.CVD.df)[names(USA.CVD.df) == "csv.CVD.USA$submission_date"] <- "Date"
names(DE.CVD.df)[names(DE.CVD.df) == "csv.CVD.DE$time_iso8601"] <- "Date"

# indices 
names(SP500.df.diff)[names(SP500.df.diff) == "SP500.dates"] <- "Date"
names(NASDAQ.df.diff)[names(NASDAQ.df.diff) == "NASDAQ.dates"] <- "Date"
names(DJI.df.diff)[names(DJI.df.diff) == "DJI.dates"] <- "Date"
names(TSX.df.diff)[names(TSX.df.diff) == "TSX.dates"] <- "Date"
names(DAX.df.diff)[names(DAX.df.diff) == "DAX.dates"] <- "Date"

# rename column names for country data frames to "Daily cases"
names(CAN.CVD.df)[names(CAN.CVD.df) == "CAN.CVD.data$numtoday"] <- "Daily cases"
names(USA.CVD.df)[names(USA.CVD.df) == "new_case"] <- "Daily cases"
names(DE.CVD.df)[names(DE.CVD.df) == "DE.daily.cases"] <- "Daily cases"

# rename column names for index data frames to "Percentage Difference"
names(SP500.df.diff)[names(SP500.df.diff) == "SP500.diff"] <- "Percentage Difference"
names(NASDAQ.df.diff)[names(NASDAQ.df.diff) == "NASDAQ.diff"] <- "Percentage Difference"
names(DJI.df.diff)[names(DJI.df.diff) == "DJI.diff"] <- "Percentage Difference"
names(TSX.df.diff)[names(TSX.df.diff) == "TSX.diff"] <- "Percentage Difference"
names(DAX.df.diff)[names(DAX.df.diff) == "DAX.diff"] <- "Percentage Difference"

# cast values in Date columns as Dates 
# countries 
CAN.CVD.df$Date <- as.Date(CAN.CVD.df$Date)
USA.CVD.df$Date <- as.Date(USA.CVD.df$Date)
DE.CVD.df$Date <- as.Date(DE.CVD.df$Date)
# indices
SP500.df.diff$Date <- as.Date(SP500.df.diff$Date)
NASDAQ.df.diff$Date <- as.Date(NASDAQ.df.diff$Date)
DJI.df.diff$Date <- as.Date(DJI.df.diff$Date)
TSX.df.diff$Date <- as.Date(TSX.df.diff$Date)
DAX.df.diff$Date <- as.Date(DAX.df.diff$Date)

# inner join merge of data frames for each country 
# such that equal dates with values match
# USA
USA.SP500.diff.merged <- merge(x= USA.CVD.df, y=SP500.df.diff, by = 'Date')
USA.NASDAQ.diff.merged <- merge(x= USA.CVD.df, y=NASDAQ.df.diff, by = 'Date')
USA.DJI.diff.merged <- merge(x= USA.CVD.df, y=DJI.df.diff, by = 'Date')
# Canada
CAN.TSX.diff.merged <- merge(x= CAN.CVD.df, y=TSX.df.diff, by = 'Date')
# Germany
DE.DAX.diff.merged <- merge(x= DE.CVD.df, y=DAX.df.diff, by = 'Date')


# test correlation 
# USA
cor(USA.SP500.diff.merged$`Percentage Difference`, USA.SP500.diff.merged$`Daily cases`)
cor(USA.NASDAQ.diff.merged$`Percentage Difference`, USA.NASDAQ.diff.merged$`Daily cases`)
cor(USA.DJI.diff.merged$`Percentage Difference`, USA.NASDAQ.diff.merged$`Daily cases`)
# Canada
cor(CAN.TSX.diff.merged$`Percentage Difference`, CAN.TSX.diff.merged$`Daily cases`)
# Germany
cor(DE.DAX.diff.merged$`Percentage Difference`, DE.DAX.diff.merged$`Daily cases`)


# test correlation with P-value?
# USA
# cor.test(USA.SP500.diff.merged$`Percentage Difference`, USA.SP500.diff.merged$`Daily cases`, method = 'pearson')
# cor.test(USA.NASDAQ.diff.merged$`Percentage Difference`, USA.NASDAQ.diff.merged$`Daily cases`, method = 'pearson')
# cor.test(USA.DJI.diff.merged$`Percentage Difference`, USA.NASDAQ.diff.merged$`Daily cases`, method = 'pearson')
# Canada
#cor.test(CAN.TSX.diff.merged$`Percentage Difference`, CAN.TSX.diff.merged$`Daily cases`, method = 'pearson')
# Germany
#cor.test(DE.DAX.diff.merged$`Percentage Difference`, DE.DAX.diff.merged$`Daily cases`, method = 'pearson')
