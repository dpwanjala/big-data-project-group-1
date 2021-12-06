# read csv files for stock exchange indices 
## we have to change this so it reads from the RAWdata Folder, 
## but I couldnt figure it out

list.files()
csv.SP500 <- read.csv("SP500.5Years.csv")
csv.NASDAQ <- read.csv("NASDAQ.5Years.csv")
csv.DJI <- read.csv("DowJones.5Years.csv")
csv.TSX <- read.csv("tsx_composite_index_canada.csv")

# data frame check for each csv file
head(csv.SP500)
str(csv.SP500)
head (csv.NASDAQ)
str(csv.NASDAQ)
head(csv.DJI)
str(csv.DJI)
head(csv.TSX)
str(csv.TSX)

# read csv files for covid19 data 
## we have to change this so it reads from the RAWdata Folder, 
## but I couldnt figure it out
list.files()
csv.CVD.CAN <- read.csv("covid19canada-download.csv")
csv.CVD.USA <- read.csv("CoronaUSDec12.csv")

# data frame check for each csv file
head (csv.CVD.CAN)
str(csv.CVD.CAN)
head (csv.CVD.USA)
str(csv.CVD.USA)

# sorting covid data from Canada by whole country
Canada.covid.data <- csv.CVD.CAN[csv.CVD.CAN$prname == "Canada",]

# create function to find start and stop dates for data 
find.dates <- function(date) {
  max.date <- max(date)
  min.date <- min(date)
  min.max <- c(min.date, max.date)
  return(min.max)
}
# create function to alter format of dates
## I need to fix the function such that it runs format 1 and then if it gets
## a NA it then runs format 2, we need this for the DJI index
alter.date.fmt <- function(date){
  dates.cr.fmt.1 <- as.Date.character(date, format = "%m/%d/%Y")
  return(dates.cr.fmt.1) 
}
# function for second date format
alter.date.fmt.2 <- function(date){
  dates.cr.fmt.2 <- as.Date.character(date, format = "%b %d,%Y")
  return(dates.cr.fmt.2)
}

#  format = "%b %d,%Y"

# creating vectors of equal length, finding start and stop dates for covid data
# Canada
Canada.dates <- csv.CVD.CAN$date
Canada.CVD.dates.max.min <- find.dates(Canada.covid.data$date)

# USA
# replace / in submitted dates with - for correct reading of calendar values
USA.covid.cr.dates <- alter.date.fmt(csv.CVD.USA$submission_date)
csv.CVD.USA$submission_date <- USA.covid.cr.dates

# 
csv.CVD.USA$new_case <- as.numeric(gsub(",","",csv.CVD.USA$new_case))
USA.covid.data <- aggregate(csv.CVD.USA['new_case'], by=csv.CVD.USA['submission_date'], sum)

USA.dates <- find.dates(csv.CVD.USA$submission_date)

# creating vectors of equal length, finding start and stop dates for indecis data
# SP500
# replace / in submitted dates with - for correct reading of calendar values
SP500.cr.date.fmt <- alter.date.fmt(csv.SP500$Date)
csv.SP500$Date <- SP500.cr.date.fmt

# SP500 dates

find.dates(csv.SP500$Date)


# NASDAQ 
find.dates(csv.NASDAQ$Date)

# DJI
DJI.dates <- csv.DJI$Date
DJI.cr.dates <- alter.date.fmt.2(csv.DJI$Date)
csv.DJI$Date <- DJI.cr.dates

find.dates(csv.DJI$Date)

# TSX
find.dates(csv.TSX$Date)

# create vectors for daily closing values from each index by Canada covid dates
SP500.dates <- csv.SP500$Date[csv.SP500$Date >= Canada.CVD.dates.max.min[1] & csv.SP500$Date <= Canada.CVD.dates.max.min[2]]
SP500.close <- csv.SP500$Close.Last[csv.SP500$Date >= Canada.CVD.dates.max.min[1] & csv.SP500$Date <= Canada.CVD.dates.max.min[2]]

SP500.df <- cbind.data.frame(SP500.dates,SP500.close)
#sp500.close.df <- cbind.data.frame(close.SP500)


#close.NASDAQ <- csv.NASDAQ$Close[csv.NASDAQ$Date >= Canada.dates[1] & csv.NASDAQ$Date <= Canada.dates[2]]

#close.DJI <- csv.DJI$Price[csv.DJI$Date >= Canada.dates[1] & csv.DJI$Date <= Canada.dates[2]]

#close.TSX <- csv.TSX$Close[csv.TSX$Date >= Canada.dates[1] & csv.TSX$Date <= Canada.dates[2]]


# create vectors for daily cases in each country 
cases.d.CAN <- Canada.covid.data$numtoday
cases.d.USA <- USA.covid.data$new_case

Canada.Cases.df <- cbind.data.frame(Canada.covid.data$date, Canada.covid.data$numtoday)

ds <- merge(Canada.Cases.df, SP500.df, by.x = " Canada.covid.data$date", by.y = "SP500.dates")




# total covid cases
total.cases.CAN <- sum(cases.d.CAN)
total.cases.USA <- 


