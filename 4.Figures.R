# === Figures ==================================================================
# 1.Main.R is required to run first to acces folder paths
#This file creates plots from the clean data
#Plots are saved as pdfs to d.figures folder

#=== Read Clean Data ===========================================================
#reading from output folder data files ready to plot
# percent difference csv files
USA.SP500.diff.merged <- read.csv(paste(p.output,
                                        "USA.SP500.diff.merged.csv", sep = ""))
USA.NASDAQ.diff.merged <- read.csv(paste(p.output,
                                         "USA.NASDAQ.diff.merged.csv", sep = ""))
USA.DJI.diff.merged <- read.csv(paste(p.output,
                                      "USA.DJI.diff.merged.csv",sep = ""))
CAN.TSX.diff.merged <- read.csv(paste(p.output,
                                      "CAN.TSX.diff.merged.csv", sep = ""))
DE.DAX.diff.merged <- read.csv(paste(p.output,
                                     "DE.DAX.diff.merged.csv", sep = ""))

# percent change csv files
USA.SP500.change.merged <- read.csv(paste(p.output,
                                    "USA.SP500.change.merged.csv", sep = ""))
USA.NASDAQ.change.merged <- read.csv(paste(p.output,
                                    "USA.NASDAQ.change.merged.csv", sep = ""))
USA.DJI.change.merged <- read.csv(paste(p.output,
                                  "USA.DJI.change.merged.csv",sep = ""))
CAN.TSX.change.merged <- read.csv(paste(p.output,
                                  "CAN.TSX.change.merged.csv", sep = ""))
DE.DAX.change.merged <- read.csv(paste(p.output,
                                 "DE.DAX.change.merged.csv", sep = ""))


#===========Variables for Graphs================================================
#This part assigns graph parameters to variables for easier use.
dotsColor <- 'grey'
dotsForm <- 16
dotsSize <-0.7

lineColor <- 'red'
lineSize <- 3

textSize <- 1


#===========Columns to Variables================================================
#This part assigns columns to variables to use in the code for plots
#
#
#
# Difference graphs
USA.PercDiff.DJI <-USA.DJI.diff.merged$Percentage.Difference
USA.Cases.DJI <- USA.DJI.diff.merged$Daily.cases
USA.PercDiff.Nasdaq <- USA.NASDAQ.diff.merged$Percentage.Difference
USA.Cases.Nasdaq <- USA.NASDAQ.diff.merged$Daily.cases
USA.PercDiff.SP500 <- USA.SP500.diff.merged$Percentage.Difference
USA.Cases.SP500 <- USA.SP500.diff.merged$Daily.cases

Can.PercDiff <- CAN.TSX.diff.merged$Percentage.Difference
Can.Cases <- CAN.TSX.diff.merged$Daily.cases

Ger.PercDiff <- DE.DAX.diff.merged$Percentage.Difference
Ger.Cases <- DE.DAX.diff.merged$Daily.cases

#Percentage change graphs
USA.PercChange.DJI <- USA.DJI.change.merged$Change.in.price
USA.PercChange.Nasdaq <- USA.NASDAQ.change.merged$Change.in.price
USA.PercChange.SP500 <- USA.SP500.change.merged$Change.in.price
Can.PercChange <- CAN.TSX.change.merged$Change.in.price
Ger.PercChange <- DE.DAX.change.merged$Change.in.price

#===========Plotting percent change for the US==================================
#plotting 3 graphs that show the percent change for three US indices
#plotting in the same pdf file
#Note that the same structure for plotting is used throughout the file
pdf(paste(p.fig, "UScorrelationCHANGE.pdf", sep = ""))
par(mfrow=c(3,1))
plot(USA.Cases.DJI,
     USA.PercChange.DJI,
     xlab =  "Covid Cases", ylab = "Change DJI",
     main = "USA - DJI correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor, las = 1)
#putting a line of best fit. linear model
abline(lm(USA.PercChange.DJI ~ USA.Cases.DJI), col = lineColor, lwd = lineSize)
#putting text with correlation in the graph
text(paste("Correlation:", round(cor(USA.Cases.DJI,
                          USA.PercChange.DJI), 2)), x = 200000, y = 4, cex = 1)
plot(USA.Cases.Nasdaq,
     USA.PercChange.Nasdaq,
     xlab =  "Covid Cases", ylab = "Change NASDAQ",
     main = "USA - NASDAQ correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor,  las = 1)
abline(lm(USA.PercChange.Nasdaq ~
            USA.Cases.Nasdaq), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(USA.Cases.Nasdaq,
                                     USA.PercChange.Nasdaq),
                                 2)), x = 200000, y = 4, cex = 1)
plot(USA.Cases.SP500,
     USA.PercChange.SP500,
     xlab =  "Covid Cases", ylab = "Change SP500",
     main = "USA - SP500 correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor, las = 1)
abline(lm(USA.PercChange.SP500 ~
            USA.Cases.SP500), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(USA.Cases.SP500,
                                     USA.PercChange.SP500),
                                 2)), x = 200000, y = 4, cex = 1)
dev.off()

#===========Plotting percent change for countries===============================
# Using percent change columns for several countries
#For the US we are using DJI data to compare with other countries
pdf(paste(p.fig, "PercChangeCountries.pdf", sep = ""))
par(mfrow=c(3,1))
plot(USA.Cases.DJI, USA.PercChange.DJI, xlab =  "Covid Cases",
     ylab = "Percentage Change DJI",
     main = "USA - DJI correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor, las = 1 )
abline(lm(USA.PercChange.DJI ~
            USA.Cases.DJI), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(USA.Cases.DJI, USA.PercChange.DJI), 3)), 
     x = 200000, y = -3, cex = textSize)

plot(Can.Cases, Can.PercChange, xlab = "Covid Cases",
     ylab = "Percentage Change TSX",
     main = "Can - TSX correlation", pch = dotsForm,
     cex = dotsSize, col = dotsColor, las = 1)
abline(lm(Can.PercChange ~
            Can.Cases), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(Can.Cases,
                                     Can.PercChange), 3)),x = 6000, y = -3,
                                      cex = textSize)
plot(Ger.Cases, Ger.PercChange, xlab =  "Covid Cases",
     ylab = "Percentage Change DAX",
     main = "Ger - DAX correlation",
     pch = dotsForm, cex = dotsSize, col = dotsColor, las = 1 )
abline(lm(Ger.PercChange ~
            Ger.Cases), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(Ger.Cases, Ger.PercChange), 3)),
     x = 60000, y = -3, cex = textSize)
dev.off()

#===========Plotting difference for the US======================================
#Similar to percent change, but uses difference data
pdf(paste(p.fig, "UScorrelationDIFF.pdf", sep = ""))
par(mfrow=c(3,1))
plot(USA.Cases.DJI,
     USA.PercDiff.DJI,
     xlab =  "Covid Cases", ylab = "Percentage Difference DJI",
     main = "USA - DJI correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor, las = 1)
abline(lm(USA.PercDiff.DJI ~ USA.Cases.DJI), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(USA.Cases.DJI,
                                     USA.PercDiff.DJI), 2)), x = 200000,
                                      y = 4, cex = 1)
plot(USA.Cases.Nasdaq,
     USA.PercDiff.Nasdaq,
     xlab =  "Covid Cases", ylab = "Percentage Difference NASDAQ",
     main = "USA - NASDAQ correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor,  las = 1)
abline(lm(USA.PercDiff.Nasdaq ~
            USA.Cases.Nasdaq), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(USA.Cases.Nasdaq,
                                     USA.PercDiff.Nasdaq),
                                 2)), x = 200000, y = 4, cex = 1)
plot(USA.Cases.SP500,
     USA.PercDiff.SP500,
     xlab =  "Covid Cases", ylab = "Percentage Difference SP500",
     main = "USA - SP500 correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor, las = 1)
abline(lm(USA.PercDiff.SP500 ~
            USA.Cases.SP500), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(USA.Cases.SP500,
                                     USA.PercDiff.SP500),
                                 2)), x = 200000, y = 4, cex = 1)
dev.off()

#===========Plotting difference for countries===================================
#Plots difference to compare with other countries
pdf(paste(p.fig, "PercDifferenceCountries.pdf", sep = ""))
par(mfrow=c(3,1))
plot(USA.Cases.DJI, 
     USA.PercDiff.DJI,
     xlab =  "Covid Cases", ylab = "Percentage Difference DJI",
     main = "USA - DJI correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor, las = 1)
abline(lm(USA.PercDiff.DJI ~
            USA.Cases.DJI), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(USA.Cases.DJI,
                                     USA.PercDiff.DJI),
                                 2)), x = 200000, y = 6, cex = 1)

plot(Can.Cases, 
     Can.PercDiff,
     xlab =  "Covid Cases", ylab = "Percentage Difference TSX",
     main = "Canada - TSX correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor, las = 1)
abline(lm(Can.PercDiff ~ Can.Cases), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(Can.Cases,
                                     Can.PercDiff),
                                 2)), x = 6000, y = 6, cex = 1)

plot(Ger.Cases, 
     Ger.PercDiff,
     xlab =  "Covid Cases", ylab = "Percentage Difference DEX",
     main = "Germany - DEX correlation", pch = dotsForm, cex = dotsSize,
     col = dotsColor, las = 1)
abline(lm(Ger.PercDiff ~
            Ger.Cases), col = lineColor, lwd = lineSize)
text(paste("Correlation:", round(cor(Ger.Cases,
                                     Ger.PercDiff),
                                 2)), x = 50000, y = 6, cex = 1)
dev.off()






#===========RegressionModel=====================================================
# We tried working on the quadratic regression for this code.
# We realized that it doesn't work on this dataset, and linear model is the
# better choice.
# We decided to leave the code so that it is possible to see what we discarded

# Squared data
#USA.Cases.DJI2 <- USA.Cases.DJI^2
#Can.Cases2 <- Can.Cases^2
#Ger.Cases2 <- Ger.Cases^2

# Doing quadratic model and checking for summary. 
# We were interested in Rsquared
# USA.PercDiff.DJI.quadratic.model <- lm(USA.PercDiff.DJI 
# ~ USA.Cases.DJI + USA.Cases.DJI2)
#summary(USA.PercDiff.DJI.quadratic.model)
#linear <- lm(USA.PercDiff.DJI ~ USA.Cases.DJI)
#summary(linear)
# coef(USA.PercDiff.DJI.quadratic.model)
# Can.PercDiff.quadratic.model <- lm(Can.PercDiff ~ Can.Cases + Can.Cases2)
# summary(Can.PercDiff.quadratic.model)
# Ger.PercDiff.quadratic.model <- lm(Ger.PercDiff ~ Ger.Cases + Ger.Cases2)
# summary(Ger.PercDiff.quadratic.model)



# Quadratic model for perc Chance
# USA.PercChange.quadratic.model <- lm(USA.PercChange ~ USA.Cases.DJI 
# + USA.Cases.DJI2)
# summary(USA.PercChange.quadratic.model)
# USA.PercChange.linear.model <- lm(USA.PercChange ~ USA.Cases.DJI)
# summary(USA.PercChange.linear.model)


#===========PlottingRegressionModel==========================================

# Need a list of values to use in prediction line
# values <- seq(0, 3000000, 1)


# USA.predicted <- predict(USA.PercDiff.DJI.quadratic.model, 
# list(USA.Cases.DJI = values, USA.Cases.DJI2 = values^2))
# Can.predicted <- predict(Can.PercDiff.quadratic.model, 
#                         list(Can.Cases = values, Can.Cases2 = values^2))
# Ger.predicted <- predict(Ger.PercDiff.quadratic.model,
#                         list(Ger.Cases = values, Ger.Cases2 = values^2))
# 
# USA.change.predicted <- predict(USA.PercChange.quadratic.model, 
#                       list(USA.Cases.DJI = values, USA.Cases.DJI2 = values^2))

# Checking for coefficients
# coefficients(USA.PercDiff.DJI.quadratic.model)

# Plotting the models on 3 plots for different countries
# Note that the structure for plotting is similar
# The code for line is different

# #saving 3 plots in one pdf
# pdf(paste(p.fig, "QuadraticCountries.pdf", sep = ""))
# par(mfrow=c(3,1))
# plot(USA.Cases.DJI, USA.PercDiff.DJI, xlab =  "Covid Cases", 
# ylab = "Percentage Difference DJI",
#      main = "USA - DJI correlation", pch = dotsForm,
# cex = dotsSize, col = dotsColor, las = 1 )
# lines(values, USA.predicted, col = lineColor, lwd = lineSize)
# #text(paste('y =', round(coef(USA.PercDiff.DJI.quadratic.model)[[2]], 
# digits = 3), '* USA.Cases.DJI', '+',
# round(coef(USA.PercDiff.DJI.quadratic.model)[[1]], 
# digits = 3), x = 200000, y = 4, cex = 1)
# plot(Can.Cases, Can.PercDiff, xlab =  "Covid Cases",
# ylab = "Percentage Difference TSX",
#      main = "Canada - TSX correlation", pch = dotsForm,
# cex = dotsSize, col = dotsColor, las = 1 )
# lines(values, Can.predicted, col = lineColor, lwd = lineSize)
# plot(Ger.Cases, Ger.PercDiff, xlab =  "Covid Cases",
# ylab = "Percentage Difference DEX",
#      main = "Germany - DEX correlation",
# pch = dotsForm, cex = dotsSize, col = dotsColor, las = 1 )
# lines(values, Ger.predicted, col = lineColor, lwd = lineSize)
# 
# dev.off()






