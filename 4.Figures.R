# 1.Main.R is required to run first to acces folder paths
# read cleaned csv files
# percent difference csv files
USA.SP500.diff.merged <- read.csv(paste(p.output, "USA.SP500.diff.merged.csv", sep = ""))
USA.NASDAQ.diff.merged <- read.csv(paste(p.output, "USA.NASDAQ.diff.merged.csv", sep = ""))
USA.DJI.diff.merged <- read.csv(paste(p.output, "USA.DJI.diff.merged.csv",sep = ""))
CAN.TSX.diff.merged <- read.csv(paste(p.output,"CAN.TSX.diff.merged.csv", sep = ""))
DE.DAX.diff.merged <- read.csv(paste(p.output, "DE.DAX.diff.merged.csv", sep = ""))

# percent change csv files
USA.SP500.change.merged <- read.csv(paste(p.output, "USA.SP500.change.merged.csv", sep = ""))
USA.NASDAQ.change.merged <- read.csv(paste(p.output, "USA.NASDAQ.change.merged.csv", sep = ""))
USA.DJI.change.merged <- read.csv(paste(p.output, "USA.DJI.change.merged.csv",sep = ""))
CAN.TSX.change.merged <- read.csv(paste(p.output,"CAN.TSX.change.merged.csv", sep = ""))
DE.DAX.change.merged <- read.csv(paste(p.output, "DE.DAX.change.merged.csv", sep = ""))


#===========CORRELATION (US/Perc Diff)==========================================
#US GRAPH for 3 indices: plotting three graphs !!!percentage difference
pdf(paste(p.fig, "UScorrelationDIFF.pdf", sep = ""))
par(mfrow=c(3,1))
plot(USA.DJI.diff.merged$`Daily cases`, 
     USA.DJI.diff.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Percentage Difference DJI",
     main = "USA - DJI correlation", pch = 16, cex = 0.7,
     col = "grey", las = 1)
abline(lm(USA.DJI.diff.merged$`Percentage Difference` ~
            USA.DJI.diff.merged$`Daily cases`), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(USA.DJI.diff.merged$`Daily cases`,
                                     USA.DJI.diff.merged$`Percentage Difference`
                                     ), 2)), x = 200000, y = 4, cex = 1)
plot(USA.NASDAQ.diff.merged$`Daily cases`, 
     USA.NASDAQ.diff.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Percentage Difference NASDAQ",
     main = "USA - NASDAQ correlation", pch = 16, cex = 0.7,
     col = "grey", las = 1)
abline(lm(USA.NASDAQ.diff.merged$`Percentage Difference` ~
            USA.NASDAQ.diff.merged$`Daily cases`), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(USA.NASDAQ.diff.merged$`Daily cases`,
                                USA.NASDAQ.diff.merged$`Percentage Difference`),
                                2)), x = 200000, y = 4, cex = 1)
plot(USA.SP500.diff.merged$`Daily cases`, 
     USA.SP500.diff.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Percentage Difference SP500",
     main = "USA - SP500 correlation", pch = 16, cex = 0.7,
     col = "grey", las = 1)
abline(lm(USA.SP500.diff.merged$`Percentage Difference` ~
            USA.SP500.diff.merged$`Daily cases`), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(USA.SP500.diff.merged$`Daily cases`,
                              USA.SP500.diff.merged$`Percentage Difference`),
                                 2)), x = 200000, y = 4, cex = 1)
dev.off()

#===========CORRELATION (US/Close Price)========================================

#US GRAPH for 3 indices: plotting three graphs !!!Close price
pdf(paste(p.fig, "UScorrelationCLOSE.pdf", sep = ""))
par(mfrow=c(3,1))
plot(USA.DJI.close.merged$`Daily cases`, 
     USA.DJI.close.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Close Price DJI",
     main = "USA - DJI correlation", pch = 16, cex = 0.7, 
     col = "grey", las = 1)
plot(USA.NASDAQ.close.merged$`Daily cases`, 
     USA.NASDAQ.close.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Close Price NASDAQ",
     main = "USA - NASDAQ correlation", pch = 16, cex = 0.7,
     col = "grey", las = 1)
plot(USA.SP500.close.merged$`Daily cases`, 
     USA.SP500.close.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Close Price SP500",
     main = "USA - SP500 correlation", pch = 16, cex = 0.7,
     col = "grey", las = 1)
dev.off()


#===========CORRELATION (Countries/Perc Diff)===================================
pdf(paste(p.fig, "CorrelationCountries.pdf", sep = ""))
par(mfrow=c(3,1))
plot(CAN.TSX.diff.merged$`Daily cases`, 
     CAN.TSX.diff.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Percentage Difference TSX",
     main = "Canada - TSX correlation", pch = 16, cex = 0.7,
     col = "grey", las = 1)
abline(lm(CAN.TSX.diff.merged$`Percentage Difference` ~
            CAN.TSX.diff.merged$`Daily cases`), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(CAN.TSX.diff.merged$`Daily cases`,
                                     CAN.TSX.diff.merged$`Percentage Difference`
                                     ), 2)), x = 2000, y = 30, cex = 1)
plot(USA.DJI.diff.merged$`Daily cases`, 
     USA.DJI.diff.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Percentage Difference DJI",
     main = "USA - DJI correlation", pch = 16, cex = 0.7,
     col = "grey", las = 1)
abline(lm(USA.DJI.diff.merged$`Percentage Difference` ~
            USA.DJI.diff.merged$`Daily cases`), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(USA.DJI.diff.merged$`Daily cases`,
                                  USA.DJI.diff.merged$`Percentage Difference`),
                                 2)), x = 2000, y = 30, cex = 1)
plot(DE.DAX.diff.merged$`Daily cases`, 
     DE.DAX.diff.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Percentage Difference DEX",
     main = "Germany - DEX correlation", pch = 16, cex = 0.7,
     col = "grey", las = 1)
abline(lm(DE.DAX.diff.merged$`Percentage Difference` ~
            DE.DAX.diff.merged$`Daily cases`), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(DE.DAX.diff.merged$`Daily cases`,
                                     DE.DAX.diff.merged$`Percentage Difference`),
                                 2)), x = 2000, y = 30, cex = 1)
dev.off()

#===========CORRELATION (Countries/Close Price)=================================

#US GRAPH for 3 indices: plotting three graphs !!!Close price
pdf(paste(p.fig, "CountriesClose.pdf", sep = ""))
par(mfrow=c(3,1))
plot(USA.DJI.close.merged$`Daily cases`, 
     USA.DJI.close.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Close Price DJI",
     main = "USA - DJI", pch = 16, cex = 0.7, 
     col = "grey", las = 1)
plot(CAN.TSX.close.merged$`Daily cases`, 
     CAN.TSX.close.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Close Price NASDAQ",
     main = "Canada - TSX", pch = 16, cex = 0.7,
     col = "grey", las = 1)
plot(DE.DAX.close.merged$`Daily cases`, 
     DE.DAX.close.merged$`Percentage Difference`,
     xlab =  "Covid Cases", ylab = "Close Price SP500",
     main = "Germany - DAX", pch = 16, cex = 0.7,
     col = "grey", las = 1)
dev.off()


