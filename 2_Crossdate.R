# Insert all of the necessary header stuff here!

library(dplR)

path.raw <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/Data/Raw Ring Widths/organized"
path.xdate <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/Data/Raw Ring Widths/crossdating"

if(!dir.exists(path.xdate)) dir.create(path.xdate, recursive = T)


series.metadata <- read.csv(file.path(path.raw, "Series-Metadata_all.csv"))
combined.rwl <- read.csv(file.path(path.raw, "Series-Measurements_all.csv"), row.names=1)

dim(series.metadata); dim(combined.rwl)

# We probably have incomplete first & last years, so lets go ahead and remove those for our own safety
for(i in 1:ncol(combined.rwl)){
  rowfirstMeas <- min(which(combined.rwl[,i]>0))
  rowlastMeas <- max(which(combined.rwl[,i]>0))
  
  combined.rwl[rowfirstMeas,i] <- NA
  combined.rwl[rowlastMeas,i] <- NA
}
rows.dat <- apply(combined.rwl, 1, FUN=function(x){!all(is.na(x))})
combined.rwl <- combined.rwl[rows.dat,]

# We'll move this to a second script on crossdating
#checking the class of combined.rwl to ensure it's a rwl file
class(combined.rwl)
#assigning it the rwl classification as well
class(combined.rwl) <- c("rwl", class(combined.rwl))
#check
class(combined.rwl)

sum.rwl <- summary(combined.rwl)
summary(sum.rwl)

plot(combined.rwl[,], plot.type="spag")
plot(combined.rwl[,1:50], plot.type="spag")
plot(combined.rwl[,51:108], plot.type="spag")

# very basic statistics
rwl.report(combined.rwl)

# plot(combined.rwl, plot.type="spag")


#saving this as a txt on google drive
#set the path
report <- file.path(path.xdate, "rwl_report-initial.txt")
# Open a connection too google drive
sink(report)
# Generate the report
rwl.report(combined.rwl)
# Close the connection to google drive
sink()

# Detrending
coreDetrend <- detrend(combined.rwl, method = "Spline")  # Detrending

# plot(coreDetrend[,1:10], plot.type="spag")

# Creating a chronology (Brendon) --> cool, but we'll circle back to this
crs <- chron(coreDetrend)  # Creating a chronology
# Print summary of the chronology
summary(crs)
# Plot the chronology
plot(crs, main = "Chronology")
plot(crs, main = "Chronology", add.spline = TRUE, nyrs=5)
summary(crs)


# Doing the raw correlations
# Correlating by segments
# Only working with trees >50 years old
serLong <- sum.rwl$series[sum.rwl$year>50]

rwlCorDefault <- corr.rwl.seg(combined.rwl[,serLong])
summary(rwlCorDefault)

corrTable <- data.frame(rwlCorDefault$overall) # Gives us the table with correlations (Spearman's rho)
summary(corrTable)

mean(corrTable$rho); sd(corrTable$rho)

series.good <- row.names(corrTable)[corrTable$rho>median(corrTable$rho)]

series.BAD <- row.names(corrTable)[corrTable$rho<0]
series.LO <- row.names(corrTable)[corrTable$rho<0.1]

corrDefault2 <- corr.rwl.seg(combined.rwl[,serLong], master=combined.rwl[,series.good])
corrTable2 <- data.frame(corrDefault2$overall) # Gives us the table with correlations (Spearman's rho)
corrTable2$series <- row.names(corrTable2)
summary(corrTable2)

# Update our rwl.sum with the correlation stats
sum.rwl <- merge(sum.rwl, corrTable2, all.x=T)
summary(sum.rwl)

sum.rwl[!is.na(sum.rwl$rho) & sum.rwl$rho==max(sum.rwl$rho, na.rm=T),] # Our best correlater
sum.rwl[!is.na(sum.rwl$rho) & sum.rwl$rho==min(sum.rwl$rho, na.rm=T),] # Our worst correlator
sum.rwl[sum.rwl$series %in% series.BAD, ] # Printing out the things that have negative corrs
sum.rwl[sum.rwl$series %in% series.LO, ] # Printing out the things that have low corrs

summary(sum.rwl[sum.rwl$series %in% series.good,]) # Comparing with the stats for things that worked well


# Lets start with the series that have negative correlation
segBest <- corr.series.seg(combined.rwl[,serLong], series=sum.rwl$series[!is.na(sum.rwl$rho) & sum.rwl$rho==max(sum.rwl$rho, na.rm=T)], seg.lenth=20)
segWorst <- corr.series.seg(combined.rwl[,serLong], series=sum.rwl$series[!is.na(sum.rwl$rho) & sum.rwl$rho==min(sum.rwl$rho, na.rm=T)], seg.lenth=20, bin.floor = 0)


# Now breaking things into the individual ones
sum.rwl[sum.rwl$series==series.BAD[1],] # Our worst correlator
ccfWorst1 <- ccf.series.rwl(combined.rwl[,serLong], series=series.BAD[1], seg.length=20, bin.floor=0)
# UNK156
# Checking the metadata of rhat sample
ccfWorst1 # Printing this gives you the actual quantitative effects on the rho value; un-comment this out to actually print it
summary(ccfWorst1$ccf)
# looking at the average shift across all segments --> this code takes the mean of all columns
apply(ccfWorst1$ccf, 1, mean)

# Note: This isn't necessarily pulling the 2nd worst -- just the 2nd in the seiresworst 
sum.rwl[sum.rwl$series == series.BAD[2],]
ccfWorst2 <- ccf.series.rwl(combined.rwl[,serLong], series=series.BAD[2], seg.length=50, bin.floor=0)
# UNK 157
ccfWorst2 # Printing this gives you the actual quantitative effects on the rho value; un-comment this out to actually print it
apply(ccfWorst2$ccf, 1, mean)

sum.rwl[sum.rwl$series == series.BAD[3],]
ccfWorst3 <- ccf.series.rwl(combined.rwl[,serLong], series=series.BAD[3], seg.length=50, bin.floor=0)
# UNK 217
ccfWorst3 # Printing this gives you the actual quantitative effects on the rho value; un-comment this out to actually print it
apply(ccfWorst3$ccf, 1, mean)

