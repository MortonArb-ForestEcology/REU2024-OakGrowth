library(dplR)

series.metadata <- read.csv(file.path(path.out, "Series-Metadata_all.csv"))
combined.rwl <- read.csv(file.path(path.out, "Series-Measurements_all.csv"))


# We'll move this to a second script on crossdating
#checking the class of comb1.rwl to ensure it's a rwl file
class(combined.rwl)
#assigning it the rwl classification as well
class(combined.rwl) <- c("rwl", class(combined.rwl))
#check
class(combined.rwl)
# very basic statistics
rwl.report(combined.rwl)

#saving this as a txt on google drive
#set the path
report <- "~/Library/CloudStorage/GoogleDrive-breidy@mortonarb.org/My Drive/rwl_report.txt"
# Open a connection too google drive
sink(report)
# Generate the report
rwl.report(comb1.rwl)
# Close the connection to google drive
sink()

coredat <- detrend(comb1.rwl, method = "Mean")  # Detrending
crs <- chron(coredat)  # Creating a chronology
# Print summary of the chronology
summary(crs)
# Plot the chronology
plot(crs, main = "Chronology")
plot(crs, main = "Chronology", add.spline = TRUE, nyrs=5)
summary(crs)

# Extract the chronology (std) component
chron.std <- crs[, "std"]
# Plot the chronology without sample depth
plot(rownames(crs), chron.std, type = "l", main = "Chronology", xlab = "Year", ylab = "Index", add.spline=TRUE, nyrs=5)

# Add a spline to the chronology plot
spline_fit <- smooth.spline(as.numeric(rownames(crs)), chron_std, spar = 0.5)
lines(spline_fit, col = "red")
