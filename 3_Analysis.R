# Doing analyses of for Miranda Chiong's REU project
# Steps have been largely copied from this doc: https://docs.google.com/document/d/1cBrNMVzRvO8HOBFZUrbWhPAdppvrLDYoRz0gSt-2cVg/edit

# Workflow: 
# 1. Determine what series/data we plan on using
#  - Crossdated (has rho value)
#  - Cut off for what is not an acceptable correlation value: p<0.05; 
#  - if multiple cores, use highest correlating
#  - Has pith (use LWCA spreadsheet)
# 2. Identify/Compute response variable
#  - **Opt A:** Raw Ring Width (have easy; no additonal calcs needed) **USING THIS ONE TO START**
#  - Opt B: Basal Area Increment (better metric of 2-dimenisional growht, but would have to calculate; likely more worthwhile if looking at more years)
#  - 2.1. re-align to be based off of age
#  - 2.2. calculate the mean for the establishment window: starting with 10 years for now (could try other windows)
# 3. Graph Establishment Conditions
#  - Plan A: Originally wanted to compare trees with known establishment vs. unknown, but we don't have good enough data
#  - **Plan B** [what we're going with]: Describing establishment Conditions
#  - 3.1. Describing the variability in year of establishment --> are they from the same year or are there groups before/after certain events like the Arb's founding or when the region was settled?
#.     -- CR Note: This is the piece that will really be of most interest to folks at the Arboretum
#  - 3.2. Describe the variability in growth rates during establishment --> is it a pretty tight/normal distribution or super variable?
#  -- [[Added from CR: can look at the *trend* in initial growth and see if it's declining (sign of geometric decline; typical of open grown) or increasing or stead]]

# # # # # # # 
# Set file paths, etc ----
# # # # # # # 
library(googlesheets4) # we need this to read google drive spreadsheets
library(ggplot2) # What Christy uses for making snazzy graphs


# Path to our data
# path.REU <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/"
path.dat <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/Data/Raw Ring Widths/organized/"
# Path to spit stuff out 
path.out <- "~/Google Drive/My Drive/URF REU 2024 - Chiong - Oaks/Data/Analysis-Output"

if(!dir.exists(path.out)) dir.create(path.out)

dir(path.dat) # Seeing what info we have available to work with

# Reading in all the stats about what tree things are form etc
series.metadata <- read.csv(file.path(path.dat, "Series-Metadata_all.csv"))
dim(series.metadata)# Checking dimensions to get a feel for how complex a set we're working with
summary(series.metadata) # Checking to get a feel for the data
head(series.metadata) # lots of stuff is characters, so spitting out the first few rows to check on what we have

# Read in the Xdate Stats file --> this is a version where we added in rho & p-vals
statsXdate <- read.csv(file.path(path.dat, "Series-Metadata-XdateStats_all.csv")) 
dim(statsXdate) # Checking dimensions to get a feel for how complex a set we're working with
summary(statsXdate) # Checking to get a feel for the data
head(statsXdate) # lots of stuff is characters, so spitting out the first few rows to check on what we have

# Read in the ring width series
seriesRW <- read.csv(file.path(path.dat, "Series-Measurements_all.csv"), row.names=1) # Years are the row names
dim(seriesRW) # Checking the dimensions
summary(seriesRW[,1:15]) # Doing a summary of just the first couple cols because there's a LOT of them

# Also pulling in our LWCA master google sheet since it has info we care about
sheetKey= "1iVB5--m29mLKhXUNgmc92wijQDg98L6FoKvsrJBDkRg" # This is the code that goes with the file called "LivingCollectionsArchive_Sample_Database
lcwaMaster <- googlesheets4::read_sheet(ss=sheetKey, sheet="LCWA Inventory")
summary(lcwaMaster)
lcwaMaster <- data.frame(lcwaMaster) # I hate working in tidyverse mode, so I put it in a data frame
summary(lcwaMaster)


# # # # # # # 


# # # # # # # 
# 1. Determine what series/data we plan on using ----
#  - Crossdated (has rho value)
#  - Cut off for what is not an acceptable correlation value: p<0.05; 
#  - if multiple cores, use highest correlating
#  - Has pith (use LWCA spreadsheet)
# # # # # # # 
summary(statsXdate) # double checking the info we have to work with
head(series.metadata)

# Setting up a dataframe to condense things from series to looking at stuff by the tree because we want one series per tree
dfTree <- data.frame(treeID = unique(series.metadata$treeID), site=NA, taxon=NA, year.first=NA, year.last=NA, pith.present=NA, file.use = NA,  xDate.rho = NA, xDate.pval = NA)

# Using a loop to go through each individual tree and find the best core
# Finding a core with multiple series for testing
for(i in 1:nrow(dfTree)){
  # i=which(dfTree$treeID=="UNK-051") # If need a test example; this one has multiple series, but none meet our criteria
  # i = which(dfTree$treeID=="UNK-217") # If need a test example; This is a good example for where only 1 is good
  # series.metadata[series.metadata$file=="QUERCS-519-544-11-3-1.xml",]
  # i = which(dfTree$treeID=="519-54*4")
  
  # Finding files that belong to this tree in our series metadata table
  rowMetaDat <- which(series.metadata$treeID==dfTree$treeID[i])
  # series.metadata[rowMetaDat,]
  
  # If for some reason we don't have any trees, lets just move to the next one 
  # (this would be VERY weird but we can deal with it later)
  if(length(rowMetaDat)==0) next 
  
  # - - - - - - 
  # If we have more than one series, we need to find the best one
  # (This is annoyingly complicated because of weird code quirks)
  # - - - - - - 
  fNow <- series.metadata$file[rowMetaDat] # These are our file names 
  fNow <- gsub("-", ".", fNow) # We can't have hyphens in column names, so we have to replace them with dots to make the names match
  
  xDateNOW <- statsXdate[statsXdate$series %in% fNow,]
  
  # If we don't have any series that crossdate (not NA & p < 0.05), skip to the next one
  rowLOOK <- which(xDateNOW$p.val<0.05) # Which rows we should look at
  if(length(rowLOOK)==0) next
  
  if(length(rowLOOK)>1){
    rowBest <- which(xDateNOW$rho[rowLOOK]==max(xDateNOW$rho[rowLOOK]))
    rowLOOK <- rowLOOK[rowBest] # Overwriting row Look with just the single best one
  }
  # - - - - - - 
  
  # rowLook should just be 1 item now, so lets subset our xDate & metadata frames to just that one so we can be consistent
  xDateNOW <- xDateNOW[rowLOOK,]
  metaNOW <- series.metadata[rowMetaDat[rowLOOK],]

  dfTree[i,c("site", "taxon", "year.first", "year.last")] <- metaNOW[,c("site", "taxon", "year.first", "year.last")]
  dfTree[i,c("file.use", "xDate.rho", "xDate.pval")] <- xDateNOW[,c("series", "rho", "p.val")]
  
  # Now seeing if there is pith on the sample
  rowMaster <- which(lcwaMaster$SpecimenID==dfTree$treeID[i])
  if(length(rowMaster)==0) next
  # lcwaMaster[rowMaster,]
  # If we have more than 1 sample for this tree, figure out what sample we're working with
  if(length(rowMaster)>1) {
    ind <- as.numeric(substr(metaNOW$core,1,1))
    rowMaster <- rowMaster[ind]
  }
  if(is.na(lcwaMaster$Pith.Present[rowMaster])) next
  dfTree[i, "pith.present"] <- ifelse(lcwaMaster$Pith.Present[rowMaster]=="Y", T, F)
  # dfTree[i,]

}

summary(dfTree)
# unique(dfTree$pith.present)

hist(dfTree$year.first) # All first years
hist(dfTree$year.first[dfTree$pith.present]) # only those with pith

# Writing out the status of trees/series we have to work with
write.csv(dfTree, file.path(path.out, "TreeData_toAnalyze.csv"), row.names=F)
# # # # # # # 

# # # # # # # 
# 2. Identify/Compute response variable ----
#  - **Opt A:** Raw Ring Width (have easy; no additonal calcs needed) **USING THIS ONE TO START**
#  - Opt B: Basal Area Increment (better metric of 2-dimenisional growht, but would have to calculate; likely more worthwhile if looking at more years)
#  - 2.1. re-align to be based off of age
#  - 2.2. calculate the mean for the establishment window: starting with 10 years for now (could try other windows)
# # # # # # # 

# # # # # # # 



# # # # # # # 
# 3. Graph Establishment Conditions ----
#  - Plan A: Originally wanted to compare trees with known establishment vs. unknown, but we don't have good enough data
#  - **Plan B** [what we're going with]: Describing establishment Conditions
#  - 3.1. Describing the variability in year of establishment --> are they from the same year or are there groups before/after certain events like the Arb's founding or when the region was settled?
#.     -- CR Note: This is the piece that will really be of most interest to folks at the Arboretum
#  - 3.2. Describe the variability in growth rates during establishment --> is it a pretty tight/normal distribution or super variable?
#  -- [[Added from CR: can look at the *trend* in initial growth and see if it's declining (sign of geometric decline; typical of open grown) or increasing or stead]]
# # # # # # # 

# # # # # # # 

