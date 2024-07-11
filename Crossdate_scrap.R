library(dplR)
#Start with one
setwd("~/Google Drive/My Drive/2024_REU_crossdate/Quercus RW Tridas")
## early bug fixing minimized
Unk008<-read.tridas("UNKNWN-UNK-008-11-1-BR1.XML", ids.from.titles = TRUE,
ids.from.identifiers = TRUE, combine.series = TRUE,trim.whitespace = TRUE,
warn.units = TRUE)

# Extract the measurements data frame
Unk8msr <- Unk008$measurements

# Check
head(Unk8msr) ## For some reason the year doesn't have column title
## read as and rwl file
rwlunk008 <- as.rwl(Unk8msr)

print (rwlunk008)



#attenpting to read in information from all of the xml files
# List all TRIDAS XML files in your working directory
trifi <- list.files(pattern = ".xml$", full.names = TRUE)
# Check if this lists all XML files- Should be 112 here at this point
print(trifi)  
#create and empty list for storage
rwl.obj <- list()
# Loop through each file
for (file in trifi) {
({
# Read TRIDAS files in from the WD location.. 
#Brendon improve pathing you dolt
tridat <- read.tridas(file,ids.from.titles = TRUE, ids.from.identifiers = TRUE, 
combine.series = TRUE,trim.whitespace = TRUE, warn.units = TRUE)
    
# Extract the measurements data frame
trimeas <- tridat$measurements
# Convert measurements to RWL object
rwl.objs <- as.rwl(trimeas)
# Store the RWL object in the list
rwl.obj[[file]] <- rwl.objs
 })}

#print the names of the rwl.obj to see if there are 112
print(names(rwl.obj)) 

##there are only 16 attmepting to fix 
# List all TRIDAS XML files in your working directory
tridas.files <- list.files(pattern = ".xml$", full.names = TRUE)
# Check if this lists all XML files - Should be 112 here at this point
print(tridas.files)
# Create an empty list for storage
rwl.objects <- list()

# Loop through each file
for (file in tridas.files) {
  tryCatch({
# Read TRIDAS file
tridas.data <- read.tridas(file, ids.from.titles = TRUE,ids.from.identifiers = TRUE,
combine.series = TRUE,trim.whitespace = TRUE, warn.units = TRUE)
    
 # Extract the measurements data frame this is what are the ring widths
 measurements <- tridas.data$measurements
# Convert measurements to RWL object
rwl.object <- as.rwl(measurements)
# Store the RWL object in the list
rwl.objects[[file]] <- rwl.object
#finish the error catch
    cat("RWL object for", file, "created.\n")
  }, error = function(e) {
    message("Error reading file:", file, "\n", conditionMessage(e))
  })
}
# Print names in rwl.objects
print(names(rwl.objects))


## there are 106 here, I need to fix the encoding error


tridas.files <- list.files(pattern = ".xml$", full.names = TRUE)
# Check if this lists all XML files - Should be 112 here at this point
print(tridas.files)
# Create an empty list for storage
rwl.objects <- list()

# Function to clean non-UTF-8 characters from a file
clean.utf8 <- function(file) {
  raw.content <- readBin(file, "raw", file.info(file)$size)
  utf8.content <- iconv(rawToChar(raw.content), from = "latin1", to = "UTF-8", sub = "")
  temp.file <- tempfile(fileext = ".xml")
  writeLines(utf8.content, temp.file, useBytes = TRUE)
  return(temp.file)
}

# Loop through each file like before but this time use the cleaning function to
# clean non UTF-08 characters
for (file in tridas.files) {
  # Clean non-UTF-8 characters before reading
  cleaned.file <- clean.utf8(file)
  #Now read inTRIDAS files from the cleaned file
  tridas.data <- tryCatch({
    read.tridas(cleaned.file, ids.from.titles = TRUE, ids.from.identifiers = TRUE, 
                combine.series = TRUE, trim.whitespace = TRUE, warn.units = TRUE)
  }, error = function(e) {
    message("Error reading file:", file, "\n", conditionMessage(e))
    NULL
  })
  #if there is no error reading the file then write the meaurments and the new rwl object like before
  if (!is.null(tridas.data)) {
    measurements <- tridas.data$measurements
    rwl.objs <- as.rwl(measurements)
    rwl.objects[[file]] <- rwl.objs
  }
}

# Print nams in rwl_objects to check if there are 112
print(names(rwl.objects))
#extra check
View(rwl.objects)
####
####
process.tridas.file <- function(file) {
  tridas.data <- read.tridas(file, ids.from.titles = TRUE, ids.from.identifiers = TRUE, 
                             combine.series = TRUE, trim.whitespace = TRUE, warn.units = TRUE)
  measurements <- tridas.data$measurements
  rwl.object <- as.rwl(measurements)
  return(rwl.object)
}

# Function to clean non-UTF-8 characters from a file
clean.utf8 <- function(file) {
  raw.content <- readBin(file, "raw", file.info(file)$size)
  utf8.content <- iconv(rawToChar(raw.content), from = "latin1", to = "UTF-8", sub = "")
  temp.file <- tempfile(fileext = ".xml")
  writeLines(utf8.content, temp.file, useBytes = TRUE)
  return(temp.file)
}

# List all TRIDAS XML files in your working directory
tridas.files <- list.files(pattern = ".xml$", full.names = TRUE)
print(tridas.files)  # Should list all XML files, expecting 112

# Create an empty list for storing RWL objects
rwl.objects <- list()

# Loop through each file, clean it, and process it
for (file in tridas.files) {
  cleaned.file <- clean.utf8(file)
  
  tridas.data <- tryCatch({
    process.tridas.file(cleaned.file)
  }, error = function(e) {
    message("Error reading file:", file, "\n", conditionMessage(e))
    NULL
  })
  
  if (!is.null(tridas.data)) {
    rwl.objects[[file]] <- tridas.data
    cat("RWL object for", file, "created.\n")
  }
}

# Print names in rwl_objects to check if there are 112
print(names(rwl.objects))

# Extra check
#View(rwl.objects)

# Function to process TRIDAS file
process.tridas.file <- function(file) {
  tridas.data <- read.tridas(file, ids.from.titles = TRUE, ids.from.identifiers = TRUE, 
                             combine.series = TRUE, trim.whitespace = TRUE, warn.units = TRUE)
  measurements <- tridas.data$measurements
  rwl.object <- as.rwl(measurements)
  return(rwl.object)
}

# Function to clean non-UTF-8 characters from a file
clean.utf8 <- function(file) {
  raw.content <- readBin(file, "raw", file.info(file)$size)
  utf8.content <- iconv(rawToChar(raw.content), from = "latin1", to = "UTF-8", sub = "")
  temp.file <- tempfile(fileext = ".xml")
  writeLines(utf8.content, temp.file, useBytes = TRUE)
  return(temp.file)
}

# List all TRIDAS XML files in your working directory
tridas.files <- list.files(pattern = ".xml$", full.names = TRUE)
print(tridas.files)  # Should list all XML files, expecting 112

# Create an empty list for storing RWL objects
rwl.objects <- list()

# Loop through each file, clean it, and process it
for (file in tridas.files) {
  cleaned.file <- clean.utf8(file)
  
  tridas.data <- tryCatch({
    process.tridas.file(cleaned.file)
  }, error = function(e) {
    message("Error reading file:", file, "\n", conditionMessage(e))
    NULL
  })
  
  if (!is.null(tridas.data)) {
    rwl.objects[[file]] <- tridas.data
    cat("RWL object for", file, "created.\n")
  }
}

# Find the union of all years
all.years <- unique(unlist(lapply(rwl.objects, rownames)))
all.years <- sort(as.numeric(all.years))

# Initialize combined data frame with years
combined.rwl <- data.frame(year = all.years)

# Add measurements from each RWL object to the combined data frame
for (file in names(rwl.objects)) {
  measurements <- rwl.objects[[file]]
  file.name <- basename(file)
  
  # Create a data frame for the current file with NA for missing years
  df <- data.frame(year = as.numeric(rownames(measurements)), measurement = measurements[,1])
  
  # Merge with combined data frame
  combined.rwl <- merge(combined.rwl, df, by = "year", all.x = TRUE)
  
  # Rename the last column to the file name
  colnames(combined.rwl)[ncol(combined.rwl)] <- file.name
}

# Print combined RWL object names to check
print(colnames(combined.rwl))


rownames(combined.rwl) <- combined.rwl$year
combined.rwl <- combined.rwl[, -1]
print(colnames(combined.rwl))
View(combined.rwl)
#need to removed column 73 now due to data entry error
comb.rwl <- combined.rwl[, -73]

View(comb.rwl)
### It looks like this fixed that error, and now I need to figure out what the first row to contain a value other that NA is

# Find the index of the first row where any column does not equal "NA"
dcomb.rwl <- which.max(apply(comb.rwl != "NA", 1, any))

# Print the first few rows of combined.rwl to verify
print(comb.rwl[dcomb.rwl, ])

#remove all the rows previous to this
# Remove all rows above dcomb.rwl
comb.rwl <- comb.rwl[dcomb.rwl:nrow(comb.rwl), ]

# Print the first few rows of combined.rwl to verify
head(comb.rwl)

rows_with_zero <- apply(comb.rwl == 0, 1, any)

# Remove rows with 0
comb.rwl <- comb.rwl[!rows_with_zero, ]

# Print the updated number of rows
print("Number of rows after removing rows with 0:", nrow(combined.rwl), "\n")
list(cat)