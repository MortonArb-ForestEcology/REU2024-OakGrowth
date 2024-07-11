library(dplR)
#Start with one
test <- "~/Google Drive/My Drive/eas"

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

##attmepting to fix 
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
