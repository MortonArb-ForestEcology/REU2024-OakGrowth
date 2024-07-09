library(dplR)
#Start with one
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

print(rwl.obj)  # Print the first RWL object, adjust index as needed
#Check
View(rwl.obj)


#### this only gives me the first 16 xml files..
### attempt to fix this
# List all TRIDAS XML files in your working directory
tridas_files <- list.files(pattern = ".xml$", full.names = TRUE)
# Check if this lists all XML files
print(tridas_files)

# Create an empty list for storage
rwl_objects <- list()

# Loop through each file
for (file in tridas_files) {
  cat("Processing file:", file, "\n")  # Print current file being processed
  
  tryCatch({
    # Read TRIDAS file with specified encoding
    tridas_data <- read.tridas(file,
                               ids.from.titles = TRUE,
                               ids.from.identifiers = TRUE,
                               combine.series = TRUE,
                               trim.whitespace = TRUE,
                               warn.units = TRUE)
    
    # Extract the measurements data frame
    measurements <- tridas_data$measurements
    
    # Convert measurements to RWL object
    rwl_object <- as.rwl(measurements)
    
    # Store the RWL object in the list
    rwl_objects[[file]] <- rwl_object
    
    # Print some information if needed
    cat("RWL object for", file, "created.\n")
    
  }, error = function(e) {
    message("Error reading file:", file, "\n", conditionMessage(e))
  })
}


# Print nams in rwl_objects
print(names(rwl_objects))

#Works but missing some of the zuzh-- also 6 files do to ecoding errors
# List all TRIDAS XML files in your working directory
trifi <- list.files(pattern = ".xml$", full.names = TRUE)
# Check if this lists all XML files - Should be 112 here at this point
print(trifi)  
# Create an empty list for storage
rwl.obj <- list()

# Function to clean non-UTF-8 characters from a file
clean_utf8 <- function(file) {
  raw_content <- readBin(file, "raw", file.info(file)$size)
  utf8_content <- iconv(rawToChar(raw_content), from = "latin1", to = "UTF-8", sub = "")
  temp_file <- tempfile(fileext = ".xml")
  writeLines(utf8_content, temp_file, useBytes = TRUE)
  return(temp_file)
}

# Loop through each file
for (file in trifi) {
  ({
    # Clean non-UTF-8 characters before reading
    cleaned_file <- clean_utf8(file)
    
    # Read TRIDAS files from the cleaned file
    tridat <- tryCatch({
      read.tridas(cleaned_file, ids.from.titles = TRUE, ids.from.identifiers = TRUE, 
                  combine.series = TRUE, trim.whitespace = TRUE, warn.units = TRUE)
    }, error = function(e) {
      message("Error reading file:", file, "\n", conditionMessage(e))
      NULL
    })
    
    if (!is.null(tridat)) {
      # Extract the measurements data frame
      trimeas <- tridat$measurements
      
      # Convert measurements to RWL object
      rwl.objs <- as.rwl(trimeas)
      
      # Store the RWL object in the list
      rwl.obj[[file]] <- rwl.objs
    }
  })
}

print(rwl.obj)  # Print the first RWL object, adjust index as needed
# Check
View(rwl.obj)
