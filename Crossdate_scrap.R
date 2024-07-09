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

### 
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

print(rwl.objs)  # Print the first RWL object, adjust index as needed
#Check
View(rwl.objs)

# List all TRIDAS XML files in your working directory
tridas_files <- list.files(pattern = ".xml$", full.names = TRUE)
print(tridas_files)

# Create an empty list for storage and a vector for failed files
rwl_objects <- list()
failed_files <- character()

# Function to clean non-UTF-8 characters from a file
clean_utf8 <- function(file) {
  raw_content <- readBin(file, what = "raw", n = file.info(file)$size)
  utf8_content <- iconv(rawToChar(raw_content), from = "latin1", to = "UTF-8", sub = "")
  temp_file <- tempfile(fileext = ".xml")
  writeLines(utf8_content, temp_file, useBytes = TRUE)
  return(temp_file)
}

# Function to read a TRIDAS file with error handling for encoding issues
read_tridas_with_encoding <- function(file) {
  tryCatch({
    # Attempt to read the TRIDAS file as is
    tridas_data <- read.tridas(file,
                               ids.from.titles = TRUE,
                               ids.from.identifiers = TRUE,
                               combine.series = TRUE,
                               trim.whitespace = TRUE,
                               warn.units = TRUE)
    return(tridas_data)
  }, error = function(e1) {
    message("Error reading file:", file, "\n", conditionMessage(e1))
    # Clean the file and try reading again
    cleaned_file <- clean_utf8(file)
    tridas_data <- read.tridas(cleaned_file,
                               ids.from.titles = TRUE,
                               ids.from.identifiers = TRUE,
                               combine.series = TRUE,
                               trim.whitespace = TRUE,
                               warn.units = TRUE)
    unlink(cleaned_file)  # Delete temporary file
    return(tridas_data)
  })
}

# Process each file
for (file in tridas_files) {
  cat("Processing file:", file, "\n")
  
  tridas_data <- tryCatch({
    read_tridas_with_encoding(file)
  }, error = function(e2) {
    message("Failed to read file after cleaning:", file, "\n", conditionMessage(e2))
    failed_files <- c(failed_files, file)
    return(NULL)
  })
  
  if (!is.null(tridas_data)) {
    # Extract the measurements data frame
    measurements <- tridas_data$measurements
    
    # Convert measurements to RWL object
    rwl_object <- as.rwl(measurements)
    
    # Store the RWL object in the list
    rwl_objects[[file]] <- rwl_object
    
    cat("RWL object for", file, "created.\n")
  }
}

# Summary
cat("Total number of RWL objects created:", length(rwl_objects), "\n")

# Example of accessing one of the RWL objects
print(rwl_objects[[1]])

# View RWL objects in a readable format
View(rwl_objects)



# List all TRIDAS XML files in your working directory
tridas_files <- list.files(pattern = ".xml$", full.names = TRUE)
print(tridas_files)

# Create an empty list for storage and a vector for failed files
rwl_objects <- list()
failed_files <- character()

# Function to clean non-UTF-8 characters from a file
clean_utf8 <- function(file) {
  raw_content <- readBin(file, what = "raw", n = file.info(file)$size)
  utf8_content <- iconv(rawToChar(raw_content), from = "latin1", to = "UTF-8", sub = "")
  temp_file <- tempfile(fileext = ".xml")
  writeLines(utf8_content, temp_file, useBytes = TRUE)
  return(temp_file)
}

# Function to read a TRIDAS file with error handling for encoding issues
read_tridas_with_encoding <- function(file) {
  tryCatch({
    # Attempt to read the TRIDAS file as is
    tridas_data <- read.tridas(file,
                               ids.from.titles = TRUE,
                               ids.from.identifiers = TRUE,
                               combine.series = TRUE,
                               trim.whitespace = TRUE,
                               warn.units = TRUE)
    return(tridas_data)
  }, error = function(e1) {
    message("Error reading file:", file, "\n", conditionMessage(e1))
    # Clean the file and try reading again
    cleaned_file <- clean_utf8(file)
    tridas_data <- read.tridas(cleaned_file,
                               ids.from.titles = TRUE,
                               ids.from.identifiers = TRUE,
                               combine.series = TRUE,
                               trim.whitespace = TRUE,
                               warn.units = TRUE)
    unlink(cleaned_file)  # Delete temporary file
    return(tridas_data)
  })
}

# Process each file
for (file in tridas_files) {
  cat("Processing file:", file, "\n")
  
  tridas_data <- tryCatch({
    read_tridas_with_encoding(file)
  }, error = function(e2) {
    message("Failed to read file after cleaning:", file, "\n", conditionMessage(e2))
    failed_files <- c(failed_files, file)
    return(NULL)
  })
  
  if (!is.null(tridas_data)) {
    # Extract the measurements data frame
    measurements <- tridas_data$measurements
    
    # Convert measurements to RWL object
    rwl_object <- as.rwl(measurements)
    
    # Store the RWL object in the list
    rwl_objects[[file]] <- rwl_object
    
    cat("RWL object for", file, "created.\n")
  }
}

# Summary
cat("Total number of RWL objects created:", length(rwl_objects), "\n")

# Example of accessing one of the RWL objects
print(rwl_objects[[1]])

# View RWL objects in a readable format
View(rwl_objects)
