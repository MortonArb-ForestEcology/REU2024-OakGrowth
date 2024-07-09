library(dplR)

Unk008<-read.tridas("UNKNWN-UNK-008-11-1-BR1.XML", ids.from.titles = TRUE,
            ids.from.identifiers = TRUE, combine.series = TRUE,
            trim.whitespace = TRUE, warn.units = TRUE)

# Extract the measurements data frame
measurements <- Unk008$measurements

# Inspect the data
head(measurements)
rwlunk008 <- as.rwl(measurements)

print (rwlunk008)

# List all TRIDAS XML files in your working directory
tridas_files <- list.files(pattern = ".xml$", full.names = TRUE)
print(tridas_files)
# Loop through each file
for (file in tridas_files) {
  # Read TRIDAS file
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
  
  # Print the RWL object (or perform other operations)
  print(rwl_object)
}
tridas_files <- list.files(pattern = ".xml$", full.names = TRUE)
print(tridas_files)  # Check if this lists all 111 files

# Loop through each file
for (file in tridas_files) {
  cat("Processing file:", file, "\n")  # Print current file being processed
  
  # Read TRIDAS file
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
  
  # Print the RWL object (or perform other operations)
  print(rwl_object)
}


# List all TRIDAS XML files in your working directory
tridas_files <- list.files(pattern = ".xml$", full.names = TRUE)
print(tridas_files)  # Check if this lists all XML files

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
    
    # Print the RWL object (or perform other operations)
    print(rwl_object)
  }, error = function(e) {
    message("Error reading file:", file, "\n", conditionMessage(e))
  })
}
