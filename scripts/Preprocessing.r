# Load necessary library
library(dplyr)

# Importing data
intel_cpu <- read.csv("~/Downloads/Intel_CPUs.csv")

# Specify the desired columns
desired_cols <- c("Processor_Base_Frequency", "nb_of_Cores", "nb_of_Threads", "TDP", "Lithography")

# Subset the data to only include the desired columns
intel_cpu_subset <- intel_cpu %>% select(one_of(desired_cols))

# Remove rows with base frequency measured in MHz
intel_cpu_subset <- intel_cpu_subset %>%
    filter(!grepl("MHz", Processor_Base_Frequency))

# Convert necessary columns to appropriate types if they are not
# Removing 'GHz' and converting Processor_Base_Frequency to numeric
intel_cpu_subset$Processor_Base_Frequency <- as.numeric(gsub(" GHz", "", intel_cpu_subset$Processor_Base_Frequency))

# Additional conversion to ensure nb_of_Cores, nb_of_Threads, TDP, and Lithography are numeric where applicable
intel_cpu_subset$nb_of_Cores <- as.numeric(intel_cpu_subset$nb_of_Cores)
intel_cpu_subset$nb_of_Threads <- as.numeric(intel_cpu_subset$nb_of_Threads)
intel_cpu_subset$TDP <- as.numeric(gsub(" W", "", intel_cpu_subset$TDP))
intel_cpu_subset$Lithography <- as.numeric(gsub(" nm", "", intel_cpu_subset$Lithography))

# Remove rows with missing values in the specific columns
cols_to_check <- c("Processor_Base_Frequency", "nb_of_Cores", "nb_of_Threads", "TDP", "Lithography")
intel_cpu_subset <- intel_cpu_subset[complete.cases(intel_cpu_subset[, cols_to_check]), ]

# Display the resulting subset
print(intel_cpu_subset)
