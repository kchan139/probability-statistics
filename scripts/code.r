# Load necessary libraries
library(dplyr)
library(stringr)

# Read the data
data <- read.csv("Intel_CPUs.csv", header = TRUE, sep = ",")
head(data)

p <- visdat::vis_dat(data)
ggsave("plots/data.png", plot = p, width = 20, height = 8, units = "in")
display_png(file="plots/data.png")

missing_data <- sapply(data, function(x) sum(is.na(x)) / length(x))
missing_data_data <- data.frame(Feature = names(missing_data), Missing_Ratio = missing_data)
p <- ggplot(missing_data_data, aes(x = Feature, y = Missing_Ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Missing Values Per Attribute",
       x = "Feature",
       y = "Missing Ratio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(filename = "plots/missing_vals.png", plot = p, width = 20, height = 6, units = "in")
display_png(file="plots/missing_vals.png")

# Specify the desired columns
desired_cols <- c("Processor_Base_Frequency", "nb_of_Cores", "nb_of_Threads", "TDP", "Lithography")

# Subset the data to only include the desired columns
data <- data %>% select(one_of(desired_cols))

# Function to convert GHz to MHz if needed
convert_units <- function(x) {
  if (str_detect(x, "GHz")) {
    value <- as.numeric(str_extract(x, "[0-9.]+")) * 1000
    return(value)
  }
  return(as.numeric(str_extract(x, "[0-9.]+")))
}

# Apply conversion and impute missing values with median
data <- data %>%
  mutate(across(everything(), ~ {
    # Convert units if necessary and replace with numeric values
    . <- sapply(., function(cell) ifelse(is.na(cell) | cell == "", NA, convert_units(cell)))
    # Impute missing values with median
    . <- ifelse(is.na(.), median(., na.rm = TRUE), .)
    return(.)
  }))

print(data)
