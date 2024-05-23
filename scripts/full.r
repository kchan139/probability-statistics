# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggpubr)
library(corrplot)
library(car)
library(stringr)

# Read the data
data <- read.csv("Intel_CPUs.csv", header = TRUE, sep = ",")
head(data)

# Data visualization
p <- visdat::vis_dat(data)
ggsave("plots/data.png", plot = p, width = 20, height = 8, units = "in")
display_png(file="plots/data.png")


# Missing data proportion
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

# Identify Outliers Function
identify_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Apply the function to identify outliers for each relevant column
outliers <- sapply(data[, desired_cols], identify_outliers)

# Combine the outliers into a single logical vector indicating any row with an outlier
combined_outliers <- apply(outliers, 1, any)

# Print number of outliers in each column
print(colSums(outliers))

# Remove rows with outliers
data <- data[!combined_outliers, ]

# Verify the changes
summary(data)

# Distribution and Histograms
# Histogram for Processor Base Frequency
processor_hist <- ggplot(data, aes(x = Processor_Base_Frequency)) + 
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") + 
  labs(title = "Histogram of Processor Base Frequency", x = "Processor Base Frequency (GHz)", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for number of cores
cores_hist <- ggplot(data, aes(x = nb_of_Cores)) + 
  geom_histogram(binwidth = 0.3, fill = "skyblue", color = "black") + 
  labs(title = "Histogram of number of Cores", x = "number of Cores", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for number of threads
threads_hist <- ggplot(data, aes(x = nb_of_Threads)) + 
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") + 
  labs(title = "Histogram of number of Threads", x = "number of Threads", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for TDP
tdp_hist <- ggplot(data, aes(x = TDP)) + 
  geom_histogram(binwidth = 2, fill = "skyblue", color = "black") + 
  labs(title = "Histogram of TDP", x = "TDP (W)", y = "Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

# Histogram for Lithography
litho_hist <- ggplot(data, aes(x = Lithography)) + 
  geom_histogram(binwidth = 3, fill = "skyblue", color = "black") + 
  labs(title = "Histogram of Lithography", x = "Lithography (nm)", y = "Frequency") + 
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(processor_hist, cores_hist, threads_hist, tdp_hist, litho_hist,
          ncol = 2, nrow = 3)

# Boxplots for Outliers
# Boxplot for Processor Base Frequency
processor_boxplot <- ggplot(data, aes(y = Processor_Base_Frequency)) + 
  geom_boxplot(fill = "skyblue") + 
  labs(title = "Boxplot of Processor Base Frequency", y = "Processor Base Frequency (GHz)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for number of Cores
cores_boxplot <- ggplot(data, aes(y = nb_of_Cores)) + 
  geom_boxplot(fill = "skyblue") + 
  labs(title = "Boxplot of number of Cores", y = "number of Cores") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for number of Threads
threads_boxplot <- ggplot(data, aes(y = nb_of_Threads)) + 
  geom_boxplot(fill = "skyblue") + 
  labs(title = "Boxplot of number of Threads", y = "number of Threads") + 
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for TDP
tdp_boxplot <- ggplot(data, aes(y = TDP)) + 
  geom_boxplot(fill = "skyblue") + 
  labs(title = "Boxplot of TDP", y = "TDP (W)") + 
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for Lithography
litho_boxplot <- ggplot(data, aes(y = Lithography)) + 
  geom_boxplot(fill = "skyblue") + 
  labs(title = "Boxplot of Lithography", y = "Lithography (nm)") + 
  theme(plot.title = element_text(hjust = 0.5))

ggarrange(processor_boxplot, cores_boxplot, threads_boxplot, tdp_boxplot, litho_boxplot,
          ncol = 2, nrow = 3)

# scatter plot for relationship between Processor_Base_Frequency with others
processor_cores_plot <- ggplot(data, aes(x = nb_of_Cores, y = Processor_Base_Frequency)) + 
  geom_point(color = "skyblue") + 
  labs(x = "number of Cores", y = "Processor Base Frequency (GHZ)")

processor_threads_plot <- ggplot(data, aes(x = nb_of_Threads, y = Processor_Base_Frequency)) + 
  geom_point(color = "skyblue") + 
  labs(x = "number of Threads", y = "Processor Base Frequency (GHZ)")

processor_tdp_plot <- ggplot(data, aes(x = TDP, y = Processor_Base_Frequency)) + 
  geom_point(color = "skyblue") + 
  labs(x = "TDP (W)", y = "Processor Base Frequency (GHZ)")

processor_litho_plot <- ggplot(data, aes(x = Lithography, y = Processor_Base_Frequency)) + 
  geom_point(color = "skyblue") + 
  labs(x = "Lithography (nm)", y = "Processor Base Frequency (GHZ)")

ggarrange(processor_cores_plot, processor_threads_plot, processor_tdp_plot, processor_litho_plot,
          ncol = 2, nrow = 2)


# Correlation Matrix
# Select relevant numeric columns for correlation
numeric_columns <- data[, c("Processor_Base_Frequency", "nb_of_Cores", "nb_of_Threads", "TDP", "Lithography")]

# Calculate correlation matrix
cor_matrix <- cor(numeric_columns, use = "complete.obs")

# Plot the correlation matrix
corrplot(cor_matrix, method = "square", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "red", tl.cex = 0.8)

# check normality 
model_normality <- lm(Processor_Base_Frequency ~ nb_of_Cores * TDP, data = data)
summary(model_normality)
ggqqplot(residuals(model_normality), xlab = "Theoretical Quantities",
         ylab = "Sample Quantities")
# shapiro's test
shapiro.test(residuals(model_normality))

# check homogeneity
leveneTest(Processor_Base_Frequency ~ factor(TDP) * factor(Lithography), data = intel_cpu_subset)

# test anova
model_proc_cores_tdp <- aov(Processor_Base_Frequency ~ factor(nb_of_Cores) * factor(TDP), data = intel_cpu_subset)
summary(model_proc_cores_tdp)

# multiple linear regression
#data slicing
smp_size <- floor(0.70 * nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train_set <- data[train_ind, ]
test_set <- data[-train_ind, ]

#base model
model2 <- lm(Processor_Base_Frequency ~ nb_of_Cores + nb_of_Threads + TDP + Lithography, data = train_set)
summary(model2)

#improved model
model3 <- lm(Processor_Base_Frequency ~ nb_of_Cores + TDP + Lithography, data = train_set)
summary(model3)

#assumptions
par(mfrow=c(2,2))
plot(model2)
par(mfrow=c(1,1))

#train plot
predicted_values <- predict(model2)
data <- data.frame(Processor_Base_Frequency = train_set$Processor_Base_Frequency, Predicted = predicted_values)
ggplot(data, aes(x = train_set$Processor_Base_Frequency, y = Predicted)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(x = "Actual Processor_Base_Frequency", y = "Predicted Processor_Base_Frequency")

#test plot
predicted_values <- predict(model2, test_set)
test_set["Predicted"] <- predicted_values
ggplot(test_set, aes(x = Processor_Base_Frequency, y = Predicted)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE) +
  labs(x = "Actual Processor_Base_Frequency", y = "Predicted Processor_Base_Frequency")

