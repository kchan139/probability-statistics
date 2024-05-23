data <- read.csv("data/Intel_CPUs.csv")

# Specify the desired columns
desired_cols <- c("Processor_Base_Frequency", "nb_of_Cores", "nb_of_Threads", "TDP", "Lithography")

library(dplyr)

# Subset the data to only include the desired columns
data <- data %>% select(one_of(desired_cols))

library(stringr)
for (i in seq(ncol(data))) {
    col <- data[i]
    max <- -Inf
    min <- Inf
    for (j in seq(nrow(col))) {
        cell <- data[j,i]
        unit <- ""
        if (typeof(cell) != "double") {
            if (typeof(cell) == "character") {
                cell <- str_split_1(cell, " ")
                value <- as.double(cell[1])
                unit <- cell[2]
                cell <- value
                result <- unit == "GHz"
                if (!is.na(result)) {
                    if (result) {
                        cell <- value * 1000
                        unit <- "MHz"
                    }
                }
            } else {
                cell <- as.double(cell)
            }
        }
        if (is.na(cell)) next
        if (cell > max) max <- cell
        if (cell < min) min <- cell
        data[j,i] <- paste(cell, unit, sep=" ")
    }
    median <- (max + min) / 2
    for (j in seq(nrow(col))) {
        cell <- data[j,i]
        if ((typeof(cell) == "character" && cell == "") || (typeof(cell) == "double" && is.na(cell))) {
            data[j,i] <- median
        }
    }
}

print(data)