data <- read.table("Intel_CPUs.csv", header=TRUE, sep=",", quote = "\"'")
library(stringr)
for (i in seq(ncol(data))) {
	col <- data[i]
	max <- -Inf
	min <- Inf
	for (j in seq(nrow(col))) {
		cell <- data[j,i]
		if (typeof(cell) != "double") {
			if (typeof(cell) == "character") {
				cell <- str_split_1(cell, " ")
				value <- as.double(cell[1])
				unit <- cell[2]
				cell <- value
			} else {
				cell <- as.double(cell)
			}
		}
		if (is.na(cell)) next
		if (cell > max) max <- cell
		if (cell < min) min <- cell
		data[j,i] <- as.double(cell)
	}
	print(cat("col ", i, " max ", max, " min ", min))
	median <- (max + min) / 2
	for (j in seq(nrow(col))) {
		cell <- data[j,i]
		if ((typeof(cell) == "character" && cell == "") || (typeof(cell) == "double" && is.na(cell))) {
			data[j,i] <- median
		}
	}
}
write.csv(data, file = "Intel_CPUs_filtered.csv")
