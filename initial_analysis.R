library(gridExtra)
library(dplyr)

data.students <- read.table("data students.txt",header=TRUE,sep=";")
dim(data.students)

# remove rows g ∗ 11, g2 and 19 ∗ g + 21.
df_raw <- data.students[-c(66, 36, 135), ]
dim(df_raw)

# Check data summary prior to cleaning
summary(df_raw)

# Row-level NA diagnosis
rows_with_na   <- which(rowSums(is.na(df_raw)) > 0)
n_rows_with_na <- length(rows_with_na)
cat("Rows with at least one NA:", n_rows_with_na, "\n")

# Given that they are only 15 out of 2497, this is almost certainly random and can be omitted
df_clean <- na.omit(df_raw)
summary(df_clean)


