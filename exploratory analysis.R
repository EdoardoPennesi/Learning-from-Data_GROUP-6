############################################################
# 1. Load libraries
############################################################

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(naniar)
library(janitor)
library(corrplot)
library(DataExplorer)

############################################################
# 2. Import dataset
############################################################

data_students <- read.csv(
  "data_students.txt",
  sep = ";",
  header = TRUE,
  stringsAsFactors = FALSE
)

# Clean column names
data_students <- clean_names(data_students)

############################################################
# 3. Convert variable types
############################################################

data_students <- data_students %>%
  mutate(
    epc_label = as.factor(epc_label),
    building_era = as.factor(building_era),
    roof_color = as.factor(roof_color),
    pet_ownership = as.factor(pet_ownership)
  )

############################################################
# 4. Inspect structure
############################################################

str(data_students)
glimpse(data_students)

############################################################
# 5. Check missing values
############################################################

# Count missing values per variable
colSums(is.na(data_students))

# Visualize missing data
vis_miss(data_students)

# Missingness summary
miss_var_summary(data_students)

############################################################
# 6. Summary statistics
############################################################

summary(data_students)

# Detailed statistics for numeric variables
data_students %>%
  select(where(is.numeric)) %>%
  summary()

############################################################
# 7. Identify numeric vs categorical variables
############################################################

numeric_vars <- data_students %>%
  select(where(is.numeric))

categorical_vars <- data_students %>%
  select(where(is.factor))

############################################################
# 8. Univariate distributions (numeric variables)
############################################################

# Histograms
numeric_vars %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of Numeric Variables")

# Boxplots
numeric_vars %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = "", y = value)) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~name, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots of Numeric Variables")

############################################################
# 9. Categorical variable exploration
############################################################

for (var in names(categorical_vars)) {
  print(
    ggplot(data_students, aes_string(x = var)) +
      geom_bar(fill = "darkorange") +
      theme_minimal() +
      labs(title = paste("Distribution of", var))
  )
}


############################################################
# 10. Correlation analysis
############################################################

cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Print matrix
print(cor_matrix)

# Correlation heatmap
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.srt = 45
)

############################################################
# 12. Pairwise scatterplots
############################################################

GGally::ggpairs(
  numeric_vars,
  title = "Pairwise Relationships Between Numeric Variables"
)

############################################################
# 13. Outlier detection
############################################################

numeric_vars %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = name, y = value)) +
  geom_boxplot() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Outlier Detection")


############################################################
# 14. A) Numeric predictors vs annual_kwh
############################################################

data_students %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = -annual_kwh,
    names_to = "predictor",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value, y = annual_kwh)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~predictor, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Numeric Predictors vs Annual Energy Consumption",
    x = "Predictor value",
    y = "Annual kWh"
  )

############################################################
# 14. B) Categorical predictors vs annual_kwh
############################################################

data_students %>%
  select(annual_kwh, where(is.factor)) %>%
  pivot_longer(
    cols = -annual_kwh,
    names_to = "variable",
    values_to = "category"
  ) %>%
  ggplot(aes(x = category, y = annual_kwh)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Categorical Predictors vs Annual Energy Consumption",
    x = "",
    y = "Annual kWh"
  )


############################################################
# 14. C) Remove bottom and top 10% of annual_kwh
############################################################

# Compute 10th and 90th percentiles
q <- quantile(data_students$annual_kwh, probs = c(0.10, 0.90), na.rm = TRUE)

data_filtered <- data_students %>%
  filter(annual_kwh >= q[1],
         annual_kwh <= q[2])

data_filtered %>%
  select(where(is.numeric)) %>%
  pivot_longer(
    cols = -annual_kwh,
    names_to = "predictor",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value, y = annual_kwh)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~predictor, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Numeric Predictors vs Annual kWh (Central 80%)",
    x = "Predictor value",
    y = "Annual kWh"
  )

data_filtered %>%
  select(annual_kwh, where(is.factor)) %>%
  pivot_longer(
    cols = -annual_kwh,
    names_to = "variable",
    values_to = "category"
  ) %>%
  ggplot(aes(x = category, y = annual_kwh)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  facet_wrap(~variable, scales = "free_x") +
  labs(
    title = "Categorical Predictors vs Annual kWh (Central 80%)",
    x = "",
    y = "Annual kWh"
  )

############################################################
# 15. Automatic EDA report
############################################################

create_report(data_students)