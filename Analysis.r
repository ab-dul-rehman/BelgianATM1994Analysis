# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load data
df <- read.csv("BelgiumATM.csv", stringsAsFactors = FALSE)

# Replace 'missing' with NA
df[df == 'missing'] <- NA

# Convert columns to appropriate data types
df$population <- as.numeric(df$population)
df$numATMs <- as.numeric(df$numATMs)
df$ATMwithdr <- as.numeric(df$ATMwithdr)
df$withdrvalue <- as.numeric(df$withdrvalue)
df$unemprate <- as.numeric(df$unemprate)
df$numbranches <- as.numeric(df$numbranches)

# Rescale population to thousands
df$population <- df$population / 1000

# Handle missing values: create a logical vector 'nm' for non-missing rows
nm <- complete.cases(df)

# Calculate mean for non-missing rows
mean_nm <- df %>% filter(nm) %>% summarise_all(mean, na.rm = TRUE)

# Replace NAs with zeros for specific columns
df$ATMwithdr[is.na(df$ATMwithdr)] <- 0
df$withdrvalue[is.na(df$withdrvalue)] <- 0

# Calculate mean for all rows
mean_all <- df %>% summarise_all(mean, na.rm = TRUE)

# Summarize the average ATMwithdr and withdrvalue by the number of ATMs
mean_atmwithdr <- df %>%
  group_by(numATMs) %>%
  summarise(mean_ATMwithdr = mean(ATMwithdr, na.rm = TRUE))

mean_withdrvalue <- df %>%
  group_by(numATMs) %>%
  summarise(mean_withdrvalue = mean(withdrvalue, na.rm = TRUE))

# Plot average ATM withdrawals per resident by number of ATMs
ggplot(mean_atmwithdr, aes(x = numATMs, y = mean_ATMwithdr)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of ATMs", y = "Average ATM withdrawals per resident") +
  ggtitle("Avg ATM Withdrawals per Resident by Number of ATMs")

# Plot average amount per withdrawal by number of ATMs
ggplot(mean_withdrvalue, aes(x = numATMs, y = mean_withdrvalue)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of ATMs", y = "Average Amount per Withdrawal") +
  ggtitle("Average Amount per Withdrawal by Number of ATMs")

# Additional insights and plots

# Distribution of population
ggplot(df, aes(x = population)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Population (in thousands)", y = "Frequency") +
  ggtitle("Distribution of Population")

# Scatter plot of ATM withdrawals vs. population
ggplot(df, aes(x = population, y = ATMwithdr)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Population (in thousands)", y = "ATM Withdrawals per Resident") +
  ggtitle("ATM Withdrawals per Resident vs. Population")

# Scatter plot of amount per withdrawal vs. unemployment rate
ggplot(df, aes(x = unemprate, y = withdrvalue)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(x = "Unemployment Rate", y = "Amount per Withdrawal") +
  ggtitle("Amount per Withdrawal vs. Unemployment Rate")

# Boxplot of ATM withdrawals by number of ATMs
ggplot(df, aes(x = factor(numATMs), y = ATMwithdr)) +
  geom_boxplot() +
  labs(x = "Number of ATMs", y = "ATM Withdrawals per Resident") +
  ggtitle("ATM Withdrawals per Resident by Number of ATMs")

# Save the cleaned dataset
write.csv(df, "BelgiumAtm_cleaned.csv", row.names = FALSE)
