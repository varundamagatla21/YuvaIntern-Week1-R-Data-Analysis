packages <- c(
  "tidyverse",
  "janitor",
  "naniar",
  "corrplot",
  "scales"
)
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
file.exists("data/hotel_bookings.csv")
library(tidyverse)
library(janitor)
hotel_raw <- read_csv(
  "data/hotel_bookings.csv",
  show_col_types = FALSE
) %>%
  clean_names()
# Check dataset dimensions
dim(hotel_raw)
# Display column names
names(hotel_raw)
# Inspect data types and structure
str(hotel_raw)
# View summary statistics
summary(hotel_raw)
# Count missing values in each column
missing_summary <- data.frame(
  column = names(hotel_raw),
  missing_count = colSums(is.na(hotel_raw)),
  missing_percent = round(
    colMeans(is.na(hotel_raw)) * 100, 2
  )
)
missing_summary <- missing_summary[
  order(-missing_summary$missing_count),
]
print(missing_summary, row.names = FALSE)
# Count exact duplicate rows
sum(duplicated(hotel_raw))
# Check text placeholders that represent missing information
sum(hotel_raw$agent == "NULL", na.rm = TRUE)
sum(hotel_raw$company == "NULL", na.rm = TRUE)
# Inspect children values
sum(is.na(hotel_raw$children))
summary(hotel_raw$children)
# Inspect invalid or unusual room rates
sum(hotel_raw$adr < 0, na.rm = TRUE)
sum(hotel_raw$adr > 1000, na.rm = TRUE)
# Inspect duplicate proportion
round(
  mean(duplicated(hotel_raw)) * 100,
  2
)
# Create a separate copy
hotel_clean <- hotel_raw
# Convert literal NULL strings to actual missing values
hotel_clean <- hotel_clean |>
  mutate(
    across(
      where(is.character),
      ~ na_if(trimws(.x), "NULL")
    )
  )
# Check missing values after placeholder conversion
missing_after_null <- data.frame(
  column = names(hotel_clean),
  missing_count = colSums(is.na(hotel_clean))
) |>
  arrange(desc(missing_count))
print(head(missing_after_null, 10))
# Impute missing children values using the median
children_median <- median(
  hotel_clean$children,
  na.rm = TRUE
)
hotel_clean$children[
  is.na(hotel_clean$children)
] <- children_median
# Represent missing agent and company identifiers
# with an explicit Unknown category
hotel_clean$agent[
  is.na(hotel_clean$agent)
] <- "Unknown"
hotel_clean$company[
  is.na(hotel_clean$company)
] <- "Unknown"
# Replace invalid negative room rates with NA,
# then impute using the median of valid room rates
hotel_clean$adr[hotel_clean$adr < 0] <- NA_real_
valid_adr_median <- median(
  hotel_clean$adr,
  na.rm = TRUE
)
hotel_clean$adr[
  is.na(hotel_clean$adr)
] <- valid_adr_median
# Remove exact duplicate rows
rows_before <- nrow(hotel_clean)
hotel_clean <- hotel_clean |>
  distinct()
rows_after <- nrow(hotel_clean)
cat("Rows before deduplication:", rows_before, "\n")
cat("Rows after deduplication:", rows_after, "\n")
cat("Duplicate rows removed:", rows_before - rows_after, "\n")
# Verify missing values
cat(
  "Remaining missing cells:",
  sum(is.na(hotel_clean)),
  "\n"
)
# Save cleaned dataset
write_csv(
  hotel_clean,
  "outputs/hotel_bookings_cleaned.csv"
)
library(naniar)
library(ggplot2)
p_missing <- gg_miss_var(hotel_raw) +
  labs(
    title = "Missing Values by Variable",
    x = "Number of missing values",
    y = "Variable"
  ) +
  theme_minimal()
print(p_missing)
ggsave(
  "outputs/missing_values.png",
  plot = p_missing,
  width = 9,
  height = 6,
  dpi = 300
)
# Count missing values in each column after cleaning
missing_remaining <- data.frame(
  column = names(hotel_clean),
  missing_count = colSums(is.na(hotel_clean))
)
missing_remaining <- missing_remaining[
  missing_remaining$missing_count > 0,
]
missing_remaining <- missing_remaining[
  order(-missing_remaining$missing_count),
]
print(missing_remaining, row.names = FALSE)
# Replace missing country values with "Unknown"
hotel_clean$country[
  is.na(hotel_clean$country)
] <- "Unknown"
# Verify missing values
cat(
  "Total missing cells:",
  sum(is.na(hotel_clean)),
  "\n"
)
# Count Unknown country entries
sum(hotel_clean$country == "Unknown")
# Save the updated cleaned dataset
write_csv(
  hotel_clean,
  "outputs/hotel_bookings_cleaned.csv"
)
# Summary of lead time and room rate
summary(hotel_clean$lead_time)
summary(hotel_clean$adr)
# Lead-time boxplot
p_lead <- ggplot(
  hotel_clean,
  aes(y = lead_time)
) +
  geom_boxplot() +
  labs(
    title = "Outlier Detection: Booking Lead Time",
    y = "Lead time (days)"
  ) +
  theme_minimal()
print(p_lead)
ggsave(
  "outputs/lead_time_boxplot.png",
  p_lead,
  width = 7,
  height = 5,
  dpi = 300
)
# ADR boxplot
p_adr <- ggplot(
  hotel_clean,
  aes(y = adr)
) +
  geom_boxplot() +
  labs(
    title = "Outlier Detection: Average Daily Rate",
    y = "ADR"
  ) +
  theme_minimal()
print(p_adr)
ggsave(
  "outputs/adr_boxplot.png",
  p_adr,
  width = 7,
  height = 5,
  dpi = 300
)
count_iqr_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr_value <- IQR(x, na.rm = TRUE)
  lower <- q1 - 1.5 * iqr_value
  upper <- q3 + 1.5 * iqr_value
  sum(x < lower | x > upper, na.rm = TRUE)
}
cat(
  "Lead-time outliers:",
  count_iqr_outliers(hotel_clean$lead_time),
  "\n"
)
cat(
  "ADR outliers:",
  count_iqr_outliers(hotel_clean$adr),
  "\n"
)
# Min-max normalization function
min_max_normalize <- function(x) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  if (max_x == min_x) {
    return(rep(0, length(x)))
  }
  (x - min_x) / (max_x - min_x)
}
# Normalize selected numerical variables
hotel_clean <- hotel_clean |>
  mutate(
    lead_time_normalized =
      min_max_normalize(lead_time),
    adr_normalized =
      min_max_normalize(adr)
  )
# Verify normalized values
summary(hotel_clean$lead_time_normalized)
summary(hotel_clean$adr_normalized)
# Save updated dataset
write_csv(
  hotel_clean,
  "outputs/hotel_bookings_cleaned.csv"
)
# Select categorical variables
categorical_data <- hotel_clean |>
  select(
    hotel,
    meal,
    market_segment,
    customer_type
  ) |>
  mutate(across(everything(), as.factor))
# Create one-hot encoded columns
encoded_matrix <- model.matrix(
  ~ . - 1,
  data = categorical_data
)
# Convert to a data frame
encoded_data <- as.data.frame(encoded_matrix)
# Inspect encoded data
dim(encoded_data)
head(encoded_data)
# Save encoded data separately
write.csv(
  encoded_data,
  "outputs/categorical_encoded.csv",
  row.names = FALSE
)
# Select numerical variables for correlation
cor_vars <- hotel_clean |>
  select(
    lead_time,
    stays_in_weekend_nights,
    stays_in_week_nights,
    adults,
    children,
    previous_cancellations,
    booking_changes,
    adr,
    total_of_special_requests,
    is_canceled
  )
# Calculate Pearson correlations
cor_matrix <- cor(
  cor_vars,
  use = "pairwise.complete.obs"
)
# Print results
print(round(cor_matrix, 2))
# Save the matrix
write.csv(
  cor_matrix,
  "outputs/correlation_matrix.csv"
)
# Create correlation heatmap
png(
  "outputs/correlation_heatmap.png",
  width = 1200,
  height = 1000,
  res = 120
)
corrplot::corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.cex = 0.75,
  number.cex = 0.6
)
dev.off()
# ==========================================
# EDA 1: CANCELLATION ANALYSIS
# ==========================================
cancellation_data <- hotel_clean |>
  count(is_canceled) |>
  mutate(
    status = ifelse(
      is_canceled == 1,
      "Canceled",
      "Not Canceled"
    ),
    percentage = n / sum(n) * 100
  )
print(cancellation_data)
p_cancellation <- ggplot(
  cancellation_data,
  aes(x = status, y = n)
) +
  geom_col() +
  labs(
    title = "Booking Cancellation Status",
    x = "Booking Status",
    y = "Number of Bookings"
  ) +
  theme_minimal()
print(p_cancellation)
ggsave(
  "outputs/cancellation_status.png",
  p_cancellation,
  width = 8,
  height = 5,
  dpi = 300
)
# ==========================================
# EDA 2: HOTEL TYPE
# ==========================================
hotel_type_summary <- hotel_clean |>
  count(hotel) |>
  mutate(
    percentage = n / sum(n) * 100
  )
print(hotel_type_summary)
p_hotel <- ggplot(
  hotel_type_summary,
  aes(x = hotel, y = n)
) +
  geom_col() +
  labs(
    title = "Bookings by Hotel Type",
    x = "Hotel Type",
    y = "Number of Bookings"
  ) +
  theme_minimal()
print(p_hotel)
ggsave(
  "outputs/bookings_by_hotel.png",
  p_hotel,
  width = 8,
  height = 5,
  dpi = 300
)
# ==========================================
# EDA 3: LEAD TIME DISTRIBUTION
# ==========================================
p_lead_distribution <- ggplot(
  hotel_clean,
  aes(x = lead_time)
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Distribution of Booking Lead Time",
    x = "Lead Time (days)",
    y = "Number of Bookings"
  ) +
  theme_minimal()
print(p_lead_distribution)
ggsave(
  "outputs/lead_time_distribution.png",
  p_lead_distribution,
  width = 8,
  height = 5,
  dpi = 300
)
# ==========================================
# EDA 4: ADR DISTRIBUTION
# ==========================================
p_adr_distribution <- ggplot(
  hotel_clean,
  aes(x = adr)
) +
  geom_histogram(
    bins = 50
  ) +
  labs(
    title = "Distribution of Average Daily Rate",
    x = "Average Daily Rate (ADR)",
    y = "Number of Bookings"
  ) +
  theme_minimal()
print(p_adr_distribution)
ggsave(
  "outputs/adr_distribution.png",
  p_adr_distribution,
  width = 8,
  height = 5,
  dpi = 300
)
# ==========================================
# EDA 5: CANCELLATION BY HOTEL TYPE
# ==========================================
cancellation_by_hotel <- hotel_clean |>
  group_by(hotel) |>
  summarise(
    total_bookings = n(),
    canceled_bookings = sum(is_canceled),
    cancellation_rate =
      mean(is_canceled) * 100
  )
print(cancellation_by_hotel)
p_cancel_hotel <- ggplot(
  cancellation_by_hotel,
  aes(x = hotel, y = cancellation_rate)
) +
  geom_col() +
  labs(
    title = "Cancellation Rate by Hotel Type",
    x = "Hotel Type",
    y = "Cancellation Rate (%)"
  ) +
  theme_minimal()
print(p_cancel_hotel)
ggsave(
  "outputs/cancellation_rate_by_hotel.png",
  p_cancel_hotel,
  width = 8,
  height = 5,
  dpi = 300
)
# ==========================================
# FINAL DESCRIPTIVE STATISTICS
# ==========================================
numeric_summary <- hotel_clean |>
  select(where(is.numeric)) |>
  summarise(
    across(
      everything(),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  )
# Convert summary to a readable table
summary_table <- tibble(
  variable = names(hotel_clean |> select(where(is.numeric))),
  mean = sapply(
    hotel_clean |> select(where(is.numeric)),
    mean, na.rm = TRUE
  ),
  median = sapply(
    hotel_clean |> select(where(is.numeric)),
    median, na.rm = TRUE
  ),
  standard_deviation = sapply(
    hotel_clean |> select(where(is.numeric)),
    sd, na.rm = TRUE
  ),
  minimum = sapply(
    hotel_clean |> select(where(is.numeric)),
    min, na.rm = TRUE
  ),
  maximum = sapply(
    hotel_clean |> select(where(is.numeric)),
    max, na.rm = TRUE
  )
)
print(summary_table, n = Inf)
write_csv(
  summary_table,
  "outputs/descriptive_statistics.csv"
)
# Final quality checks
cat("Final rows:", nrow(hotel_clean), "\n")
cat("Final columns:", ncol(hotel_clean), "\n")
cat("Missing cells:", sum(is.na(hotel_clean)), "\n")
cat(
  "Exact duplicate rows remaining:",
  sum(duplicated(hotel_clean)),
  "\n"
)
savehistory("C:/Users/Admin/OneDrive/Desktop/clerar.Rhistory")
