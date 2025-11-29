# load libraries
library(dplyr)
library(ggplot2)
library(caret)
library(cluster)
library(tidyr)
library(reshape2)
library(ggrepel)
library(ggforce)

set.seed(123)  # set seed for reproducibility

# generate main data
n <- 10000  # Number of traffic stops
traffic_stops <- data.frame(
  traffic_stop_id = 1:n,
  time_of_day = sample(c("Morning", "Afternoon", "Evening", "Night"), n, replace = TRUE, prob = c(0.25, 0.30, 0.20, 0.25)),
  subject_age = sample(18:70, n, replace = TRUE),
  subject_gender = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.6, 0.4)),
  subject_race = sample(c("White", "Black", "Latino", "Asian"), n, replace = TRUE, prob = c(0.50, 0.20, 0.20, 0.10)),
  badge_id = sample(1001:1050, n, replace = TRUE),
  division = sample(c("North", "South", "East", "West"), n, replace = TRUE)
)

# introduce anomalies: certain officers misreport race
anomalous_officers <- sample(1001:1050, 5)  # 5 officers out of 50 are anomalous
anomaly_entries <- sample(1:n, 500)  # 500 entries out of 10,000 will be anomalies

# function to misreport race
misreport_race <- function(race) {
  race_sample <- c("White", "Black", "Latino", "Asian")
  return(sample(race_sample[race_sample != race], 1))
}

# apply anomalies to dataset
traffic_stops$subject_race[traffic_stops$badge_id %in% anomalous_officers & 1:n %in% anomaly_entries] <- 
  sapply(traffic_stops$subject_race[traffic_stops$badge_id %in% anomalous_officers & 1:n %in% anomaly_entries], misreport_race)

### chi-square test

# chi2 test
# calculate observed counts of race reports per officer
officer_race_counts <- traffic_stops %>%
  group_by(badge_id, subject_race) %>%
  summarise(count = n(), .groups = "drop")

# convert to wide format to facilitate calculations
wide_race_counts <- pivot_wider(officer_race_counts, names_from = subject_race, values_from = count, values_fill = list(count = 0))

# calculate total stops per officer to determine expected counts
wide_race_counts$total_stops <- rowSums(wide_race_counts[, -1])

# using overall race probabilities to calculate expected counts
overall_race_probs <- prop.table(table(traffic_stops$subject_race))
race_names <- names(overall_race_probs)

# expected counts for each race per officer based on their total stops
for(race in race_names) {
  wide_race_counts[[paste("expected", race, sep = "_")]] <- wide_race_counts$total_stops * overall_race_probs[race]
}

# calculate observed and expected counts for chi2 test
observed_and_expected <- wide_race_counts %>%
  mutate_at(vars(matches("^White$|^Black$|^Latino$|^Asian$")), list(observed = ~ .)) %>%
  mutate(across(starts_with("expected"), ~ as.numeric(.), .names = "{.col}_num")) %>%
  rowwise() %>%
  mutate(
    chi_square_result = list(chisq.test(
      x = c(White_observed, Black_observed, Latino_observed, Asian_observed),
      p = c(expected_White_num, expected_Black_num, expected_Latino_num, expected_Asian_num),
      rescale.p = TRUE
    ))
  ) %>%
  ungroup()

# extract p-values from the chi2 test results
observed_and_expected$chi_square_p_value <- sapply(observed_and_expected$chi_square_result, function(x) x$p.value)

# visualize the chi2 test results
ggplot(observed_and_expected, aes(x = badge_id, y = chi_square_p_value)) +
  geom_point(aes(color = chi_square_p_value < 0.05), alpha = 0.6) + 
  geom_text_repel(
    aes(label = ifelse(chi_square_p_value < 0.05, as.character(badge_id), "")),
    box.padding = 0.35, 
    point.padding = 0.5, 
    segment.color = 'grey50', 
    size = 3.5,  
    min.segment.length = 0.1 
  ) + 
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue")) +
  labs(title = "P-values for each officer", x = "Badge ID", y = "P-value", colour = "p < 0.05") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### k-means clustering

# 2. k-means clustering
race_profiles <- dcast(officer_race_counts, badge_id ~ subject_race, value.var = "count", fill = 0)

clusters <- kmeans(scale(race_profiles[,-1]), centers = 5)

race_profiles$cluster <- clusters$cluster

# calculate centroids of each cluster
centroids <- race_profiles %>%
  group_by(cluster) %>%
  summarise(centroid_White = mean(White), centroid_Black = mean(Black))

# join centroids back to the main data
race_profiles <- race_profiles %>%
  left_join(centroids, by = "cluster")

# calculate distance from centroid
race_profiles <- race_profiles %>%
  mutate(distance = sqrt((White - centroid_White)^2 + (Black - centroid_Black)^2))

# determine outliers - customize threshold as needed
threshold <- mean(race_profiles$distance) + sd(race_profiles$distance) * 2
race_profiles <- race_profiles %>%
  mutate(outlier = ifelse(distance > threshold, "Outlier", "Normal"))

# plotting with enhancements
ggplot(race_profiles, aes(x = White, y = Black, color = factor(cluster))) +
  geom_point(aes(size = distance, shape = outlier), alpha = 0.7) +
  scale_size_continuous(range = c(2, 8)) + 
  scale_shape_manual(values = c("Normal" = 16, "Outlier" = 4)) +
  labs(title = "Cluster of officer race reporting patterns",
       x = "Number of times white reported", 
       y = "Number of times Black reported") +
  theme_minimal() +
  theme(legend.position = "right")

### residual analysis

# 3. logistic regression residuals
traffic_stops$subject_race <- as.factor(traffic_stops$subject_race)
model <- train(subject_race ~ time_of_day + subject_age + subject_gender + division, data = traffic_stops, method = "multinom", trControl = trainControl(method = "none"))
predicted_prob <- predict(model, traffic_stops, type = "prob")
residuals <- rowSums((model.matrix(~ subject_race - 1, traffic_stops) - predicted_prob)^2)

# visualizing residuals
ggplot(traffic_stops, aes(x = badge_id, y = residuals)) +
  geom_jitter(alpha = 0.4) +
  labs(title = "Residuals from logistic regression by officer", x = "Badge ID", y = "Residuals")