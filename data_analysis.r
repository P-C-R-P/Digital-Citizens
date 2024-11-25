# Read cleaned data

data <- read.csv("final-data.csv", fileEncoding = "UTF-8")

data <- data[, !names(data) %in% "romansh_canton"]

# Create extra categorical variable column to compare 3 regions

data$ro_macro <- ifelse(data$region_origin == "non-swiss, european",
  "non-swiss, european",
  ifelse(data$region_origin == "non-swiss, non-european",
    "non-swiss, non-european",
    "swiss"
  )
)

# Calculate mean, median, standard deviation and range of numerical demographic variables

# AGE

mean_age <- mean(data$age)
median_age <- median(data$age)
sd_age <- sd(data$age)
range_age <- range(data$age)

# YEARS OF INTERNET USAGE

mean_yi <- mean(data$years_internet)
median_yi <- median(data$years_internet)

# Calculate frequency and percentages of categorical and binary variables

# AT UNIVERSITY OF LAUSANNE

frequency_ul <- table(data$university_lausanne)
percentage_ul <- prop.table(frequency_ul) * 100

# GENDER IDENTITY

frequency_gi <- table(data$gender_identity)
percentage_gi <- prop.table(frequency_gi) * 100

# REGION OF ORIGIN

frequency_ro <- table(data$region_origin)
percentage_ro <- prop.table(frequency_ro) * 100

# LEVEL OF EDUCATION COMPLETED

frequency_ec <- table(data$education_completed)
percentage_ec <- prop.table(frequency_ec) * 100

# INTEREST IN FUTURE APPLICATION

frequency_ia <- table(data$interested_application)
percentage_ia <- prop.table(frequency_ia) * 100

# TYPES OF ORGANISATIONS CONTRIBUTED TO

co_columns <- data[, grep("^co_", colnames(data))]

for (column in colnames(co_columns)) {
  frequencies_co <- table(co_columns[[column]])
  percentages_co <- prop.table(frequencies_co) * 100
}

# MOTIVATIONS FOR SHARING DATA WITH RESEARCH

ms_columns <- data[, grep("^ms_", colnames(data))]

for (column in colnames(ms_columns)) {
  frequencies_ms <- table(ms_columns[[column]])
  percentages_ms <- prop.table(frequencies_ms) * 100
}

# Perform chi-square tests

# GENDER IDENTITY VS UNIVERSITY OF LAUSANNE (SIGNIFICANT)

contingency_table <- table(data$gender_identity, data$university_lausanne)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS REGION OF ORIGIN (SIGNIFICANT)

contingency_table <- table(data$university_lausanne, data$region_origin)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS REGION OF ORIGIN (MACRO) (SIGNIFICANT)

contingency_table <- table(data$university_lausanne, data$ro_macro)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS AGE (SIGNIFICANT)

contingency_table <- table(data$university_lausanne, data$age)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS YEARS OF INTERNET USAGE (SIGNIFICANT)

contingency_table <- table(data$university_lausanne, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN VS YEARS OF INTERNET USAGE (SIGNIFICANT)

contingency_table <- table(data$region_origin, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN (MACRO) VS YEARS OF INTERNET USAGE (SIGNIFICANT)

contingency_table <- table(data$ro_macro, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN VS INTERESTED IN FUTURE APPLICATION (SIGNIFICANT)

contingency_table <- table(data$region_origin, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN (MACRO) VS INTERESTED IN FUTURE APPLICATION (SIGNIFICANT)

contingency_table <- table(data$ro_macro, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# LEVEL OF EDUCATION COMPLETED VS AGE (SIGNIFICANT)

contingency_table <- table(data$education_completed, data$age)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# LEVEL OF EDUCATION COMPLETED VS YEARS OF INTERNET USAGE (SIGNIFICANT)

contingency_table <- table(data$education_completed, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# AGE VS YEARS OF INTERNET USAGE (SIGNIFICANT)

contingency_table <- table(data$age, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# GENDER IDENTITY VS LEVEL OF EDUCATION COMPLETED (SIGNIFICANT)

contingency_table <- table(data$gender_identity, data$education_completed)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN (MACRO) VS AGE (SIGNIFICANT)

contingency_table <- table(data$ro_macro, data$age)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# GENDER IDENTITY VS REGION OF ORIGIN

contingency_table <- table(data$gender_identity, data$region_origin)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# GENDER IDENTITY VS AGE

contingency_table <- table(data$gender_identity, data$age)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# GENDER IDENTITY VS YEARS OF INTERNET USAGE

contingency_table <- table(data$gender_identity, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# GENDER IDENTITY VS INTERESTED IN FUTURE APPLICATION

contingency_table <- table(data$gender_identity, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS LEVEL OF EDUCATION COMPLETED

contingency_table <- table(data$university_lausanne, data$education_completed)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS INTERESTED IN FUTURE APPLICATION

contingency_table <- table(data$university_lausanne, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN VS LEVEL OF EDUCATION COMPLETED

contingency_table <- table(data$region_origin, data$education_completed)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN (MACRO) VS LEVEL OF EDUCATION COMPLETED

contingency_table <- table(data$ro_macro, data$education_completed)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN VS AGE

contingency_table <- table(data$region_origin, data$age)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# LEVEL OF EDUCATION COMPLETED VS INTERESTED IN FUTURE APPLICATION

contingency_table <- table(data$education_completed, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# AGE VS INTERESTED IN FUTURE APPLICATION

contingency_table <- table(data$age, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# YEARS OF INTERNET USAGE VS INTERESTED IN FUTURE APPLICATION

contingency_table <- table(data$years_internet, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# Clustering to understand key groups

numeric_data <- data[, sapply(data, is.numeric)]

scaled_data <- scale(numeric_data)

summary(scaled_data)

set.seed(123)
wss <- numeric()
for (k in 1:10) {
  kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss
}

plot(1:10, wss,
  type = "b", pch = 19, frame = FALSE,
  xlab = "Number of clusters K", ylab = "Total within-clusters sum of squares",
  main = "Elbow Method for Determining Optimal K"
)
kmeans_result <- kmeans(scaled_data, centers = 3)

kmeans_result$cluster
kmeans_result$centers

kmeans_result$tot.withinss

data$cluster <- kmeans_result$cluster

pca_result <- prcomp(scaled_data)

plot(pca_result$x[, 1:2], col = data$cluster, pch = 16, main = "PCA - Clusters")

text(pca_result$x[, 1:2], labels = data$ro_macro, pos = 3, cex = 0.7, col = "black")

loadings <- pca_result$rotation

loadings[, 1:2]