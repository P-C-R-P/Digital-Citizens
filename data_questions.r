library(ggplot2)

data <- read.csv("Datasets/final-data.csv")
data <- data[, !names(data) %in% "romansh_canton"]
data <- subset(data, select = -concerns_score)
data <- subset(data, select = -familiarity_score)

data$ro_macro <- ifelse(data$region_origin == "non-swiss, european",
  "non-swiss, european",
  ifelse(data$region_origin == "non-swiss, non-european",
    "non-swiss, non-european",
    "swiss"
  )
)

data$normalised_literacy <- data$digital_literacy / 30
data$combined_literacy <- (data$normalised_literacy / 7) * (data$competent_digital / 5)
data$normalised_paralanguage <- data$paralanguage_communication / 7
data$general <- (data$productivity + data$finances) / 2

communication <- c("pa_uima", "pa_slof", "pa_mvocc", "pa_svmomvc", "pa_usmp", "pa_sospov", "pa_womsrv", "pa_womsrv", "pa_stomm", "pa_e", "pa_piofogc")

data$communication <- rowSums(data[communication], na.rm = TRUE) / length(communication)

instrumental <- c("pa_uatcwat", "pa_ucor", "pa_saot", "pa_b", "pa_l", "pa_bw", "pa_si", "pa_ucocc", "pa_ml", "pa_sogdcs")
expressive <- c("pa_usmp", "pa_womsrv", "pa_svmomvc", "pa_mgom", "pa_epov", "pa_tpov", "pa_sospov", "pa_stomm", "pa_piofogc")

data$instrumental <- rowSums(data[instrumental], na.rm = TRUE) / length(instrumental)
data$expressive <- rowSums(data[expressive], na.rm = TRUE) / length(expressive)

data$university_lausanne <- as.factor(data$university_lausanne)
data$gender_identity <- as.factor(data$gender_identity)
data$education_completed <- as.factor(data$education_completed)
data$ro_macro <- as.factor(data$ro_macro)
data$age <- as.numeric(data$age)
data$years_internet <- as.numeric(data$years_internet)

pa_columns <- grep("^pa_", colnames(data), value = TRUE)

diversity_column <- apply(data[, pa_columns], 1, function(x) sum(x >= 3 & x <= 7))

data$diversity_score <- diversity_column / 30

cd_columns <- grep("^cd_", colnames(data), value = TRUE)

data$concern_score <- rowSums(data[, cd_columns], na.rm = TRUE) / 8
data$acknowledgement <- (data$ms_aic + data$ms_cip) / 2

colnames(data)[colnames(data) == "self-development"] <- "self_development"

data$sociolinguistics_score <- rowSums(data[, c("ft_cmc", "ft_pos", "ft_p", "ft_i")], na.rm = TRUE) / 4
data$previous_research <- rowSums(data[, c("co_auoei", "co_ahomrc", "co_ago", "co_acono", "co_acofc")], na.rm = TRUE) / 5

co_columns <- grep("^co_", colnames(data), value = TRUE)

data$bw_pg <- rowSums(data[, c("pa_bw", "pa_pg")], na.rm = TRUE) / 2

co_application <- c(co_columns, "interested_application")

application <- data[, co_application]

data$past_present <- rowSums(data[, c("contributed_research", "interested_application")], na.rm = TRUE) / 2

cd_application <- c(cd_columns, "interested_application")

limiting_factors <- data[, cd_application]

cd_pp <- c(cd_columns, "past_present")

lf_pp <- data[, cd_pp]

pa_application <- c(pa_columns, "interested_application")

application <- data[, pa_application]

pa_pp <- c(pa_columns, "past_present")

application_pp <- data[, pa_pp]

egotistical <- c("ms_fc", "ms_aic", "ms_cip", "ms_no", "ms_lasor", "ms_lam", "ms_las", "ms_ca")

collective <- c("ms_hamaritp", "ms_no", "ms_tptswfaf", "ms_lasor", "ms_ca")

data$egotistical_motive <- rowSums(data[egotistical], na.rm = TRUE) / length(egotistical)
data$collective_motive <- rowSums(data[collective], na.rm = TRUE) / length(collective)

ms_columns <- grep("^ms_", colnames(data), value = TRUE)
ms_application <- c(ms_columns, "interested_application")
ms_ia <- data[, ms_application]
ms_research <- c(ms_columns, "past_present")

research_motives <- data[, ms_research]

co_pa <- c(pa_columns, co_columns)

# OUTPUT

sink("output.txt", type = "output")

# Descriptive statistics

print("Descriptive statistics")

summary(data)

# Standard Deviations

# Age

sd_age <- sd(data$age)

print("Age:")
print(sd_age)

# Competency

sd_cd <- sd(data$competent_digital)

print("Competency:")
print(sd_cd)

# Algorithms

sd_ua <- sd(data$understand_algorithms)

print("Algorithms:")
print(sd_ua)

# Familiarity

sd_cs <- sd(data$ft_cs)

print("Familiarity:")
print(sd_cs)

# Diversity

sd_ds <- sd(data$diversity_score)

print("Diversity:")
print(sd_ds)

# Interest

sd_pp <- sd(data$past_present)

print("Interest:")
print(sd_pp)

# Chi2 Tests

# GENDER IDENTITY VS UNIVERSITY OF LAUSANNE (SIGNIFICANT)

print("UNIVERSITY OF LAUSANNE VS REGION OF ORIGIN (SIGNIFICANT)")

contingency_table <- table(data$gender_identity, data$university_lausanne)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS REGION OF ORIGIN (SIGNIFICANT)

print("UNIVERSITY OF LAUSANNE VS REGION OF ORIGIN (SIGNIFICANT)")

contingency_table <- table(data$university_lausanne, data$region_origin)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS REGION OF ORIGIN (MACRO) (SIGNIFICANT)

print("UNIVERSITY OF LAUSANNE VS REGION OF ORIGIN (MACRO) (SIGNIFICANT)")

contingency_table <- table(data$university_lausanne, data$ro_macro)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS AGE (SIGNIFICANT)

print("UNIVERSITY OF LAUSANNE VS AGE (SIGNIFICANT)")

contingency_table <- table(data$university_lausanne, data$age)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# UNIVERSITY OF LAUSANNE VS YEARS OF INTERNET USAGE (SIGNIFICANT)

print("UNIVERSITY OF LAUSANNE VS YEARS OF INTERNET USAGE (SIGNIFICANT)")

contingency_table <- table(data$university_lausanne, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN VS YEARS OF INTERNET USAGE (SIGNIFICANT)

print("REGION OF ORIGIN VS YEARS OF INTERNET USAGE (SIGNIFICANT)")

contingency_table <- table(data$region_origin, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN (MACRO) VS YEARS OF INTERNET USAGE (SIGNIFICANT)

print("REGION OF ORIGIN (MACRO) VS YEARS OF INTERNET USAGE (SIGNIFICANT)")

contingency_table <- table(data$ro_macro, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN VS INTERESTED IN FUTURE APPLICATION (SIGNIFICANT)

print("REGION OF ORIGIN VS INTERESTED IN FUTURE APPLICATION (SIGNIFICANT)")

contingency_table <- table(data$region_origin, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# REGION OF ORIGIN (MACRO) VS INTERESTED IN FUTURE APPLICATION (SIGNIFICANT)

print("REGION OF ORIGIN (MACRO) VS INTERESTED IN FUTURE APPLICATION (SIGNIFICANT)")

contingency_table <- table(data$ro_macro, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# LEVEL OF EDUCATION COMPLETED VS AGE (SIGNIFICANT)

print("LEVEL OF EDUCATION COMPLETED VS AGE (SIGNIFICANT)")

contingency_table <- table(data$education_completed, data$age)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# LEVEL OF EDUCATION COMPLETED VS YEARS OF INTERNET USAGE (SIGNIFICANT)

print("LEVEL OF EDUCATION COMPLETED VS YEARS OF INTERNET USAGE (SIGNIFICANT)")

contingency_table <- table(data$education_completed, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# AGE VS YEARS OF INTERNET USAGE (SIGNIFICANT)

print("AGE VS YEARS OF INTERNET USAGE (SIGNIFICANT)")

contingency_table <- table(data$age, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# GENDER IDENTITY VS LEVEL OF EDUCATION COMPLETED (SIGNIFICANT)

print("GENDER IDENTITY VS LEVEL OF EDUCATION COMPLETED (SIGNIFICANT)")

contingency_table <- table(data$gender_identity, data$education_completed)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# GENDER IDENTITY VS REGION OF ORIGIN (SIGNIFICANT)

print("GENDER IDENTITY VS REGION OF ORIGIN (SIGNIFICANT)")

contingency_table <- table(data$gender_identity, data$region_origin)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# GENDER IDENTITY VS YEARS OF INTERNET USAGE (SIGNIFICANT)

print("GENDER IDENTITY VS YEARS OF INTERNET USAGE (SIGNIFICANT)")

contingency_table <- table(data$gender_identity, data$years_internet)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# Who uses digital technology the most frequently?

print("Who uses digital technology the most frequently?")

# Plots

ggplot(data, aes(x = gender_identity, y = normalised_literacy)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Digital Use", x = "Gender Identity", y = "Digital Use")

ggplot(data, aes(x = education_completed, y = normalised_literacy)) +
  geom_boxplot() +
  labs(title = "Education Level and Digital Use", x = "Education Level", y = "Digital Use") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = normalised_literacy)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Digital Use", x = "Region of Origin", y = "Digital Use")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "normalised_literacy")], use = "complete.obs")

print(correlation)

test <- cor.test(data$age, data$normalised_literacy)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(normalised_literacy ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(normalised_literacy ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(normalised_literacy ~ ro_macro, data = data)

summary(anova_region)

# Who uses digital technology the most competently?

print("Who uses digital technology the most competently?")

# Plots

ggplot(data, aes(x = gender_identity, y = competent_digital)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Digital Competency", x = "Gender Identity", y = "Digital Competency")

ggplot(data, aes(x = education_completed, y = competent_digital)) +
  geom_boxplot() +
  labs(title = "Education Level and Digital Competency", x = "Education Level", y = "Digital Competency") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = competent_digital)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Digital Competency", x = "Region of Origin", y = "Digital Competency")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "competent_digital")], use = "complete.obs")

print(correlation)

test <- cor.test(data$age, data$competent_digital)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$years_internet, data$normalised_literacy)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(competent_digital ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(competent_digital ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(competent_digital ~ ro_macro, data = data)

summary(anova_region)

# Who is the most digitally literate?

print("Who is the most digitally literate?")

# Plots

ggplot(data, aes(x = gender_identity, y = combined_literacy)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Digital Literacy", x = "Gender Identity", y = "Digital Literacy")

ggplot(data, aes(x = education_completed, y = combined_literacy)) +
  geom_boxplot() +
  labs(title = "Education Level and Digital Literacy", x = "Education Level", y = "Digital Literacy") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = combined_literacy)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Digital Literacy", x = "Region of Origin", y = "Digital Literacy")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "combined_literacy")], use = "complete.obs")

print(correlation)

test <- cor.test(data$age, data$combined_literacy)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$years_internet, data$combined_literacy)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(combined_literacy ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(combined_literacy ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(combined_literacy ~ ro_macro, data = data)

summary(anova_region)

# Does frequency of use relate to digital competence?

print("Does frequency of use relate to digital competence?")

# Linear Regression

model <- lm(competent_digital ~ normalised_literacy, data = data)

summary(model)

# Does age relate to frequency of use?

print("Does age relate to frequency of use?")

# Linear Regression

model <- lm(normalised_literacy ~ age, data = data)

summary(model)

# Does age relate to digital competence?

print("Does age relate to digital competence?")

# Linear Regression

model <- lm(competent_digital ~ age, data = data)

summary(model)

# Does age relate to digital literacy?

print("Does age relate to digital literacy?")

# Linear Regression

model <- lm(combined_literacy ~ age, data = data)

summary(model)

# What demographic produces the most paralanguage output?

print("What demographic produces the most paralanguage output?")

# Plots

ggplot(data, aes(x = gender_identity, y = normalised_paralanguage)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Paralanguage Use", x = "Gender Identity", y = "Paralanguage Use")

ggplot(data, aes(x = education_completed, y = normalised_paralanguage)) +
  geom_boxplot() +
  labs(title = "Education Level and Paralanguage Use", x = "Education Level", y = "Paralanguage Use") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = normalised_paralanguage)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Paralanguage Use", x = "Region of Origin", y = "Paralanguage Use")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "normalised_paralanguage")], use = "complete.obs")

print(correlation)

test <- cor.test(data$age, data$normalised_paralanguage)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$years_internet, data$normalised_paralanguage)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(normalised_paralanguage ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(normalised_paralanguage ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(normalised_paralanguage ~ ro_macro, data = data)

summary(anova_region)

# What demographic produces the most communication output?

print("What demographic produces the most communication output?")

# Plots

ggplot(data, aes(x = gender_identity, y = communication)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Communication Use", x = "Gender Identity", y = "Communication Use")

ggplot(data, aes(x = education_completed, y = communication)) +
  geom_boxplot() +
  labs(title = "Education Level and Communication Use", x = "Education Level", y = "Communication Use") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = communication)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Communication Use", x = "Region of Origin", y = "Communication Use")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "communication")], use = "complete.obs")

print(correlation)

test <- cor.test(data$years_internet, data$communication)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$age, data$communication)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(communication ~ gender_identity, data = data)
summary(anova_gender)

anova_education <- aov(communication ~ education_completed, data = data)
summary(anova_education)

anova_region <- aov(communication ~ ro_macro, data = data)
summary(anova_region)

# Do demographic characteristics relate to instrumental use?

print("Do demographic characteristics relate to instrumental use?")

# Plots

ggplot(data, aes(x = gender_identity, y = instrumental)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Instrumental Use", x = "Gender Identity", y = "Instrumental Use")

ggplot(data, aes(x = education_completed, y = instrumental)) +
  geom_boxplot() +
  labs(title = "Education Level and Instrumental Use", x = "Education Level", y = "Instrumental Use") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = instrumental)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Instrumental Use", x = "Region of Origin", y = "Instrumental Use")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "instrumental")], use = "complete.obs")

print(correlation)

test <- cor.test(data$years_internet, data$instrumental)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$age, data$instrumental)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(instrumental ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(instrumental ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(instrumental ~ ro_macro, data = data)

summary(anova_region)

# Do demographic characteristics relate to expressive use?

print("Do demographic characteristics relate to expressive use?")

# Plots

ggplot(data, aes(x = gender_identity, y = expressive)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Expressive Use", x = "Gender Identity", y = "Expressive Use")

ggplot(data, aes(x = education_completed, y = expressive)) +
  geom_boxplot() +
  labs(title = "Education Level and Expressive Use", x = "Education Level", y = "Expressive Use") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = expressive)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Expressive Use", x = "Region of Origin", y = "Expressive Use")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "expressive")], use = "complete.obs")

print(correlation)

test <- cor.test(data$years_internet, data$expressive)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$age, data$expressive)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(expressive ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(expressive ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(expressive ~ ro_macro, data = data)

summary(anova_region)

# EDIT How do people use digital technology?

print("How do people use digital technology?")

# Clustering

pa_data <- data[, pa_columns]

scaled_data <- scale(pa_data)

summary(scaled_data)

set.seed(123)

wcss <- numeric()

for (k in 1:10) {
  kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)
  wcss[k] <- kmeans_result$tot.withinss
}

plot(1:10, wcss,
  type = "b", pch = 19, frame = FALSE,
  xlab = "Number of clusters", ylab = "Total",
  main = "Elbow Method"
)

slope <- (wcss[10] - wcss[5]) / (10 - 5)

intercept <- wcss[5] - slope * 5

abline(a = intercept, b = slope, col = "#0089c4", lwd = 2, lty = 2)

kmeans_result <- kmeans(scaled_data, centers = 5, nstart = 25)

kmeans_result$cluster
kmeans_result$centers

data$cluster <- kmeans_result$cluster

# PCA

pca_result <- prcomp(scaled_data)

plot(pca_result$x[, 1:2],
  col = data$cluster, pch = 16,
  main = "PCA", xlab = "PC1", ylab = "PC2"
)

text(pca_result$x[, 1:2], labels = data$ro_macro, pos = 3, cex = 0.5, col = "black")

loadings <- pca_result$rotation
loadings[, 1:2]

# Who engages in more diverse forms of using digital technology?

print("Who engages in more diverse forms of using digital technology?")

# Plots

ggplot(data, aes(x = gender_identity, y = diversity_score)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Diversity of Use", x = "Gender Identity", y = "Diversity of Use")

ggplot(data, aes(x = education_completed, y = diversity_score)) +
  geom_boxplot() +
  labs(title = "Education Level and  Diversity of Use", x = "Education Level", y = "Diversity of Use") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = diversity_score)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Diversity of Use", x = "Region of Origin", y = "Diversity of Use")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "diversity_score")], use = "complete.obs")

print(correlation)

test <- cor.test(data$years_internet, data$diversity_score)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$age, data$diversity_score)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(diversity_score ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(diversity_score ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(diversity_score ~ ro_macro, data = data)

summary(anova_region)

# Who does not trust data collection processes?

print("Who does not trust data collection processes?")

# Plots

ggplot(data, aes(x = gender_identity, y = concern_score)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Data Concern", x = "Gender Identity", y = "Data Concern")

ggplot(data, aes(x = education_completed, y = concern_score)) +
  geom_boxplot() +
  labs(title = "Education Level and Data Concern", x = "Education Level", y = "Data Concern") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = concern_score)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Data Concern", x = "Region of Origin", y = "Data Concern")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "concern_score")], use = "complete.obs")

print(correlation)

test <- cor.test(data$years_internet, data$concern_score)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$age, data$concern_score)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(concern_score ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(concern_score ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(concern_score ~ ro_macro, data = data)

summary(anova_region)

# Who uses social media the most?

print("Who uses social media the most?")

# Chi2

contingency_table <- table(data$gender_identity, data$pa_usmp)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$education_completed, data$pa_usmp)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$region_origin, data$pa_usmp)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# Plots

ggplot(data, aes(x = gender_identity, y = pa_usmp)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Social Media Use", x = "Gender Identity", y = "Social Media Use")

ggplot(data, aes(x = education_completed, y = pa_usmp)) +
  geom_boxplot() +
  labs(title = "Education Level and Social Media Use", x = "Education Level", y = "Social Media Use") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = pa_usmp)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Social Media Use", x = "Region of Origin", y = "Social Media Use")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "pa_usmp")], use = "complete.obs")

print(correlation)

test <- cor.test(data$years_internet, data$pa_usmp)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$age, data$pa_usmp)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(pa_usmp ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(pa_usmp ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(pa_usmp ~ ro_macro, data = data)

summary(anova_region)

# Are people who use technology more frequently more concerned about data collection processes?

print("Are people who use technology more frequently more concerned about data collection processes?")

# Correlation

correlation <- cor(data$normalised_literacy, data$concern_score, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = normalised_literacy, y = concern_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Frequency of Use and Concern Score", x = "Frequency of Use", y = "Concern Score")

# Linear Regression

model <- lm(concern_score ~ digital_literacy, data = data)

summary(model)

# Are people who use technology in more diverse ways more concerned about data collection processes?

print("Are people who use technology in more diverse ways more concerned about data collection processes?")

# Correlation

correlation <- cor(data$diversity_score, data$concern_score, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = diversity_score, y = concern_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Diversity of Use and Concern Score", x = "Diversity of Use", y = "Concern Score")

# Linear Regression

model <- lm(concern_score ~ diversity_score, data = data)

summary(model)

# Do people with algorithmic literacy have higher self-perceived competence?

print("Do people with algorithmic literacy have higher self-perceived competence?")

# Correlation

correlation <- cor(data$understand_algorithms, data$competent_digital, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = understand_algorithms, y = competent_digital)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Algorithmic Literacy and Digital Competency", x = "Algorithmic Literacy", y = "Digital Competency")

# Linear Regression

model <- lm(competent_digital ~ understand_algorithms, data = data)

summary(model)

# Does acknowledgement in citations appeal to specific demographics?

print("Does acknowledgement in citations appeal to specific demographics?")

# Chi2

contingency_table <- table(data$gender_identity, data$ms_aic)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$education_completed, data$ms_aic)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$region_origin, data$ms_aic)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# Plot

ggplot(data, aes(x = gender_identity, y = ms_aic)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Acknowledgemnt in Citations", x = "Gender Identity", y = "Acknowledgement in Citations")

ggplot(data, aes(x = education_completed, y = ms_aic)) +
  geom_boxplot() +
  labs(title = "Education Level and Acknowledgemnt in Citations", x = "Education Level", y = "Acknowledgement in Citations") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = ms_aic)) +
  geom_boxplot() +
  labs(title = "Region of Origin and and Acknowledgemnt in Citations", x = "Region of Origin", y = "Acknowledgemnt in Citations")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "ms_aic")], use = "complete.obs")

print(correlation)

# ANOVA

anova_gender <- aov(ms_aic ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(ms_aic ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(ms_aic ~ ro_macro, data = data)

summary(anova_region)

# Does general acknowledgement appeal to specific demographics?

print("Does general acknowledgement appeal to specific demographics?")

# Plots

ggplot(data, aes(x = gender_identity, y = acknowledgement)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Acknowledgement", x = "Gender Identity", y = "Acknowledgement")

ggplot(data, aes(x = education_completed, y = acknowledgement)) +
  geom_boxplot() +
  labs(title = "Education Level and Acknowledgement", x = "Education Level", y = "Acknowledgement") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = acknowledgement)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Acknowledgement", x = "Region of Origin", y = "Acknowledgement")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "acknowledgement")], use = "complete.obs")

print(correlation)

# ANOVA

anova_gender <- aov(acknowledgement ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(acknowledgement ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(acknowledgement ~ ro_macro, data = data)

summary(anova_region)

# Does greater involvement in a project motivate participation?

print("Does greater involvement in a project motivate participation?")

# Chi2

contingency_table <- table(data$ms_hamaritp, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(cs_result)
  print(contingency_table)
} else {
  print("Not significant")
}

# Correlation

correlation <- cor(data$ms_hamaritp, data$interested_application, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = ms_hamaritp, y = interested_application)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Active Role and Application Interest", x = "Active Role", y = "Application Interest")

# Linear Regression

model <- lm(interested_application ~ ms_hamaritp, data = data)

summary(model)

# Who is interested in learning a skill?

print("Who is interested in learning a skill?")

# Chi2

contingency_table <- table(data$gender_identity, data$ms_las)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$education_completed, data$ms_las)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$region_origin, data$ms_las)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# Plots

ggplot(data, aes(x = gender_identity, y = ms_las)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Learning a Skill", x = "Gender Identity", y = "Learning a Skill")

ggplot(data, aes(x = education_completed, y = ms_las)) +
  geom_boxplot() +
  labs(title = "Education Level and Learning a Skill", x = "Education Level", y = "Learning a Skill") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = ms_las)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Learning a Skill", x = "Region of Origin", y = "Learning a Skill")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "ms_las")], use = "complete.obs")

print(correlation)

# ANOVA

anova_gender <- aov(ms_las ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(ms_las ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(ms_las ~ ro_macro, data = data)

summary(anova_region)

# Is there a relationship between use of social media and interest in a mobile application?

print("Is there a relationship between use of social media and interest in a mobile application?")

# Chi2

contingency_table <- table(data$pa_usmp, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# Correlation

correlation <- cor(data$pa_usmp, data$interested_application, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = pa_usmp, y = interested_application)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Social Media Use and Application Interest", x = "Social Media Use", y = "Application Interest")

# Linear Regression

model <- lm(interested_application ~ pa_usmp, data = data)

summary(model)

# What demographics are interested in a mobile application?

print("What demographics are interested in a mobile application?")

# Chi2

contingency_table <- table(data$gender_identity, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$education_completed, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$ro_macro, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(cs_result)
  print(contingency_table)
} else {
  print("Not significant")
}

# Plots

ggplot(data, aes(x = gender_identity, y = interested_application)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Application Interest", x = "Gender Identity", y = "Application Interest")

ggplot(data, aes(x = education_completed, y = interested_application)) +
  geom_boxplot() +
  labs(title = "Education Level and Application Interest", x = "Education Level", y = "Application Interest") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = interested_application)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Application Interest", x = "Region of Origin", y = "Application Interest")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "interested_application")], use = "complete.obs")

print(correlation)

test <- cor.test(data$years_internet, data$interested_application)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$age, data$interested_application)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(interested_application ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(interested_application ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(interested_application ~ ro_macro, data = data)

summary(anova_region)

# What individuals are more likely to use technology for personal development?

print("What individuals are more likely to use technology for personal development?")

# Plots

ggplot(data, aes(x = gender_identity, y = self_development)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Self-Development Use", x = "Gender Identity", y = "Self-Development Use")

ggplot(data, aes(x = education_completed, y = self_development)) +
  geom_boxplot() +
  labs(title = "Education Level and Self-Development Use", x = "Education Level", y = "Self-Development Use") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = self_development)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Self-Development Use", x = "Region of Origin", y = "Self-Development Use")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "self_development")], use = "complete.obs")

print(correlation)

test <- cor.test(data$years_internet, data$self_development)


if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$age, data$self_development)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(self_development ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(self_development ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(self_development ~ ro_macro, data = data)

summary(anova_region)

# Is there a relationship between knowledge of sociolinguistics and knowledge of citizen science?

print("Is there a relationship between knowledge of sociolinguistics and knowledge of citizen science?")

# Correlation

correlation <- cor(data$ft_cs, data$sociolinguistics_score, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = ft_cs, y = sociolinguistics_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Citizen Science and Sociolinguistics Familiarity", x = "Citizen Science Familiarity", y = "Sociolinguistics Familiarity")

# Linear Regression

model <- lm(sociolinguistics_score ~ ft_cs, data = data)

summary(model)

# Does familiarity with sociolinguistics mean greater interest in research participation?

print("Does familiarity with sociolinguistics mean greater interest in research participation?")

# Correlation

correlation <- cor(data$interested_application, data$sociolinguistics_score, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = interested_application, y = sociolinguistics_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Application Interest and Sociolinguistics Familiarity", x = "Application Interest", y = "Sociolinguistics Familiarity")

# Linear Regression

model <- lm(sociolinguistics_score ~ interested_application, data = data)

summary(model)

# Does familiarity with citizen science encourage interest in participation?

print("Does familiarity with citizen science encourage interest in participation?")

# Correlation

correlation <- cor(data$interested_application, data$ft_cs, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = interested_application, y = ft_cs)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Application Interest and Citizen Science Familiarity", x = "Application Interest", y = "Citizen Science Familiarity")

# Linear Regression

model <- lm(ft_cs ~ interested_application, data = data)

summary(model)

# Does concern with data practices link to interest in participation?

print("Does concern with data practices link to interest in participation?")

# Correlation

correlation <- cor(data$interested_application, data$concern_score, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = interested_application, y = concern_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Application Interest and Concern Score", x = "Application Interest", y = "Concern Score")

# Linear Regression

model <- lm(concern_score ~ interested_application, data = data)

summary(model)

# Is familiarity with citizen science linked to previous participation in research?

print("Is familiarity with citizen science linked to previous participation in research?")

# Correlation

correlation <- cor(data$ft_cs, data$contributed_research, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = ft_cs, y = contributed_research)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Citizen Science Familiarity and Previous Research Participation", x = "Citizen Science Familiarity", y = "Previous Research Participation")

# Linear Regression

model <- lm(contributed_research ~ ft_cs, data = data)

summary(model)

# Does familiarity with research processes alter interest in research?

print("Does familiarity with research processes alter interest in research?")

# Chi2

contingency_table <- table(data$interested_application, data$contributed_research)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

# Correlation

correlation <- cor(data$interested_application, data$contributed_research, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = interested_application, y = contributed_research)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Application Interest and Previous Research Participation", x = "Application Interest", y = "Previous Research Participation")

# Linear Regression

model <- lm(contributed_research ~ interested_application, data = data)

summary(model)

# EDIT Does type of project contributed to link to technology use?

print("Does type of project contributed to link to technology use?")

# Clustering

cp_data <- data[, co_pa]

scaled_data <- scale(cp_data)

summary(scaled_data)

set.seed(123)

wcss <- numeric()

for (k in 1:10) {
  kmeans_result <- kmeans(scaled_data, centers = k, nstart = 25)
  wcss[k] <- kmeans_result$tot.withinss
}

plot(1:10, wcss,
  type = "b", pch = 19, frame = FALSE,
  xlab = "Number of clusters", ylab = "Total",
  main = "Elbow Method"
)

kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

kmeans_result$cluster
kmeans_result$centers

data$cluster <- kmeans_result$cluster

# PCA

pca_result <- prcomp(scaled_data)

plot(pca_result$x[, 1:2],
  col = data$cluster, pch = 16,
  main = "PCA", xlab = "PC1", ylab = "PC2"
)

text(pca_result$x[, 1:2], labels = data$ro_macro, pos = 3, cex = 0.5, col = "black")

loadings <- pca_result$rotation
loadings[, 1:2]

# What demographics have engaged in research before?

print("What demographics have engaged in research before?")

# Chi2

contingency_table <- table(data$gender_identity, data$contributed_research)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$education_completed, data$contributed_research)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(cs_result)
  print(contingency_table)
} else {
  print("Not significant")
}

contingency_table <- table(data$ro_macro, data$contributed_research)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
} else {
  print("Not significant")
}

# Plots

ggplot(data, aes(x = gender_identity, y = contributed_research)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Previous Research Participation", x = "Gender Identity", y = "Previous Research Participation")

ggplot(data, aes(x = education_completed, y = contributed_research)) +
  geom_boxplot() +
  labs(title = "Education Level and Previous Research Participation", x = "Education Level", y = "Previous Research Participation") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = contributed_research)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Previous Research Participation", x = "Region of Origin", y = "Previous Research Participation")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "contributed_research")], use = "complete.obs")

print(correlation)

# ANOVA

anova_gender <- aov(contributed_research ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(contributed_research ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(contributed_research ~ ro_macro, data = data)

summary(anova_region)

# Who is more likely to spend time using digital technology for entertainment?

print("Who is more likely to spend time using digital technology for entertainment?")

# Plots

ggplot(data, aes(x = gender_identity, y = entertainment)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Entertainment Use", x = "Gender Identity", y = "Entertainment Use")

ggplot(data, aes(x = education_completed, y = entertainment)) +
  geom_boxplot() +
  labs(title = "Education Level and Entertainment Use", x = "Education Level", y = "Entertainment Use") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = entertainment)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Entertainment Use", x = "Region of Origin", y = "Entertainment Use")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "entertainment")], use = "complete.obs")

print(correlation)

test <- cor.test(data$age, data$entertainment)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$years_internet, data$entertainment)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(entertainment ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(entertainment ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(entertainment ~ ro_macro, data = data)

summary(anova_region)

# Who is more likely to spend time using digital technology for gaming and browsing?

print("Who is more likely to spend time using digital technology for gaming and browsing?")

# Plots

ggplot(data, aes(x = gender_identity, y = bw_pg)) +
  geom_boxplot() +
  labs(title = "Gender Identity, Gaming and Browsing", x = "Gender Identity", y = "Gaming and Browsing")

ggplot(data, aes(x = education_completed, y = bw_pg)) +
  geom_boxplot() +
  labs(title = "Education Level, Gaming and Browsing", x = "Education Level", y = "Gaming and Browsing") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = bw_pg)) +
  geom_boxplot() +
  labs(title = "Region of Origin, Gaming and Browsing", x = "Region of Origin", y = "Gaming and Browsing")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "bw_pg")], use = "complete.obs")

print(correlation)

test <- cor.test(data$age, data$bw_pg)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$years_internet, data$bw_pg)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}


# ANOVA

anova_gender <- aov(bw_pg ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(bw_pg ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(bw_pg ~ ro_macro, data = data)

summary(anova_region)

# What type of project you have contributed to links to engagement?

print("What type of project you have contributed to links to engagement?")

# Correlation

correlation <- cor(application, use = "complete.obs")

print(correlation)

# Does gamification interest those who are interested in an application?

print("Does gamification interest those who are interested in an application?")

# Chi2

contingency_table <- table(data$ms_ca, data$interested_application)

cs_result <- chisq.test(contingency_table)

print(cs_result)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(cs_result)
  print(contingency_table)
} else {
  print("Not significant")
}

# Correlation

correlation <- cor(data$interested_application, data$ms_ca, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = interested_application, y = ms_ca)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Application Interest and Gamification Motivation", x = "Application Interest", y = "Gamification Motivation")

# Linear Regression

model <- lm(ms_ca ~ interested_application, data = data)

summary(model)

# Does gamification interest those who are interested in research in the past and present?

print("Does gamification interest those who are interested in research in the past and present?")

# Logistic Regression

model <- glm(ms_ca ~ past_present, family = binomial(), data = data)

summary(model)

# Correlation

correlation <- cor(data$past_present, data$ms_ca, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = past_present, y = ms_ca)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Research Interest vs Gamification Motivation", x = "Research Interest", y = "Gamification Motivation")

# Linear Regreession

model <- lm(ms_ca ~ past_present, data = data)

summary(model)

# Do concerns over data relate to interest in an application?

print("Do concerns over data relate to interest in an application?")

model <- glm(interested_application ~ concern_score, family = binomial(), data = data)

summary(model)

# Correlation

correlation <- cor(data$interested_application, data$concern_score, use = "complete.obs")

print(correlation)

ggplot(data, aes(x = interested_application, y = concern_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Application Interest and Concern Score", x = "Application Interest", y = "Concern Score")

# Linear Regression

model <- lm(concern_score ~ interested_application, data = data)

summary(model)

# Do concerns over data relate to interest research in the past and present?

print("Do concerns over data relate to interest research in the past and present?")

# Correlation

correlation <- cor(data$past_present, data$concern_score, use = "complete.obs")

print(correlation)

# Plot

ggplot(data, aes(x = past_present, y = concern_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "#0089c4") +
  labs(title = "Research Interest and Concern Score", x = "Research Interest", y = "Concern Score")

# Linear Regression

model <- lm(concern_score ~ past_present, data = data)

summary(model)

# What concerns over data limit interest in an application?

print("What concerns over data limit interest in an application?")

# Correlation

correlation <- cor(limiting_factors, use = "complete.obs")

print(correlation)

# What concerns over data limit interest in research in the past and present?

print("What concerns over data limit interest in research in the past and present?")

# Correlation

correlation <- cor(lf_pp, use = "complete.obs")

print(correlation)

# What kind of digital citizen is interested in an application?

print("What kind of digital citizen is interested in an application?")

# Correlation

correlation <- cor(application, use = "complete.obs")

print(correlation)

# Logistic Regression

model <- glm(interested_application ~ pa_svmomvc, family = binomial(), data = data)

summary(model)

model <- glm(interested_application ~ pa_womsrv, family = binomial(), data = data)

summary(model)

model <- glm(interested_application ~ pa_ucocc, family = binomial(), data = data)

summary(model)

model <- glm(interested_application ~ pa_uatcwat, family = binomial(), data = data)

summary(model)

# Correlation

correlation <- cor(data$pa_svmomvc, data$interested_application, use = "complete.obs")

print(correlation)

test <- cor.test(data$pa_svmomvc, data$interested_application)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

correlation <- cor(data$pa_womsrv, data$interested_application, use = "complete.obs")

print(correlation)

test <- cor.test(data$pa_womsrv, data$interested_application)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

correlation <- cor(data$pa_ucocc, data$interested_application, use = "complete.obs")

print(correlation)

test <- cor.test(data$pa_ucocc, data$interested_application)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

correlation <- cor(data$pa_uatcwat, data$interested_application, use = "complete.obs")

print(correlation)

test <- cor.test(data$pa_uatcwat, data$interested_application)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# What kind of digital citizen is interested in research in the past and present?

print("What kind of digital citizen is interested in research in the past and present?")

# Correlation

correlation <- cor(application_pp, use = "complete.obs")

print(correlation)

# What demographics are interested in participating in research in the past and present?

print("What demographics are interested in participating in research in the past and present?")

# Plots

ggplot(data, aes(x = gender_identity, y = past_present)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Research Interest", x = "Gender Identity", y = "Research Interest")

ggplot(data, aes(x = education_completed, y = past_present)) +
  geom_boxplot() +
  labs(title = "Education Level and Research Interest", x = "Education Level", y = "Research Interest") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = past_present)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Research Interest", x = "Region of Origin", y = "Research Interest")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "past_present")], use = "complete.obs")

print(correlation)

test <- cor.test(data$age, data$past_present)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$years_internet, data$past_present)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(past_present ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(past_present ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(past_present ~ ro_macro, data = data)

summary(anova_region)

# Which people are more motivated by egotistical motivations?

print("Which people are more motivated by egotistical motivations?")

# Plots

ggplot(data, aes(x = gender_identity, y = egotistical_motive)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Egotistical Motivations", x = "Gender Identity", y = "Egotistical Motivations")

ggplot(data, aes(x = education_completed, y = egotistical_motive)) +
  geom_boxplot() +
  labs(title = "Education Level and Egotistical Motivations", x = "Education Level", y = "Egotistical Motivations") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = egotistical_motive)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Egotistical Motivations", x = "Region of Origin", y = "Egotistical Motivations")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "egotistical_motive")], use = "complete.obs")

print(correlation)

test <- cor.test(data$age, data$egotistical_motive)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$years_internet, data$egotistical_motive)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(egotistical_motive ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(egotistical_motive ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(egotistical_motive ~ ro_macro, data = data)

summary(anova_region)

# Which people are more motivated by collective motivations?

print("Which people are more motivated by collective motivations?")

# Plots

ggplot(data, aes(x = gender_identity, y = collective_motive)) +
  geom_boxplot() +
  labs(title = "Gender Identity and Collective Motivations", x = "Gender Identity", y = "Collective Motivations")

ggplot(data, aes(x = education_completed, y = collective_motive)) +
  geom_boxplot() +
  labs(title = "Education Level and Collective Motivations", x = "Education Level", y = "Collective Motivations") +
  scale_x_discrete(labels = function(x) gsub(" ", "\n", x))

ggplot(data, aes(x = ro_macro, y = collective_motive)) +
  geom_boxplot() +
  labs(title = "Region of Origin and Collective Motivations", x = "Region of Origin", y = "Collective Motivations")

# Correlation

correlation <- cor(data[, c("age", "years_internet", "collective_motive")], use = "complete.obs")

print(correlation)

test <- cor.test(data$age, data$collective_motive)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

test <- cor.test(data$years_internet, data$collective_motive)

if (test$p.value < 0.05) {
  print("Significant")
  print(test)
} else {
  print("Not significant")
}

# ANOVA

anova_gender <- aov(collective_motive ~ gender_identity, data = data)

summary(anova_gender)

anova_education <- aov(collective_motive ~ education_completed, data = data)

summary(anova_education)

anova_region <- aov(collective_motive ~ ro_macro, data = data)

summary(anova_region)

# What motivates individuals to be interested in an application?

print("What motivates individuals to be interesed in an application?")

contingency_table <- table(data$ms_fc, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_aic, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_cip, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_hamaritp, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_lasor, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_lam, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_las, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_no, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_tptswfaf, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_ca, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}

contingency_table <- table(data$ms_nota, data$interested_application)

cs_result <- chisq.test(contingency_table)

if (cs_result$p.value < 0.05) {
  print("Significant")
  print(contingency_table)
  print(cs_result)
} else {
  print("Not significant")
}


# Correlation

correlation <- cor(ms_ia, use = "complete.obs")

print(correlation)

# What motivates individuals to be interested in research in the past and present?

print("What motivates individuals to be interested in research in the past and present?")

# Correlation

correlation <- cor(research_motives, use = "complete.obs")

print(correlation)

# OUTPUT

sink(type = "output")