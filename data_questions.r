library(ggplot2)

data <- read.csv("final-data.csv")

data <- data[, !names(data) %in% "romansh_canton"]

data$ro_macro <- ifelse(data$region_origin == "non-swiss, european",
  "non-swiss, european",
  ifelse(data$region_origin == "non-swiss, non-european",
    "non-swiss, non-european",
    "swiss"
  )
)

data$normalised_literacy <- data$digital_literacy / 30

data$combined_literacy <- data$normalised_literacy * data$competent_digital

data$normalised_paralanguage <- data$paralanguage_communication / 5

data$general <- (data$productivity + data$finances) / 2

communication <- c("pa_uima", "pa_slof", "pa_mvocc", "pa_svmomvc", "pa_usmp", "pa_sospov", "pa_womsrv", "pa_womsrv", "pa_stomm", "pa_e", "pa_piofogc")

data$communication <- rowSums(data[communication], na.rm = TRUE) / length(communication)

instrumental <- c("pa_uatcwat", "pa_ucor", "pa_saot", "pa_b", "pa_l", "pa_bw", "pa_si", "pa_ucocc", "pa_ml", "pa_sogdcs")

expressive <- c("pa_usmp", "pa_womsrv", "pa_svmomvc", "pa_mgom", "pa_epov", "pa_tpov", "pa_sospov", "pa_stomm", "pa_piofogc")

data$instrumental <- rowSums(data[instrumental], na.rm = TRUE) / length(instrumental)

data$expressive <- rowSums(data[expressive], na.rm = TRUE) / length(expressive)

data$gender_identity <- as.factor(data$gender_identity)
data$education_completed <- as.factor(data$education_completed)
data$ro_macro <- as.factor(data$ro_macro)
data$age <- as.numeric(data$age)
data$years_internet <- as.numeric(data$years_internet)
data$swiss <- as.factor(data$swiss)
data$european <- as.factor(data$european)

# Who uses digital technology regularly?

regularly <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "normalised_literacy")]

summary(regularly)

ggplot(regularly, aes(x = gender_identity, y = normalised_literacy)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(regularly, aes(x = education_completed, y = normalised_literacy)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(regularly, aes(x = ro_macro, y = normalised_literacy)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- regularly[, c("age", "years_internet", "normalised_literacy")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(normalised_literacy ~ gender_identity, data = regularly)
summary(anova_gender)

anova_education <- aov(normalised_literacy ~ education_completed, data = regularly)
summary(anova_education)

anova_region <- aov(normalised_literacy ~ ro_macro, data = regularly)
summary(anova_region)

# Who uses digital technology effectively?

effectively <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "competent_digital")]

summary(effectively)

ggplot(effectively, aes(x = gender_identity, y = competent_digital)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(effectively, aes(x = education_completed, y = competent_digital)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(effectively, aes(x = ro_macro, y = competent_digital)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- effectively[, c("age", "years_internet", "competent_digital")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(competent_digital ~ gender_identity, data = effectively)
summary(anova_gender)

anova_education <- aov(competent_digital ~ education_completed, data = effectively)
summary(anova_education)

anova_region <- aov(competent_digital ~ ro_macro, data = effectively)
summary(anova_region)

###

effectively_combined <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "combined_literacy")]

summary(effectively_combined)

ggplot(effectively_combined, aes(x = gender_identity, y = combined_literacy)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(effectively_combined, aes(x = education_completed, y = combined_literacy)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(effectively_combined, aes(x = ro_macro, y = combined_literacy)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- effectively_combined[, c("age", "years_internet", "combined_literacy")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(combined_literacy ~ gender_identity, data = effectively_combined)
summary(anova_gender)

anova_education <- aov(combined_literacy ~ education_completed, data = effectively_combined)
summary(anova_education)

anova_region <- aov(combined_literacy ~ ro_macro, data = effectively_combined)
summary(anova_region)

# Does digital frequency predict digital confidence?

model <- lm(competent_digital ~ normalised_literacy, data = data)

summary(model)

# Does age predict digital literacy?

model <- lm(normalised_literacy ~ age, data = data)

summary(model)

model <- lm(combined_literacy ~ age, data = data)

summary(model)

model <- lm(competent_digital ~ age, data = data)

summary(model)

# What demographic produces the most paralanguage output?

paralanguage <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "normalised_paralanguage")]

summary(paralanguage)

ggplot(paralanguage, aes(x = gender_identity, y = normalised_paralanguage)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(paralanguage, aes(x = education_completed, y = normalised_paralanguage)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(paralanguage, aes(x = ro_macro, y = normalised_paralanguage)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- paralanguage[, c("age", "years_internet", "normalised_paralanguage")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(normalised_paralanguage ~ gender_identity, data = paralanguage)
summary(anova_gender)

anova_education <- aov(normalised_paralanguage ~ education_completed, data = paralanguage)
summary(anova_education)

anova_region <- aov(normalised_paralanguage ~ ro_macro, data = paralanguage)
summary(anova_region)

# What demographic produces the most communication output?

all_communication <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "communication")]

summary(all_communication)

ggplot(all_communication, aes(x = gender_identity, y = communication)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(all_communication, aes(x = education_completed, y = communication)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(all_communication, aes(x = ro_macro, y = communication)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- all_communication[, c("age", "years_internet", "communication")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(communication ~ gender_identity, data = all_communication)
summary(anova_gender)

anova_education <- aov(communication ~ education_completed, data = all_communication)
summary(anova_education)

anova_region <- aov(communication ~ ro_macro, data = all_communication)
summary(anova_region)

# Do demographic characteristics predict instrumental and expressive uses?

instrumental_use <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "instrumental")]

summary(instrumental_use)

ggplot(instrumental_use, aes(x = gender_identity, y = instrumental)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(instrumental_use, aes(x = education_completed, y = instrumental)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(instrumental_use, aes(x = ro_macro, y = instrumental)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- instrumental_use[, c("age", "years_internet", "instrumental")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(instrumental ~ gender_identity, data = instrumental_use)
summary(anova_gender)

anova_education <- aov(instrumental ~ education_completed, data = instrumental_use)
summary(anova_education)

anova_region <- aov(instrumental ~ ro_macro, data = instrumental_use)
summary(anova_region)

###

expressive_use <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "expressive")]

summary(expressive_use)

ggplot(expressive_use, aes(x = gender_identity, y = expressive)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(expressive_use, aes(x = education_completed, y = expressive)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(expressive_use, aes(x = ro_macro, y = expressive)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- expressive_use[, c("age", "years_internet", "expressive")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(expressive ~ gender_identity, data = expressive_use)
summary(anova_gender)

anova_education <- aov(expressive ~ education_completed, data = expressive_use)
summary(anova_education)

anova_region <- aov(expressive ~ ro_macro, data = expressive_use)
summary(anova_region)

# How do people use digital technology?

pa_columns <- grep("^pa_", colnames(data), value = TRUE)

pa_data <- data[, pa_columns]

scaled_data <- scale(pa_data)

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

slope <- (wss[10] - wss[5]) / (10 - 5)
intercept <- wss[5] - slope * 5

abline(a = intercept, b = slope, col = "red", lwd = 2, lty = 2)

kmeans_result <- kmeans(scaled_data, centers = 5, nstart = 25)

kmeans_result$cluster
kmeans_result$centers

data$cluster <- kmeans_result$cluster

pca_result <- prcomp(scaled_data)

plot(pca_result$x[, 1:2],
  col = data$cluster, pch = 16,
  main = "PCA - Clusters", xlab = "PC1", ylab = "PC2"
)

text(pca_result$x[, 1:2], labels = data$ro_macro, pos = 3, cex = 0.7, col = "black")

loadings <- pca_result$rotation
loadings[, 1:2]

# Do young people engage in more diverse uses of digital technology?

diversity_column <- apply(data[, pa_columns], 1, function(x) sum(x >= 3 & x <= 7))

data$diversity_score <- diversity_column / 30

diversity <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "diversity_score")]

summary(diversity)

ggplot(diversity, aes(x = gender_identity, y = diversity_score)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(diversity, aes(x = education_completed, y = diversity_score)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(diversity, aes(x = ro_macro, y = diversity_score)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- diversity[, c("age", "years_internet", "diversity_score")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(diversity_score ~ gender_identity, data = diversity)
summary(anova_gender)

anova_education <- aov(diversity_score ~ education_completed, data = diversity)
summary(anova_education)

anova_region <- aov(diversity_score ~ ro_macro, data = diversity)
summary(anova_region)

# How much do people trust digital technology?

cd_columns <- grep("^cd_", colnames(data), value = TRUE)

cd_data <- data[, cd_columns]

data$concern_score <- rowSums(cd_data, na.rm = TRUE) / length(cd_columns)

level_concern <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "concern_score")]

summary(level_concern)

ggplot(level_concern, aes(x = gender_identity, y = concern_score)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(level_concern, aes(x = education_completed, y = concern_score)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(level_concern, aes(x = ro_macro, y = concern_score)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- level_concern[, c("age", "years_internet", "concern_score")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(concern_score ~ gender_identity, data = level_concern)
summary(anova_gender)

anova_education <- aov(concern_score ~ education_completed, data = level_concern)
summary(anova_education)

anova_region <- aov(concern_score ~ ro_macro, data = level_concern)
summary(anova_region)

# Do young people tend to use social media more than older people?

social_media <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "pa_usmp")]

summary(social_media)

ggplot(social_media, aes(x = gender_identity, y = pa_usmp)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(social_media, aes(x = education_completed, y = pa_usmp)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(social_media, aes(x = ro_macro, y = pa_usmp)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- social_media[, c("age", "years_internet", "pa_usmp")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(pa_usmp ~ gender_identity, data = social_media)
summary(anova_gender)

anova_education <- aov(pa_usmp ~ education_completed, data = social_media)
summary(anova_education)

anova_region <- aov(pa_usmp ~ ro_macro, data = social_media)
summary(anova_region)

# Do people who use technology more frequently understand more about data privacy?

cor_matrix <- cor(data$normalised_literacy, data$concern_score, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = normalised_literacy, y = concern_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(concern_score ~ digital_literacy, data = data)
summary(model)

# Do people who use technology in more diverse ways understand more about data privacy?

cor_matrix <- cor(data$diversity_score, data$concern_score, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = diversity_score, y = concern_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(concern_score ~ diversity_score, data = data)
summary(model)

# Do people with algorithmic literacy have higher self-perceived competence?

cor_matrix <- cor(data$understand_algorithms, data$competent_digital, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = understand_algorithms, y = competent_digital)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(competent_digital ~ understand_algorithms, data = data)
summary(model)

# Does acknowledgement appeal more to younger demographics?

acknowledge <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "ms_aic")]

summary(acknowledge)

ggplot(acknowledge, aes(x = gender_identity, y = ms_aic)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(acknowledge, aes(x = education_completed, y = ms_aic)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(acknowledge, aes(x = ro_macro, y = ms_aic)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- acknowledge[, c("age", "years_internet", "ms_aic")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(ms_aic ~ gender_identity, data = acknowledge)
summary(anova_gender)

anova_education <- aov(ms_aic ~ education_completed, data = acknowledge)
summary(anova_education)

anova_region <- aov(ms_aic ~ ro_macro, data = acknowledge)
summary(anova_region)

###

data$acknowledgement <- (data$ms_aic + data$ms_cip) / 2

acknowledge <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "acknowledgement")]

summary(acknowledge)

ggplot(acknowledge, aes(x = gender_identity, y = acknowledgement)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(acknowledge, aes(x = education_completed, y = acknowledgement)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(acknowledge, aes(x = ro_macro, y = acknowledgement)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- acknowledge[, c("age", "years_internet", "acknowledgement")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(acknowledgement ~ gender_identity, data = acknowledge)
summary(anova_gender)

anova_education <- aov(acknowledgement ~ education_completed, data = acknowledge)
summary(anova_education)

anova_region <- aov(acknowledgement ~ ro_macro, data = acknowledge)
summary(anova_region)

# Does greater involvement in a project motivate participation?

cor_matrix <- cor(data$ms_hamaritp, data$interested_application, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = ms_hamaritp, y = interested_application)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(interested_application ~ ms_hamaritp, data = data)
summary(model)

# Are young people interested more in learning a skill than other groups?

learning_skill <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "ms_las")]

summary(learning_skill)

ggplot(learning_skill, aes(x = gender_identity, y = ms_las)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(learning_skill, aes(x = education_completed, y = ms_las)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(learning_skill, aes(x = ro_macro, y = ms_las)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- learning_skill[, c("age", "years_internet", "ms_las")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(ms_las ~ gender_identity, data = learning_skill)
summary(anova_gender)

anova_education <- aov(ms_las ~ education_completed, data = learning_skill)
summary(anova_education)

anova_region <- aov(ms_las ~ ro_macro, data = learning_skill)
summary(anova_region)

# Is there a relationship between use of social media and interest in a mobile application?

cor_matrix <- cor(data$pa_usmp, data$interested_application, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = pa_usmp, y = interested_application)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(interested_application ~ pa_usmp, data = data)
summary(model)

# What demographics are interested in a mobile application?

application <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "interested_application")]

summary(application)

ggplot(application, aes(x = gender_identity, y = interested_application)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(application, aes(x = education_completed, y = interested_application)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(application, aes(x = ro_macro, y = interested_application)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- application[, c("age", "years_internet", "interested_application")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(interested_application ~ gender_identity, data = application)
summary(anova_gender)

anova_education <- aov(interested_application ~ education_completed, data = application)
summary(anova_education)

anova_region <- aov(interested_application ~ ro_macro, data = application)
summary(anova_region)

# Are better-educated individuals more likely to use technology for personal development?

colnames(data)[colnames(data) == "self-development"] <- "self_development"

development <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "self_development")]

summary(development)

ggplot(development, aes(x = gender_identity, y = self_development)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(development, aes(x = education_completed, y = self_development)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(development, aes(x = ro_macro, y = self_development)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- development[, c("age", "years_internet", "self_development")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(self_development ~ gender_identity, data = development)
summary(anova_gender)

anova_education <- aov(self_development ~ education_completed, data = development)
summary(anova_education)

anova_region <- aov(self_development ~ ro_macro, data = development)
summary(anova_region)

# Is there a relationship between knowledge of sociolinguistics and citizen science?

data$sociolinguistics_score <- rowSums(data[, c("ft_cmc", "ft_pos", "ft_p", "ft_i")], na.rm = TRUE) / 4

cor_matrix <- cor(data$ft_cs, data$sociolinguistics_score, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = ft_cs, y = sociolinguistics_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(sociolinguistics_score ~ ft_cs, data = data)
summary(model)

# Does familiarity with sociolinguistics mean greater interest in research participation?

cor_matrix <- cor(data$interested_application, data$sociolinguistics_score, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = interested_application, y = sociolinguistics_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(sociolinguistics_score ~ interested_application, data = data)
summary(model)

# Does familiarity with citizen science encourage interest in participation?

cor_matrix <- cor(data$interested_application, data$ft_cs, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = interested_application, y = ft_cs)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(ft_cs ~ interested_application, data = data)
summary(model)

# Does higher concern with data practices link to lower interest in participation?

cor_matrix <- cor(data$interested_application, data$concern_score, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = interested_application, y = concern_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(concern_score ~ interested_application, data = data)
summary(model)

# Is familiarity with citizen science linked to previous participation in research?

data$previous_research <- rowSums(data[, c("co_auoei", "co_ahomrc", "co_ago", "co_acono", "co_acofc")], na.rm = TRUE) / 5

cor_matrix <- cor(data$ft_cs, data$previous_research, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = ft_cs, y = previous_research)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(previous_research ~ ft_cs, data = data)
summary(model)

# Does familiarity with research processes alter interest in research?

cor_matrix <- cor(data$interested_application, data$previous_research, use = "complete.obs")

print(cor_matrix)

ggplot(data, aes(x = interested_application, y = previous_research)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Digital Literacy vs Concern Score", x = "Digital Literacy", y = "Concern Score")

model <- lm(previous_research ~ interested_application, data = data)
summary(model)

# What type of project you have contributed to links to technology use?

co_columns <- grep("^co_", colnames(data), value = TRUE)

co_pa <- c(pa_columns, co_columns)

cp_data <- data[, co_pa]

scaled_data <- scale(cp_data)

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

kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)

kmeans_result$cluster
kmeans_result$centers

data$cluster <- kmeans_result$cluster

pca_result <- prcomp(scaled_data)

plot(pca_result$x[, 1:2],
  col = data$cluster, pch = 16,
  main = "PCA - Clusters", xlab = "PC1", ylab = "PC2"
)

text(pca_result$x[, 1:2], labels = data$ro_macro, pos = 3, cex = 0.7, col = "black")

loadings <- pca_result$rotation
loadings[, 1:2]

# What demographics have engaged in research before?

research <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "previous_research")]

summary(research)

ggplot(research, aes(x = gender_identity, y = previous_research)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(research, aes(x = education_completed, y = previous_research)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(research, aes(x = ro_macro, y = previous_research)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- research[, c("age", "years_internet", "previous_research")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(previous_research ~ gender_identity, data = research)
summary(anova_gender)

anova_education <- aov(previous_research ~ education_completed, data = research)
summary(anova_education)

anova_region <- aov(previous_research ~ ro_macro, data = research)
summary(anova_region)

# Are less educated individuals more likely to spend time gaming and scrolling?

entertain_use <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "entertainment")]

summary(entertain_use)

ggplot(entertain_use, aes(x = gender_identity, y = entertainment)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(entertain_use, aes(x = education_completed, y = entertainment)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(entertain_use, aes(x = ro_macro, y = entertainment)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- entertain_use[, c("age", "years_internet", "entertainment")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(entertainment ~ gender_identity, data = entertain_use)
summary(anova_gender)

anova_education <- aov(entertainment ~ education_completed, data = entertain_use)
summary(anova_education)

anova_region <- aov(entertainment ~ ro_macro, data = entertain_use)
summary(anova_region)

###

data$bw_pg <- rowSums(data[, c("pa_bw", "pa_pg")], na.rm = TRUE) / 2

scrolling_use <- data[, c("gender_identity", "education_completed", "ro_macro", "age", "years_internet", "bw_pg")]

summary(scrolling_use)

ggplot(scrolling_use, aes(x = gender_identity, y = bw_pg)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Gender Identity", x = "Gender Identity", y = "Digital Literacy")

ggplot(scrolling_use, aes(x = education_completed, y = bw_pg)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Education Level", x = "Education Level", y = "Digital Literacy")

ggplot(scrolling_use, aes(x = ro_macro, y = bw_pg)) +
  geom_boxplot() +
  labs(title = "Digital Literacy by Region of Origin", x = "Region of Origin", y = "Digital Literacy")

numeric_vars <- scrolling_use[, c("age", "years_internet", "bw_pg")]

cor_matrix <- cor(numeric_vars, use = "complete.obs")
print(cor_matrix)

anova_gender <- aov(bw_pg ~ gender_identity, data = scrolling_use)
summary(anova_gender)

anova_education <- aov(bw_pg ~ education_completed, data = scrolling_use)
summary(anova_education)

anova_region <- aov(bw_pg ~ ro_macro, data = scrolling_use)
summary(anova_region)

# Does gamification interest those who are interested in engaging in research?
# combined research score + app

# Do concerns over data limit engagement?
# concerns limit app + combined

# What kind of digital citizen is interested in contributing to research?
# what use best predicts app + combined

# What type of project you have contributed to links to engagement?
# type vs mobile app

# Which people are more motivated by egotistical or community-based motivations?
# create egotistical / community-based categories

# RESEARCH:

# What demographics are interested in participating in research?
# simple mobile app + combined score with prior research + previous

# What motivates individuals to contribute to research?
# combined research score and simple motivation of app

# What inhibits individuals from contributing to research?
# combined research score and simple motivation of app
