# Open and read .csv file

file <- "public-test.csv"

data <- read.csv(file, fileEncoding = "UTF-8")

# head(data)

# Initiate dictionary to convert questions into column names

questions <- list("X7..How.much.do.you.perform.the.following.activities.on.a.computer..tablet..including.e.readers..or.mobile.phone....." = "perform_activities_", "X12..How.concerned.are.you.about.the.following.when.sharing.data.of.online.communications.with.research......." = "concerned_data_", "X15..How.familiar.are.you.with.the.following.terms...." = "familiar_terms_")

dictionary <- list("Timestamp" = "timestamp", "X1..Are.you.currently.or.have.you.ever.been.a.student.or.staff.member.at.the.University.of.Lausanne." = "university_lausanne",
  "X2..What.is.your.gender.identity." = "gender_identity",
  "X3..What.is.the.highest.level.of.education.you.have.completed." = "level_education",
  "X4..What.is.your.region.of.origin." = "region_origin",
  "X5..What.is.your.age." = "age",
  "X6..For.approximately.how.many.years.have.you.used.the.internet."  = "years_internet",
  "X8..As.a.whole..how.competent.do.you.consider.yourself.in.using.digital.tools.and.services..i.e..the.activities.mentioned.in.question.7.." = "competent_digital",
  "X9..How.well.do.you.understand.how.algorithms.are.used.on.your.data.online...." = "understand_algorithms",
  "X10..Have.you.ever.contributed.data..time..or.skills.to.a.research.project." = "contributed_research",
  "X11..If.you.have.contributed.to.a.research.project..to.which.of.the.following.organisations.have.you.contributed.." = "contributed_organisations",
  "X13..What.would.motivate.you.to.share.data.of.online.communications.with.research..I.would.be.motivated.by..." = "motivate_share",
  "X14..Would.you.be.interested.and.comfortable.in.using.a.mobile.application.that.facilitates.the.collection.and.analysis.of.your.online.communication.data..if.your.concerns.and.motivations.were.met." = "interested_application"
)

# Replace names of basic question columns

replace_names <- function(name) {
  if (name %in% names(dictionary)) {
    return(dictionary[[name]])
  } else {
    return(name)
  }
}

colnames(data) <- sapply(colnames(data), replace_names)

# Clean names of multiple choice answer columns

clean_names <- function(name) {
  for (question in names(questions)) {
    if (startsWith(name, question)) {
      column_name <- sub(question, questions[[question]], name)
      column_name <- gsub("[^[:alnum:]]", "_", column_name)
      column_name <- tolower(column_name)
      column_name <- sub("_+$", "", column_name)
      column_name <- gsub("_+", "_", column_name)
      return(column_name)
    }
  }
  return(name)
}

colnames(data) <- sapply(colnames(data), clean_names)

# colnames(data)

# colSums(is.na(data))

# Calculate and omit missing age data

total_rows <- nrow(data)

missing_age <- is.na(data$age)

not_numbers <- sum(missing_age)

data <- na.omit(data)

# Convert gender identity to binary variable

data$gender_identity <- ifelse(data$gender_identity == "Male", 1,
  ifelse(data$gender_identity == "Female", 0, NA)
)

# Calculate and omit alternative gender data

alternative_gender <- is.na(data$gender_identity)

not_numbers <- not_numbers + sum(alternative_gender)

data <- na.omit(data)

omitted_rows <- (not_numbers / total_rows) * 100

# Note how much data was omitted

cat("Omitted data (missing age, non-binary gender): ", round(omitted_rows, 2), "%\n")

cat("Remaining individuals after data cleaning: ", length(data$gender_identity))

# Recode binary variables

data$university_lausanne <- ifelse(data$university_lausanne == "Yes", 1, 0)

data$contributed_research <- ifelse(data$contributed_research == "Yes", 1, 0)

data$interested_application <- ifelse(data$interested_application == "Yes", 1, 0)

# Recode internet experience variable into quantitative variable

experience <- list("Less than 1 year" = 0.5, "1-2 years" = 1.5, "3-5 years" = 4, "6-10 years" = 8, "11-15 years" = 13, "15-20 years" = 17.5, "More than 20 years" = 25)

data$years_internet <- experience[data$years_internet]

# Convert activities into 7-point Likert scale

activities_scale <- function(data) {
  for (column in colnames(data)) {
    if (startsWith(column, "perform_activities")) {
      data[[column]] <- ifelse(data[[column]] == "Never", 1,
        ifelse(data[[column]] == "Less than once a year", 2,
          ifelse(data[[column]] == "Yearly", 3,
            ifelse(data[[column]] == "Monthly", 4,
              ifelse(data[[column]] == "Weekly", 5,
                ifelse(data[[column]] == "Daily", 6, 7)
              )
            )
          )
        )
      )
    }
  }
  return(data)
}

data <- activities_scale(data)

# Create overall digital literacy score from activities

literacy_score <- function(data) {
  all_activities <- grep("^perform_activities_", colnames(data), value = TRUE)
  data$digital_literacy <- rowSums(data[, all_activities], na.rm = TRUE)
  return(data)
}

data <- literacy_score(data)

# Convert concerns into 5-part Likert scale

concerns_scale <- function(data) {
  for (column in colnames(data)) {
    if (startsWith(column, "concerned_data_")) {
      data[[column]] <- ifelse(data[[column]] == "Not concerned at all", 1,
        ifelse(data[[column]] == "Slightly concerned", 2,
          ifelse(data[[column]] == "Moderately concerned", 3,
            ifelse(data[[column]] == "Very concerned", 4, 5
            )
          )
        )
      )
    }
  }
  return(data)
}

data <- concerns_scale(data)

# Create overall score from concerns

concerns_score <- function(data) {
  all_concerns <- grep("^concerned_data_", colnames(data), value = TRUE)
  data$concerns_score <- rowSums(data[, all_concerns], na.rm = TRUE)
  return(data)
}

data <- concerns_score(data)

# Convert terms into 3-part Likert scale

familiarity_scale <- function(data) {
  for (column in colnames(data)) {
    if (startsWith(column, "familiar_terms_")) {
      data[[column]] <- ifelse(data[[column]] == "Don’t recognise and don’t understand", 1,
        ifelse(data[[column]] == "Recognise but don’t understand", 2, 3)
      )
    }
  }
  return(data)
}

data <- familiarity_scale(data)

# Create overall score from familiarity of terms

familiarity_score <- function(data) {
  all_terms <- grep("^familiar_terms_", colnames(data), value = TRUE)
  data$familiarity_score <- rowSums(data[, all_terms], na.rm = TRUE)
  return(data)
}

data <- familiarity_score(data)

# CHANGE EDUCATION DATA POINTS ***
# GROUP ACTIVITIES INTO COLUMNS ***
# CONVERT MOTIVATIONS INTO DUMMY VARIABLES ***
# COMBINE 3 TOTALLING FUNCTIONS INTO ONE REUSABLE FUNCTION
# PUT DICTIONARIES IN EXTERNAL FILE TO IMPORT