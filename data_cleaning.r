# Translate French questionnaire

source("translations.r")

french_files <- c("public-french.csv", "university-french.csv")

french_data <- lapply(french_files, function(file) {
  read.csv(file, fileEncoding = "UTF-8")
})

french_data <- do.call(rbind, french_data)

colnames(french_data) <- translations_columns[colnames(french_data)]

translate_individuals <- function(data, translations) {
  for (column in colnames(data)) {
    if (is.character(data[[column]])) {
      data[[column]] <- sapply(data[[column]], function(value) {
        if (!is.na(value) && value %in% names(translations)) {
          return(translations[[value]])
        } else {
          return(value)
        }
      }, USE.NAMES = FALSE)
    }
  }
  return(data)
}

french_data <- translate_individuals(french_data, translations_individuals)

# Open and read .csv files

english_files <- c("digital-english.csv", "public-english.csv", "university-english.csv")

english_data <- lapply(english_files, function(file) {
  read.csv(file, fileEncoding = "UTF-8")
})

english_data <- do.call(rbind, english_data)

data <- rbind(french_data, english_data)

# Replace names of basic question columns

# Initiate dictionary to convert questions into column names

questions <- list("X7..How.much.do.you.perform.the.following.activities.on.a.computer..tablet..including.e.readers..or.mobile.phone....." = "pa_", "X12..How.concerned.are.you.about.the.following.when.sharing.data.of.online.communications.with.research......." = "cd_", "X15..How.familiar.are.you.with.the.following.terms...." = "ft_")

dictionary <- list(
  "Timestamp" = "timestamp", "X1..Are.you.currently.or.have.you.ever.been.a.student.or.staff.member.at.the.University.of.Lausanne." = "university_lausanne",
  "X2..What.is.your.gender.identity." = "gender_identity",
  "X3..What.is.the.highest.level.of.education.you.have.completed." = "education_completed",
  "X4..What.is.your.region.of.origin." = "region_origin",
  "X5..What.is.your.age." = "age",
  "X6..For.approximately.how.many.years.have.you.used.the.internet." = "years_internet",
  "X8..As.a.whole..how.competent.do.you.consider.yourself.in.using.digital.tools.and.services..i.e..the.activities.mentioned.in.question.7.." = "competent_digital",
  "X9..How.well.do.you.understand.how.algorithms.are.used.on.your.data.online...." = "understand_algorithms",
  "X10..Have.you.ever.contributed.data..time..or.skills.to.a.research.project." = "contributed_research",
  "X11..If.you.have.contributed.to.a.research.project..to.which.of.the.following.organisations.have.you.contributed.." = "contributed_organisations",
  "X13..What.would.motivate.you.to.share.data.of.online.communications.with.research..I.would.be.motivated.by..." = "motivate_share",
  "X14..Would.you.be.interested.and.comfortable.in.using.a.mobile.application.that.facilitates.the.collection.and.analysis.of.your.online.communication.data..if.your.concerns.and.motivations.were.met." = "interested_application"
)

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

# Create new dummy variables for regional origin

data$region_origin <- tolower(data$region_origin)

german_cantons <- c("zürich", "bern", "luzern", "uri", "schwyz", "obwalden", "nidwalden", "glarus", "zug", "fribourg", "solothurn", "basel-stadt", "basel-landschaft", "schaffhausen", "appenzell ausserrhoden", "appenzell innerhoden", "st. gallen", "graubünden", "aargau", "thurgau", "valais")

romansh_cantons <- c("graubünden")

french_cantons <- c("bern", "fribourg", "vaud", "valais", "neuchâtel", "genève", "jura")

italian_cantons <- c("graubünden", "ticino")

swiss <- c(german_cantons, romansh_cantons, french_cantons, italian_cantons)

swiss <- unique(swiss)

non_swiss <- c("non-swiss, european", "non-swiss, non-european")

european <- c(swiss, "non-swiss, european")

non_european <- c("non-swiss, non-european")

data$german_canton <- ifelse(data$region_origin %in% german_cantons, 1, 0)
data$romansh_canton <- ifelse(data$region_origin %in% romansh_cantons, 1, 0)
data$french_canton <- ifelse(data$region_origin %in% french_cantons, 1, 0)
data$italian_canton <- ifelse(data$region_origin %in% italian_cantons, 1, 0)
data$swiss <- ifelse(data$region_origin %in% swiss, 1, 0)
data$non_swiss <- ifelse(data$region_origin %in% non_swiss, 1, 0)
data$european <- ifelse(data$region_origin %in% european, 1, 0)
data$non_european <- ifelse(data$region_origin == non_european, 1, 0)

# Recode internet experience variable into quantitative variable

experience <- list("Less than 1 year" = 0.5, "1-2 years" = 1.5, "3-5 years" = 4, "6-10 years" = 8, "11-15 years" = 13, "15-20 years" = 17.5, "More than 20 years" = 25)

data$years_internet <- experience[data$years_internet]

# Convert activities into 7-point Likert scale

activities_scale <- function(data) {
  for (column in colnames(data)) {
    if (startsWith(column, "pa_")) {
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
  all_activities <- grep("^pa_", colnames(data), value = TRUE)
  data$digital_literacy <- rowSums(data[, all_activities], na.rm = TRUE)
  return(data)
}

data <- literacy_score(data)

# Convert concerns into 5-part Likert scale

concerns_scale <- function(data) {
  for (column in colnames(data)) {
    if (startsWith(column, "cd_")) {
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
  all_concerns <- grep("^cd_", colnames(data), value = TRUE)
  data$concerns_score <- rowSums(data[, all_concerns], na.rm = TRUE)
  return(data)
}

data <- concerns_score(data)

# Convert terms into 3-part Likert scale

familiarity_scale <- function(data) {
  for (column in colnames(data)) {
    if (startsWith(column, "ft_")) {
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
  all_terms <- grep("^ft_", colnames(data), value = TRUE)
  data$familiarity_score <- rowSums(data[, all_terms], na.rm = TRUE)
  return(data)
}

data <- familiarity_score(data)

# Shorten names of activities for ease of access

modify_names <- function(names) {
  names <- sapply(names, function(name) {
    pa <- grepl("^pa_", name)
    cd <- grepl("^cd_", name)
    ft <- grepl("^ft_", name)
    if (pa | cd | ft) {
      prefix <- ifelse(pa, "pa", ifelse(cd, "cd", "ft"))
      rest <- sub(paste0("^", prefix, "_"), "", name)
      words <- unlist(strsplit(rest, "[^[:alnum:]]+"))
      code <- paste(tolower(substr(words, 1, 1)), collapse = "")
      return(paste(prefix, code, sep = "_"))
    } else {
      return(name)
    }
  })
  return(names)
}

colnames(data) <- modify_names(colnames(data))

# Group activities into categories

self_development <- c("pa_l", "pa_rbboa", "pa_si")

entertainment <- c("pa_wftov", "pa_ltmpoa", "pa_pg", "pa_rbboa", "pa_bw")

productivity <- c("pa_ucor", "pa_saot", "pa_ctt", "pa_si", "pa_ml", "pa_bw", "pa_ugont", "pa_uatcwat")

creativity <- c("pa_mgom", "pa_epov", "pa_tpov", "pa_womsrv")

finances <- c("pa_b", "pa_sogdcs", "pa_ucocc")

rt_communication <- c("pa_uima", "pa_slof", "pa_mvocc", "pa_svmomvc")

asynchronous_communication <- c("pa_usmp", "pa_slof", "pa_sospov", "pa_womsrv", "pa_stomm", "pa_e")

group_communication <- c("pa_piofogc", "pa_womsrv", "pa_usmp")

audio_communication <- c("pa_svmomvc", "pa_mvocc")

visual_communication <- c("pa_mvocc", "pa_sospov", "pa_womsrv", "pa_slof")

written_communication <- c("pa_stomm", "pa_e", "pa_usmp", "pa_uima")

multimedia_communication <- c("pa_usmp", "pa_uima", "pa_mvocc")

paralanguage_communication <- c("pa_uima", "pa_usmp", "pa_stomm", "pa_e", "pa_piofogc")

# Create new columns for categories with scores

data$self_development <- rowSums(data[self_development], na.rm = TRUE) / length(self_development)
data$entertainment <- rowSums(data[entertainment], na.rm = TRUE) / length(entertainment)
data$productivity <- rowSums(data[productivity], na.rm = TRUE) / length(productivity)
data$creativity <- rowSums(data[creativity], na.rm = TRUE) / length(creativity)
data$finances <- rowSums(data[finances], na.rm = TRUE) / length(finances)
data$rt_communication <- rowSums(data[rt_communication], na.rm = TRUE) / length(rt_communication)
data$asynchronous_communication <- rowSums(data[asynchronous_communication], na.rm = TRUE) / length(asynchronous_communication)
data$group_communication <- rowSums(data[group_communication], na.rm = TRUE) / length(group_communication)
data$audio_communication <- rowSums(data[audio_communication], na.rm = TRUE) / length(audio_communication)
data$visual_communication <- rowSums(data[visual_communication], na.rm = TRUE) / length(visual_communication)
data$written_communication <- rowSums(data[written_communication], na.rm = TRUE) / length(written_communication)
data$multimedia_communication <- rowSums(data[multimedia_communication], na.rm = TRUE) / length(multimedia_communication)
data$paralanguage_communication <- rowSums(data[paralanguage_communication], na.rm = TRUE) / length(paralanguage_communication)

# Convert motivations for sharing data into dummy variables

motivations <- list(
  "financial compensation" = "compensation financière",
  "acknowledgement in citations" = "reconnaissance dans les citations",
  "co-authorship in publications"= "co-auteur dans les publications",
  "having a more active role in the project" = "avoir un rôle plus actif dans le projet",
  "learning about science or research" = "apprendre sur la science ou la recherche",
  "learning about myself" = "apprendre sur moi-même",
  "learning a skill" = "apprendre une compétence", "networking opportunities" = "opportunités de mise en réseau", "the possibility to share with friends and family" = "la possibilité de partager avec des ami.e.s et la famille",
  "competitive aspects" = "aspects compétitifs",
  "none of the above" = "aucune des réponses ci-dessus"
)

# First create function to convert French into English

for (individual in seq_along(motivations)) {
  french <- motivations[[individual]]
  english <- names(motivations)[individual]
  data$motivate_share <- gsub(french, english, data$motivate_share, ignore.case = TRUE)
}

motivation_columns <- sapply(names(motivations), function(english) {
  words <- unlist(strsplit(gsub("[^a-zA-Z ]", "", english), " "))
  paste0("ms_", paste0(substr(words, 1, 1), collapse = ""))
})

# Set default value to zero

for (column in motivation_columns) {
  data[[column]] <- 0
}

# Check if each motivation is included in response and if so change value to 1

for (individual in seq_along(motivations)) {
  motivation <- names(motivations)[individual]
  code <- motivation_columns[individual]
  data[[code]] <- ifelse(grepl(motivation, data$motivate_share, ignore.case = TRUE), 1, 0)
}

# If value of "none of the above" is 1 then set all other motivation-related columns to 0

for (row in seq_len(nrow(data))) {
  if (data$ms_nota[row] == 1) {
    data[row, setdiff(motivation_columns, "ms_nota")] <- 0
  }
}

# Convert research project contributions into dummy variables

projects <- list(
  "A university or educational institution" = "Une université ou une institution éducative",
  "A hospital or medical research centre" = "Un hôpital ou un centre de recherche médicale",
  "A government organisation" = "Une organisation gouvernementale",
  "A charity or non-government organisation" = "Une organisation caritative ou non gouvernementale",
  "A commercial or for-profit company" = "Une entreprise commerciale ou à but lucratif",
  "I have not contributed to a research project." = "Je n'ai pas contribué à un projet de recherche."
)

# First convert French into English

for (individual in seq_along(projects)) {
  french <- projects[[individual]]
  english <- names(projects)[individual]
  data$contributed_organisations <- gsub(french, english, data$contributed_organisations, ignore.case = TRUE)
}

research_columns <- sapply(names(projects), function(english) {
  words <- unlist(strsplit(gsub("[^a-zA-Z ]", "", english), " "))
  tolower(paste0("co_", paste0(substr(words, 1, 1), collapse = "")))
})

# Set default value to zero

for (column in research_columns) {
  data[[column]] <- 0
}

# Check if each motivation is included in response and if so change value to 1

for (individual in seq_along(projects)) {
  project <- names(projects)[individual]
  code <- research_columns[individual]
  data[[code]] <- ifelse(grepl(project, data$contributed_organisations, ignore.case = TRUE), 1, 0)
}

# If value of "I have not contributed to a research project" is 1 then set all other project-related columns to 0

for (row in seq_len(nrow(data))) {
  if (data$co_ihnctarp[row] == 1) {
    data[row, setdiff(research_columns, "co_ihnctarp")] <- 0
  }
}

# Flatten the data

data <- lapply(data, unlist)

write.csv(data, "cleaned-data.csv", row.names = FALSE)