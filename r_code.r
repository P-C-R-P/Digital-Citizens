file <- "public-test.csv"

data <- read.csv(file, fileEncoding = "UTF-8")

# head(data)

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

replace_names <- function(name) {
  if (name %in% names(dictionary)) {
    return(dictionary[[name]])
  } else {
    return(name)
  }
}

colnames(data) <- sapply(colnames(data), replace_names)

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

colnames(data)

# CONVERT TIMESTAMP INTO ID
# BINARY YES/NO
# GET RID OF UNIDENTIFIED GENDER
# CONVERT MALE/FEMALE BINARY
# HOW TO CONVERT EDUCATION LEVEL?
# HOW TO CONVERT REGION?
# GET RID OF MISSING AGES
# GROUP AGES
# HOW DO I NEED TO TREAT AGE FURTHER?
# HOW TO CONVERT EXPERIENCE?
# CONVERT INTO SCALE
# GROUP ACTIVITIES INTO COLUMNS
# CREATE OVERALL DIGITAL LITERACY SCORE
# CONVERT YES/NO BINARY
# CONVERT CONCERN INTO SCALE
# CONVERT CONCERN INTO TOTAL SCORE
# CONVERT MOTIVATIONS INTO DUMMY VARIABLES
# CONVERT YES/NO BINARY
# CONVERT FAMILIARITY INTO SCALE THEN TOTAL SCORE