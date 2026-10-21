
# ============================================
# MVA Project - Heart Disease Dataset
# 01 - Data Loading and Initial Inspection
# ============================================

# Load original Cleveland Heart Disease dataset
url <- paste0(
  "https://archive.ics.uci.edu/ml/",
  "machine-learning-databases/heart-disease/",
  "processed.cleveland.data"
)

# Define column names
column_names <- c(
  "age", "sex", "cp", "trestbps", "chol",
  "fbs", "restecg", "thalach", "exang",
  "oldpeak", "slope", "ca", "thal", "num"
)

# Import data
heart <- read.csv(
  url,
  header = FALSE,
  col.names = column_names,
  na.strings = "?"
)

# Save original data locally
write.csv(heart, "data/heart_disease_raw.csv",
          row.names = FALSE, na = "")

# Initial inspection
dim(heart)
head(heart)
str(heart)
summary(heart)

# Missing values
colSums(is.na(heart))

