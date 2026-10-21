
# ============================================
# MVA Project - Heart Disease Dataset
# 02 - Data Preprocessing
# ============================================

# --------------------------------------------
# 1. Load data
# --------------------------------------------

heart_raw <- read.csv(
  "data/heart_disease_raw.csv",
  na.strings = c("", "?")
)

# Preserve original data
heart_clean <- heart_raw


# --------------------------------------------
# 2. Rename variables
# --------------------------------------------

names(heart_clean) <- c(
  "age",
  "sex",
  "chest_pain",
  "rest_bp",
  "cholesterol",
  "fasting_bs",
  "rest_ecg",
  "max_hr",
  "exercise_angina",
  "st_depression",
  "st_slope",
  "vessels",
  "thal",
  "heart_disease"
)


# --------------------------------------------
# 3. Convert categorical variables
# --------------------------------------------

# Sex
heart_clean$sex <- factor(
  heart_clean$sex,
  levels = c(0, 1),
  labels = c("Female", "Male")
)

# Chest pain type
heart_clean$chest_pain <- factor(
  heart_clean$chest_pain,
  levels = c(1, 2, 3, 4),
  labels = c(
    "Typical",
    "Atypical",
    "Non-anginal",
    "Asymptomatic"
  )
)

# Fasting blood sugar
heart_clean$fasting_bs <- factor(
  heart_clean$fasting_bs,
  levels = c(0, 1),
  labels = c("<=120", ">120")
)

# Resting ECG
heart_clean$rest_ecg <- factor(
  heart_clean$rest_ecg,
  levels = c(0, 1, 2),
  labels = c(
    "Normal",
    "ST-T abnormality",
    "LV hypertrophy"
  )
)

# Exercise-induced angina
heart_clean$exercise_angina <- factor(
  heart_clean$exercise_angina,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# ST segment slope
heart_clean$st_slope <- factor(
  heart_clean$st_slope,
  levels = c(1, 2, 3),
  labels = c(
    "Upsloping",
    "Flat",
    "Downsloping"
  )
)

# Thallium test result
heart_clean$thal <- factor(
  heart_clean$thal,
  levels = c(3, 6, 7),
  labels = c(
    "Normal",
    "Fixed defect",
    "Reversible defect"
  )
)

# Heart disease diagnosis
heart_clean$heart_disease <- factor(
  heart_clean$heart_disease,
  levels = c(0, 1, 2, 3, 4),
  labels = c(
    "None",
    "Level 1",
    "Level 2",
    "Level 3",
    "Level 4"
  )
)


# --------------------------------------------
# 4. Validate preprocessing
# --------------------------------------------

# Dataset dimensions
dim(heart_clean)

# Variable types
str(heart_clean)

# Summary statistics
summary(heart_clean)

# Identify categorical variables
categorical_vars <- names(heart_clean)[
  sapply(heart_clean, is.factor)
]

# Frequency tables
lapply(
  heart_clean[categorical_vars],
  table,
  useNA = "ifany"
)

# Missing values per variable
colSums(is.na(heart_clean))

# Check number of duplicates
sum(duplicated(heart_clean))


# --------------------------------------------
# 5. Save preprocessed data
# --------------------------------------------

write.csv(
  heart_clean,
  "data/heart_disease_preprocessed.csv",
  row.names = FALSE,
  na = ""
)

# View dataset
View(heart_clean)
