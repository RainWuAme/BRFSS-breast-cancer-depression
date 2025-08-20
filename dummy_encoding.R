# 0. 載入必要的函式庫
library(tidyverse) # 包含 dplyr 和 ggplot2
library(readr)
library(VIM) # 用於 KNN 填補
library(pscl) # 用於 Pseudo R-squared (McFadden)
library(caTools) # 用於資料分割

cat("--- 開始偵錯程序 ---\n\n")

# 1. 資料路徑和設定
# data_path <- "C:/Users/XPG/Desktop/BRFSS_Analysis/data/LLCP2023.ASC"
data_path <- "./data/LLCP2023.ASC"

brfss_positions <- list(
    c(316, 317), # Cancer
    c(146, 146), # Depression
    c(314, 315), # Age told had cancer
    c(72, 72), # Gender
    c(623, 624), # Race
    c(2090, 2090), # Income
    c(187, 187), # Education
    c(108, 109), # Health insurance
    c(1402, 1402), # Urban rural
    c(2098, 2098), # Binge drinking
    c(225, 225), # Ever smoking
    c(2087, 2087), # Obese
    c(113, 113), # Exercise in Past 30 Days
    c(114, 115), # Type of Physical Activity 1
    c(116, 118), # Physical Activity Frequency 1
    c(119, 121), # Exercise duration each time1
    c(122, 123), # Type of Physical Activity 2
    c(124, 126), # Physical Activity Frequency 2
    c(127, 129), # Exercise duration each time2
    c(130, 132), # Anaerobic exercise time
    c(134, 134), # Hypertension
    c(137, 137), # Dyslipidemia
    c(138, 138), # Heart attack
    c(139, 139), # Coronary heart disease
    c(149, 149) # Diabetes
)

column_names <- c(
    "cancer",
    "depression",
    "age_told_had_cancer",
    "gender",
    "race",
    "income",
    "education",
    "health_insurance",
    "urban_rural",
    "binge_drinking",
    "ever_smoking",
    "obese",
    "exercise_past_30_days",
    "physical_activity_type_1",
    "physical_activity_frequency_1",
    "exercise_duration_each_time_1",
    "physical_activity_type_2",
    "physical_activity_frequency_2",
    "exercise_duration_each_time_2",
    "anaerobic_exercise_time",
    "hypertension",
    "dyslipidemia",
    "heart_attack",
    "Coronary_heart_disease",
    "diabetes"
)

if (length(brfss_positions) != length(column_names)) {
    stop("錯誤: `brfss_positions_list` 和 `column_names` 的長度不匹配。請檢查定義。")
}

# Load data
brfss_data <- read_fwf(
    file = data_path,
    col_positions = fwf_positions(
        start = sapply(brfss_positions, function(x) x[1]),
        end = sapply(brfss_positions, function(x) x[2]),
        col_names = column_names
    ),
    col_types = cols(.default = col_character()) # equivalent to dtype=str
)

# Clean data
brfss_data_cleaned <- brfss_data %>%
    mutate(across(
        c(
            gender, race, income, education, health_insurance, urban_rural,
            binge_drinking, ever_smoking, obese, exercise_past_30_days,
            hypertension, dyslipidemia, heart_attack, Coronary_heart_disease,
            diabetes, age_told_had_cancer, depression
        ),
        ~ case_when(
            . %in% c(8, 9) ~ NA_real_, # 將 8 和 9 轉換為 NA
            TRUE ~ as.numeric(.) # 確保其他值為數字
        )
    )) %>%
    mutate(cancer = as.numeric(cancer)) # 確保 cancer 也是數值型

# Extract only the breast cancer data (cancer == 5)
breast_cancer_data <- brfss_data_cleaned %>%
    filter(cancer == 5)

# Remove the 'cancer' column as it is not needed for further analysis
breast_cancer_data <- breast_cancer_data %>%
    select(-cancer)

# Save the original data
write_csv(breast_cancer_data, "./data/breast_cancer_data.csv")

# ---------------------------
# Encoding and cleaning steps
# ---------------------------
# Change depression 2 to 0
breast_cancer_data <- breast_cancer_data %>%
    mutate(depression = ifelse(depression == 2, 0, depression))
# Set depression 7, 9 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(depression = ifelse(depression %in% c(7, 9), NA, depression))

# # Remove the rows with NA in depression
# breast_cancer_data <- breast_cancer_data %>%
#     filter(!is.na(depression))
# # Dimension reduce from 2243 to 2232

# Set age_told_had_cancer to NA if it is >= 98
breast_cancer_data <- breast_cancer_data %>%
    mutate(age_told_had_cancer = ifelse(age_told_had_cancer >= 98, NA, age_told_had_cancer))

# # Set gener 1, 2 to 0, 1
# breast_cancer_data <- breast_cancer_data %>%
#     mutate(gender = ifelse(gender == 1, 0, ifelse(gender == 2, 1, NA)))


# Set 77 (Don't know) and 99 (Refused) to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(race = ifelse(race %in% c(77, 99), NA, race))

# Create dummy variables (White = 1 as reference group)
breast_cancer_data <- breast_cancer_data %>%
    mutate(
        race_black = ifelse(race == 2, 1, 0), # Black or African American
        race_hispanic = ifelse(race == 3, 1, 0), # Hispanic or Latino
        race_asian = ifelse(race == 4, 1, 0), # Asian
        race_native_hawaiian = ifelse(race == 5, 1, 0), # Native Hawaiian or Other Pacific Islander
        race_american_indian = ifelse(race == 6, 1, 0), # American Indian or Alaska Native
        race_mixed = ifelse(race == 7, 1, 0), # Mixed Race
        race_other = ifelse(race == 8, 1, 0) # Some other group
    )
# Remove the 'race' column as it is not needed for further analysis
breast_cancer_data <- breast_cancer_data %>%
    select(-race)

# Set income 77 and 99 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(income = ifelse(income %in% c(77, 99), NA, income))
# # Set income 1 to 7 to 0 to 6
# breast_cancer_data <- breast_cancer_data %>%
#     mutate(income = ifelse(income >= 1 & income <= 11, income - 1, income))

# Set education 9 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(education = ifelse(education == 9, NA, education))

# Set health_insurance 77 and 99 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(health_insurance = ifelse(health_insurance %in% c(77, 99), NA, health_insurance))
# Dummy variables for health insurance 1 ~ 10 and 88
breast_cancer_data <- breast_cancer_data %>%
    mutate(
        health_insurance_2 = ifelse(health_insurance == 2, 1, 0),
        health_insurance_3 = ifelse(health_insurance == 3, 1, 0),
        health_insurance_4 = ifelse(health_insurance == 4, 1, 0),
        health_insurance_5 = ifelse(health_insurance == 5, 1, 0),
        health_insurance_6 = ifelse(health_insurance == 6, 1, 0),
        health_insurance_7 = ifelse(health_insurance == 7, 1, 0),
        health_insurance_8 = ifelse(health_insurance == 8, 1, 0),
        health_insurance_9 = ifelse(health_insurance == 9, 1, 0),
        health_insurance_10 = ifelse(health_insurance == 10, 1, 0),
        health_insurance_88 = ifelse(health_insurance == 88, 1, 0)
    )
# Remove the 'health_insurance' column as it is not needed for further analysis
breast_cancer_data <- breast_cancer_data %>%
    select(-health_insurance)

# Set ever_smoking 7 and 9 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(ever_smoking = ifelse(ever_smoking %in% c(7, 9), NA, ever_smoking))



#### Exercise and Physical Activity ####

# Set exercise_past_30_days 7 and 9 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(exercise_past_30_days = ifelse(exercise_past_30_days %in% c(7, 9), NA, exercise_past_30_days))

# Set physical_activity_type_1 77 and 99 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(physical_activity_type_1 = ifelse(physical_activity_type_1 %in% c(77, 99), NA, physical_activity_type_1))
# Dummy variables for physical activity type 1 from number 1 to 11
breast_cancer_data <- breast_cancer_data %>%
    mutate(
        physical_activity_type_1_2 = ifelse(physical_activity_type_1 == "02", 1, 0),
        physical_activity_type_1_3 = ifelse(physical_activity_type_1 == "03", 1, 0),
        physical_activity_type_1_4 = ifelse(physical_activity_type_1 == "04", 1, 0),
        physical_activity_type_1_5 = ifelse(physical_activity_type_1 == "05", 1, 0),
        physical_activity_type_1_6 = ifelse(physical_activity_type_1 == "06", 1, 0),
        physical_activity_type_1_7 = ifelse(physical_activity_type_1 == "07", 1, 0),
        physical_activity_type_1_8 = ifelse(physical_activity_type_1 == "08", 1, 0),
        physical_activity_type_1_9 = ifelse(physical_activity_type_1 == "09", 1, 0),
        physical_activity_type_1_10 = ifelse(physical_activity_type_1 == "10", 1, 0),
        physical_activity_type_1_11 = ifelse(physical_activity_type_1 == "11", 1, 0)
    )
# Remove the 'physical_activity_type_1' column as it is not needed for further analysis
breast_cancer_data <- breast_cancer_data %>%
    select(-physical_activity_type_1)

# Set physical_activity_frequency_1 777 and 999 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(physical_activity_frequency_1 = ifelse(physical_activity_frequency_1 %in% c("777", "999"), NA, physical_activity_frequency_1))
# Convert exercise frequency to monthly frequency
# Original coding: 101-199 (times per week), 201-299 (times per month)
# Target: all converted to times per month
breast_cancer_data <- breast_cancer_data %>%
    mutate(
        # Convert string to numeric first
        physical_activity_frequency_1_num = as.numeric(physical_activity_frequency_1),

        # Convert to monthly frequency
        physical_activity_frequency_1_monthly = case_when(
            # Weekly frequency (101-199): subtract 100, then multiply by 4
            physical_activity_frequency_1_num >= 101 & physical_activity_frequency_1_num <= 199 ~
                (physical_activity_frequency_1_num - 100) * 4,

            # Monthly frequency (201-299): subtract 200 to get actual monthly frequency
            physical_activity_frequency_1_num >= 201 & physical_activity_frequency_1_num <= 299 ~
                physical_activity_frequency_1_num - 200,
        )
    )
# Remove the original frequency column and the numeric conversion
breast_cancer_data <- breast_cancer_data %>%
    select(-physical_activity_frequency_1, -physical_activity_frequency_1_num)

# Set exercise_duration_each_time_1 777 and 999 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(exercise_duration_each_time_1 = ifelse(exercise_duration_each_time_1 %in% c("777", "999"), NA, exercise_duration_each_time_1))

# Set physical_activity_type_2 77 and 99 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(physical_activity_type_2 = ifelse(physical_activity_type_2 %in% c(77, 99), NA, physical_activity_type_2))
# Dummy variables for physical activity type 2 from number 1 to 11 and 88
breast_cancer_data <- breast_cancer_data %>%
    mutate(
        physical_activity_type_2_2 = ifelse(physical_activity_type_2 == "02", 1, 0),
        physical_activity_type_2_3 = ifelse(physical_activity_type_2 == "03", 1, 0),
        physical_activity_type_2_4 = ifelse(physical_activity_type_2 == "04", 1, 0),
        physical_activity_type_2_5 = ifelse(physical_activity_type_2 == "05", 1, 0),
        physical_activity_type_2_6 = ifelse(physical_activity_type_2 == "06", 1, 0),
        physical_activity_type_2_7 = ifelse(physical_activity_type_2 == "07", 1, 0),
        physical_activity_type_2_8 = ifelse(physical_activity_type_2 == "08", 1, 0),
        physical_activity_type_2_9 = ifelse(physical_activity_type_2 == "09", 1, 0),
        physical_activity_type_2_10 = ifelse(physical_activity_type_2 == "10", 1, 0),
        physical_activity_type_2_11 = ifelse(physical_activity_type_2 == "11", 1, 0),
        physical_activity_type_2_88 = ifelse(physical_activity_type_2 == "88", 1, 0)
    )
# Remove the 'physical_activity_type_2' column as it is not needed for further analysis
breast_cancer_data <- breast_cancer_data %>%
    select(-physical_activity_type_2)

# Set physical_activity_frequency_2 777 and 999 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(physical_activity_frequency_2 = ifelse(physical_activity_frequency_2 %in% c("777", "999"), NA, physical_activity_frequency_2))
# Convert exercise frequency to monthly frequency
# Original coding: 101-199 (times per week), 201-299 (times per month)
# Target: all converted to times per month
breast_cancer_data <- breast_cancer_data %>%
    mutate(
        # Convert string to numeric first
        physical_activity_frequency_2_num = as.numeric(physical_activity_frequency_2),

        # Convert to monthly frequency
        physical_activity_frequency_2_monthly = case_when(
            # Weekly frequency (101-199): subtract 100, then multiply by 4
            physical_activity_frequency_2_num >= 101 & physical_activity_frequency_2_num <= 199 ~
                (physical_activity_frequency_2_num - 100) * 4,

            # Monthly frequency (201-299): subtract 200 to get actual monthly frequency
            physical_activity_frequency_2_num >= 201 & physical_activity_frequency_2_num <= 299 ~
                physical_activity_frequency_2_num - 200,
        )
    )
# Remove the original frequency column and the numeric conversion
breast_cancer_data <- breast_cancer_data %>%
    select(-physical_activity_frequency_2, -physical_activity_frequency_2_num)

# Set exercise_duration_each_time_2 777 and 999 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(exercise_duration_each_time_2 = ifelse(exercise_duration_each_time_2 %in% c("777", "999"), NA, exercise_duration_each_time_2))

# Set anaerobic_exercise_time 777 and 999 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(anaerobic_exercise_time = ifelse(anaerobic_exercise_time %in% c("777", "999"), NA, anaerobic_exercise_time))
# Set anaerobic_exercise_time 888 to 0
breast_cancer_data <- breast_cancer_data %>%
    mutate(anaerobic_exercise_time = ifelse(anaerobic_exercise_time == "888", 0, anaerobic_exercise_time))
# Convert anaerobic_exercise_time to monthly frequency
# Original coding: 101-199 (times per week), 201-299 (times per month)
# Target: all converted to times per month
breast_cancer_data <- breast_cancer_data %>%
    mutate(
        # Convert string to numeric first
        anaerobic_exercise_time_num = as.numeric(anaerobic_exercise_time),

        # Convert to monthly frequency
        anaerobic_exercise_time_monthly = case_when(
            # Weekly frequency (101-199): subtract 100, then multiply by 4
            anaerobic_exercise_time_num >= 101 & anaerobic_exercise_time_num <= 199 ~
                (anaerobic_exercise_time_num - 100) * 4,

            # Monthly frequency (201-299): subtract 200 to get actual monthly frequency
            anaerobic_exercise_time_num >= 201 & anaerobic_exercise_time_num <= 299 ~
                anaerobic_exercise_time_num - 200,
        )
    )
# Remove the original frequency column and the numeric conversion
breast_cancer_data <- breast_cancer_data %>%
    select(-anaerobic_exercise_time, -anaerobic_exercise_time_num)



#### Health Conditions ####

# Set hypertension 7 and 9 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(hypertension = ifelse(hypertension %in% c(7, 9), NA, hypertension))

# Set dyslipidemia 7 and 9 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(dyslipidemia = ifelse(dyslipidemia %in% c(7, 9), NA, dyslipidemia))

# Set heart_attack 7 and 9 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(heart_attack = ifelse(heart_attack %in% c(7, 9), NA, heart_attack))

# Set Coronary_heart_disease 7 and 9 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(Coronary_heart_disease = ifelse(Coronary_heart_disease %in% c(7, 9), NA, Coronary_heart_disease))

# Set diabetes 7 and 9 to NA
breast_cancer_data <- breast_cancer_data %>%
    mutate(diabetes = ifelse(diabetes %in% c(7, 9), NA, diabetes))
# Dummy variables for diabetes 1 to 4
# breast_cancer_data <- breast_cancer_data %>%
#     mutate(
#         diabetes_2 = ifelse(diabetes == 2, 1, 0),
#         diabetes_3 = ifelse(diabetes == 3, 1, 0),
#         diabetes_4 = ifelse(diabetes == 4, 1, 0)
#     )
breast_cancer_data <- breast_cancer_data %>%
    mutate(
        diabetes_234 = ifelse(diabetes %in% c(2, 3, 4), 1, 0)
    )
# Remove the 'diabetes' column as it is not needed for further analysis
breast_cancer_data <- breast_cancer_data %>%
    select(-diabetes)

# Save the cleaned data
write_csv(breast_cancer_data, "./data/breast_cancer_data_encoded.csv")
