# 0. 載入必要的函式庫
library(tidyverse) # 包含 dplyr 和 ggplot2
library(readr)
library(VIM) # 用於 KNN 填補
library(pscl) # 用於 Pseudo R-squared (McFadden)
library(caTools) # 用於資料分割

cat("--- 開始偵錯程序 ---\n\n")

# 1. 資料路徑和設定
# data_path <- "C:/Users/XPG/Desktop/BRFSS_Analysis/data/LLCP2023.ASC"
data_path <- "C:/Users/chee1/repository/BRFSS-breast-cancer-depression/data/LLCP2023.ASC"

# 根據您原始 Python 程式碼的 (start, end) 定義列位置
brfss_positions <- list(
  c(316, 317), # Cancer
  c(146, 146), # Depression
  c(313, 315), # Age told had cancer
  c(72, 72), # Gender
  c(623, 624), # Race
  c(2090, 2090), # Income
  c(186, 187), # Education
  c(107, 109), # Health insurance
  c(1401, 1402), # Urban rural
  c(2097, 2098), # Binge drinking
  c(224, 225), # Ever smoking
  c(2086, 2087), # Obese
  c(112, 113), # Exercise in Past 30 Days
  c(113, 115), # Type of Physical Activity 1
  c(115, 118), # Physical Activity Frequency 1
  c(121, 123), # Type of Physical Activity 2
  c(123, 126), # Physical Activity Frequency 2
  c(129, 132), # Anaerobic exercise time
  c(133, 134), # Hypertension
  c(136, 137), # Dyslipidemia
  c(137, 138), # Heart attack
  c(138, 139), # Coronary heart disease
  c(148, 149) # Diabetes
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
  "physical_activity_type_2",
  "physical_activity_frequency_2",
  "anaerobic_exercise_time",
  "hypertension",
  "dyslipidemia",
  "heart_attack",
  "Coronary_heart_disease",
  "diabetes"
)

if (length(brfss_positions) != length(column_names)) {
  stop("錯誤: `brfss_positions` 和 `column_names` 的長度不匹配。請檢查定義。")
}

col_specs_named <- fwf_positions(
  start = sapply(brfss_positions, `[`, 1),
  end = sapply(brfss_positions, `[`, 2),
  col_names = column_names
)

# 2. 載入原始數據
cat("--- 2. 載入原始數據 ---\n")
brfss_data <- tryCatch(
  {
    read_fwf(
      file = data_path,
      col_positions = col_specs_named,
      col_types = paste(rep("c", length(column_names)), collapse = "")
    )
  },
  error = function(e) {
    stop(paste("讀取檔案時發生錯誤，請檢查檔案路徑和權限：", e$message))
  }
)

# 修正了這裡的打字錯誤：brffs_data -> brfss_data
cat(paste0("原始數據載入完成。維度: (", nrow(brfss_data), ", ", ncol(brfss_data), ")\n"))
cat("原始數據前 6 行:\n")
print(head(brfss_data, 6))
cat("原始數據各列類型:\n")
print(sapply(brfss_data, class))
cat("原始數據各列 NA 數量:\n")
print(colSums(is.na(brfss_data)))
# 特別檢查有問題的列在原始數據中的 NA 數量
problem_cols <- c(
  "depression", "obese", "exercise_past_30_days",
  "hypertension", "dyslipidemia", "Coronary_heart_disease"
)
cat("\n原始數據中特定問題列的 NA 數量:\n")
print(colSums(is.na(brfss_data[, problem_cols, drop = FALSE])))
cat("\n")

# 3. 數據預處理 (清洗和轉換) - 創建 brfss_data_cleaned
cat("--- 3. 數據預處理 (創建 brfss_data_cleaned) ---\n")

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

# 以下是具體的轉換邏輯
brfss_data_cleaned <- brfss_data_cleaned %>%
  mutate(
    gender = case_when(gender == 1 ~ 0, gender == 2 ~ 1, TRUE ~ NA_real_),
    race = case_when(race == 1 ~ 0, TRUE ~ 1),
    income = case_when(income >= 7 ~ 1, TRUE ~ 0),
    education = case_when(education >= 5 ~ 1, TRUE ~ 0),
    health_insurance = case_when(health_insurance == 1 ~ 1, health_insurance == 2 ~ 0, TRUE ~ NA_real_),
    urban_rural = case_when(urban_rural == 1 ~ 1, TRUE ~ 0),
    binge_drinking = case_when(binge_drinking == 1 ~ 1, binge_drinking == 2 ~ 0, TRUE ~ NA_real_),
    ever_smoking = case_when(ever_smoking == 1 ~ 1, ever_smoking == 2 ~ 0, TRUE ~ NA_real_),
    obese = case_when(obese == 1 ~ 1, obese == 2 ~ 0, TRUE ~ NA_real_),
    exercise_past_30_days = case_when(exercise_past_30_days == 1 ~ 1, exercise_past_30_days == 2 ~ 0, TRUE ~ NA_real_),
    hypertension = case_when(hypertension == 1 ~ 1, hypertension == 2 ~ 0, TRUE ~ NA_real_),
    dyslipidemia = case_when(dyslipidemia == 1 ~ 1, dyslipidemia == 2 ~ 0, TRUE ~ NA_real_),
    heart_attack = case_when(heart_attack == 1 ~ 1, heart_attack == 2 ~ 0, TRUE ~ NA_real_),
    Coronary_heart_disease = case_when(Coronary_heart_disease == 1 ~ 1, Coronary_heart_disease == 2 ~ 0, TRUE ~ NA_real_),
    diabetes = case_when(diabetes %in% c(1, 2, 3, 4) ~ 1, TRUE ~ 0),
    age_told_had_cancer = case_when(age_told_had_cancer %in% c(998, 999) ~ NA_real_, TRUE ~ age_told_had_cancer),
    depression = case_when(depression == 1 ~ 1, depression == 2 ~ 0, TRUE ~ NA_real_)
  )

cat("數據預處理完成。brfss_data_cleaned 維度: (", nrow(brfss_data_cleaned), ", ", ncol(brfss_data_cleaned), ")\n")
cat("brfss_data_cleaned 前 6 行:\n")
print(head(brfss_data_cleaned, 6))
cat("brfss_data_cleaned 各列 NA 數量:\n")
print(colSums(is.na(brfss_data_cleaned)))
# 特別檢查有問題的列在清理後的 NA 數量
cat("\n清理後特定問題列的 NA 數量:\n")
print(colSums(is.na(brfss_data_cleaned[, problem_cols, drop = FALSE])))
cat("\n")


# 4. 提取乳腺癌病例 (cancer == 5)
cat("--- 4. 提取乳腺癌病例 ---\n")
cat("brfss_data_cleaned$cancer 的唯一值和頻率:\n")
print(table(brfss_data_cleaned$cancer, useNA = "always"))

breast_cancer_cleaned <- brfss_data_cleaned %>%
  filter(cancer == 5)

cat(paste0("篩選出 cancer == 5 的行數 (breast_cancer_cleaned): ", nrow(breast_cancer_cleaned), "\n"))
if (nrow(breast_cancer_cleaned) == 0) {
  stop("錯誤: 在 `brfss_data_cleaned` 中找不到 `cancer == 5` 的行。請檢查原始數據的 'cancer' 欄位值和預處理步驟。")
}
cat("breast_cancer_cleaned 前 6 行:\n")
print(head(breast_cancer_cleaned, 6))
cat("breast_cancer_cleaned 各列 NA 數量:\n")
print(colSums(is.na(breast_cancer_cleaned)))
# 特別檢查有問題的列在篩選後的 NA 數量
cat("\n篩選後特定問題列的 NA 數量 (breast_cancer_cleaned):\n")
print(colSums(is.na(breast_cancer_cleaned[, problem_cols, drop = FALSE])))
cat("\n")


# 5. 分離 X (特徵) 和 Y (目標變數)
cat("--- 5. 分離 X 和 Y ---\n")
Y_original <- breast_cancer_cleaned$age_told_had_cancer
X_bc <- breast_cancer_cleaned %>% select(-cancer, -age_told_had_cancer)

cat(paste0("Y_original 的長度: ", length(Y_original), "\n"))
cat(paste0("X_bc 的維度: (", nrow(X_bc), ", ", ncol(X_bc), ")\n"))
cat("Y_original 的 NA 數量: ", sum(is.na(Y_original)), "\n")
cat("Y_original 的非 NA 數量: ", sum(!is.na(Y_original)), "\n")

# if (sum(!is.na(Y_original)) == 0) {
#   stop("錯誤: `Y_original` 不包含任何非缺失值。無法進行歸一化或模型建立。請檢查 `age_told_had_cancer` 欄位。")
# }
# cat("\n")

# # 6. 歸一化 Y (Y_original -> Y_normalized)
# cat("--- 6. 歸一化 Y ---\n")
# Y_normalized <- (Y_original - min(Y_original, na.rm = TRUE)) / (max(Y_original, na.rm = TRUE) - min(Y_original, na.rm = TRUE))

# cat(paste0("Y_normalized 的 NA 數量: ", sum(is.na(Y_normalized)), "\n"))
# cat("Y_normalized 的前 6 個值:\n")
# print(head(Y_normalized, 6))
# cat("\n")

# # 7. 將歸一化後的 Y 轉換為二元類別 (Y_normalized -> Y_binary)
# cat("--- 7. Y 轉換為二元類別 ---\n")
# Y_binary <- ifelse(Y_normalized > 0.5, 1, 0)

# cat(paste0("Y_binary 的 NA 數量: ", sum(is.na(Y_binary)), "\n"))
# cat("Y_binary 的值分佈:\n")
# print(table(Y_binary, useNA = "always"))
# if (all(is.na(Y_binary))) {
#   stop("錯誤: `Y_binary` 包含所有 NA 值。歸一化或二元化過程可能出錯。")
# }
# if (length(unique(Y_binary[!is.na(Y_binary)])) < 2) {
#   warning("警告: `Y_binary` 只有一個類別 (0 或 1)。邏輯迴歸可能無法有效運作。")
# }
# cat("\n")

# # 8. 對 X 進行缺失值填補 (X_bc -> X_bc_imputed)
# cat("--- 8. X 進行缺失值填補 ---\n")
# X_bc_numeric <- X_bc %>% mutate(across(everything(), as.numeric))
# cat("X_bc 轉換為數值型後各列 NA 數量:\n")
# print(colSums(is.na(X_bc_numeric)))

# # **重要修正：檢查並移除所有 NA 的欄位，避免 kNN 警告和後續錯誤**
# # 識別所有值都為 NA 的欄位
# all_na_cols <- names(X_bc_numeric)[colSums(is.na(X_bc_numeric)) == nrow(X_bc_numeric)]
# if (length(all_na_cols) > 0) {
#   warning(paste(
#     "警告: 偵測到以下欄位所有值都為 NA，將從 X_bc_numeric 中移除以避免 kNN 錯誤: ",
#     paste(all_na_cols, collapse = ", ")
#   ))
#   X_bc_numeric <- X_bc_numeric %>% select(-all_of(all_na_cols))
# }

# if (nrow(X_bc_numeric) == 0) {
#   stop("錯誤: `X_bc_numeric` 為空。無法進行 KNN 填補。")
# }

# X_bc_imputed_full <- VIM::kNN(X_bc_numeric, k = 5)
# X_bc_imputed <- X_bc_imputed_full %>% select(-ends_with("_imp"))

# cat("X_bc_imputed (填補後) 各列 NA 數量:\n")
# print(colSums(is.na(X_bc_imputed)))
# cat("X_bc_imputed 前 6 行:\n")
# print(head(X_bc_imputed, 6))
# cat("\n")

# # 9. 合併 Y 和 X，並移除包含 NA 的行 (Y_binary & X_bc_imputed -> Y_final & X_final)
# cat("--- 9. 合併 Y 和 X，並移除 NA 行 ---\n")
# if (length(Y_binary) != nrow(X_bc_imputed)) {
#   stop("錯誤: `Y_binary` 和 `X_bc_imputed` 的行數不匹配。這不應該發生在前面的步驟中。")
# }

# valid_rows <- !is.na(Y_binary) & !apply(is.na(X_bc_imputed), 1, any)
# cat(paste0("在移除 NA 值後，用於建模的有效行數: ", sum(valid_rows), "\n"))

# Y_final <- Y_binary[valid_rows]
# X_final <- X_bc_imputed[valid_rows, ]

# cat(paste0("最終用於建模的 Y_final 長度: ", length(Y_final), "\n"))
# cat(paste0("最終用於建模的 X_final 維度: (", nrow(X_final), ", ", ncol(X_final), ")\n"))

# if (length(Y_final) == 0 || all(is.na(Y_final))) {
#   stop("錯誤: `Y_final` 在所有過濾和處理後為空或只包含 NA 值。無法擬合模型。請檢查前面的偵錯輸出。")
# }
# if (nrow(X_final) == 0) {
#   stop("錯誤: `X_final` 在所有過濾和處理後為空。無法擬合模型。請檢查前面的偵錯輸出。")
# }

# Y_final <- as.factor(Y_final)

# cat("\n--- 偵錯程序結束，如果沒有錯誤訊息，數據應該已準備好進行模型建立。---\n")

# cat("\n--- 10. 數據分割 ---\n")
# # 10. 數據分割
# # 假設您想將數據分割為訓練集和測試集 (例如 70% 訓練, 30% 測試)
# # set.seed 確保每次執行分割的結果一致
# set.seed(123) # 設定隨機種子，確保結果可重現

# # 創建訓練集和測試集的索引
# sample_indices <- sample.split(Y_final, SplitRatio = 0.70)

# # 根據索引分割數據
# X_train <- X_final[sample_indices, ]
# X_test <- X_final[!sample_indices, ]

# Y_train <- Y_final[sample_indices]
# Y_test <- Y_final[!sample_indices]

# cat(paste0("數據分割完成。訓練集行數: ", nrow(X_train), "\n"))
# cat(paste0("測試集行數: ", nrow(X_test), "\n"))
# cat("訓練集 Y 分佈:\n")
# print(table(Y_train))
# cat("測試集 Y 分佈:\n")
# print(table(Y_test))
# cat("\n")


# cat("--- 11. 模型訓練與評估 ---\n")
# # 11. 模型訓練與評估
# # 將訓練數據合併為一個數據框，供 glm 使用
# # 注意：這裡我們使用 cbind 將 Y_train 和 X_train 組合起來。
# # 或者更常見的做法是創建一個包含所有變量的數據框。
# # 為了邏輯迴歸的方便，我們可以將 X_train 和 Y_train 放入一個新的數據框。

# # 確保列名是有效的
# # 這裡應該不需要手動創建 formula_str，直接使用 Y_train ~ . 即可
# # 但如果 X_train 包含太多列，或者有非數值列，可能會出錯
# # 由於我們已經確保 X_final 只有數值列，可以使用 .
# # 如果您想指定排除某些列，可以像 Y_train ~ . - column_to_exclude 這樣
# model_data_train <- cbind(Y_train = Y_train, X_train)

# # 擬合邏輯迴歸模型
# # family = binomial 表示邏輯迴歸
# # 這裡使用 as.formula(Y_train ~ .) 會自動包含 X_train 中的所有列作為預測變數
# logistic_model <- glm(Y_train ~ ., data = model_data_train, family = binomial(link = "logit"))

# cat("=== 模型摘要 ===\n")
# print(summary(logistic_model))

# cat("\n=== Pseudo R-squared (McFadden) ===\n")
# # 使用 pscl 函式庫計算 Pseudo R-squared (McFadden)
# if (requireNamespace("pscl", quietly = TRUE)) {
#   print(pscl::pR2(logistic_model)["McFadden"])
# } else {
#   cat("pscl 函式庫未安裝，無法計算 McFadden Pseudo R-squared。\n")
#   cat("請執行 install.packages(\"pscl\") 安裝它。\n")
# }

# cat("\n=== P-值 ===\n")
# # 提取並列印 P-值
# p_values <- summary(logistic_model)$coefficients[, "Pr(>|z|)"]
# for (variable_name in names(p_values)) {
#   p_value <- p_values[variable_name]
#   cat(paste0(variable_name, ": ", sprintf("%.6f", p_value)))
#   if (p_value < 0.05) {
#     cat(" *") # 標記顯著變數
#   }
#   cat("\n")
# }
# cat("(* 表示 p < 0.05)\n")

# cat("\n--- 模型訓練與評估完成 ---\n")
