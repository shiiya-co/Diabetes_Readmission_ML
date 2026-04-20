library(tidyverse)
raw_data <- read.csv("diabetic_data.csv", na.strings = "?", stringsAsFactors = TRUE)

cleaned_data <- raw_data %>%
  select(-weight, -payer_code, -medical_specialty) %>%

  select(-id, -encounter_id, -patient_nbr) %>% 
  mutate(readmitted_binary = ifelse(readmitted == "NO", "No", "Yes")) %>%
  mutate(readmitted_binary = as.factor(readmitted_binary)) %>%
  select(-readmitted) %>%
  drop_na()

write.csv(cleaned_data, "cleaned_diabetes_data.csv", row.names = FALSE)
print("✅ ID 已移除，数据清洗完毕。")
