# === A角色的工作：数据检查 ===

# 1. 读取数据
# 确保文件已经在右下角的 Files 面板里了！
data <- read.csv("diabetic_data.csv", na.strings = "?", stringsAsFactors = TRUE)

# 2. 只有读取成功了，才能运行下面的
print("读取成功！")
print(paste("行数:", nrow(data)))
print(paste("列数:", ncol(data)))

# 3. 检查 Target (是否再入院)
print("目标列 (readmitted) 分布:")
print(table(data$readmitted))

# 4. 检查 Weight (体重) 缺失率
missing_weight <- sum(is.na(data$weight))
percent_missing <- (missing_weight / nrow(data)) * 100
print(paste("体重列缺失了:", round(percent_missing, 2), "%"))
