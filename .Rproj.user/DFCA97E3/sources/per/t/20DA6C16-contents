library(caret)
library(randomForest)
data <- read.csv("cleaned_diabetes_data.csv", stringsAsFactors = TRUE)


set.seed(123)
idx <- createDataPartition(data$readmitted_binary, p = 0.02, list = FALSE)
small_data <- data[idx, ]

bad <- nearZeroVar(small_data)
if(length(bad) > 0) small_data <- small_data[, -bad]


ctrl <- trainControl(method = "cv", number = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
print("开始训练...")
model_rf <- train(readmitted_binary ~ ., data = small_data, method = "rf", metric = "ROC", trControl = ctrl, ntree = 50, importance = TRUE)

saveRDS(model_rf, "final_model.rds")
print("✅ 新模型已保存 (final_model.rds)。请把这个发给队友！")
