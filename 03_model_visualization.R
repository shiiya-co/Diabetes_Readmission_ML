library(pROC)
library(caret)
library(randomForest)


print("正在加载新模型和数据...")

model_rf <- readRDS("final_model.rds")
data <- read.csv("cleaned_diabetes_data.csv", stringsAsFactors = TRUE)

set.seed(42) 
test_idx <- sample(nrow(data), 1000)
test_data <- data[test_idx, ]

print("正在计算预测概率...")
probs <- predict(model_rf, test_data, type = "prob")

roc_obj <- roc(test_data$readmitted_binary, probs$Yes)


print("正在绘制 ROC 曲线...")

pdf("Result_ROC_Curve.pdf", width = 6, height = 6)

plot(roc_obj, 
     print.auc = TRUE,          
     auc.polygon = TRUE,        
     grid = c(0.1, 0.2),        
     grid.col = c("green", "red"), 
     max.auc.polygon = TRUE,
     auc.polygon.col = "skyblue", 
     print.auc.col = "black",     
     main = "ROC Curve (Diabetes Prediction)", 
     col = "blue",              
     lwd = 3                      
)

dev.off()

print("✅ 更新完成！Result_ROC_Curve.pdf 已经是最新版了。")

