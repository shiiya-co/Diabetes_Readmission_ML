if(!require(shapviz)) install.packages("shapviz")

library(fastshap)
library(shapviz)
library(caret)
library(randomForest)

model_rf <- readRDS("final_model.rds")
data <- read.csv("cleaned_diabetes_data.csv", stringsAsFactors = TRUE)

set.seed(123)
plot_idx <- sample(nrow(data), 300)
X_subset <- data[plot_idx, ]

X_features <- X_subset[, colnames(X_subset) != "readmitted_binary"]


pfun <- function(object, newdata) {
  predict(object, newdata, type = "prob")[, "Yes"]
}

print("正在计算 SHAP 值 (Beeswarm版)...")
shap_vals <- fastshap::explain(
  model_rf, 
  X = X_features, 
  pred_wrapper = pfun, 
  nsim = 10
)


sv <- shapviz(shap_vals, X = X_features)

pdf("Result_SHAP_Beeswarm.pdf", width = 8, height = 6)

sv_importance(sv, kind = "beeswarm", max_display = 20, show_numbers = TRUE)
dev.off()

print("✅ 图一 (Beeswarm) 已生成！")







library(ggplot2)
library(RColorBrewer) 


imp <- varImp(model_rf)$importance
colnames(imp)[1] <- "Importance" 
imp$Feature <- rownames(imp)


plot_data <- imp %>%
  arrange(desc(Importance)) %>%
  head(20)


plot_data$Feature <- factor(plot_data$Feature, levels = rev(plot_data$Feature))


colourCount <- length(unique(plot_data$Feature))
getPalette <- colorRampPalette(brewer.pal(9, "Spectral"))

print("正在绘制渐变色特征重要性图...")

pdf("Result_Feature_Importance_Spectral.pdf", width = 8, height = 6)

ggplot(plot_data, aes(x = Feature, y = Importance, fill = Feature)) +
  geom_col(width = 0.8) +
  coord_flip() +
  theme_minimal() +

  scale_fill_manual(values = getPalette(colourCount)) +
  labs(
    title = "Feature Importance (Top 20)",
    x = "Features", 
    y = "Importance Score"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "none" 
  )

dev.off()

print("✅ 图二 (Spectral渐变) 已生成！")

# ==========================================
# 特征重要性美化版 (带侧边图例)
# ==========================================

library(ggplot2)
library(RColorBrewer) 
library(caret)
library(dplyr)


if(!exists("model_rf")) model_rf <- readRDS("final_model.rds")

imp <- varImp(model_rf)$importance
colnames(imp)[1] <- "Importance"
imp$Feature <- rownames(imp)


plot_data <- imp %>%
  arrange(desc(Importance)) %>%
  head(20)


plot_data$Feature <- factor(plot_data$Feature, levels = rev(plot_data$Feature))


colourCount <- length(unique(plot_data$Feature))
getPalette <- colorRampPalette(brewer.pal(9, "Spectral"))

print("正在绘制带图例的特征重要性图...")

pdf("Result_Feature_Importance_Spectral.pdf", width = 10, height = 7) 


ggplot(plot_data, aes(x = Feature, y = Importance, fill = Feature)) +
  geom_col(width = 0.8) +
  coord_flip() +
  theme_minimal() +
  

  scale_fill_manual(values = getPalette(colourCount)) +
  
  labs(
    title = "Feature Importance (Top 20)",
    x = "Features", 
    y = "Importance Score",
    fill = "Feature Name"
  ) +

  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text.y = element_text(size = 11, color = "black"), 
    axis.text.x = element_text(size = 10),
    

    legend.position = "right", 
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  
  ) +

  guides(fill = guide_legend(reverse = TRUE))

dev.off()

print("✅ 更新完成！Result_Feature_Importance_Spectral.pdf 现在有漂亮的侧边栏了！")

