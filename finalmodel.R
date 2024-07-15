rm(list=ls())

# Library statements
# ------------------
library(randomForest)
library(dplyr)

# Load datasets
# -------------------
df <- read.csv("calldata.csv")

# Generate random forest
# -------------------
set.seed(222)
rfbal <- randomForest(formula=as.factor(mobbing_context) ~ Delta_Time_s + Low_Freq_Hz + High_Freq_Hz + Delta_Freq_Hz + Avg_Entropy_bits + 
                        BW_90percent_Hz + Center_Freq_Hz + Center_Time_Rel + Peak_Freq_Hz + Peak_Time_Relative + 
                        Time_5percent_Rel + Avg_Power_Density_dB_FSperHz + Time_95percent_Rel, data=df, mtry=4, ntree=500)

# Print results
# -------------------
print(rfbal)
plot(rfbal, main = "OOB Error vs. # of Trees")

# Calculate variable importance
# -------------------
rfbal$importance
varImpPlot(rfbal, main = "Weighted Variable Importance")

# Validation tests
# -------------------
testdata = read.csv("validation_data.csv")
pred <- predict(rfbal, testdata) # your model, rfbal, categorizes your validation data
print(pred) # this displays the predictions

# Generate .csv for validation test
# -------------------
write.csv(pred, "predictions.csv", row.names = FALSE)

# Calculate error %
# -------------------
error_df <- read.csv("predictions.csv")

error_pct <- nrow(filter(error_df, x == "breeding"))/nrow(error_df)