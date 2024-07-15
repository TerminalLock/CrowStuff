rm(list=ls())

# Library statements
# ------------------
library(dplyr)
library(ggplot2)
library(car)

# Load datasets
# -------------------
df <- read.csv("avg_calldata.csv")
df2 <- read.csv("calldata.csv")

# Filtering/cleaning datasets
# -------------------
df$mobbing_context <- as.factor(df$mobbing_context)

df_b <- df %>% 
  filter(mobbing_context == "breeding")
df_nb <- df %>% 
  filter(mobbing_context == "non-breeding")

# Statistics
# -------------------
#p1_s <- shapiro.test(df$Delta_Time_s)
#p1_l <- leveneTest(Delta_Time_s ~ mobbing_context, data = df)
#p1_w <- wilcox.test(Delta_Time_s ~ mobbing_context, data = df)

#p3_s <- shapiro.test(df$High_Freq_Hz)
#p3_l <- leveneTest(High_Freq_Hz ~ mobbing_context, data = df)
#p3_w <- wilcox.test(High_Freq_Hz ~ mobbing_context, data = df)



# Making Custom Axis Titles
# -------------------
x_labels <- c("Breeding", "Non-breeding")
y_labels_hz <- function(x) {
  paste(x, "hz")
}
y_labels_sec <- function(x) {
  paste(x, "sec")
}

# Graphs
# -------------------
p1 <- df %>% 
  ggplot(aes(x=mobbing_context, y=Delta_Time_s)) + 
  geom_boxplot() + 
  labs(x="Mobbing Context", y = "Delta Time") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12)) +
  scale_x_discrete(label = x_labels) +
  scale_y_continuous(label = y_labels_sec)
#plot(p1) # Delta Time vs. Seasonality

p2 <- ggplot(df, aes(x=mobbing_context, y=High_Freq_Hz)) + geom_boxplot() +
  labs(x="Mobbing Context")
#plot(p2) # High Freq vs. Seasonality

p3 <- df %>%
  ggplot(aes(x = mobbing_context, y = High_Freq_Hz)) + 
  geom_violin() +
  geom_jitter(width=0.1) +
  labs(x = "Seasonality", y = "Upper Frequency Limit") +
  theme(axis.title = element_text(size = 18), axis.text = element_text(size = 12)) +
  scale_x_discrete(label = x_labels) +
  scale_y_continuous(label = y_labels_sec)
#plot(p3) # High Freq vs. Seasonality, but violin

p4 <- ggplot(df, aes(x=mobbing_context, y=Center_Freq_Hz)) + geom_boxplot() + 
  labs(x="Mobbing Context")
#plot(p4) # Center Freq vs. Seasonality

p5 <- ggplot(df, aes(x=mobbing_context, y=Delta_Freq_Hz)) + geom_violin() +
  labs(x="Mobbing Context")
#plot(p5) # Delta Freq vs. Seasonality

p6 <- ggplot(df, aes(x=mobbing_context, y=Avg_Power_Density_dB_FSperHz)) + geom_boxplot() +
  labs(x="Mobbing Context")
plot(p6) # Avg. Power Density vs. Seasonality

p7 <- ggplot(df, aes(x=mobbing_context, y=Avg_Power_Density_dB_FSperHz)) + geom_boxplot() +
  labs(x="Mobbing Context")
plot(p6) # Avg. Power Density vs. Seasonality