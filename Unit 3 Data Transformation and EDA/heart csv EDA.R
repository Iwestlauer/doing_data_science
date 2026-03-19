library(readr)
library(tidyverse)
library(ggplot2)
library(GGally)

heart_data = read_csv("heart.csv")
spec(heart_data)
heart_df = data.frame(heart_data)
colSums(is.na(heart_df)) # no nulls

heart_df$oldpeak_cat <- cut(
  heart_df$oldpeak,
  breaks = c(-Inf, 0, 1.5, Inf),
  labels = c("zero", "normal", "high")
)


heart_df$target <- factor(heart_df$target, levels = c(0, 1), labels = c("No Disease", "Heart Disease"))

heart_df %>%
  mutate(sex = factor(sex, levels = c(0, 1), labels = c("Female", "Male"))) %>%
  select(age, sex, oldpeak_cat, cp, target) %>%
  ggpairs(aes(color = sex))

heart_df %>%
  ggplot(aes(x = oldpeak_cat, fill = target)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c(
    "No Disease" = "grey30",
    "Heart Disease" = "red"
  )) +
  labs(
    title = "Oldpeak Category vs Heart Disease Diagnosis",
    x = "ST Depression Category (oldpeak)",
    y = "Count",
    fill = "Diagnosis"
  )









