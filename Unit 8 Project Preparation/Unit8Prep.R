# unit 8 -9 Case Study 1 Preparation
# Exploratory Data Analysis
# what is in the dataset?

### libraries for EDA and Initial Modeling
library(skimr)
library(gt)
library(tidyverse)
library(DataExplorer)
library(ggplot2)
library(plotly)
library(GGally)
##########################################################
### Data intake and skimming
raw_data <- read.csv("C:\Users\isabe\OneDrive - Southern Methodist University\MSDS_6306_Doing-Data-Science\Unit 8 and 9 Case Study 1\CaseStudy 1 Spring 2026.docx")
str(raw_data)
summary(raw_data)
skim_results<- skim(raw_data)
skim_table <- as.data.frame(skim_results)
skim_table %>%
  gt()
create_report(raw_data) # from DataExplorer library
##########################################################
# Count and Percent of Attrition from original dataset
attrition_positive = filter(raw_data, Attrition == "No")
dim(attrition_positive)[1] #730, 83.9%
attrition_negative = filter(raw_data, Attrition == "Yes")
dim(attrition_negative)[1] #140, 16.1%
##########################################################
# get a table of summaries (the five number summary)
column_names <- colnames(raw_data)
summaries <- list()
for (i in seq_along(column_names)) {
  summaries[[column_names[i]]] <- summary(raw_data[[i]])
}
summaries <- data.frame(summaries)
write.csv(summaries, file = "summaries.csv")
##########################################################
# Get the unique values in each categorical field
char_cols <- raw_data[sapply(raw_data, is.character)]
unique_values <- lapply(char_cols, unique)
unique_values
##########################################################
# Create new dataframe without the irrelevant columns
# relevant_df <- raw_data %>%
#   select(-"StandardHours", 
#          -"EmployeeCount", 
#          -"ID",
#          -"EmployeeNumber",
#          -"Over18")
# plot_density(relevant_df)


#plot correlations of all numeric vars
raw_data$Attrition_num <- ifelse(raw_data$Attrition == "Yes", 1, 0)
numeric_vars <- raw_data %>%
  select(where(is.numeric), 
         -Attrition_num)
cor_df <- data.frame(
  Feature = names(numeric_vars),
  Correlation = sapply(numeric_vars, function(x)
    cor(x, raw_data$Attrition_num, use = "complete.obs"))
)
cor_df <- cor_df %>%
  arrange(Correlation) %>%
  mutate(
    Direction = ifelse(Correlation > 0, "Positive", "Negative"),
    Feature = factor(Feature, levels = Feature)
  )
cor_df <- cor_df %>%
  filter(abs(Correlation) > 0.05)
ggplot(cor_df, aes(x = Feature, y = Correlation, fill = Direction)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(
    values = c("Positive" = "#f8766d", "Negative" = "#5f99cf")
  ) +
  labs(
    title = "Correlation of Numerical Features with Attrition",
    x = NULL,
    y = "Correlation with Attrition (filtered to absolute value >0.05)"
  ) +
  coord_flip() +
  theme_bw(base_size = 14) +
  theme(legend.position = "none")
################################################################################

raw_data %>%
  plot_boxplot(by = "Attrition")

# split into smaller datasets and view correlation heatmaps
plot_correlation(raw_data %>%
                   select(
                     "Attrition",
                    "WorkLifeBalance",
                    "DistanceFromHome",
                    "JobInvolvement",
                    "OverTime",
                    "YearsSinceLastPromotion",
                    "JobRole"))
# Plots
raw_data %>%
  select(Attrition, 
         MonthlyIncome, 
         # OverTime, 
         StockOptionLevel, 
         JobInvolvement, 
         TotalWorkingYears) %>%
  pivot_longer(cols = -Attrition) %>%
  ggplot(aes(x = value, fill = Attrition)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~name, scales = "free") +
  theme_minimal(base_size = 14)

# sales_reps = raw_data %>% filter(JobRole == "Sales Representative") #6%
# human_res =  raw_data %>% filter(JobRole == "Human Resources") #3
# lab_techs = raw_data %>% filter(JobRole == "Laboratory Technician") #17.5%
# research_sci = raw_data %>% filter(JobRole == "Research Scientist") #19.8%
# sales_exec = raw_data %>% filter(JobRole == "Sales Executive") #22.9%
# healthcare_reps = raw_data %>% filter(JobRole == "Healthcare Representative") #8.7%
# managers = raw_data %>% filter(JobRole == "Manager") #5.9%
# manufacturing_dirs = raw_data %>% filter(JobRole == "Manufacturing Director") #10%
# research_dirs = raw_data %>% filter(JobRole == "Research Director") #5.9%
# 
# 
# dim(research_dirs)[1]/dim(raw_data)[1]

# look at attrition by job role
# attrition_by_role <- raw_data %>%
#   group_by(JobRole) %>%
#   summarize(
#     n = n(),
#     attrition_rate = mean(Attrition == "Yes") * 100
#   ) %>%
#   arrange(desc(attrition_rate))
# 
# ggplot(attrition_by_role, 
#        aes(x = reorder(JobRole, attrition_rate), 
#            y = attrition_rate,
#            fill = JobRole)) +
#   geom_col() +coord_flip() +
#   geom_text(aes(label = 
#                   paste0(round(attrition_rate, 1), "%")),
#             vjust = 0,
#             hjust = 1.1,
#             size = 4,
#             fontface = "bold") +
#   labs(
#     title = "Attrition Rate by Job Role",
#     x = NULL,
#     y = "Attrition Rate (%)"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(legend.position = "none")
# 
# 
# attrition_by_marital <- raw_data %>%
#   group_by(MaritalStatus) %>%
#   summarize(n = n(),
#     attrition_rate = mean(Attrition == "Yes") * 100
#   ) %>%
#   arrange(desc(attrition_rate))
# 
# ggplot(attrition_by_marital, 
#        aes(x = reorder(MaritalStatus, attrition_rate), 
#            y = attrition_rate,
#            fill = MaritalStatus)) +
#   geom_col() +coord_flip() +
#   geom_text(aes(label = 
#                   paste0(round(attrition_rate, 1), "%")),
#             vjust = 0,
#             hjust = 1.1,
#             size = 5,
#             fontface = "bold") +
#   labs(
#     title = "Attrition Rate by Marital Status",
#     x = NULL,
#     y = "Attrition Rate (%)"
#   ) +
#   theme_minimal(base_size = 14)+
#   theme(legend.position = "none")
# 
# attrition_by_department <- raw_data %>%
#   group_by(Department) %>%
#   summarize(n = n(),
#             attrition_rate = mean(Attrition == "Yes") * 100
#   ) %>%
#   arrange(desc(attrition_rate))
# 
# ggplot(attrition_by_department, 
#        aes(x = reorder(Department, attrition_rate), 
#            y = attrition_rate,
#            fill = Department)) +
#   geom_col() +coord_flip() +
#   geom_text(aes(label = 
#                   paste0(round(attrition_rate, 1), "%")),
#             vjust = 0,
#             hjust = 1.1,
#             size = 5,
#             fontface = "bold") +
#   labs(
#     title = "Attrition Rate by Department",
#     x = NULL,
#     y = "Attrition Rate (%)"
#   ) +
#   theme_minimal(base_size = 14)+
#   theme(legend.position = "none")

# standardize the numeric variables??
relevant_df_numeric <- relevant_df %>% select(where(is.numeric))
relevant_df_standardized <- relevant_df_numeric %>%
  mutate(across(where(is.numeric), ~ as.vector(scale(.))))

raw_data$AttritionLevel <- factor(raw_data$Attrition,
                                           labels = c("Retained", "Lost"))
# raw_data_Selected <- filter(raw_data, 
#                                     JobRole == "Manufacturing Director" |
#                                       JobRole == "Sales Representative")
# pal <- c("blue", "red")
# plot_ly(raw_data,
#         x = ~MonthlyIncome,
#         color = ~Attrition_num,
#         colors = pal,
#         type = "scattercarpet"
#         # ,
#         # jitter = 0.3
#         )

# raw_data$Jobs <- factor(raw_data$JobRole,
#                         levels = unique(raw_data$JobRole))

# reorder is close to order, but is made to change the order of the factor levels.
# mpg$class = with(mpg, reorder(class, hwy, median))

# raw_data$OverTime_num <- ifelse(raw_data$OverTime == "Yes", 1, 0)

# raw_data %>%
#   ggplot(aes(x = "", 
#              y = TotalWorkingYears, 
#              fill = Attrition)) +
#   geom_violin(alpha = 0.5, position = "identity") +
#   labs(
#     title = "Total Working Years Distribution by Attrition",
#     x = NULL,
#     y = "Total Working Years"
#   ) +
#   theme_minimal(base_size = 14) +
#   coord_flip()+ 
#   scale_fill_manual(values = c("Yes" = "#f8766d",
#                                              "No"  = "#00bfc4"))

# stock_attrition <- raw_data %>%
#   group_by(StockOptionLevel) %>%
#   summarise(
#     n = n(),
#     attrition_rate = mean(Attrition == "Yes") * 100
#   ) %>%
#   arrange(StockOptionLevel)
# 
# job_inv_attrition <- raw_data %>%
#   group_by(JobInvolvement) %>%
#   summarise(
#     n = n(),
#     attrition_rate_job_inv = mean(Attrition == "Yes") * 100
#   ) %>%
#   arrange(JobInvolvement)
# 
# ggplot(job_inv_attrition,
#        aes(x = factor(JobInvolvement),
#            y = attrition_rate_job_inv)) +
#   geom_col(fill = "steelblue", width = 0.6) +
#   geom_text(aes(label = 
#                   paste0(round(attrition_rate_job_inv, 1), "%")),
#             vjust = -0.4,
#             size = 5,
#             fontface = "bold") +
#   geom_hline(yintercept = overall_rate,
#              linetype = "dashed",
#              color = "red",
#              size = 1) +
#   labs(
#     title = "Attrition Rate by Job Involvement",
#     x = "Job Involvement",
#     y = "Attrition Rate (%)"
#   ) +
#   theme_minimal(base_size = 14)

# overall_rate <- mean(raw_data$Attrition == "Yes") * 100

# ggplot(stock_attrition,
#        aes(x = factor(StockOptionLevel),
#            y = attrition_rate)) +
#   geom_col(fill = "steelblue", width = 0.6) +
#   geom_text(aes(label = 
#                   paste0(round(attrition_rate, 1), "%")),
#             vjust = -0.4,
#             size = 5,
#             fontface = "bold") +
#   
#   geom_hline(yintercept = overall_rate,
#              linetype = "dashed",
#              color = "red",
#              size = 1) +
#   labs(
#     title = "Attrition Rate by Stock Option Level",
#     subtitle = paste("Overall Attrition Rate:", round(overall_rate, 1), "%"),
#     x = "Stock Option Level",
#     y = "Attrition Rate (%)"
#   ) +
#   theme_minimal(base_size = 14)

# Violin plot, box plot, scatter plot, and density plot
# library(ggstatsplot)
# plt <- ggbetweenstats(
#   data = raw_data,
#   x = Attrition,
#   y = TotalWorkingYears
# )
# 


# # Overtime visuals
# ot_attrition <- raw_data %>%
#   group_by(OverTime) %>%
#   summarise(
#     n = n(),
#     attrition_rate = mean(Attrition == "Yes") * 100
#   )
# 
# # overall_rate <- mean(raw_data$Attrition == "Yes") * 100
# 
# ggplot(ot_attrition,
#        aes(x = OverTime,
#            y = attrition_rate,
#            fill = OverTime)) +
#   geom_col(width = 0.6) +
#   geom_text(aes(label = paste0(round(attrition_rate, 1), "%")),
#             vjust = -0.4,
#             size = 5,
#             fontface = "bold") +
#   geom_hline(yintercept = overall_rate,
#              linetype = "dashed",
#              color = "red",
#              size = 1) +
#   labs(
#     title = "Attrition Rate by Overtime Status",
#     subtitle = paste("Overall Attrition Rate:", round(overall_rate, 1), "%"),
#     x = "Overtime",
#     y = "Attrition Rate (%)"
#   ) +
#   theme_minimal(base_size = 14) +
#   scale_fill_manual(values = c("Yes" = "#e88631",
#                                "No"  = "#7c3896")) + 
#   theme(legend.position = "none")

# raw_data %>%
#   ggplot(aes(x = factor(StockOptionLevel), 
#              fill = Attrition)) +
#   geom_bar(position = "dodge") +
#   labs(
#     title = "Attrition by Stock Option Level",
#     x = "Stock Option Level",
#     y = "Proportion"
#   ) +
#   scale_fill_manual(values = c("Yes" = "#f8766d",
#                                "No"  = "#00bfc4")) +
#   theme_minimal(base_size = 14)

# Correlation Heatmap of some interesting variables...
# raw_data %>% select(
#   "Attrition",
#   "JobInvolvement",
#   "JobLevel",
#   "StockOptionLevel",
#   "MonthlyIncome",
#   "TotalWorkingYears",
#   "MaritalStatus",
#   "JobRole",
#   "OverTime"
# ) %>% plot_correlation()

# frequency of attrition per feature
# plot_bar(interesting_df)
# head(interesting_df)
# do a ggpairs of the selected features
# ggpairs(interesting_df, aes(color = Attrition))


##########################################################
##########################################################
##########################################################
# #Classification Models
# library(class)
# library(caret)
# library(e1071) # Naive Bayes
# raw_data$Attrition <- as.factor(raw_data$Attrition)
# raw_data$MaritalStatus <- as.factor(raw_data$MaritalStatus)
# raw_data$OverTime <- as.factor(raw_data$OverTime)
# raw_data$MaritalStatusSingle <- ifelse(raw_data$MaritalStatus == "Single", 1, 0)
##########################################################
model_data  <- select(raw_data, "Attrition",
                      "MaritalStatusSingle",
                      "MonthlyIncome",
                      "OverTime_num",
                      "JobInvolvement",
                      "TotalWorkingYears",
                      "StockOptionLevel",
                      "Age")
##########DERIVED FEATURES###############################################################
# model_data$StockOptionOverTime <- 
#   model_data$StockOptionLevel * model_data$OverTime_num
# model_data$MaritalSingle_Age <- 
#   model_data$MaritalStatusSingle * model_data$Age
# model_data$Income_OT <- 
#   model_data$MonthlyIncome * (1+model_data$OverTime_num)
# model_data$JobInvolvement_OT <-
#   model_data$JobInvolvement * model_data$OverTime_num
##########################################################
# #split the raw_data into training 70% and test 30%
# set.seed(6)
# obs = dim(model_data)[1]
# trainIndices = sample(seq(1:obs),round(.7*obs))
# trainSet = model_data[trainIndices,]
# testSet = model_data[-trainIndices,]
##########################################################
# Split income into levels in training data
# Compute quartile cut points from training data
# income_breaks <- quantile(trainSet$MonthlyIncome,
#                           probs = c(0, 0.25, 0.5, 0.75, 1))
# trainSet$IncomeQuartile <- cut(trainSet$MonthlyIncome,
#                                breaks = income_breaks,
#                                include.lowest = TRUE,
#                                labels = c("Q1_Low",
#                                           "Q2_MidLow",
#                                           "Q3_MidHigh",
#                                           "Q4_High"))

# testSet$IncomeQuartile <- cut(testSet$MonthlyIncome,
#                               breaks = income_breaks,
#                               include.lowest = TRUE,
#                               labels = c("Q1_Low",
#                                          "Q2_MidLow",
#                                          "Q3_MidHigh",
#                                          "Q4_High"))
# Naive Bayes model with Monthly Income split into 4 levels based on quartile
model <- naiveBayes(trainSet[, c("IncomeQuartile",
                                 "OverTime_num")],
                    trainSet$Attrition)

pred_probs <- predict(model,
                 testSet[, c("IncomeQuartile",
                             "OverTime_num")],
                 type = "raw")
pred_class <- ifelse(pred_probs[, "Yes"] > 0.2, "Yes", "No")
confusionMatrix(factor(pred_class, levels=c("No","Yes")),
                testSet$Attrition)

# confusionMatrix(table(preds, testSet$Attrition))
##########################################################
# next, combine income quartile with TotalWorkingYears
##########################################################
# Try total working years or job involvement
model <- naiveBayes(trainSet[, c("IncomeQuartile",
                                 "JobInvolvement_OT",
                                 "OverTime_num")],
                    trainSet$Attrition)

preds <- predict(model,
                 testSet[, c("IncomeQuartile",
                             "JobInvolvement_OT",
                             "OverTime_num")])

confusionMatrix(table(preds, testSet$Attrition))
##########################################################
# simple knn classification model
# don't forget to scale numerical vars!!
# trainSet$IncomeQuartile_num <- as.numeric(trainSet$IncomeQuartile)
# testSet$IncomeQuartile_num  <- as.numeric(testSet$IncomeQuartile)

train_knn <- scale(trainSet[,c("OverTime_num",
                               "IncomeQuartile_num")])

test_knn  <- scale(testSet[,c("OverTime_num",
                              "IncomeQuartile_num")],
                   center = attr(train_knn,"scaled:center"),
                   scale  = attr(train_knn,"scaled:scale"))

classifications_attr <- knn(train_knn,
                            test_knn,
                            trainSet$Attrition,
                            k = 3)

confusionMatrix(table(classifications_attr,
                      testSet$Attrition))

##########################################################

classifications_attr = knn(trainSet[,c("OverTime_num",
                                       "IncomeQuartile")],
                      testSet[,c("OverTime_num",
                                 "IncomeQuartile")],
                      trainSet$Attrition, 
                      prob = TRUE, k = 3)
confusionMatrix(table(classifications_attr,testSet$Attrition))
##########################################################
# simple naive bayes classification model
model = naiveBayes(trainSet[,c("StockOptionLevel",
                                "OverTime_num", 
                               "MaritalStatusSingle")],
                   trainSet$Attrition)
table(predict(model,
              testSet[,c("StockOptionLevel",
                         "OverTime_num", 
                         "MaritalStatusSingle")]),
      testSet$Attrition)%>%
  confusionMatrix()
############################################################
# prop.table(table(trainSet$Attrition)) # 17.7% Yes










