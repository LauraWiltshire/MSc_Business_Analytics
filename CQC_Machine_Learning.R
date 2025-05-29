
# Load necessary libraries
library(tidyverse) # for analysis
library(naniar) # for missing data analysis
library(patchwork)  # for arranging plots
library(Hmisc) # for correlation
library(reshape2) # Convert correlation and p-values matrices into long format
library(caret) # partitioning and machine learning
library(plotly) # 3d plot
library(rpart) # decision tree
library(rpart.plot) # decision tree plot
library(pROC) # ROC-AUC analysis
library(e1071) # SVM model
library(broom) # coefficient plot
library(car) # binary log reg VIF calculation

# Get system info for reproducibility
Sys.info()

# Set font to Times New Roman for visualisations
font <- windowsFonts(TimesNewRoman = windowsFont("Times New Roman"))

# Get Data from directory
setwd("C:/Users/wiltshl/OneDrive - BANES Council/Documents/MSc/Dissertation")
data <- read_csv('dataset_for_msc.csv')

# Filter to show only overall cqc ratings
overall_ratings <- data %>%
  filter(Domain == 'Overall')
colnames(overall_ratings)
overall_ratings_clean <- overall_ratings %>%
  rename(
    `Ongoing QA` = `Ongoing QA`,
    `CQC Registration Number` = `CQC Registration Number`,
    `Name of Service` = `Name of Service`,
    `Packages` = `Packages`,
    `Number of Beds` = `Number of beds`,
    `Provider Name` = `Provider Name`,
    `Registered Manager Y/N` = `RegisteredManagerYN`,
    `Registered Manager Name` = `Registered Manager Name`,
    `Known Manager Name` = `Known Manager Name`,
    `Length Manager in Post` = `Length Manager in post`,
    `Days Since Contract Meeting` = `Days since  contract meeting`,
    `Last Contract Meeting Date` = `Last Contract Meeting Date`,
    `Contract Review Score` = `Contract Review Score`,
    `Risk Rating` = `Risk Rating`,
    `Packages per Bed` = `PackagesPerBed`,
    `Provider ID` = `Provider ID.y`,
    `Location Name` = `Location Name`,
    `Location Postcode` = `Location Post Code`,
    `Primary Inspection Category` = `Location Primary Inspection Category`,
    `Domain` = `Domain`,
    `Latest Rating` = `Latest Rating`,
    `Previous Overall Rating` = `Previous_Overall_Rating`,
    `Publication Date` = `Publication Date`,
    `Is First Inspection` = `IsFirst`
  )
  
vis_miss(overall_ratings_clean, sort_miss = TRUE) +
  theme(
    plot.margin = margin(t = 10, r = 70, b = 10, l = 10)
  )

# Order Latest Rating factor levels
overall_ratings$`Latest Rating` <- factor(
  overall_ratings$`Latest Rating`,
  levels = c("Outstanding", "Good", "Requires improvement", "Idequate", "Insufficient evidence to rate")
)

# Assign numeric scores to Risk Ratings
overall_ratings <- overall_ratings %>%
  mutate(
    Risk_Rating_Score = case_when(
      `Risk Rating` == "Red" ~ 3,
      `Risk Rating` == "Amber" ~ 2,
      `Risk Rating` == "Green" ~ 1,
      TRUE ~ NA_real_  # Handle missing or unknown values
    )
  )

# Assign numeric scores to CQC Ratings
overall_ratings <- overall_ratings %>%
  mutate(
    CQC_Rating_Score = case_when(
      `Latest Rating` == "Idequate" ~ 4,
      `Latest Rating` == "Requires improvement" ~ 3,
      `Latest Rating` == "Good" ~ 2,
      `Latest Rating` == "Outstanding" ~ 1,
      TRUE ~ NA_real_  # Handle missing or unknown values
    )
  )

# Assign numeric scores to previous CQC ratings
overall_ratings <- overall_ratings %>%
  mutate(
    Previous_Overall_Rating = case_when(
      Previous_Overall_Rating == "Inadequate" ~ 4,
      Previous_Overall_Rating == "Requires improvement" ~ 3,
      Previous_Overall_Rating == "Good" ~ 2,
      Previous_Overall_Rating == "Outstanding" ~ 1,
      Previous_Overall_Rating == "First" ~ NA
      
    )
  )

# Assign numeric scores to registered manager
overall_ratings <- overall_ratings %>%
  mutate(
    ManagerYNNumeric = case_when(
      `RegisteredManagerYN` == "Y" ~ 1,
      `RegisteredManagerYN` == "N" ~ 0,
      TRUE ~ NA_real_  # Handle missing or unknown values
    )
  )

# Assign numeric scores to IsFirst (first CQC rating)
overall_ratings <- overall_ratings %>%
  mutate(
    IsFirst = case_when(
      `IsFirst` == "Yes" ~ 0,
      `IsFirst` == "No" ~ 1,
      TRUE ~ NA_real_  # Handle missing or unknown values
    )
  ) 

# Select relevant variables for Spearman's rank correlation
spearman_data <- overall_ratings %>%
  select(
    Risk_Rating_Score,
    CQC_Rating_Score,
    `Number of beds`,
    `Packages`,
    `Days since  contract meeting`,
    ManagerYNNumeric,
    Previous_Overall_Rating,
    IsFirst
  )

# Calculate Spearman's rank correlation matrix and p-values
spearman_results <- Hmisc::rcorr(as.matrix(spearman_data), type = "spearman")

# Extract correlation coefficients and p-values
cor_matrix <- spearman_results$r
p_matrix <- spearman_results$P

# Convert correlation and p-values matrices into long format
cor_long <- melt(cor_matrix, varnames = c("Var1", "Var2"), value.name = "Correlation")
p_long <- melt(p_matrix, varnames = c("Var1", "Var2"), value.name = "p_value")

# Merge correlation coefficients and p-values into one dataset
cor_data <- merge(cor_long, p_long, by = c("Var1", "Var2"))

# Rename variables for better readability on the plot
variable_labels <- c(
  "Risk_Rating_Score" = "Commissioner Risk Rating",
  "CQC_Rating_Score" = "CQC Rating",
  "Previous_Overall_Rating" = "Previous CQC Rating",
  "IsFirst" = "Is First CQC Rating",
  "Number of beds" = "Number of beds",
  "Packages" = "Packages",
  "Days since  contract meeting" = "Days since contract meeting",
  "ManagerYNNumeric" = "Registered Manager"
 
)

cor_data <- cor_data %>%
  mutate(
    Var1 = recode(Var1, !!!variable_labels),
    Var2 = recode(Var2, !!!variable_labels)
  )

# Create a label combining the correlation coefficient and an asterisk for significance
cor_data <- cor_data %>%
  mutate(label = ifelse(
    p_value < 0.05, 
    paste0(round(Correlation, 2), "*"),  # Add an asterisk if p < 0.05
    paste0(round(Correlation, 2))       # Otherwise, just the correlation
  ))

# Plot the heatmap
correlation_plot <- ggplot(data = cor_data, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +                      # Tile plot with white grid lines
  geom_text(aes(label = label), size = 4) +         # Add correlation coefficient with asterisk
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name = "Spearman Correlation") +  # Color gradient
  theme_minimal() +                                 # Minimal theme
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis text
    axis.text = element_text(size = 10),                           # Adjust text size
    panel.grid.major = element_blank(),           # Remove grid
    panel.grid.minor = element_blank()
  ) +
  coord_fixed()  +                                  # Ensure tiles are square
  theme(text = element_text(family = "TimesNewRoman"))
  correlation_plot

# Filter data to only overall CQC domain
df <- overall_ratings %>% 
  filter(overall_ratings$Domain == 'Overall') 

# Extract publication year from date
df$`Publication Date` <- substr(df$`Publication Date`, nchar(df$`Publication Date`) - 3, nchar(df$`Publication Date`))

# Drop missing CQC ratings
df <- df %>% select(
  Packages,
  `Number of beds`,
  RegisteredManagerYN,
  `Latest Rating`,
  `Contract Review Score`,
  Previous_Overall_Rating,
  `Publication Date` ,
  IsFirst
) %>% drop_na(`Latest Rating`, Previous_Overall_Rating)
glimpse(df)

# Re-code CQC ratings to numeric
df <- df %>%
  mutate(
    CQC_Rating_Score = case_when(
      `Latest Rating` == "Idequate" ~ 4,
      `Latest Rating` == "Requires improvement" ~ 3,
      `Latest Rating` == "Good" ~ 2,
      `Latest Rating` == "Outstanding" ~ 1,
      TRUE ~ NA_real_  # Handle missing or unknown values
    )
  )

# Rename publication date
df <- df %>%
  mutate(
    Publication_Date = `Publication Date`
    )
  
# Re-code registered manager to binary numeric
df <- df %>%
  mutate(
    ManagerYNNumeric = case_when(
      `RegisteredManagerYN` == "Y" ~ 1,
      `RegisteredManagerYN` == "N" ~ 0,
      TRUE ~ NA_real_  # Handle missing or unknown values
    )
  )

# Select Data for Modelling
df <- df %>% select(
  Packages,
  CQC_Rating_Score,
  Previous_Overall_Rating,
  Publication_Date
) %>% drop_na(CQC_Rating_Score)


# Code to train 
set.seed(1234)


# Tree model with 2 classes instead (due to class imbalance)
df2 <- df %>%
  mutate(
    CQC_Rating_Score = case_when(
      CQC_Rating_Score == 4 ~ 4,
      CQC_Rating_Score == 3 ~ 4,
      CQC_Rating_Score == 2 ~ 1,
      CQC_Rating_Score == 1 ~ 1,
      TRUE ~ NA_real_  # Handle missing or unknown values
    )
  )

glimpse(df2)
df2 <- df2 %>% select(
  Packages,
  CQC_Rating_Score,
  Previous_Overall_Rating,
  Publication_Date,
) %>% drop_na(CQC_Rating_Score)


df2$Publication_Date <- as.numeric(df2$Publication_Date)
df2$Previous_Overall_Rating <- as.numeric(df2$Previous_Overall_Rating)

# rPart Decision Tree Training with 2 classes
# Code to train 
set.seed(1234)

#test train split
cutoff <- createDataPartition(df2$CQC_Rating_Score, p=0.80, list=FALSE) 

# select 20% of the data for validation 
testdf <- df2[-cutoff,] 
glimpse(testdf) # 21

# use the remaining 80% of data to training and testing the models 
traindf <- df2[cutoff,]
glimpse(traindf) # 87

# Check differences between test and train sets
testdf_vis <- testdf
traindf_vis <-traindf

# Add labels for test and train sets, then combine into one set
traindf_vis$Dataset <- "Train"
testdf_vis$Dataset <- "Test"
df_combined <- bind_rows(traindf_vis, testdf_vis) 

df_long <- df_combined %>%
  pivot_longer(
    cols = -Dataset,
    names_to = "Feature",
    values_to = "Value"
  )

# Plot 1: categorical features as bar charts
cat_vars <- c("CQC_Rating_Score", "Previous_Overall_Rating", "ManagerYNNumeric")

df_cat <- df_long %>% filter(Feature %in% cat_vars)

plot_cat <- ggplot(df_cat, aes(x = Value, fill = Dataset)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  facet_wrap(~Feature, scales = "free_x") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman")) +
  labs(title = "Categorical Feature Distributions: Train vs Test")

# Plot 2: continuous features as density plots
cont_vars <- c("Packages", "Publication_Date")

df_melted <- df_combined %>%
  pivot_longer(
    cols = -Dataset,
    names_to = "Feature",
    values_to = "Value"
  )

df_cont <- df_melted %>% filter(Feature %in% cont_vars)
plot_cont <- ggplot(df_cont, aes(x = as.numeric(Value), fill = Dataset)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Feature, scales = "free") +
  scale_fill_manual(values = c("Train" = "blue", "Test" = "red")) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman")) +
  labs(title = "Continuous Feature Distributions: Train vs Test")

# Combine the plots
plot_cat / plot_cont

# Train Decision Tree with rpart
tree_model2 <- rpart(
  CQC_Rating_Score ~ .,
  data = traindf,
  method = "class",
  control = rpart.control(cp = 0.01) 
)

# 0.022989 may be lowest xerror best cp
printcp(tree_model2, digits = 3) # display the results
plotcp(tree_model2, minline = TRUE,
       upper = c("size")) # visualise cross-validation results
summary(tree_model2) # detailed summary of splits

# Visualise tree
rpart.plot::rpart.plot(tree_model2, type = 2, extra = 105,
                       shadow.col = "gray",
                       box.palette = "GnBu",
                       branch.lty = 3,
                       nn = TRUE)

# Predict class labels
predictions <- predict(tree_model2, 
                       newdata = testdf, 
                       type = "class",
                       shadow.col = "gray",
                       nn = TRUE)

# Create confusion matrix 
conf_matrix_tree <- confusionMatrix(predictions, as.factor(testdf$CQC_Rating_Score))
print(conf_matrix_tree)

# ROC-AUC
pred_probs <- predict(tree_model2, newdata = testdf, type = "prob")
pred_classes <- predict(tree_model2, newdata = testdf, type = "class")
roc_obj <- roc(testdf$CQC_Rating_Score, pred_probs[, "4"])
auc(roc_obj) # Area under the curve: 0.4992

#Precision and F1-score
conf_matrix_tree$byClass["Precision"]
conf_matrix_tree$byClass["F1"]

# Prune tree using optimal cp
# Balancing relative error (training set error at each stage) and xerror (cross-validation error)
pruned_tree <- prune(tree_model2, cp = 0.022989)

# Visualise tree
rpart.plot::rpart.plot(pruned_tree, type = 2,
                       extra = 105,
                       shadow.col = "gray",
                       box.palette = "GnBu",
                       branch.lty = 3,
                       nn = TRUE)

# Predict class labels
predictions <- predict(pruned_tree, newdata = testdf, type = "class")

# Create confusion matrix for pruned tree
conf_matrix_prune <- confusionMatrix(predictions, as.factor(testdf$CQC_Rating_Score))
print(conf_matrix_prune)
conf_matrix_prune$byClass["F1"]
conf_matrix_prune$byClass["Precision"]

# roc auc for pruned tree
probs <- predict(pruned_tree, newdata = testdf, type = "prob")
positive_probs <- probs[, "1"]
actual <- as.factor(testdf$CQC_Rating_Score)
roc_obj <- roc(actual, positive_probs, levels = c("4", "1"), direction = "<")
plot(roc_obj, col = "blue", main = "ROC Curve for Decision Tree")
auc(roc_obj) # Area under the curve: 0.6304


# SVM 

# Start by setting the seed
set.seed(1234) #critical, never forget to do this

# Tidy data types
df2$CQC_Rating_Score = as.factor(df2$CQC_Rating_Score)

df2$Publication_Date = as.integer(df2$Publication_Date)

df2$Previous_Overall_Rating[is.na(df2$Previous_Overall_Rating)] <- 0

# Is data linearly separable - 3D plot?
fig <- plot_ly(df2,
               x = ~Packages,
               y = ~Previous_Overall_Rating,
               z = ~Publication_Date,
               color = ~CQC_Rating_Score,
               colors = c("lightgreen", "darkblue"),
               type = "scatter3d",
               mode = "markers",
               symbol = ~CQC_Rating_Score,
               symbols = c('x','circle'),
               marker = list(size = 5, opacity = 0.5)
) %>%
  layout(
    title = list(text = "3D Scatter Plot", font = list(family = "Times New Roman")),
    scene = list(
      xaxis = list(title = "Packages", titlefont = list(family = "Times New Roman")),
      yaxis = list(title = "Previous Overall Rating", titlefont = list(family = "Times New Roman")),
      zaxis = list(title = "Publication Date", titlefont = list(family = "Times New Roman"))
    )
  )

fig

library(htmlwidgets)
saveWidget(fig, "3D_PLOT.html", selfcontained = TRUE)

# Is the data separable after radial basis function transformation?
library(kernlab)
kpca_result <- kpca(~., data = df2[, -which(names(df2) == "CQC_Rating_Score")],
                    kernel = "rbfdot", fetures = 3)
rbf_features <- as.data.frame(rotated(kpca_result))
rbf_features$CQC_Rating_Score <- df2$CQC_Rating_Score
fig_rbf <- plot_ly(rbf_features,
                   x = ~V1,
                   y = ~V2,
                   z = ~V3,
                   color = ~CQC_Rating_Score,
                   colors = c("lightgreen", "darkblue"),
                   type = "scatter3d",
                   mode = "markers",
                   symbol = ~CQC_Rating_Score,
                   symbols = c('x','circle'),
                   marker = list(size = 5, opacity = 0.5)) %>%
  layout(title = "3D RBF Transformed Feature Space")
fig_rbf
saveWidget(fig_rbf, "3D_PLOT_RBF.html", selfcontained = TRUE)

#test train 
cutoff <- createDataPartition(df2$CQC_Rating_Score, p=0.80, list=FALSE) 

# select 20% of the data for validation 
test_set <- df2[-cutoff, ] 

# use the remaining 80% of data to training and testing the models 
train_set <- df2[cutoff, ]

# Use train() from the caret package for the SVM Model
# Rename class levels
levels(train_set$CQC_Rating_Score) <- make.names(levels(train_set$CQC_Rating_Score))
levels(test_set$CQC_Rating_Score) <- levels(train_set$CQC_Rating_Score)

ctrl <- trainControl(
  method = "none",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)
svm_model <- train(
  CQC_Rating_Score ~ .,
  data = train_set,
  method = "svmRadial",
  trControl = ctrl,
  preProcess = c("center", "scale"),
  metric = "ROC"
)
 
svm_model$bestTune
summary(svm_model)

# test SVM on training data
pred <- predict(svm_model, newdata = train_set)
conf_matrix_ <- confusionMatrix(pred, train_set$CQC_Rating_Score)
conf_matrix_

# test SVM on test data
pred <- predict(svm_model, newdata = test_set)
conf_matrix_svm <- confusionMatrix(pred, test_set$CQC_Rating_Score)
conf_matrix_svm
conf_matrix_svm$byClass["F1"]
conf_matrix_svm$byClass["Precision"]
svm_probs <- predict(svm_model, newdata = test_set, type = "prob")
roc_obj <- roc(test_set$CQC_Rating_Score, svm_probs[,"X4"])
auc(roc_obj)

# vis
svm_data <- df2 %>%
  select(Packages, Previous_Overall_Rating, CQC_Rating_Score) %>%
  drop_na()

svm_data$CQC_Rating_Score <- as.factor(svm_data$CQC_Rating_Score)
svm_fit <- svm(CQC_Rating_Score ~ Packages + Previous_Overall_Rating,
               data = svm_data,
               kernel = "linear",
               probability = TRUE)

x_range <- seq(min(svm_data$Packages), max(svm_data$Packages), length = 100)
y_range <- seq(min(svm_data$Previous_Overall_Rating), max(svm_data$Previous_Overall_Rating), length = 100)
grid <- expand.grid(Packages = x_range, Previous_Overall_Rating = y_range)
grid$Prediction <- predict(svm_fit, newdata = grid)

ggplot() +
  geom_point(data = svm_data, aes(x = Packages, 
                                  y = Previous_Overall_Rating, 
                                  color = CQC_Rating_Score), alpha = 0.6) +
  geom_tile(data = grid, aes(x = Packages, y = Previous_Overall_Rating, fill = Prediction),
               breaks = c(2.5), color = "black") +
  labs(title = "SVM Decision Boundary",
       x = "Packages",
       y = "Previous Overall Rating") +
  theme_minimal()


# Binary Logistic Regression

# Select Data for Modelling
bin_df <- df %>% 
  mutate(Publication_Date = as.numeric(Publication_Date)) %>%
  #mutate(Previous_Overall_Rating = as.factor(Previous_Overall_Rating)) %>%
  drop_na(CQC_Rating_Score) %>%
  select(
    Packages,
    CQC_Rating_Score,
    Previous_Overall_Rating,
    Publication_Date
    
    )

str(bin_df)

# With 2 classes for CQC Rating Score
bin_df <- bin_df %>%
  mutate(
    CQC_Rating_Score = case_when(
      CQC_Rating_Score == 4 ~ 4,
      CQC_Rating_Score == 3 ~ 4,
      CQC_Rating_Score == 2 ~ 1,
      CQC_Rating_Score == 1 ~ 1,
      TRUE ~ NA_real_  # Handle missing or unknown values
    )
  )
bin_df <- bin_df %>%
  mutate(
    CQC_Rating_Score = as.factor(CQC_Rating_Score)
  ) %>% drop_na(CQC_Rating_Score)
overall_ratings <- overall_ratings %>%
  mutate(
    Previous_Overall_Rating = case_when(
      Previous_Overall_Rating == 4 ~ 4,
      Previous_Overall_Rating == 3 ~ 3,
      Previous_Overall_Rating == 2 ~ 2,
      Previous_Overall_Rating == 1 ~ 1,
      Previous_Overall_Rating == -1 ~ 0
      
    )
  )%>% drop_na(Previous_Overall_Rating)

#df2$Previous_Overall_Rating <- as.factor(df2$Previous_Overall_Rating)

# Code to train 
set.seed(1234)

#test train split
cutoff <- createDataPartition(bin_df$CQC_Rating_Score, p=0.80, list=FALSE) 

# select 20% of the data for validation 
testdf <- na.omit(bin_df[-cutoff,])

# use the remaining 80% of data to training and testing the models 
traindf <- na.omit(bin_df[cutoff,])


# Train binary logistic regression
logit_model <- caret::train(
  CQC_Rating_Score ~ .,
  data = traindf,
  method = "glm",
  family = "binomial"
)

# Summarise model
print(logit_model)
summary(logit_model)

# Plot coefficients
coef_df <- tidy(logit_model$finalModel)

# Covariance
vif(logit_model$finalModel)

# Deviance analysis p-value
pchisq((107.771-97.626), df = 3, lower.tail = FALSE)

# Log odds ratio plot
coef_vals <- exp(coef(logit_model$finalModel))
conf_vals <- exp(confint(logit_model$finalModel))

coef_df <- data.frame(
  Variable = names(coef_vals),
  OR = coef_vals,
  Lower_CI = conf_vals[, 1],
  Upper_CI = conf_vals[, 2]
)

coef_df <- coef_df[coef_df$Variable != "(Intercept)", ]

ggplot(coef_df, aes(x = reorder(Variable, OR), y = OR)) +
  geom_point(color = "blue", size = 3) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_log10() +
  coord_flip() +
  labs(
    title = "Logistic Regression Coefficient Plot",
    x = "Predictor Variable",
    y = "Odds Ratio (log10 scale)"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "TimesNewRoman"))
  

# Predict class labels

testdf <-  testdf %>% mutate(
    CQC_Rating_Score = as.factor(CQC_Rating_Score)) %>%
    select(
      Packages,
      CQC_Rating_Score,
      Previous_Overall_Rating,
      Publication_Date
    )

# Generate ROC curve
testdf$predicted_prob <- predict(logit_model$finalModel, newdata = testdf, type = "response")
roc_obj <- roc(testdf$CQC_Rating_Score, testdf$predicted_prob)
best_coords <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")
best_coords # 0.2175483: ROC curve evaluates all thresholds and identifies the best trade-off between sensitivity and specificity

testdf$predicted_class <- ifelse(testdf$predicted_prob > 0.2175483, 4, 1)

# Create confusion matrix
testdf$predicted_class <- ifelse(testdf$predicted_prob > 0.2175483, 4, 1)
conf_matrix_log <- confusionMatrix(as.factor(testdf$predicted_class), testdf$CQC_Rating_Score)
conf_matrix_log
conf_matrix_log$byClass["F1"]

# ROC Curve and AUC
roc_curve <- roc(testdf$CQC_Rating_Score, testdf$predicted_prob)
plot(roc_curve, col = "blue", main = "ROC Curve for Logistic Regression")

# Area under the curve: 0.7556
auc_value <- auc(roc_curve)
print(auc_value)

conf_matrix_log
conf_matrix_tree
conf_matrix_prune
conf_matrix_svm
