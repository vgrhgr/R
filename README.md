# R
# ----------------------------
# 1. SIMPLE LINEAR REGRESSION
# ----------------------------
# Step 1: Import Libraries and Data
library(ggplot2)
library(caTools)
data <- read.csv("Salary_Data.csv")

# Step 2: Structure and Summary
str(data); summary(data)

# Step 3: Split Data
set.seed(123)
split <- sample.split(data$Salary, SplitRatio = 2/3)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# Step 4: Train Model
model <- lm(Salary ~ YearsExperience, data = train)
summary(model)

# Step 5: Predict and Plot
pred <- predict(model, newdata = test)
ggplot() +
  geom_point(aes(x = test$YearsExperience, y = test$Salary), colour = 'red') +
  geom_line(aes(x = train$YearsExperience, y = predict(model, newdata = train)), colour = 'blue') +
  ggtitle("Salary vs Experience (Test Set)") +
  xlab("Years Experience") + ylab("Salary")


# ----------------------------
# 2. MULTIPLE LINEAR REGRESSION
# ----------------------------
# Step 1: Import Libraries and Data
library(caTools); library(ggplot2)
Startups <- read.csv("50_Startups.csv")

# Step 2: Structure and Summary
str(Startups); summary(Startups)

# Step 3: Encode Categorical Variables
Startups$State <- as.factor(Startups$State)

# Step 4: Split Data
set.seed(123)
split <- sample.split(Startups$Profit, SplitRatio = 2/3)
train <- subset(Startups, split == TRUE)
test <- subset(Startups, split == FALSE)

# Step 5: Train Model
model <- lm(Profit ~ ., data = train)
summary(model)

# Step 6: Predict and Plot
pred <- predict(model, newdata = test)
ggplot() +
  geom_point(aes(x = test$Profit, y = pred), colour = 'red') +
  geom_abline(intercept = 0, slope = 1, colour = 'blue') +
  ggtitle("Actual vs Predicted Profit") + xlab("Actual Profit") + ylab("Predicted Profit")


# ----------------------------
# 3. TIME SERIES FORECASTING
# ----------------------------
# Step 1: Import Libraries and Data
library(forecast); library(ggplot2)
it <- read.csv("it.csv", sep = ";")
colnames(it) <- c("ID", "Value")

# Step 2: Create Time Series Object
myts <- ts(it$Value, start = 1, frequency = 12)

# Step 3: Plot Time Series
plot(myts); monthplot(myts); seasonplot(myts)
autoplot(myts); ggseasonplot(myts); ggmonthplot(myts)

# Step 4: Forecast with ARIMA
plot(forecast(auto.arima(myts)))


# ----------------------------
# 4. DECISION TREE CLASSIFICATION
# ----------------------------
# Step 1: Import Data
data <- read.csv("it_modified_for_decision_tree.csv")

# Step 2: Structure and Summary
str(data); summary(data)

# Step 3: Factor Target
data$Purchased <- factor(data$Purchased, levels = c(0, 1))

# Step 4: Split Data
library(caTools); library(rpart)
set.seed(123)
split <- sample.split(data$Purchased, SplitRatio = 0.75)
train <- subset(data, split == TRUE)
test <- subset(data, split == FALSE)

# Step 5: Scale Inputs
train[,1:2] <- scale(train[,1:2])
test[,1:2] <- scale(test[,1:2])

# Step 6: Build Tree
classifier <- rpart(formula = Purchased ~ ., data = train)
summary(classifier)

# Step 7: Predict and Evaluate
y_pred <- predict(classifier, newdata = test[,1:2], type = 'class')
cm <- table(test$Purchased, y_pred)
cm


# ----------------------------
# 5. LOGISTIC REGRESSION
# ----------------------------
# Step 1: Import Data
dataset <- read.csv("social_network.csv")
dataset <- dataset[,3:5]

# Step 2: Structure and Summary
str(dataset); summary(dataset)

# Step 3: Factor Target
dataset$Purchased <- factor(dataset$Purchased, levels = c(0, 1))

# Step 4: Split Data
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
train <- subset(dataset, split == TRUE)
test <- subset(dataset, split == FALSE)

# Step 5: Scale Features
train[,1:2] <- scale(train[,1:2])
test[,1:2] <- scale(test[,1:2])

# Step 6: Train Model
classifier <- glm(formula = Purchased ~ ., family = binomial, data = train)
summary(classifier)

# Step 7: Predict and Evaluate
prob_pred <- predict(classifier, type = "response", newdata = test[,1:2])
y_pred <- ifelse(prob_pred > 0.5, 1, 0)
cm <- table(test$Purchased, y_pred)
cm
