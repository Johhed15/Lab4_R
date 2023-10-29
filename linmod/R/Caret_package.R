install_github("wountoto/Lab5_AdvR",subdir = "BaDaAn")
install_github("Johhed15/Lab4_R", subdir = "linmod")

library(linmod)
library(devtools)
library(BaDaAn)
library(caret)
library(leaps)

#### 1-3 ####
set.seed(123)
df <- data_dl()
df1 <- tidyr::pivot_wider(df,id_cols = c(municipality,period) ,names_from = kpi,values_from=values)
df1$municipality<-as.factor(df1$municipality)
df1$period<-as.factor(df1$period)

trainIndex <- createDataPartition(df1$residents, p = .7, list = FALSE)


Train <- df1[ trainIndex,]
Test <- df1[-trainIndex,]

#linear regression model with forward selection
covariates <- ncol(df1)-1

model_lm <- lm(residents ~ .-municipality-period,data = Train)
model_fwd <- caret::train( residents ~ .-municipality-period,data = Train,method = "leapForward",tuneGrid = expand.grid(nvmax = covariates))
model_intercept <- lm(residents~ 1, data = Train)


model_fwd_sum <- summary(model_fwd)
best_model <- data.frame("AdjR2" = which.max(model_fwd_sum$adjr2),
                         "CP" = which.min(model_fwd_sum$cp),
                         "BIC" = which.min(model_fwd_sum$bic))

best_model
model_fwd_sum


#6
model_fwd_best6 <- lm(residents~ political_spend + leisure_spend + education_spend + elder_disabled_spend + refugee_spend + labor_spend - 
                        municipality - period, data = Train)

#3
model_fwd_best3 <- lm(residents~ leisure_spend + education_spend + elder_disabled_spend - 
                        municipality - period, data = Train)

#3
model_fwd_best5 <- lm(residents~ political_spend + leisure_spend + education_spend + elder_disabled_spend +labor_spend - 
                        municipality - period, data = Train)

# 3 evaluate on training dataset
summary(model_fwd_best3)
summary(model_fwd_best5)
summary(model_fwd_best6)
summary(model_intercept)
summary(model_lm)


#### 4 ####

# Create object
ridg3 <- list(type = "Regression",
              library = NULL,
              loop = NULL,
              prob = NULL)

# Parameters
prm <- data.frame(parameter = "lambda",
                  class = "numeric",
                  label = "Lambda")
ridg3$parameters <- prm

# Lambda grid 
lambda <- function (x, y, len = NULL, search = "grid") 
{data.frame(lambda = c(1:5))
}

ridg3$grid <- lambda


ridgFit <- function(x, y, wts, param, lev, last, classProbs, ...){
  x_df <- as.data.frame(x)
  x_df$.result <- y
  final <- ridgereg$new(.result~., data = x_df, lambda = param$lambda, ...)
  final
}

ridg3$fit <- ridgFit

# Pred
ridg3$predict <- function(modelFit, newdata, submodels = NULL){
  newdata <- as.data.frame(newdata)
  newdata[,apply(newdata, MARGIN=2, sd)!=0] <- scale(newdata[apply(newdata, MARGIN=2, sd)!=0])
  modelFit$pred(newdata)
}


# Sort
ridgSort <- function(x){
  x[order(-x$lambda),]
}

ridg3$sort <- ridgSort


model_try <- caret::train( residents ~ .-municipality-period,data = Train,method = ridg3)

model_try

# 5 
cv <- trainControl(method = "repeatedcv", number =10, repeats = 10)
model_ridge <- train(residents~.-municipality-period, data = Train, method = ridg3, trControl = cv)
model_ridge


# 6
pred_b3 <- predict(model_fwd_best3, Test)
pred_b5 <- predict(model_fwd_best5, Test)
pred_b6 <- predict(model_fwd_best6, Test)
pred_lm <- predict(model_lm, Test)
pred_intercept <- predict(model_intercept, Test)
pred_ridge <- predict(model_ridge, Test)


postResample(pred_b3, Test$residents)
postResample(pred_b5, Test$residents)
postResample(pred_b6, Test$residents)
postResample(pred_lm, Test$residents)
postResample(pred_intercept, Test$residents)
postResample(pred_ridge, Test$residents)

