###################################################################
## Code from RUG talk "Optimization Methods for Tuning Predictive 
## Models" by Max Kuhn


library(caret)
library(data.table)
library(rBayesianOptimization)
install.packages("data.table")
data(Sacramento)
str(Sacramento)

## Create split of data

set.seed(955)
df_test <- df_night_model
df_test[is.na(df_test)] <- 0
in_train <- createDataPartition(df_test$NEE_cor, p = .8, list = FALSE)
head(in_train)
#training <- Sacramento[ in_train,]
#testing  <- Sacramento[-in_train,]

training <- df_test[ in_train,]
testing  <- df_test[-in_train,]

mins_data <- apply(training, 2, min, na.rm = T)
maxs_data <- apply(training, 2, max, na.rm = T)

df_test <- as.data.frame(scale(df_test, center = mins_data, 
                            scale = maxs_data - mins_data))

training <- df_test[ in_train,]
testing  <- df_test[-in_train,]

## These fold splits will be used in several functions so save them
set.seed(3313)
index <- createFolds(training$price, returnTrain = TRUE, list = TRUE)
ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 2)

## Bayesian optimization search
## function is used here because of how the `BayesianOptimization`
## function wants the output

svm_fit_bayes <- function(size, batch_size) {
    mod <- train(NEE_cor ~ ., data = training,
                 method = "model_try",
                 #preProc = c("range", "zv"),
                 metric = "RMSE",
                 trControl = trainControl("none"),
                 tuneGrid = data.frame(size = size, batch_size = batch_size))
    
    bh_pred <- predict(mod, testing[,1:18])
    rmse <- postResample(pred = bh_pred, obs = testing$NEE_cor)

  list(Score = -rmse[[1]], Pred = 0)
}

bounds <- list(size = c(30L, 110L), batch_size = c(30L, 100L))

set.seed(8606)
bo_search <- BayesianOptimization(svm_fit_bayes,
                                  bounds = bounds,
                                  init_points = 3,
                                  n_iter = 10,
                                  acq = "ucb",
                                  kappa = 1)
bo_search


params <- fun_params(layer = 3L, epochs = 100)
bo_first <- fun_bo(df_train = df_night_model, params = params, type = "pred")
