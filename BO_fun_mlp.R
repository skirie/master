library(mlrMBO)
library(rgenoud)
install.packages("rgenoud")

configureMlr(on.learner.warning = "quiet", show.learner.output = FALSE)

units <- 100
layer <- 3
j <- 1
i <- 2

nn_fit_bayes <- function(x) {
  units <- x$units
  layer <- x$layer
  all_rmse <- NULL
  
  units <- as.integer(rep(units, layer))
  if (layer > 1){
    units[2] <- as.integer(units[2]*0.5)
    if (layer == 3){
      units[3] <- as.integer(units[2]*0.5)
    }
  } 
  
  for (j in 1:2){
    set.seed(j*5)
    indices <- sample(1:nrow(df_train))
    folds <- cut(indices, breaks = 5, labels = FALSE)
    
    for (i in 1:5) {
      cat("processing fold #", paste0(j, ".", i), "\n", "Layer:", layer, "Units:", units, "\n")
      
      model <- fun_build_model(df_train = df_train, layer = layer, optimizer = "adam", units = units, lr = 0.001)
      
      # Prepare the train, validation and test data: data from partition # k
      val_test_indices <- which(folds == i, arr.ind = TRUE)
      
      ## different datatype (array, matrix) for varying number of predictors 
      val_test_data <- as.matrix(df_train[val_test_indices, -ncol(df_train)])
      partial_train_data <- as.matrix(df_train[-val_test_indices, -ncol(df_train)])
        
      # min and max for mormalization
      mins_data <- apply(partial_train_data, 2, min, na.rm = T)
      maxs_data <- apply(partial_train_data, 2, max, na.rm = T)
      
      ## same for target data
      val_test_targets <- as.array(df_train[val_test_indices, ncol(df_train)])
      partial_train_targets <- as.array(df_train[-val_test_indices, ncol(df_train)])
      
      # normalize all data
      mins_targets <- min(partial_train_targets, na.rm = T)
      maxs_targets <- max(partial_train_targets, na.rm = T)
      
      # split combined val_test-data into separate validation and test data
      set.seed(i*5)
      vt_indices <- sample(1:nrow(val_test_data))
      vt_folds <- cut(vt_indices, breaks = 2, labels = FALSE)
      
      val_test_indices_2 <- which(vt_folds == 1, arr.ind = TRUE)
      
      # again split and different indices  (array <-> matrix)
      val_data <- val_test_data[val_test_indices_2,]
      test_data <- val_test_data[-val_test_indices_2,]
      
      val_targets <- val_test_targets[val_test_indices_2]
      test_targets <- val_test_targets[-val_test_indices_2]
      
      # predict = x
      partial_train_data <- scale(partial_train_data, center = mins_data, 
                                  scale = maxs_data - mins_data)
      val_data <- scale(val_data, center = mins_data, 
                        scale = maxs_data - mins_data)
      test_data <- scale(test_data, center = mins_data, 
                         scale = maxs_data - mins_data)
      
      # target = y
      partial_train_targets <- scale(partial_train_targets, center = mins_targets, 
                                     scale = maxs_targets - mins_targets)
      val_targets <- scale(val_targets, center = mins_targets, 
                           scale = maxs_targets - mins_targets)
      test_targets <- scale(test_targets, center = mins_targets, 
                            scale = maxs_targets - mins_targets)
      
      # fit model
      model %>% fit(
        partial_train_data, partial_train_targets,
        validation_data = list(val_data, val_targets),
        epochs = 150, batch_size = 100, verbose = 2,
        callbacks = list(callback_early_stopping(patience = 6)))
      
      # predict and scale back
      pred_test <- model %>% predict(test_data)
      rmse_ <- sqrt(mse(test_targets, pred_test))
      all_rmse <- c(all_rmse, rmse_)
      #r2 <- cor(test_targets, pred_test) ^ 2
      #all_r2 <- rbind(all_r2, r2)
    }
  }
  return(mean(all_rmse, na.rm = T))
}

iters = 2
df_train <- df_night_model
## set NA's to zero
df_train[is.na(df_train)] <- 0

par.set = makeParamSet(
  makeIntegerParam("layer", 1, 3),
  makeIntegerParam("units", 50, 110)
)

surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))

svm = makeSingleObjectiveFunction(name = "svm.tuning",
                                  fn = nn_fit_bayes,
                                  par.set = par.set,
                                  has.simple.signature = FALSE,
                                  minimize = TRUE
)

des = generateDesign(n = 12, par.set = getParamSet(svm))
#des$y = apply(des, 1, svm)

ctrl = makeMBOControl()
ctrl = setMBOControlTermination(ctrl, iters = iters)
ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

res = mbo(svm, design = des, learner = surr.km, control = ctrl, show.info = T)

