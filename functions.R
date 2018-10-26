#### ------------------------------------------- ####
#### Functions for modeling trace gas emissions with ANNs ####
#### ------------------------------------------- ####

## Target function ####
fun_tagret <- function(df_train, batchsize = c(30,60,90), k = 5, epochs = 200, lr = 1e-4, layer = 2, optimizer = "rmsprop"){
  df_results_ms <- NULL
  ## best Model structure 
  ## modelrun for different batchsizes
  for (i in 1:length(batchsize)){
    batchsize_ <- batchsize[i]
    cat("Models with Batchsize: ", batchsize_, "!!", sep = "", "\n")
    params <- fun_params(batchsize = batchsize_, k = k, epochs = epochs, lr = lr, layer = layer, optimizer = optimizer)
    results_ <- fun_model_run_ms(df_train = df_train, params = params)
    df_results_ms <- rbind(df_results_ms, results_)
  }
  save(df_results_ms, file = paste0(mypath, "/master/RData/results_model_", layer, "l_", Sys.Date(), ".RData"))
  
  ## best Model
  params <- fun_best_model(df_results = df_results_ms, params = params, type = "nodes")
  
  ## Predictoranalysis
  df_results_pa <- fun_model_run_pa(df_train = df_train, params = params)
  params <- fun_best_model(df_results = df_results_pa, params = params, type = "pred")
  
  save(df_results_pa, file = paste0(mypath, "/master/RData/results_pred_", Sys.Date(), ".RData"))
  
  ## best model structur for best predictor composition ####
  df_train_best <- df_train[,c(params$best_preds_full$predictors, colnames(df_train)[ncol(df_train)])]
  df_results_pa_ms <- NULL
  
  ## best Model structure 
  ## modelrun for different batchsizes
  for (i in 1:length(batchsize)){
    batchsize_ <- batchsize[i]
    cat("PA-Models with Batchsize: ", batchsize_, "!!", sep = "", "\n")
    results_2 <- fun_model_run_ms(df_train = df_train_best, params = params)
    df_results_pa_ms <- rbind(df_results_pa_ms, results_2)
  }
  save(df_results_pa_ms, file = paste0(mypath, "/master/RData/results_pa_ms_", Sys.Date(), ".RData"))
  
  return(list(df_results_ms, df_results_pa, df_results_pa_ms, params))
}

## Funktion Parameter ####
fun_params <- function(batchsize = 64, k = 5, epochs = 200, optimizer = "rmsprop", lr = 1e-4, layer = 2, Nmin = 8, Nmax = 100, by_ = 12, layer_balance = 0.5, times_cv = 4){
  params <- list()
  params[["batchsize"]] <- batchsize
  params[["k"]] <- k
  params[["epochs"]] <- epochs
  params[["optimizer"]] <- optimizer
  params[["lr"]] <- lr
  params[["layer"]] <- layer
  params[["Nmin"]] <- Nmin
  params[["Nmax"]] <- Nmax
  params[["by_"]] <- by_
  params[["layer_balance"]] <- layer_balance
  params[["times_cv"]] <- times_cv
  return(params)
}

## Function model build ####
fun_build_model <- function(df_train, layer, optimizer, units, lr){
  ## optimizer ####
  if (optimizer == "rmsprop"){
    optim_ <- optimizer_rmsprop(lr = lr)  
  } else if (optimizer == "adam"){
    optim_ <- optimizer_adam(lr = lr)
  }
  
  ## Model ####
  if (layer == 1){
    model <- keras_model_sequential() %>% 
      layer_dense(units = units[1], activation = "relu", 
                  input_shape = dim(df_train)[[2]]-1) %>% 
      layer_dense(units = 1) 
  } else if (layer == 2){
    model <- keras_model_sequential() %>% 
      layer_dense(units = units[1], activation = "relu", 
                  input_shape = dim(df_train)[[2]]-1) %>% 
      layer_dense(units = units[2], activation = "relu") %>% 
      layer_dense(units = 1)
  } else if (layer == 3){
    model <- keras_model_sequential() %>% 
      layer_dense(units = units[1], activation = "relu", 
                  input_shape = dim(df_train)[[2]]-1) %>% 
      layer_dense(units = units[2], activation = "relu") %>% 
      layer_dense(units = units[3], activation = "relu") %>% 
      layer_dense(units = 1) 
  } else {
    return("only three layer possible. pls set layer from 1-3")
  }
  
  model %>% compile(
    optimizer = optim_, 
    loss = "mse", 
    metrics = c("mae"))
  
  return(model)
}

## Function model run model structure ####
fun_model_run_ms <- function(df_train, params){
  ## params 
  Nmin <- params[["Nmin"]]
  Nmax <- params[["Nmax"]]
  by_ <- params[["by_"]]
  layer <- params[["layer"]]
  layer_balance <- params[["layer_balance"]]
  
  ## tagret data.frame
  all_r2 <- NULL
  all_rmse <- NULL
  all_sem <- NULL
  all_std <- NULL
  performance <- NULL
  
  ## matrix with Nodes
  N <- seq(Nmin, Nmax, by_)
  N_ <- matrix(data = c(N, N*layer_balance, N*layer_balance*layer_balance), nrow = length(N), ncol = 3)
  
  ## key 
  if (layer == 1){
    key <- c(paste0("N_", N_[,1], "_b_", params[["batchsize"]]))
  } else if (layer == 2){
    key <- c(paste0("N_", N_[,1], "_b_", params[["batchsize"]]), paste0("N_", N_[,1], "_", N_[,2], "_b_", params[["batchsize"]]))
  } else if (layer == 3){
    key <- c(paste0("N_", N_[,1], "_b_", params[["batchsize"]]), paste0("N_", N_[,1], "_", N_[,2], "_b_", params[["batchsize"]]), 
             paste0("N_", N_[,1], "_", N_[,2], "_", N_[,3], "_b_", params[["batchsize"]])) 
  }
  
  ## Model computing
  for (l in 1:layer){
    params[["layer"]] <- l
    for (i in 1:nrow(N_)){
      start_time <- Sys.time()
      cv_ <- fun_model_compute_full(df_train = df_train, params = params, units = N_[i,1:l])
      end_time <- Sys.time()
      
      rmse_ <- mean(cv_[[1]])
      r2_ <- mean(cv_[[2]])
      std_ <- sd(cv_[[1]])
      sem_ <- sd(cv_[[1]])/sqrt(length(cv_[[1]]))
      time_ <- end_time - start_time
      
      all_rmse <- rbind(all_rmse, rmse_)
      all_r2 <- rbind(all_r2, r2_)
      all_std <- rbind(all_std, std_)
      all_sem <- rbind(all_sem, se_m)
      performance <- rbind(performance, time_)
      
      cat("Complete model: N_", N_[i,1:l], "! Time: ", time_, "\n", sep = "")
    }
  }
  df_results <- data.frame(key = key, rmse = all_rmse, r2 = all_r2, std = all_std, sem = all_sem, performance = performance)
  return(df_results)
}

## Function model compute ####
fun_model_compute <- function(df_train, params, units){
  ## params
  k <- params[["k"]]
  num_epochs <- params[["epochs"]]
  optimizer <- params[["optimizer"]]
  lr <- params[["lr"]]
  layer <- params[["layer"]]
  batch <- params[["batchsize"]]
  
  ## NA to 0
  df_train[is.na(df_train)] <- 0
  
  ## empty vectors for Results
  #all_rme_histories <- matrix(NA, nrow = k, ncol = num_epochs) 
  all_rmse <- NULL
  all_r2 <- NULL

  ## Callback - early stopping ####
  callback_list <- list(callback_early_stopping(patience = 6))
  
  ## Model
  model <- fun_build_model(df_train = df_train, layer = layer, optimizer = optimizer, units = units, lr = lr)
  
  ## Crossvalidation 2x k-fold crossvalidation
  for (j in 1:2){
    set.seed(j*5)
    indices <- sample(1:nrow(df_train))
    folds <- cut(indices, breaks = k, labels = FALSE)
    
    for (i in 1:k) {
      cat("processing fold #", paste0(j, ".", i), "\n")
      
      # Prepare the validation and test data: data from partition # k
      val_test_indices <- which(folds == i, arr.ind = TRUE) 
      val_test_data <- as.matrix(df_train[, 1:18][val_test_indices,])
      val_test_targets <- as.array(df_train[, 19][val_test_indices])
      
      # split into validation and test data
      set.seed(i*5)
      vt_indices <- sample(1:nrow(val_test_data))
      vt_folds <- cut(vt_indices, breaks = 2, labels = FALSE)
      
      val_test_indices_2 <- which(vt_folds == 1, arr.ind = TRUE) 
      val_data <- as.matrix(val_test_data[val_test_indices_2,])
      val_targets <- as.array(val_test_targets[val_test_indices_2])
      test_data <- as.matrix(val_test_data[-val_test_indices_2,])
      test_targets <- as.array(val_test_targets[-val_test_indices_2])
      
      # Prepare the training data: data from all other partitions
      partial_train_data <- as.matrix(df_train[, 1:18][-val_test_indices,])
      partial_train_targets <- as.array(df_train[, 19][-val_test_indices])
      
      # normalize all data
      mins_data <- apply(partial_train_data, 2, min, na.rm = T)
      maxs_data <- apply(partial_train_data, 2, max, na.rm = T)
      mins_targets <- min(partial_train_targets, na.rm = T)
      maxs_targets <- max(partial_train_targets, na.rm = T)
      
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
      history <- model %>% fit(
        partial_train_data, partial_train_targets,
        validation_data = list(val_data, val_targets),
        epochs = num_epochs, batch_size = batch, verbose = 0,
        callbacks = callback_list)
      
      # Evaluate the model on the validation data
      #mae_history <- history$metrics$val_mean_absolute_error
      #all_mae_histories[i,1:length(mae_history)] <- mae_history
      
      # predict and scale back
      pred_test <- model %>% predict(test_data)
      rmse_ <- rmse(test_targets, pred_test)
      all_rmse <- rbind(all_rmse, rmse_)
      r2 <- cor(test_targets, pred_test) ^ 2
      all_r2 <- rbind(all_r2, r2)
    }
  }
  return(list(all_rmse, all_r2))
}

## Function model compute full ####
fun_model_compute_full <- function(df_train, params, units){
  ## params
  k <- params[["k"]]
  num_epochs <- params[["epochs"]]
  optimizer <- params[["optimizer"]]
  lr <- params[["lr"]]
  layer <- params[["layer"]]
  batch <- params[["batchsize"]]
  times_cv <- params[["times_cv"]]
  
  ## NA to 0
  df_train[is.na(df_train)] <- 0
  
  ## empty vectors for Results
  #all_rme_histories <- matrix(NA, nrow = k, ncol = num_epochs) 
  all_rmse <- NULL
  all_r2 <- NULL
  
  ## Callback - early stopping ####
  callback_list <- list(callback_early_stopping(patience = 6))
  
  ## Model
  model <- fun_build_model(df_train = df_train, layer = layer, optimizer = optimizer, units = units, lr = lr)
  
  ## Crossvalidation j times k-fold crossvalidation
  for (j in 1:times_cv){
    set.seed(j*5)
    indices <- sample(1:nrow(df_train))
    folds <- cut(indices, breaks = k, labels = FALSE)
    
    for (i in 1:k) {
      cat("processing fold #", paste0(j, ".", i), "\n")
      
      # Prepare the train, validation and test data: data from partition # k
      val_test_indices <- which(folds == i, arr.ind = TRUE)
      
      ## different datatype (array, matrix) for varying number of predictors 
      # -> indices are different (array <-> matrix)
      if (ncol(df_train) == 2){
        val_test_data <- as.array(df_train[val_test_indices, -ncol(df_train)])
        partial_train_data <- as.array(df_train[-val_test_indices, -ncol(df_train)])
        
        # min and max for mormalization
        mins_data <- min(partial_train_data, na.rm = T)
        maxs_data <- max(partial_train_data, na.rm = T)
      } else {
        val_test_data <- as.matrix(df_train[val_test_indices, -ncol(df_train)])
        partial_train_data <- as.matrix(df_train[-val_test_indices, -ncol(df_train)])
        
        # min and max for mormalization
        mins_data <- apply(partial_train_data, 2, min, na.rm = T)
        maxs_data <- apply(partial_train_data, 2, max, na.rm = T)
      }
      
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
      if (ncol(df_train) == 2){
        val_data <- val_test_data[val_test_indices_2]
        test_data <- val_test_data[-val_test_indices_2]
      } else {
        val_data <- val_test_data[val_test_indices_2,]
        test_data <- val_test_data[-val_test_indices_2,]
      }
      
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
      history <- model %>% fit(
        partial_train_data, partial_train_targets,
        validation_data = list(val_data, val_targets),
        epochs = num_epochs, batch_size = batch, verbose = 0,
        callbacks = callback_list)
      
      # Evaluate the model on the validation data
      #mae_history <- history$metrics$val_mean_absolute_error
      #all_mae_histories[i,1:length(mae_history)] <- mae_history
      
      # predict and scale back
      pred_test <- model %>% predict(test_data)
      rmse_ <- rmse(test_targets, pred_test)
      all_rmse <- rbind(all_rmse, rmse_)
      r2 <- cor(test_targets, pred_test) ^ 2
      all_r2 <- rbind(all_r2, r2)
    }
  }
  return(list(all_rmse, all_r2))
}
## Function best model ####
fun_best_model <- function(df_results, params){
  # print ordered results
  print(df_results[order(df_results$rmse),])
  
  # model with lowest rmse
  w_best <- which(df_results$rmse == min(df_results$rmse))
  
  # extract layer, nodes and batchsize from key
  str <- suppressWarnings(as.numeric(strsplit(as.character(df_results$key[w_best]), "_")[[1]]))
  str_ <- str[!is.na(str)]
  batch_size <- str_[length(str_)]
  nodes <- str_[-length(str_)]
  layer <- length(nodes)
  
  params[["best"]] <- list("layer" = layer, "nodes" = nodes, "batch_size" = batch_size, "model" = df_results[w_best,]) 
  return(params)
}
## Function best model NEW ####
fun_best_model <- function(df_results, params, type){
  # print ordered results
  print(df_results[order(df_results$rmse),][1:10,])
  
  # model with lowest rmse
  w_best <- which(df_results$rmse == min(df_results$rmse))
  
  # extract layer, nodes and batchsize from key
  if (type == "nodes"){
    str <- suppressWarnings(as.numeric(strsplit(as.character(df_results$key[w_best]), "_")[[1]]))
    str_ <- str[!is.na(str)]
    batch_size <- str_[length(str_)]
    nodes <- str_[-length(str_)]
    layer <- length(nodes)
    
    params[["best"]] <- list("layer" = layer, "nodes" = nodes, "batch_size" = batch_size, "model" = df_results[w_best,]) 
  } else if(type == "pred"){
    str_full <- strsplit(as.character(df_results$predictors[w_best]), "+", fixed = TRUE)[[1]][-1]
    params[["best_preds_full"]] <- list("predictors" = str_full, "model" = df_results[w_best,]) 
    
    w_best_7 <- which(df_results$rmse[df_results$level <= 7] == min(df_results$rmse[df_results$level <= 7]))
    w_best_12 <- which(df_results$rmse[df_results$level <= 12] == min(df_results$rmse[df_results$level <= 12]))
    str_7 <- strsplit(as.character(df_results$predictors[w_best_7]), "+", fixed = TRUE)[[1]][-1]
    str_12 <- strsplit(as.character(df_results$predictors[w_best_12]), "+", fixed = TRUE)[[1]][-1]
    params[["best_preds_7"]] <- list("predictors" = str_7, "model" = df_results[w_best_7,]) 
    params[["best_preds_12"]] <- list("predictors" = str_12, "model" = df_results[w_best_12,])
  }
  return(params)
}

## Function model run predictor analysis ####
fun_model_run_pa <- function(df_train, params){
  ## params 
  params[["layer"]] <- params[["best"]]$layer
  N <-  params[["best"]]$nodes
  params[["batchsize"]] <- params[["best"]]$batch_size
  
  ## key
  key <- NULL
  level <- 1
  for (i in 18:1){
    key_ <- as.character(level+c(1:i)/100)
    key <- c(key,key_)
    level <- level+1
  }
  
  ## required empty features / target 
  all_rmse <- vector("list", 18)
  all_r2 <- vector("list", 18)
  k <- 18
  col_ <- colnames(df_night_model)
  level_ <- NULL
  pred_ <- NULL
  best_pred_name <- vector("list", 18)
  count <- 1
  
  ## loop for every predictor composition
  for (i in c(1:18)){
    for (j in c(1:k)){
      # print computed predictor combination      
      pred_[count] <- paste0(best_pred_name[[i]], "+", col_[j])
      cat("Predictor Posibility: ", count, "/171. Used predictors:", pred_[count], sep = "", "\n")
      
      # choose of one predictor and always Y. Y needs to be at last column position.
      train_ <- df_night_model[, c(col_[j], col_[length(col_)])]
      if (i == 1){
        all_train <- train_
      } else {
        all_train <- cbind(best_train, train_)
      }
      
      # compute Model for this predictor composition
      cv_ <- fun_model_compute_full(df_train = all_train, params = params, units = N)
      rmse_ <- mean(cv_[[1]])
      r2_ <- mean(cv_[[2]])
      all_rmse[[i]] <- c(all_rmse[[i]], rmse_)
      all_r2[[i]] <- c(all_r2[[i]], r2_)
      level_ <- rbind(level_, i)
      
      count <- count + 1
    }
    # best model / predictor
    w_best <- which(all_rmse[[i]] == min(all_rmse[[i]]))
    
    # extract best predictor
    best_pred <- data.frame("dummy" = df_night_model[, w_best])
    colnames(best_pred) <- col_[w_best]
    
    best_pred_name[i:length(best_pred_name)] <- paste0(best_pred_name[[i]], "+", col_[w_best])
    
    # new data.frame with best predictors
    if (i == 1){
      best_train <- best_pred
    } else {
      best_train <- cbind(best_train, best_pred)
    }
    
    # adjust k and possible predictor pool
    k <- k-1
    print(paste0("Level: ", i, "! Predictors: ", best_pred_name[[i]]))
    col_ <- col_[-w_best]
  }
  
  df_results <- data.frame(key = key, level = level_, predictors = pred_, 
                           rmse = unlist(all_rmse, use.names=FALSE), r2 = unlist(all_r2, use.names=FALSE))
  
  return(df_results)
}
#### ####

#### ####
