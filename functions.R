#### ------------------------------------------- ####
#### Functions for modeling trace gas emissions with ANNs ####
#### ------------------------------------------- ####

#### ----------------------- ####
#### Target Functions ####
#### ----------------------- ####
## Target function GRID ####
fun_tagret_grid <- function(df_train, batchsize = c(30,60,90), k = 5, epochs = 200, lr = 1e-4, layer = 2, optimizer = "rmsprop", path){
  df_results_ms <- NULL
  all_mae_history <- NULL
  params <- fun_params(k = k, epochs = epochs, lr = lr, layer = layer, optimizer = optimizer)
  
  ## Best Model structure (Layers & Nodes)
  ## modelrun for different batchsizes
  for (i in 1:length(batchsize)){
    params[["batchsize"]] <- batchsize[i]
    cat("Models with Batchsize: ", params[["batchsize"]], "!!", sep = "", "\n")
    
    results_ms <- fun_model_run_ms(df_train = df_train, params = params)
    results_ <- results_ms[[1]]
    mae_history <- results_ms[[2]]
    df_results_ms <- rbind(df_results_ms, results_)
    all_mae_history <- rbind(all_mae_history, mae_history)
  }
  save(df_results_ms, all_mae_history, file = c(paste0(path, "/RData/results_model_", layer, "l_", Sys.Date(), ".RData")))
  
  ## Best Model (Layers & Nodes)
  params <- fun_best_model(df_results = df_results_ms, params = params, type = "nodes")
  
  ## Predictoranalysis - Best Predictor Subset
  df_results_pa <- fun_model_run_pa(df_train = df_train, params = params)
  params <- fun_best_model(df_results = df_results_pa, params = params, type = "pred")
  
  save(df_results_pa, params, file = paste0(path, "/RData/results_pred_", Sys.Date(), ".RData"))
  
  ## best model structur for best predictor Subset ####
  df_train_best <- df_train[,c(params$best_preds_full$predictors, colnames(df_train)[ncol(df_train)])]
  df_results_pa_ms <- NULL
  
  ## best Model structure 
  ## modelrun for different batchsizes
  for (i in 1:length(batchsize)){
    params[["batchsize"]] <- batchsize[i]
    cat("PA-Models with Batchsize: ", batchsize[i], "!!", sep = "", "\n")
    
    results_2 <- fun_model_run_ms(df_train = df_train_best, params = params)
    df_results_pa_ms <- rbind(df_results_pa_ms, results_2)
  }
  save(df_results_pa_ms, file = paste0(path, "/RData/results_p_m_", Sys.Date(), ".RData"))
  
  return(list(df_results_ms, df_results_pa, df_results_pa_ms, params))
}


## Target function BO ####
fun_tagret_bo <- function(df_train, batchsize = c(40, 80), k = 5, epochs = 200, lr = 1e-3, layer = 3, optimizer = "adam", path){
  results_ms <- list()
  df_results_ms <- NULL
  params <- fun_params(k = k, epochs = epochs, lr = lr, layer = layer, optimizer = optimizer)
  
  ## Best Model structure (Layers & Nodes)
  ## modelrun for different batchsizes
  for (i in 1:length(batchsize)){
    params[["batchsize"]] <- batchsize[i]
    cat("Models with Batchsize: ", params[["batchsize"]], "!!", sep = "", "\n")
    
    results_ms[[i]] <- fun_bo_mlr(df_train = df_train, params = params)
    df_results_ms <- rbind(df_results_ms, c(results_ms[[i]]$x$layer, results_ms[[i]]$x$units, batchsize[i], results_ms[[i]]$y))
  }
  save(df_results_ms, results_ms, file = c(paste0(path, "/RData/results_model_", layer, "l_", Sys.Date(), ".RData")))
  
  ## Best Model (Layers & Nodes)
  params[["best"]]$layer <- df_results_ms[which(df_results_ms[,4] == min(df_results_ms[,4])), 1]
  params[["best"]]$units <- df_results_ms[which(df_results_ms[,4] == min(df_results_ms[,4])), 2]
  params[["best"]]$batch_size <- df_results_ms[which(df_results_ms[,4] == min(df_results_ms[,4])), 3]
  cat("Best Model: Layer: ", params[["best"]]$layer, "! Units: ", params[["best"]]$units, "! Batchsize: ", params[["best"]]$batch_size, "!", "\n")
  
  ## Predictoranalysis - Best Predictor Subset
  df_results_pa <- fun_model_run_pa(df_train = df_train, params = params)
  params <- fun_best_model(df_results = df_results_pa, params = params, type = "pred")
  
  save(df_results_pa, params, file = paste0(path, "/RData/results_pred_", Sys.Date(), ".RData"))
  return(list(df_results_ms, df_results_pa, params))
}
#### ----------------------- ####
#### Parameter Model Functions ####
#### ----------------------- ####

## Function Parameter ####
fun_params <- function(batchsize = 100, k = 5, epochs = 200, optimizer = "adam", lr = 1e-3, layer = 3L, Nmin = 50L, Nmax = 120L, by_ = 12, layer_balance = 0.5, times_cv = 4, iters_bo = 20){
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
  params[["iters_bo"]] <- iters_bo
  return(params)
}

## Function model build ####
fun_build_model <- function(df_train, layer, optimizer, units, lr, dropout = F){
  ## optimizer ####
  if (optimizer == "rmsprop"){
    optim_ <- optimizer_rmsprop(lr = lr)  
  } else if (optimizer == "adam"){
    optim_ <- optimizer_adam(lr = lr)
  }
  
  if (is.numeric(dropout)){
    if (layer == 1){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train)-1) %>%
        layer_dropout(rate = dropout) %>% 
        layer_dense(units = 1) 
    } else if (layer == 2){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train)-1) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = units[2], activation = "relu", initializer_he_uniform(seed = 5)) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = 1) # No activation. Chollet et al. p. 78
    } else if (layer == 3){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train)-1) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = units[2], activation = "relu", initializer_he_uniform(seed = 5)) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = units[3], activation = "relu", initializer_he_uniform(seed = 5)) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = 1) 
    } else {
      return("only three layer possible. pls set layer from 1-3")
    }
  } else {
    ## Model ####
    if (layer == 1){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train)-1) %>% 
        layer_dense(units = 1) 
    } else if (layer == 2){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train)-1) %>% 
        layer_dense(units = units[2], activation = "relu", initializer_he_uniform(seed = 5)) %>% 
        layer_dense(units = 1) # No activation. Chollet et al. p. 78
    } else if (layer == 3){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train)-1) %>% 
        layer_dense(units = units[2], activation = "relu", initializer_he_uniform(seed = 5)) %>% 
        layer_dense(units = units[3], activation = "relu", initializer_he_uniform(seed = 5)) %>% 
        layer_dense(units = 1) 
    } else {
      return("only three layer possible. pls set layer from 1-3")
    }
  }
  
  model %>% compile(
    optimizer = optim_, 
    loss = "mse", 
    metrics = c("mae"))
  
  return(model)
}

## Function best model NEW ####
fun_best_model <- function(df_results, params, type){
  # print ordered results
  print("Best Models")
  print(df_results[order(df_results$mse),][1:10,])
  print("Best Performance")
  print(df_results[order(df_results$performance),][1:10,])
  
  # extract layer, nodes and batchsize from key
  if (type == "nodes"){
    # models with MSE < lowest mse + sem | One standard error rule
    w_best <- which(df_results$mse < min(df_results$mse, na.rm = T) + df_results$sem[which(df_results$mse == min(df_results$mse, na.rm = T))])
    w_perf <- w_best[which(df_results$performance[w_best] == min(df_results$performance[w_best], na.rm = T))]
    
    str <- suppressWarnings(as.numeric(strsplit(as.character(df_results$key[w_perf]), "_")[[1]]))
    str_ <- str[!is.na(str)]
    batch_size <- str_[length(str_)]
    nodes <- str_[-length(str_)]
    layer <- length(nodes)
    
    params[["best"]] <- list("layer" = layer, "nodes" = nodes, "batch_size" = batch_size, "model" = df_results[w_perf,]) 
  } else if(type == "pred"){
    w_best <- which(df_results$mse == min(df_results$mse, na.rm = T))
    
    str_full <- strsplit(as.character(df_results$predictors[w_best]), "+", fixed = TRUE)[[1]][-1]
    params[["best_preds_full"]] <- list("predictors" = str_full, "model" = df_results[w_best,]) 
    
    w_best_5 <- which(df_results$mse[df_results$level <= 5] == min(df_results$mse[df_results$level <= 5], na.rm = T))
    #w_best_12 <- which(df_results$mse[df_results$level <= 12] == min(df_results$mse[df_results$level <= 12], na.rm = T))
    str_5 <- strsplit(as.character(df_results$predictors[w_best_5]), "+", fixed = TRUE)[[1]][-1]
    #str_12 <- strsplit(as.character(df_results$predictors[w_best_12]), "+", fixed = TRUE)[[1]][-1]
    params[["best_preds_5"]] <- list("predictors" = str_5, "model" = df_results[w_best_5,]) 
    #params[["best_preds_12"]] <- list("predictors" = str_12, "model" = df_results[w_best_12,])
  }
  return(params)
}

#### ----------------------- ####
#### Model Evaluation Functions ####
#### ----------------------- ####

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
  all_mse <- NULL
  all_sem <- NULL
  all_std <- NULL
  performance <- NULL
  all_mae_history <- NULL
  
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
    for (i in 1:nrow(N_)){
      start_time <- Sys.time()
      ## create Model
      model <- fun_build_model(df_train = df_train, layer = l, optimizer = params[["optimizer"]], 
                               units = N_[i,1:l], lr = params[["lr"]])
      
      cv_ <- fun_model_compute_full(df_train = df_train, params = params, model = model)
      end_time <- Sys.time()
      
      mse_ <- mean(cv_[[1]], na.rm = T)
      r2_ <- mean(cv_[[2]], na.rm = T)
      std_ <- sd(cv_[[1]], na.rm = T)
      sem_ <- sd(cv_[[1]], na.rm = T)/sqrt(length(cv_[[1]]))
      time_ <- end_time - start_time
      mae_history <- apply(cv_[[3]], 2, mean, na.rm = T)
      
      all_mse <- rbind(all_mse, mse_)
      all_r2 <- rbind(all_r2, r2_)
      all_std <- rbind(all_std, std_)
      all_sem <- rbind(all_sem, sem_)
      performance <- rbind(performance, time_)
      all_mae_history <- rbind(all_mae_history, mae_history)
      
      cat("Complete model: N_", N_[i,1:l], "! Time: ", time_, "\n", sep = "")
    }
  }
  df_results <- data.frame(key = key, mse = all_mse, r2 = all_r2, std = all_std, sem = all_sem, performance = performance)
  rownames(all_mae_history) <- key
  
  return(list(df_results, all_mae_history))
}

## Function model run Bayesian Opt. ####
fun_bo_mlr <- function(df_train, params){
  
  nn_fit_bayes <- function(x) {
    units <- x$units
    layer <- x$layer
    
    units <- as.integer(rep(units, layer))
    if (layer > 1){
      units[2] <- as.integer(units[2]*0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2]*0.5)
      }
    }
    
    params[["units"]] <- units
    params[["layer"]] <- layer
    
    cat("Model: # Layer:", layer, " # units:", units, "\n")
    #model <- fun_build_model(df_train = df_train, layer = layer, optimizer = params[["optimizer"]], units = units, lr = params[["lr"]])
    results_ <- fun_model_compute_full(df_train = df_train, params = params, type = "full")
    
    return(sqrt(mean(results_[[1]], na.rm = T)))
  }
  
  ## set hyperparameterspace
  par.set = makeParamSet(
    makeIntegerParam("layer", 1, params[["layer"]]),
    makeIntegerParam("units", params[["Nmin"]], params[["Nmax"]]))
  
  ## create learner -> dicekriging (GaussianProcess)
  surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
  
  ## objective Function
  obj.fun = makeSingleObjectiveFunction(name = "svm.tuning",
                                        fn = nn_fit_bayes,
                                        par.set = par.set,
                                        has.simple.signature = FALSE,
                                        minimize = TRUE)
  
  ## design -> pre hyperparameter calulations
  set.seed(352)
  des = generateDesign(n = 10, par.set = getParamSet(obj.fun))
  
  ## control
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = params[["iters_bo"]])
  ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  #ctrl = setMBOControlInfill(ctrl, filter.proposed.points = TRUE)
  
  res_ = mbo(obj.fun, design = des, learner = surr.km, control = ctrl, show.info = T)
  
  return(res_)
}

## Function model run Bayesian Opt. ####
fun_bo_mlr_2 <- function(df_train, params){
  
  nn_fit_bayes <- function(x) {
    units <- x$units
    layer <- x$layer
    
    units <- as.integer(rep(units, layer))
    if (layer > 1){
      units[2] <- as.integer(units[2]*0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2]*0.5)
      }
    }
    
    params[["units"]] <- units
    params[["layer"]] <- layer
    
    cat("Model: # Layer:", layer, " # units:", units, "\n")
    #model <- fun_build_model(df_train = df_train, layer = layer, optimizer = params[["optimizer"]], units = units, lr = params[["lr"]])
    results_ <- fun_model_compute_full(df_train = df_train, params = params, type = "full")
    
    return(sqrt(mean(results_[[1]], na.rm = T)))
  }
  
  ## set hyperparameterspace
  par.set = makeParamSet(
    makeIntegerParam("layer", 1, params[["layer"]]),
    makeIntegerParam("units", params[["Nmin"]], params[["Nmax"]]))
  
  ## create learner -> dicekriging (GaussianProcess)
  surr.km = makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
  
  ## objective Function
  obj.fun = makeSingleObjectiveFunction(name = "svm.tuning",
                                        fn = nn_fit_bayes,
                                        par.set = par.set,
                                        has.simple.signature = FALSE,
                                        minimize = TRUE,
                                        noisy = TRUE)
  
  ## design -> pre hyperparameter calulations
  set.seed(352)
  des = generateDesign(n = 10, par.set = getParamSet(obj.fun))
  
  ## control
  ctrl = makeMBOControl()
  ctrl = setMBOControlTermination(ctrl, iters = params[["iters_bo"]])
  ctrl = setMBOControlInfill(ctrl, crit = crit.aei)
  #ctrl = setMBOControlInfill(ctrl, filter.proposed.points = TRUE)
  
  res_ = mbo(obj.fun, design = des, learner = surr.km, control = ctrl, show.info = T)
  
  return(res_)
}
## Function model compute full ####
fun_model_compute_full <- function(df_train, params, type = "full"){
  ## params
  k <- params[["k"]]
  num_epochs <- params[["epochs"]]
  optimizer <- params[["optimizer"]]
  lr <- params[["lr"]]
  layer <- params[["layer"]]
  batch <- params[["batchsize"]]
  
  if (type == "pred"){
    times_cv <- 2
    units <- rep(params[["units"]], params[["layer"]])
    if (layer > 1){
      units[2] <- as.integer(units[2]*0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2]*0.5)
      }
    }
  } else {
    times_cv <- params[["times_cv"]]
    units <- params[["units"]]
  }
  
  ## NA to 0
  df_train[is.na(df_train)] <- 0
  
  ## empty vectors for Results
  all_mae_histories <- NULL
  all_mse <- NULL
  all_r2 <- NULL
  
  ## Callback - early stopping ####
  callback_list <- list(callback_early_stopping(patience = 6))
  
  ## Crossvalidation j times k-fold crossvalidation
  for (j in 1:times_cv){
    set.seed(j*5)
    indices <- sample(1:nrow(df_train))
    folds <- cut(indices, breaks = k, labels = FALSE)
    
    for (i in 1:k) {
      cat("processing fold #", paste0(j, ".", i), "\n")
      
      ## Model
      model <- fun_build_model(df_train = df_train, layer = layer, optimizer = optimizer, units = units, lr = lr)
      
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
      mae_history <- history$metrics$val_mean_absolute_error
      mae_history_ <- rep(NA, num_epochs)
      mae_history_[1:length(mae_history)] <- mae_history
      all_mae_histories <- rbind(all_mae_histories, mae_history_)
      
      # predict and scale back
      pred_test <- model %>% predict(test_data)
      mse_ <- mse(test_targets, pred_test)
      all_mse <- rbind(all_mse, mse_)
      r2 <- cor(test_targets, pred_test) ^ 2
      all_r2 <- rbind(all_r2, r2)
      
      K <- backend()
      K$clear_session()
      gc()
    }
  }
  return(list(all_mse, all_r2, all_mae_histories))
}
## Function model run predictor analysis ####
fun_model_run_pa <- function(df_train, params){
  ## params 
  params[["layer"]] <- params[["best"]]$layer
  params[["batchsize"]] <- params[["best"]]$batch_size
  params[["units"]] <- params[["best"]]$units
  
  ## key
  key <- NULL
  level <- 1
  for (i in c(ncol(df_train)-1):1){
    key_ <- as.character(level+c(1:i)/100)
    key <- c(key,key_)
    level <- level+1
  }
  
  ## required empty features / target 
  all_mse <- vector("list", ncol(df_train)-1)
  all_r2 <- vector("list", ncol(df_train)-1)
  k <- ncol(df_train)-1
  col_ <- colnames(df_train)
  level_ <- NULL
  pred_ <- NULL
  best_pred_name <- vector("list", ncol(df_train)-1)
  count <- 1
  
  ## loop for every predictor composition
  for (i in c(1:(ncol(df_train)-1))){
    for (j in c(1:k)){
      # print computed predictor combination      
      pred_[count] <- paste0(best_pred_name[[i]], "+", col_[j])
      cat("Predictor Posibility: ", count, "/", ncol(df_train)*(ncol(df_train)-1)/2, ". Used predictors:", pred_[count], sep = "", "\n")
      
      # choose of one predictor and always Y. Y needs to be at last column position.
      train_ <- df_train[, c(col_[j], col_[length(col_)])]
      if (i == 1){
        all_train <- train_
      } else {
        all_train <- cbind(best_train, train_)
      }
      
      ## create Model 
      #model <- fun_build_model(df_train = all_train, layer = params[["best"]]$layer, optimizer = params[["optimizer"]], 
                               #units = N, lr = params[["lr"]])
      
      # compute Model for this predictor composition
      cv_ <- fun_model_compute_full(df_train = all_train, params = params, type = "pred")
      mse_ <- mean(cv_[[1]], na.rm = T)
      r2_ <- mean(cv_[[2]], na.rm = T)
      all_mse[[i]] <- c(all_mse[[i]], mse_)
      all_r2[[i]] <- c(all_r2[[i]], r2_)
      level_ <- rbind(level_, i)
      
      count <- count + 1
    }
    # best model / predictor
    w_best <- which(all_mse[[i]] == min(all_mse[[i]], na.rm = T))
    
    # extract best predictor
    best_pred <- data.frame("dummy" = df_train[, w_best])
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
                           mse = unlist(all_mse, use.names=FALSE), r2 = unlist(all_r2, use.names=FALSE))
  
  return(df_results)
}
#### ----------------------- ####
#### Additional Functions ####
#### ----------------------- ####
## Function model dropout analysis  ####
fun_model_drop <- function(df_train, params) {
  df_train[is.na(df_train)] <- 0
  
  optimizer <- params[["optimizer"]]
  lr <- params[["lr"]]
  layer <- params[["best"]]$layer
  batch <- params[["best"]]$batch_size
  units <- params[["best"]]$nodes
  
  dropout <- seq(0.1,0.5,0.1)
  
  all_mae_history <- NULL
  all_mse <- NULL
  all_r2 <- NULL
  all_std <- NULL
  all_sem <- NULL
  performance <- NULL
  
  ## Model computing
  for (i in 1:length(dropout)){
    start_time <- Sys.time()
    ## create Model
    model <- fun_build_model(df_train = df_train, layer = layer, optimizer = optimizer, units = units, 
                             lr = lr, dropout = dropout[i])
    
    cv_ <- fun_model_compute_full(df_train = df_train, params = params, model = model, type = "pred")
    end_time <- Sys.time()
    
    mse_ <- mean(cv_[[1]])
    r2_ <- mean(cv_[[2]])
    std_ <- sd(cv_[[1]])
    sem_ <- sd(cv_[[1]])/sqrt(length(cv_[[1]]))
    time_ <- end_time - start_time
    mae_history <- apply(cv_[[3]], 2, mean, na.rm = T)
    
    all_mse <- rbind(all_mse, mse_)
    all_r2 <- rbind(all_r2, r2_)
    all_std <- rbind(all_std, std_)
    all_sem <- rbind(all_sem, sem_)
    performance <- rbind(performance, time_)
    all_mae_history <- rbind(all_mae_history, mae_history)
    
    cat("Complete model: dropout_rate_", i, "! Time: ", time_, "\n", sep = "")
  }
  
  df_results <- data.frame(mse = all_mse, r2 = all_r2, std = all_std, sem = all_sem, performance = performance)
  rownames(all_mae_history) <- as.character(dropout)
  
  return(list(df_results, all_mae_history))
  
}

## Restart R session and load all pacakges ####
fun_rest <- function(){
  ## Restart R session 
  .rs.restartR()
  
  ## Packages 
  check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  packages <- c("keras", "ggplot2", "Metrics", "httpuv", "rdrop2", "mlrMBO", "corrplot", "rgenoud", "betareg", "MASS")
  check.packages(packages) 
  use_condaenv("r-tensorflow")
  
}
#### ####
