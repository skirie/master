#### ------------------------------------------- ##
#### Functions for modeling trace gas emissions with ANNs ##
#### ------------------------------------------- ##

#### ----------------------- ##
#### 0 - Package Function  ##
#### ----------------------- ##

CheckPackages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

#### ----------------------- ##
#### 1 - Check data availability ##
#### ----------------------- ##

CheckData <- function(){
  if (!("df_night_model" %in% ls(envir = .GlobalEnv))){
    if (file.exists(paste0(mypath, "/RData/df_model.RData")) == T){
      print("Load Data from Project folder!")
      load(paste0(mypath, "/RData/df_model.RData"), envir = .GlobalEnv)
    } else {
      print("Download Data from Dropbox!")
      drop_download('Master/R/df_model.RData', paste0(mypath, "/RData/df_model.RData"), overwrite = T, dtoken = token)
      
      load(paste0(mypath, "/RData/df_model.RData"), envir = .GlobalEnv)
    }
  }
  
  if (!("df_night_model" %in% ls(envir = .GlobalEnv))){
    print("Excecute Pre-Processing Script! This can take a few minutes...")
    source("Data_Preprocessing.R")
  }
}

#### ----------------------- ##
#### 2 - Pre analysis Predictors ##
#### ----------------------- ##
  
  ## These functions doing a pretictor pre selection of the different soil depths (soil temperatur and moisture)

## Pre analysis Predictors ##
PreAnalysisPredictors <- function(df_train, params){
  ## required empty features / target 
  all_mse <- vector("list", ncol(df_train) - 1)
  all_r2 <- vector("list", ncol(df_train) - 1)
  k <- ncol(df_train) - 1
  col_ <- colnames(df_train)
  
  ## loop for every predictor
  for (i in 1:k){
    # print computed predictor combination      
    cat("Predictor: ", i, "/", ncol(df_train) - 1, ". Used predictors:", col_[i], sep = "", "\n")
    
    # choose of one predictor and always Y. Y needs to be at last column position.
    train_ <- df_train[, c(col_[i], col_[ncol(df_train)])]
    
    # compute Model for this predictor composition
    cv_ <- ComputeModel(df_train = train_, params = params, type = "prepred")
    mse_ <- mean(cv_[[1]], na.rm = T)
    r2_ <- mean(cv_[[2]], na.rm = T)
    all_mse[[i]] <- c(all_mse[[i]], mse_)
    all_r2[[i]] <- c(all_r2[[i]], r2_)
  }
  
  df_results <- data.frame(predictors = col_[1:k], mse = unlist(all_mse, use.names=FALSE), 
                           r2 = unlist(all_r2, use.names=FALSE))
  df_results <- df_results[order(df_results$mse), ]
  
  return(df_results)
}

## Predictor preanalysis target data frame ##
TargetPreAnalysisPredictors <- function(df_train, cluster = T, method_norm = "range_1_1", 
                                        batchsize = 30, units = 40, layer = 2, variable = "NEE_cor"){
  names_soil_t <- c("Ts1", "Ts2", "Ts3", "Ts4", "Ts5", "Ts6", "TS_mean")
  names_soil_m <- c("Soil.moisture1", "Soil.moisture2", "Soil.moisture3", "Soil.moisture4", "Soil.moisture_main", "MS_mean")
  # names_rad <- c("SWin", "PPFDin")
  
  names_ <- colnames(df_train)
  w_soil_t <- which(names_ %in% names_soil_t)
  w_soil_m <- which(names_ %in% names_soil_m)
  # w_rad <- which(names_ %in% names_rad)
  
  params <- ParamsFun(layer = layer, batchsize = batchsize, units = units, cluster = cluster, method_norm = method_norm,
                      variable = variable)
  
  res_soil_t <- PreAnalysisPredictors(df_train = df_train[,c(w_soil_t, ncol(df_train))], params = params)
  res_soil_m <- PreAnalysisPredictors(df_train = df_train[,c(w_soil_m, ncol(df_train))], params = params)
  # res_rad <- PreAnalysisPredictors(df_train = df_train[,c(w_rad, ncol(df_train))], params = params)
  
  print(res_soil_t)
  print(res_soil_m)
  # print(res_rad)
  
  pred_t <-  as.character(res_soil_t$predictors[which(res_soil_t$mse == min(res_soil_t$mse))])
  pred_m <-  as.character(res_soil_m$predictors[which(res_soil_m$mse == min(res_soil_m$mse))])
  # pred_rad <-  as.character(res_rad$predictors[which(res_rad$mse == min(res_rad$mse))])
  
  if (variable == "NEE_cor"){
    pred_model <- c(pred_t, pred_m, c("airT", "RH", "LWin", "LWout", "h_last_precip", "precip_30d", 
                                      "year_ws_sin", "year_sa_sin", "day_sin"), variable)
  } else if (variable == "GPP"){
    pred_model <- c(pred_t, pred_m, c("airT", "RH", "LWin", "LWout", "SWout", "PPFDin", "h_last_precip", "precip_30d", 
                                      "year_ws_sin", "year_sa_sin", "day_sin"), variable)
  }

  df_train_ <- df_train[, pred_model]
  return(list(df_train_, res_soil_t, res_soil_m))
}

#### ----------------------- ##
#### 3 - Target Functions ##
#### ----------------------- ##

## These functions do the whole Modelselection.
  # They are using the functions of 4. and 5. 

## Target function GRID ##
TargetFunGrid <- function(df_train, batchsize = c(30,60,90), k = 5, epochs = 200, lr = 1e-4, layer = 2, 
                          optimizer = "rmsprop", path){
  df_results_ms <- NULL
  all_mae_history <- NULL
  params <- ParamsFun(k = k, epochs = epochs, lr = lr, layer = layer, optimizer = optimizer)
  
  ## Best Model structure (Layers & Nodes)
  ## modelrun for different batchsizes
  for (i in 1:length(batchsize)){
    params[["batchsize"]] <- batchsize[i]
    cat("Models with Batchsize: ", params[["batchsize"]], "!!", sep = "", "\n")
    
    results_ms <- RunModel.GridOpt(df_train = df_train, params = params)
    results_ <- results_ms[[1]]
    mae_history <- results_ms[[2]]
    df_results_ms <- rbind(df_results_ms, results_)
    all_mae_history <- rbind(all_mae_history, mae_history)
  }
  save(df_results_ms, all_mae_history, file = c(paste0(path, "/RData/results_model_", layer, "l_", 
                                                       format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData")))
  
  ## Best Model (Layers & Nodes)
  params <- BestModelSelection(df_results = df_results_ms, params = params, type = "nodes")
  
  ## Predictoranalysis - Best Predictor Subset
  df_results_pa <- RunModel.PredictorAnalysis(df_train = df_train, params = params)
  params <- BestModelSelection(df_results = df_results_pa, params = params, type = "pred")
  
  save(df_results_pa, params, file = paste0(path, "/RData/results_pred_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData"))
  
  ## best model structur for best predictor Subset ####
  df_train_best <- df_train[, c(params$best_preds_full$predictors, colnames(df_train)[ncol(df_train)])]
  df_results_pa_ms <- NULL
  
  ## best Model structure 
  ## modelrun for different batchsizes
  for (i in 1:length(batchsize)){
    params[["batchsize"]] <- batchsize[i]
    cat("PA-Models with Batchsize: ", batchsize[i], "!!", sep = "", "\n")
    
    results_2 <- RunModel.GridOpt(df_train = df_train_best, params = params)
    df_results_pa_ms <- rbind(df_results_pa_ms, results_2)
  }
  save(df_results_pa_ms, file = paste0(path, "/RData/results_p_m_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData"))
  
  return(list(df_results_ms, df_results_pa, df_results_pa_ms, params))
}

## Target function Bayesian Opitimization ##
TargetFunBO <- function(df_train, batchsize = c(20, 40, 80), k = 5, epochs = 200, lr = 1e-3, layer = 3, 
                        optimizer = "adam", path, opt.batch = T, ANN = "seq", cluster = T, method_norm = "range_1_1",
                        variable = "NEE_cor"){
  results_ms <- list()
  df_results_ms <- NULL
  params <- ParamsFun(k = k, epochs = epochs, lr = lr, layer = layer, optimizer = optimizer, cluster = cluster, method_norm = method_norm,
                      variable = variable)
  
  ## Best Model structure (Layers & Nodes)
  ## modelrun for different batchsizes
  if (opt.batch == F){
    for (i in 1:length(batchsize)){
      params[["batchsize"]] <- batchsize[i]
      cat("Models with Batchsize: ", params[["batchsize"]], "!!", sep = "", "\n")
      
      results_ms[[i]] <- RunModel.BayesianOpt(df_train = df_train, params = params, ANN = ANN)  
      
      df_results_ms <- rbind(df_results_ms, c(results_ms[[i]]$x$layer, results_ms[[i]]$x$units, batchsize[i], results_ms[[i]]$y))
    }
  } else if (opt.batch == T){
    results_ms <- RunModel.BayesianOpt.B(df_train = df_train, params = params, ANN = ANN)
    df_results_ms <- data.frame(results_ms$x$layer, results_ms$x$units, results_ms$x$batch, results_ms$y)
  }
  
  # save(df_results_ms, results_ms, file = c(paste0(path, "/RData/results_model_", layer, "l_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData")))
  
  ## Best Model (Layers & Nodes)
  params[["best"]]$layer <- df_results_ms[which(df_results_ms[, 4] == min(df_results_ms[, 4])), 1]
  params[["best"]]$units <- df_results_ms[which(df_results_ms[, 4] == min(df_results_ms[, 4])), 2]
  params[["best"]]$batch_size <- df_results_ms[which(df_results_ms[, 4] == min(df_results_ms[, 4])), 3]
  cat("Best Model: Layer: ", params[["best"]]$layer, "! Units: ", params[["best"]]$units, "! Batchsize: ", params[["best"]]$batch_size, "!", "\n")
  
  ## Predictoranalysis - Best Predictor Subset
  df_results_pa <- RunModel.PredictorAnalysis(df_train = df_train, params = params, ANN = ANN)
  params <- BestModelSelection(df_results = df_results_pa, params = params, type = "pred")
  
  # save(df_results_pa, params, file = paste0(path, "/RData/results_pred_", format(Sys.time(), "%Y-%m-%d_%H-%M"), ".RData"))
  return(list(df_results_ms, df_results_pa, params))
}

#### ----------------------- ##
#### 4 - Parameter and Model building Functions ##
#### ----------------------- ##

## Function Parameter ##
ParamsFun <- function(batchsize = 100, k = 5, epochs = 200, optimizer = "adam", lr = 1e-3, layer = 3L, units = 30,
                       Nmin = 50L, Nmax = 120L, by_ = 12, layer_balance = 0.5, times_cv = 6, iters_bo = 25,
                       dropout = F, cluster = T, method_norm = "range_1_1", variable = "NEE_cor"){
  params <- list()
  params[["batchsize"]] <- batchsize
  params[["k"]] <- k
  params[["epochs"]] <- epochs
  params[["optimizer"]] <- optimizer
  params[["lr"]] <- lr
  params[["units"]] <- units
  params[["layer"]] <- layer
  params[["Nmin"]] <- Nmin
  params[["Nmax"]] <- Nmax
  params[["by_"]] <- by_
  params[["layer_balance"]] <- layer_balance
  params[["times_cv"]] <- times_cv
  params[["iters_bo"]] <- iters_bo
  params[["dropout"]] <- dropout
  params[["cluster"]] <- cluster
  params[["method_norm"]] <- method_norm
  params[["variable"]] <- variable
  
  # if (variable == "NEE_cor"){
  #   params[["layer"]] <- layer
  #   params[["Nmin"]] <- Nmin
  #   params[["Nmax"]] <- Nmax
  # } else if (variable == "GPP"){
  #   params[["layer"]] <- 4L
  #   params[["Nmin"]] <- Nmin
  #   params[["Nmax"]] <- 150L
  # }
  return(params)
}

## Function model build ##
BuildModel <- function(df_train, layer, optimizer, units, lr, dropout = F){
  ## optimizer ##
  if (optimizer == "rmsprop"){
    optim_ <- optimizer_rmsprop(lr = lr)  
  } else if (optimizer == "adam"){
    optim_ <- optimizer_adam(lr = lr)
  }
  
  ## dropout model
  if (is.numeric(dropout)){
    if (layer == 1){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train) - 1) %>%
        layer_dropout(rate = dropout) %>% 
        layer_dense(units = 1) 
    } else if (layer == 2){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train) - 1) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = units[2], activation = "relu", initializer_he_uniform(seed = 5)) %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = 1) # No activation. Chollet et al. p. 78
    } else if (layer == 3){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train) - 1) %>%
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
    ## normal Model 
    if (layer == 1){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train) - 1) %>% 
        layer_dense(units = 1) 
    } else if (layer == 2){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train) - 1) %>% 
        layer_dense(units = units[2], activation = "relu", initializer_he_uniform(seed = 5)) %>% 
        layer_dense(units = 1) # No activation. Chollet et al. p. 78
    } else if (layer == 3){
      model <- keras_model_sequential() %>% 
        layer_dense(units = units[1], activation = "relu", initializer_he_uniform(seed = 5),
                    input_shape = ncol(df_train) - 1) %>% 
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

## Function model build LSTM ##
BuildModelLSTM <- function(df_train, layer, optimizer, units, lr){
  ## optimizer ##
  if (optimizer == "rmsprop"){
    optim_ <- optimizer_rmsprop(lr = lr)  
  } else if (optimizer == "adam"){
    optim_ <- optimizer_adam(lr = lr)
  }
  
  ## Model ####
  if (layer == 1){
    model <- keras_model_sequential() %>% 
      layer_gru(units = units[1], input_shape = c(1, ncol(df_train)-1)) %>% 
      layer_dense(units = 1) 
  } else if (layer == 2){
    model <- keras_model_sequential() %>% 
      layer_gru(units = units[1], input_shape = c(1, ncol(df_train)-1),
                return_sequences = T) %>% 
      layer_gru(units = units[2]) %>% 
      layer_dense(units = 1)
  } else if (layer == 3){
    model <- keras_model_sequential() %>% 
      layer_gru(units = units[1], input_shape = c(1, ncol(df_train)-1),
                return_sequences = T) %>% 
      layer_gru(units = units[2], return_sequences = T) %>% 
      layer_gru(units = units[3]) %>% 
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

## Function best model NEW ##
BestModelSelection <- function(df_results, params, type){
  # print ordered results
  print("Best Models")
  print(df_results[order(df_results$mse),][1:10, ])
  #print("Best Performance")
  #print(df_results[order(df_results$performance),][1:10,])
  
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
    
    params[["best"]] <- list("layer" = layer, "nodes" = nodes, "batch_size" = batch_size, "model" = df_results[w_perf, ]) 
  } else if(type == "pred"){
    w_best <- which(df_results$mse == min(df_results$mse, na.rm = T))
    
    str_full <- strsplit(as.character(df_results$predictors[w_best]), "+", fixed = TRUE)[[1]][-1]
    params[["best_preds_full"]] <- list("predictors" = str_full, "model" = df_results[w_best, ]) 
    
    w_best_5 <- which(df_results$mse[df_results$level <= 5] == min(df_results$mse[df_results$level <= 5], na.rm = T))
    #w_best_12 <- which(df_results$mse[df_results$level <= 12] == min(df_results$mse[df_results$level <= 12], na.rm = T))
    str_5 <- strsplit(as.character(df_results$predictors[w_best_5]), "+", fixed = TRUE)[[1]][-1]
    #str_12 <- strsplit(as.character(df_results$predictors[w_best_12]), "+", fixed = TRUE)[[1]][-1]
    params[["best_preds_5"]] <- list("predictors" = str_5, "model" = df_results[w_best_5, ]) 
    #params[["best_preds_12"]] <- list("predictors" = str_12, "model" = df_results[w_best_12,])
  }
  return(params)
}

#### ----------------------- ##
#### 5 - Model Evaluation Functions ##
#### ----------------------- ##

## Function model run model structure ##
RunModel.GridOpt <- function(df_train, params){
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
    key <- c(paste0("N_", N_[, 1], "_b_", params[["batchsize"]]))
  } else if (layer == 2){
    key <- c(paste0("N_", N_[, 1], "_b_", params[["batchsize"]]), paste0("N_", N_[, 1], "_", N_[, 2], "_b_", params[["batchsize"]]))
  } else if (layer == 3){
    key <- c(paste0("N_", N_[, 1], "_b_", params[["batchsize"]]), paste0("N_", N_[, 1], "_", N_[, 2], "_b_", params[["batchsize"]]), 
             paste0("N_", N_[, 1], "_", N_[, 2], "_", N_[, 3], "_b_", params[["batchsize"]])) 
  }
  
  ## Model computing
  for (l in 1:layer){
    for (i in 1:nrow(N_)){
      start_time <- Sys.time()
      ## create Model
      model <- BuildModel(df_train = df_train, layer = l, optimizer = params[["optimizer"]],
                          units = N_[i, 1:l], lr = params[["lr"]])
      
      cv_ <- ComputeModel(df_train = df_train, params = params, model = model)
      end_time <- Sys.time()
      
      mse_ <- mean(cv_[[1]], na.rm = T)
      r2_ <- mean(cv_[[2]], na.rm = T)
      std_ <- sd(cv_[[1]], na.rm = T)
      sem_ <- sd(cv_[[1]], na.rm = T) / sqrt(length(cv_[[1]]))
      time_ <- end_time - start_time
      mae_history <- apply(cv_[[3]], 2, mean, na.rm = T)
      
      all_mse <- rbind(all_mse, mse_)
      all_r2 <- rbind(all_r2, r2_)
      all_std <- rbind(all_std, std_)
      all_sem <- rbind(all_sem, sem_)
      performance <- rbind(performance, time_)
      all_mae_history <- rbind(all_mae_history, mae_history)
      
      cat("Complete model: N_", N_[i, 1:l], "! Time: ", time_, "\n", sep = "")
    }
  }
  df_results <- data.frame(key = key, mse = all_mse, r2 = all_r2, std = all_std, sem = all_sem, performance = performance)
  rownames(all_mae_history) <- key
  
  return(list(df_results, all_mae_history))
}

## Function model run Bayesian Opt. ##
RunModel.BayesianOpt <- function(df_train, params, ANN = "seq"){
  
  nn_fit_bayes <- function(x) {
    units <- x$units
    layer <- x$layer
    
    units <- as.integer(rep(units, layer))
    if (layer > 1){
      units[2] <- as.integer(units[2] * 0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2] * 0.5)
      }
    }
    
    params[["units"]] <- units
    params[["layer"]] <- layer
    
    cat("Model: # Layer:", layer, " # units:", units, "\n")
    #model <- BuildModel(df_train = df_train, layer = layer, optimizer = params[["optimizer"]], units = units, lr = params[["lr"]])
    if (ANN == "seq"){
      results_ <- ComputeModel(df_train = df_train, params = params, type = "full")  
    } else if (ANN == "LSTM"){
      results_ <- ComputeModelLSTM(df_train = df_train, params = params, type = "full")
    }
    
    return(mean(results_[[1]], na.rm = T))
  }
  
  ## set hyperparameterspace
  par.set <- makeParamSet(
    makeIntegerParam("layer", 1, params[["layer"]]),
    makeIntegerParam("units", params[["Nmin"]], params[["Nmax"]]))
  
  ## create learner -> dicekriging (GaussianProcess)
  surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
  
  ## objective Function
  obj.fun <- makeSingleObjectiveFunction(name = "svm.tuning",
                                        fn = nn_fit_bayes,
                                        par.set = par.set,
                                        has.simple.signature = FALSE,
                                        minimize = TRUE)
  
  ## design -> pre hyperparameter calulations
  set.seed(352)
  des <- generateDesign(n = 10, par.set = getParamSet(obj.fun))
  
  ## control
  ctrl <- makeMBOControl()
  ctrl <- setMBOControlTermination(ctrl, iters = params[["iters_bo"]])
  ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  #ctrl = setMBOControlInfill(ctrl, filter.proposed.points = TRUE)
  
  res_ <- mbo(obj.fun, design = des, learner = surr.km, control = ctrl, show.info = T)
  
  return(res_)
}

## Function model run Bayesian Opt. (incl. Batchsize) ##
RunModel.BayesianOpt.B <- function(df_train, params, ANN = "seq"){
  
  nn_fit_bayes <- function(x) {
    units <- x$units
    layer <- x$layer
    batch <- x$batch
    
    units <- as.integer(rep(units, layer))
    if (layer > 1){
      units[2] <- as.integer(units[2] * 0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2] * 0.5)
      }
    }
    
    params[["units"]] <- units
    params[["layer"]] <- layer
    params[["batchsize"]] <- batch
    
    cat("Model: # Layer:", layer, " # units:", units, " # batchsize:", batch, "\n")
    #model <- BuildModel(df_train = df_train, layer = layer, optimizer = params[["optimizer"]], units = units, lr = params[["lr"]])
    if (ANN == "seq"){
      results_ <- ComputeModel(df_train = df_train, params = params, type = "full")  
    } else if (ANN == "LSTM"){
      results_ <- ComputeModelLSTM(df_train = df_train, params = params, type = "full")
    }
    
    return(mean(results_[[1]], na.rm = T))
  }
  
  ## set hyperparameterspace
  par.set <- makeParamSet(
    makeIntegerParam("layer", 1, params[["layer"]]),
    makeIntegerParam("units", params[["Nmin"]], params[["Nmax"]]),
    makeIntegerParam("batch", 5, 80))

  ## create learner -> dicekriging (GaussianProcess)
  surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
  
  ## objective Function
  obj.fun <- makeSingleObjectiveFunction(name = "svm.tuning",
                                         fn = nn_fit_bayes,
                                         par.set = par.set,
                                         has.simple.signature = FALSE,
                                         minimize = TRUE)
  
  ## design -> pre hyperparameter calulations
  set.seed(352)
  des <- generateDesign(n = 20, par.set = getParamSet(obj.fun))
  
  ## control
  ctrl <- makeMBOControl()
  ctrl <- setMBOControlTermination(ctrl, iters = params[["iters_bo"]])
  ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
  #ctrl = setMBOControlInfill(ctrl, filter.proposed.points = TRUE)
  
  res_ <- mbo(obj.fun, design = des, learner = surr.km, control = ctrl, show.info = T)
  
  return(res_)
}

## Function model run Bayesian Opt. Noisy ##
RunModel.BayesianOptNoisy <- function(df_train, params){
  
  nn_fit_bayes <- function(x) {
    units <- x$units
    layer <- x$layer
    
    units <- as.integer(rep(units, layer))
    if (layer > 1){
      units[2] <- as.integer(units[2] * 0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2] * 0.5)
      }
    }
    
    params[["units"]] <- units
    params[["layer"]] <- layer
    
    cat("Model: # Layer:", layer, " # units:", units, "\n")
    #model <- BuildModel(df_train = df_train, layer = layer, optimizer = params[["optimizer"]], units = units, lr = params[["lr"]])
    results_ <- ComputeModel(df_train = df_train, params = params, type = "full")
    
    return(mean(results_[[1]], na.rm = T))
  }
  
  ## set hyperparameterspace
  par.set <- makeParamSet(
    makeIntegerParam("layer", 1, params[["layer"]]),
    makeIntegerParam("units", params[["Nmin"]], params[["Nmax"]]))
  
  ## create learner -> dicekriging (GaussianProcess)
  surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
  
  ## objective Function
  obj.fun <- makeSingleObjectiveFunction(name = "svm.tuning",
                                        fn = nn_fit_bayes,
                                        par.set = par.set,
                                        has.simple.signature = FALSE,
                                        minimize = TRUE,
                                        noisy = TRUE)
  
  ## design -> pre hyperparameter calulations
  set.seed(352)
  des <- generateDesign(n = 10, par.set = getParamSet(obj.fun))
  
  ## control
  ctrl <- makeMBOControl()
  ctrl <- setMBOControlTermination(ctrl, iters = params[["iters_bo"]])
  ctrl <- setMBOControlInfill(ctrl, crit = crit.aei)
  #ctrl = setMBOControlInfill(ctrl, filter.proposed.points = TRUE)
  
  res_ <- mbo(obj.fun, design = des, learner = surr.km, control = ctrl, show.info = T)
  
  return(res_)
}

## Function model compute full ##
ComputeModel <- function(df_train, params, type = "full"){
  ## params
  k <- params[["k"]]
  num_epochs <- params[["epochs"]]
  optimizer <- params[["optimizer"]]
  lr <- params[["lr"]]
  layer <- params[["layer"]]
  batch <- params[["batchsize"]]
  dropout <- params[["dropout"]]
  cluster <- params[["cluster"]]
  method_norm <- params[["method_norm"]]
  
  if (type == "pred"){
    times_cv <- 4
    units <- rep(params[["units"]], params[["layer"]])
    if (layer > 1){
      units[2] <- as.integer(units[2] * 0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2] * 0.5)
      }
    }
  } else if (type == "prepred") {
    times_cv <- 10
    units <- rep(params[["units"]], params[["layer"]])
    if (layer > 1){
      units[2] <- as.integer(units[2] * 0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2] * 0.5)
      }
    }
  } else {
    times_cv <- params[["times_cv"]]
    units <- params[["units"]]
  }
  
  ## NA to 0
  # df_train[is.na(df_train)] <- 0
  
  ## empty vectors for Results
  all_mae_histories <- NULL
  all_mse <- NULL
  all_r2 <- NULL
  
  ## Callback - early stopping 
  callback_list <- list(callback_early_stopping(patience = 6))
  
  ## Crossvalidation j times k-fold crossvalidation
  for (j in 1:times_cv){
    set.seed(j * 5)
    
    if(cluster == T){
      ## cluster 
      clusters <- kmeans(df_train[, ncol(df_train)], 4)
      
      c_1 <- sample(which(clusters$cluster == 1), as.integer(0.9 * min(clusters$size, na.rm = T)))
      c_2 <- sample(which(clusters$cluster == 2), as.integer(0.9 * min(clusters$size, na.rm = T)))
      c_3 <- sample(which(clusters$cluster == 3), as.integer(0.9 * min(clusters$size, na.rm = T)))
      c_4 <- sample(which(clusters$cluster == 4), as.integer(0.9 * min(clusters$size, na.rm = T)))
      
      df_train <- df_train[c(c_1, c_2, c_3, c_4), ]
    }
    
    indices <- sample(1:nrow(df_train))
    folds <- cut(indices, breaks = k, labels = FALSE)
    
    for (i in 1:k) {
      cat("processing fold #", paste0(j, ".", i), "\n")
      
      ## Model
      model <- BuildModel(df_train = df_train, layer = layer, optimizer = optimizer, units = units, lr = lr, dropout = dropout)
      
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
        mean_data <- mean(partial_train_data, na.rm = T)
        sd_data <- sd(partial_train_data, na.rm = T)
      } else {
        val_test_data <- as.matrix(df_train[val_test_indices, -ncol(df_train)])
        partial_train_data <- as.matrix(df_train[-val_test_indices, -ncol(df_train)])
        
        # min and max for mormalization
        mins_data <- apply(partial_train_data, 2, min, na.rm = T)
        maxs_data <- apply(partial_train_data, 2, max, na.rm = T)
        mean_data <- apply(partial_train_data, 2, mean, na.rm = T)
        sd_data <- apply(partial_train_data, 2, sd, na.rm = T)
      }
      
      ## same for target data
      val_test_targets <- as.array(df_train[val_test_indices, ncol(df_train)])
      partial_train_targets <- as.array(df_train[-val_test_indices, ncol(df_train)])
      
      # normalize all data
      mins_targets <- min(partial_train_targets, na.rm = T)
      maxs_targets <- max(partial_train_targets, na.rm = T)
      mean_targets <- mean(partial_train_targets, na.rm = T)
      sd_targets <- sd(partial_train_targets, na.rm = T)
      
      # split combined val_test-data into separate validation and test data
      set.seed(i * 5)
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
      
      ## normalisation
      if(method_norm == "range_0_1"){
        ## normalisation range 0 - 1
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
      } else if(method_norm == "range_1_1"){
        ## normalisation range -1 to 1
        # predict = x
        partial_train_data <-   as.matrix(2 *  t(t(t(t(partial_train_data) - mins_data)) / (maxs_data - mins_data)) - 1)
        val_data <-             as.matrix(2 *  t(t(t(t(val_data)           - mins_data)) / (maxs_data - mins_data)) - 1)
        test_data <-            as.matrix(2 *  t(t(t(t(test_data)          - mins_data)) / (maxs_data - mins_data)) - 1)
        
        # target = y
        partial_train_targets <-  as.matrix(2 * t(t(t(t(partial_train_targets) - mins_targets)) / (maxs_targets - mins_targets)) - 1)
        val_targets <-            as.matrix(2 * t(t(t(t(val_targets)           - mins_targets)) / (maxs_targets - mins_targets)) - 1)
        test_targets <-           as.matrix(2 * t(t(t(t(test_targets)          - mins_targets)) / (maxs_targets - mins_targets)) - 1)
      } else if(method_norm == "standarize"){
        ## normalisation mean = 0, sd =  1
        # predict = x
        partial_train_data <- scale(partial_train_data, center = mean_data,
                                    scale = sd_data)
        val_data <- scale(val_data, center = mean_data,
                          scale = sd_data)
        test_data <- scale(test_data, center = mean_data,
                           scale = sd_data)

        # target = y
        partial_train_targets <- scale(partial_train_targets, center = mean_targets,
                                       scale = sd_targets)
        val_targets <- scale(val_targets, center = mean_targets,
                             scale = sd_targets)
        test_targets <- scale(test_targets, center = mean_targets,
                              scale = sd_targets)
      }

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

## Function model compute full for LSTM ##
ComputeModelLSTM <- function(df_train, params, type = "full"){
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
      units[2] <- as.integer(units[2] * 0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2] * 0.5)
      }
    }
  } else if (type == "prepred") {
    times_cv <- 4
    units <- rep(params[["units"]], params[["layer"]])
    if (layer > 1){
      units[2] <- as.integer(units[2] * 0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2] * 0.5)
      }
    }
  } else {
    times_cv <- params[["times_cv"]]
    units <- params[["units"]]
  }
  
  ## NA to 0
  df_train[is.na(df_train)] <- 0
  
  ## Callback - early stopping ####
  callback_list <- list(callback_early_stopping(patience = 6))
  
  ## empty lists 
  all_mae_histories <- NULL
  all_mse <- NULL
  all_r2 <- NULL
  
  ## CV loop ####
  set.seed(5)
  indices <- sample(1:nrow(df_train))
  folds <- cut(indices, breaks = k, labels = FALSE)
  
  for (j in 1:times_cv){
    set.seed(j * 5)
    indices <- sample(1:nrow(df_train))
    folds <- cut(indices, breaks = k, labels = FALSE)
    
    for (i in 1:k) {
      cat("processing fold #", paste0(j, ".", i), "\n")
      
      ## Model
      model <- BuildModelLSTM(df_train = df_train, layer = layer, optimizer = optimizer, units = units, lr = lr)
      
      # Prepare the validation data: data from partition # k | Also needed for model build (input shape)
      val_indices <- which(folds == i, arr.ind = TRUE)
      set.seed(i*5)
      vt_indices <- sample(1:length(val_indices))
      vt_folds <- cut(vt_indices, breaks = 2, labels = FALSE)
      val_test_indices_2 <- which(vt_folds == 1, arr.ind = TRUE)
      
      ## normalize all data
      df_part_train_scale <- df_train[-val_indices,]
      
      mins_data <- apply(df_part_train_scale, 2, min, na.rm = T)
      maxs_data <- apply(df_part_train_scale, 2, max, na.rm = T)
      
      df_train_ <- scale(df_train, center = mins_data, 
                         scale = maxs_data - mins_data)
      
      # Prepare the training and validation data: data from all other partitions
      partial_train_data <- array(df_train_[-val_indices, 1:ncol(df_train_)-1], 
                                  dim = c(nrow(df_train_[-val_indices,]), 1, ncol(df_train_)-1))
      
      partial_train_targets <- array(df_train_[-val_indices, ncol(df_train_)], 
                                     dim = nrow(df_train_[-val_indices,]))
      
      val_data <- array(df_train_[val_indices[val_test_indices_2], 1:ncol(df_train_)-1], 
                        dim = c(nrow(df_train_[val_indices[val_test_indices_2],]), 1, ncol(df_train_)-1))
      
      val_targets <- array(df_train_[val_indices[val_test_indices_2], ncol(df_train_)], 
                           dim = nrow(df_train_[val_indices[val_test_indices_2],]))
      
      test_data <- array(df_train_[val_indices[-val_test_indices_2], 1:ncol(df_train_)-1], 
                         dim = c(nrow(df_train_[val_indices[-val_test_indices_2],]), 1, ncol(df_train_)-1))
      
      test_targets <- as.numeric(df_train_[val_indices[-val_test_indices_2], ncol(df_train_)])
      
      
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
  
  return(list(all_mae_histories, all_mse, all_r2))
}

## Function model run predictor analysis ##
RunModel.PredictorAnalysis <- function(df_train, params, ANN = "seq"){
  ## params 
  params[["layer"]] <- params[["best"]]$layer
  params[["batchsize"]] <- params[["best"]]$batch_size
  params[["units"]] <- params[["best"]]$units
  
  ## key
  key <- NULL
  level <- 1
  for (i in c(ncol(df_train) - 1):1){
    key_ <- as.character(level + c(1:i) / 100)
    key <- c(key, key_)
    level <- level + 1
  }
  
  ## required empty features / target 
  all_mse <- vector("list", ncol(df_train) - 1)
  all_r2 <- vector("list", ncol(df_train) - 1)
  k <- ncol(df_train) - 1
  col_ <- colnames(df_train)
  level_ <- NULL
  pred_ <- NULL
  best_pred_name <- vector("list", ncol(df_train) - 1)
  count <- 1
  
  ## loop for every predictor composition
  for (i in c(1:(ncol(df_train) - 1))){
    for (j in c(1:k)){
      # print computed predictor combination      
      pred_[count] <- paste0(best_pred_name[[i]], "+", col_[j])
      cat("Predictor Posibility: ", count, "/", ncol(df_train) * (ncol(df_train) - 1) / 2, ". Used predictors:", pred_[count], sep = "", "\n")
      
      # choose of one predictor and always Y. Y needs to be at last column position.
      train_ <- df_train[, c(col_[j], col_[length(col_)])]
      if (i == 1){
        all_train <- train_
      } else {
        all_train <- cbind(best_train, train_)
      }
      
      ## create Model 
      #model <- BuildModel(df_train = all_train, layer = params[["best"]]$layer, optimizer = params[["optimizer"]], 
                               #units = N, lr = params[["lr"]])
      
      # compute Model for this predictor composition
      if (ANN == "seq"){
        cv_ <- ComputeModel(df_train = all_train, params = params, type = "pred")  
      } else if (ANN == "LSTM"){
        cv_ <- ComputeModelLSTM(df_train = all_train, params = params, type = "pred")
      }
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
    k <- k - 1
    print(paste0("Level: ", i, "! Predictors: ", best_pred_name[[i]]))
    col_ <- col_[-w_best]
  }
  
  df_results <- data.frame(key = key, level = level_, predictors = pred_, 
                           mse = unlist(all_mse, use.names=FALSE), r2 = unlist(all_r2, use.names=FALSE))
  
  return(df_results)
}

## Function for Bootstrapping the best model -> error evaluation ##
BootstrapPrediction <- function(pre_predictor_results, model_selection_results, complete_data, rep = 100, variable = "NEE_cor"){
  
  # callback
  callback_list <- list(callback_early_stopping(patience = 6))
  
  # units
  layer <- model_selection_results[[3]]$best$layer
  units <- model_selection_results[[3]]$best$units
  batch <- model_selection_results[[3]]$best$batch_size
  
  units <- as.integer(rep(units, layer))
  if (layer > 1){
    units[2] <- as.integer(units[2] * 0.5)
    if (layer == 3){
      units[3] <- as.integer(units[2] * 0.5)
    }
  }
  
  # extract columns
  if (variable == "NEE_cor"){
    # prediction data (all data which is not in train data)
    # Extract Night and Day Data ####
    df_night <- complete_data[which(complete_data$flag_night == 1), ]
    # night data without PPFDin > 5, not used in model
    df_night <- df_night[-which(df_night$PPFDin > 5),]
    # u* correction 
    # Jassal et al. 2009: 0.19 | Krishnan et al. 2009: 0.16 | Jassal et al. 2010: 0.19 
    df_night$NEE_cor[df_night$ustar < 0.19] <- NA
    
    # data frame for model
    df_night_model <-  df_night[!is.na(df_night$NEE_cor), ]
    
    df_final_pred <- complete_data[-which(complete_data$dt %in% df_night_model$dt),]
    df_final_pred_2 <- df_final_pred[, c(model_selection_results[[3]]$best_preds_full$predictors, variable)]
    
    # train data
    df_final_train <- pre_predictor_results[[1]][, c(model_selection_results[[3]]$best_preds_full$predictors, variable)]
  } else if(variable == "GPP"){
    # Extract Day Data and PPFDin > 5 ####
    df_day <- complete_data[which(complete_data$flag_night == 0 & complete_data$PPFDin > 5), ]
    df_final_pred <- df_day[which(is.na(df_day$GPP)), ]
    df_final_pred_2 <- df_final_pred[, c(model_selection_results[[3]]$best_preds_full$predictors, variable)]
    
    df_final_train <- pre_predictor_results[[1]][, c(model_selection_results[[3]]$best_preds_full$predictors, variable)]
  }
  
  # cluster
  cluster <- model_selection_results[[3]]$cluster
  
  # variable 
  variable <- model_selection_results[[3]]$variable
  
  # normalization method
  method_norm <- model_selection_results[[3]]$method_norm
  
  # prediction matrix 
  pred_mat <- array(dim=c(nrow(df_final_pred_2), 1, rep))
  
  ## bootstrap loop
  for(i in 1:rep){
    set.seed(i * 5)
    
    if(cluster == T){
      clusters <- kmeans(df_final_train[, ncol(df_final_train)], 4)
      
      c_1 <- sample(which(clusters$cluster == 1), as.integer(0.9 * min(clusters$size, na.rm = T)))
      c_2 <- sample(which(clusters$cluster == 2), as.integer(0.9 * min(clusters$size, na.rm = T)))
      c_3 <- sample(which(clusters$cluster == 3), as.integer(0.9 * min(clusters$size, na.rm = T)))
      c_4 <- sample(which(clusters$cluster == 4), as.integer(0.9 * min(clusters$size, na.rm = T)))
      
      df_final_train_2 <- df_final_train[c(c_1, c_2, c_3, c_4), ]
    } else {
      df_final_train_2 <- df_final_train
    }
   
    # min and max for mormalization
    mins_data <- apply(df_final_train_2, 2, min, na.rm = T)
    maxs_data <- apply(df_final_train_2, 2, max, na.rm = T)
    mean_data <- apply(df_final_train_2, 2, mean, na.rm = T)
    sd_data <- apply(df_final_train_2, 2, sd, na.rm = T)
    
    ## normalization
    if(method_norm == "range_0_1"){
      # normalization range 0 - 1
      df_final_train_n <- scale(df_final_train_2, center = mins_data,
                                scale = maxs_data - mins_data)
      df_final_pred_n <- scale(df_final_pred_2, center = mins_data,
                               scale = maxs_data - mins_data)
    } else if(method_norm == "range_1_1"){
      # normalization range -1 - 1
      df_final_train_n <- 2 * t(t(t(t(df_final_train_2) - mins_data)) / (maxs_data - mins_data)) - 1
      df_final_pred_n <- 2 * t(t(t(t(df_final_pred_2) - mins_data)) / (maxs_data - mins_data)) - 1
    } else if(method_norm == "standarize"){
      # normalization mean = 0, sd = 1
      df_final_train_n <- scale(df_final_train_2, center = mean_data,
                                scale = sd_data)
      df_final_pred_n <- scale(df_final_pred_2, center = mean_data,
                               scale = sd_data)
    }
    
    # change class
    train_data_model <- as.matrix(df_final_train_n[, -ncol(df_final_train_n)])
    train_targets_model <- as.array(df_final_train_n[, ncol(df_final_train_n)])
    pred_data_model <- as.matrix(df_final_pred_n[, -ncol(df_final_pred_n)])
    
    # index_gas <- which(is.na(data_pred[,flux]))
    # build model
    model <- BuildModel(df_train = df_final_train_n, layer = layer, units = units, optimizer = "adam", lr = 1e-3)
    
    # bootstrap data
    index_ <- sample(1:nrow(train_data_model), nrow(train_data_model), replace = T)
    
    # train model
    model %>% fit(
      train_data_model[index_, ], train_targets_model[index_], 
      epochs = 100, batch_size = batch, 
      validation_split = 0.2, verbose = 0,
      callbacks = callback_list)
    
    # predict and "re-normalization" 
    test_predictions <- model %>% predict(pred_data_model)
    
    # back scaliling
    if(method_norm == "range_0_1"){
      # normalization range 0 - 1
      final_result <- test_predictions[,1] * (maxs_data[length(maxs_data)] - mins_data[length(mins_data)]) + mins_data[length(mins_data)]
    } else if(method_norm == "range_1_1"){
      final_result <- (test_predictions[,1] + 1) / 2 * (maxs_data[length(maxs_data)] - mins_data[length(mins_data)]) + mins_data[length(mins_data)]
    } else if(method_norm == "standarize"){
      final_result <- test_predictions[,1] * sd_data[length(sd_data)] + mean_data[length(mean_data)]
    }
    pred_mat[,,i] <- final_result
    
    # clear session
    K <- backend()
    K$clear_session()
    
    print(paste("Bootstrap", i, "completed."))
  }
  
  pred_mean <- apply(pred_mat, 1, mean, na.rm = T)
  pred_quan <- apply(pred_mat, 1, quantile, c(0.025, 0.5, 0.975), na.rm = T)
  pred_se <- apply(pred_mat, 1, sd, na.rm = T)

  ## 95 % konfidenzintervall
  pred_qt <- data.frame(t(pred_quan))
  pred_qt$konf <- (pred_qt[,3] - pred_qt[,1]) / 2

  df_results <- cbind("dt" = df_final_pred$dt, "mean" = pred_mean, pred_qt, "se" = pred_se)
  
  if (variable == "NEE_cor"){
    ## final data frame
    complete_data$Re_gap_filled <- NA
    complete_data$Re_gap_filled_sd <- NA
    complete_data$Re_gap_filled_95.conf <- NA
    complete_data$Re_final <- NA
    
    if (summary(complete_data$dt[which(complete_data$dt %in% df_results$dt)] == df_results$dt)[[2]] == nrow(df_results)) {
      complete_data$Re_gap_filled[which(complete_data$dt %in% df_results$dt)] <- df_results$mean
      
      complete_data$Re_final[which(complete_data$dt %in% df_night_model$dt)] <- df_night_model$NEE_cor
      complete_data$Re_final[which(complete_data$dt %in% df_results$dt)] <- df_results$mean
      
      complete_data$Re_gap_filled_sd[which(complete_data$dt %in% df_results$dt)] <- df_results$se
      complete_data$Re_gap_filled_95.conf[which(complete_data$dt %in% df_results$dt)] <- df_results$konf
    } else {
      print("Order of rows do not match.")
      df_results <- df_results[order(df_results$dt), ]
      try(if (summary(complete_data$dt[which(complete_data$dt %in% df_results$dt)] == df_results$dt)[[2]] == nrow(df_results)) {
        print("Reordered and matched.")
        complete_data$Re_gap_filled[which(complete_data$dt %in% df_results$dt)] <- df_results$mean
        
        complete_data$Re_final[which(complete_data$dt %in% df_night_model$dt)] <- df_night_model$NEE_cor
        complete_data$Re_final[which(complete_data$dt %in% df_results$dt)] <- df_results$mean
        
        complete_data$Re_gap_filled_sd[which(complete_data$dt %in% df_results$dt)] <- df_results$se
        complete_data$Re_gap_filled_95.conf[which(complete_data$dt %in% df_results$dt)] <- df_results$konf
      })
    }
  } else if (variable == "GPP"){
    ## final data frame
    complete_data$GPP_gap_filled <- NA
    complete_data$GPP_gap_filled_sd <- NA
    complete_data$GPP_gap_filled_95.conf <- NA
    
    if (summary(complete_data$dt[which(complete_data$dt %in% df_results$dt)] == df_results$dt)[[2]] == nrow(df_results)) {
      complete_data$GPP_gap_filled[which(complete_data$dt %in% df_results$dt)] <- df_results$mean
      
      complete_data$GPP_final<- complete_data$GPP
      complete_data$GPP_final[which(complete_data$dt %in% df_results$dt)] <- df_results$mean
      
      complete_data$GPP_gap_filled_sd[which(complete_data$dt %in% df_results$dt)] <- df_results$se
      complete_data$GPP_gap_filled_95.conf[which(complete_data$dt %in% df_results$dt)] <- df_results$konf
    } else {
      print("Order of rows do not match.")
      df_results <- df_results[order(df_results$dt), ]
      try(if (summary(complete_data$dt[which(complete_data$dt %in% df_results$dt)] == df_results$dt)[[2]] == nrow(df_results)) {
        print("Reordered and matched.")
        complete_data$GPP_gap_filled[which(complete_data$dt %in% df_results$dt)] <- df_results$mean
        
        complete_data$GPP_final<- complete_data$GPP
        complete_data$GPP_final[which(complete_data$dt %in% df_results$dt)] <- df_results$mean
        
        complete_data$GPP_gap_filled_sd[which(complete_data$dt %in% df_results$dt)] <- df_results$se
        complete_data$GPP_gap_filled_95.conf[which(complete_data$dt %in% df_results$dt)] <- df_results$konf
      })
    }
  }

  return(list(df_results, complete_data))
}

#### ----------------------- ##
#### 6 - Additional Functions ##
#### ----------------------- ##

## Function model dropout analysis ##
RunModel.BayesianOpt.Dropout <- function(df_train, params){
  nn_fit_bayes <- function(x) {
    units <- x$units
    layer <- x$layer
    dropout <- x$dropout
    
    units <- as.integer(rep(units, layer))
    if (layer > 1){
      units[2] <- as.integer(units[2] * 0.5)
      if (layer == 3){
        units[3] <- as.integer(units[2] * 0.5)
      }
    }
    
    params[["units"]] <- units
    params[["layer"]] <- layer
    params[["dropout"]] <- dropout
    
    cat("Model: # Layer:", layer, " # units:", units, " # dropout:", dropout, "\n")
    #model <- BuildModel(df_train = df_train, layer = layer, optimizer = params[["optimizer"]], units = units, lr = params[["lr"]])
    results_ <- ComputeModel(df_train = df_train, params = params, type = "full")
    
    return(mean(results_[[1]], na.rm = T))
  }
  
  ## set hyperparameterspace
  par.set <- makeParamSet(
    makeIntegerParam("layer", 1, params[["layer"]]),
    makeIntegerParam("units", params[["Nmin"]], params[["Nmax"]]))
    makeNumericParam("dropout", 0.1, 0.4)
  
  ## create learner -> dicekriging (GaussianProcess)
  surr.km <- makeLearner("regr.km", predict.type = "se", covtype = "matern5_2", control = list(trace = FALSE))
  
  ## objective Function
  obj.fun <- makeSingleObjectiveFunction(name = "svm.tuning",
                                        fn = nn_fit_bayes,
                                        par.set = par.set,
                                        has.simple.signature = FALSE,
                                        minimize = TRUE,
                                        noisy = TRUE)
  
  ## design -> pre hyperparameter calulations
  set.seed(352)
  des <- generateDesign(n = 12, par.set = getParamSet(obj.fun))
  
  ## control
  ctrl <- makeMBOControl()
  ctrl <- setMBOControlTermination(ctrl, iters = params[["iters_bo"]])
  ctrl <- setMBOControlInfill(ctrl, crit = crit.aei)
  #ctrl = setMBOControlInfill(ctrl, filter.proposed.points = TRUE)
  
  res_ <- mbo(obj.fun, design = des, learner = surr.km, control = ctrl, show.info = T)
  
  return(res_)
}

#### ##
