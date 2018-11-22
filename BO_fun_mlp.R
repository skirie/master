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
    
    cat("Model: # Layer:", layer, " # units:", units, "\n")
    model <- fun_build_model(df_train = df_train, layer = layer, optimizer = params[["optimizer"]], units = units, lr = params[["lr"]])
    results_ <- fun_model_compute_full(df_train = df_train, params = params, type = "pred", model = model)
    
    return(mean(results_[[1]], na.rm = T))
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
    des = generateDesign(n = 12, par.set = getParamSet(obj.fun))
    
    ## control
    ctrl = makeMBOControl()
    ctrl = setMBOControlTermination(ctrl, iters = params[["iters_bo"]])
    ctrl = setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())
    
    res_ = mbo(obj.fun, design = des, learner = surr.km, control = ctrl, show.info = T)
    
    return(res_)
  }
  
  params <- fun_params()
  first_bo <- fun_bo_mlr(df_train = df_night_model, params = params)
    
  ## Target function BO ####
  fun_tagret_bo <- function(df_train, batchsize = c(40, 80), k = 5, epochs = 200, lr = 1e-3, layer = 4, optimizer = "adam", path){
    df_results_ms <- NULL
    params <- fun_params(k = k, epochs = epochs, lr = lr, layer = layer, optimizer = optimizer)
    
    ## Best Model structure (Layers & Nodes)
    ## modelrun for different batchsizes
    for (i in 1:length(batchsize)){
      params[["batchsize"]] <- batchsize[i]
      cat("Models with Batchsize: ", params[["batchsize"]], "!!", sep = "", "\n")
      
      results_ms <- fun_bo_mlr(df_train = df_train, params = params)
      df_results_ms <- c(df_results_ms, c(results_ms$x$x, batchsize[i], results_ms$y))
    }
    save(df_results_ms, file = c(paste0(path, "/RData/results_model_", layer-1, "l_", Sys.Date(), ".RData")))
    
    ## Best Model (Layers & Nodes)
    params[["best"]]$layer <- df_results_ms[which(df_results_ms[,4] == min(df_results_ms[,3])),1]
    params[["best"]]$nodes <- df_results_ms[which(df_results_ms[,4] == min(df_results_ms[,3])),2]
    params[["best"]]$batch_size <- df_results_ms[which(df_results_ms[,4] == min(df_results_ms[,3])),3]
    
    ## Predictoranalysis - Best Predictor Subset
    df_results_pa <- fun_model_run_pa(df_train = df_train, params = params)
    params <- fun_best_model(df_results = df_results_pa, params = params, type = "pred")
    
    save(df_results_pa, params, file = paste0(path, "/RData/results_pred_", Sys.Date(), ".RData"))
    return(list(df_results_ms, df_results_pa, params))
  }
  
#    ## best model structur for best predictor Subset ####
#    df_train_best <- df_train[,c(params$best_preds_full$predictors, colnames(df_train)[ncol(df_train)])]
#    df_results_pa_ms <- NULL
#    
#    ## best Model structure 
#    ## modelrun for different batchsizes
#    for (i in 1:length(batchsize)){
#      params[["batchsize"]] <- batchsize[i]
#      cat("PA-Models with Batchsize: ", batchsize[i], "!!", sep = "", "\n")
#      
#      results_2 <- fun_model_run_ms(df_train = df_train_best, params = params)
#      df_results_pa_ms <- rbind(df_results_pa_ms, results_2)
#    }
#    save(df_results_pa_ms, file = paste0(path, "/RData/results_p_m_", Sys.Date(), ".RData"))
#    
#    return(list(df_results_ms, df_results_pa, df_results_pa_ms, params))
#  }