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
    
