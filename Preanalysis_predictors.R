
params <- fun_params()

params[["best"]]$layer <- 2
params[["best"]]$batch_size <- 30
params[["best"]]$units <- 40

df_ts <- df_night_model[,c(2:8,34)]
df_ms <- df_night_model[,c(9:13,34)]

fun_model_run_preanalysis_pred <- function(df_train, params){
  ## params 
  params[["layer"]] <- params[["best"]]$layer
  params[["batchsize"]] <- params[["best"]]$batch_size
  params[["units"]] <- params[["best"]]$units
  
  ## required empty features / target 
  all_mse <- vector("list", ncol(df_train)-1)
  all_r2 <- vector("list", ncol(df_train)-1)
  k <- ncol(df_train)-1
  col_ <- colnames(df_train)
  
  ## loop for every predictor
  for (i in 1:k){
    # print computed predictor combination      
    cat("Predictor: ", i, "/", ncol(df_train)-1, ". Used predictors:", col_[i], sep = "", "\n")
      
    # choose of one predictor and always Y. Y needs to be at last column position.
    train_ <- df_train[, c(col_[i], col_[ncol(df_train)])]

    # compute Model for this predictor composition
    cv_ <- fun_model_compute_full(df_train = train_, params = params, type = "pred")
    mse_ <- mean(cv_[[1]], na.rm = T)
    r2_ <- mean(cv_[[2]], na.rm = T)
    all_mse[[i]] <- c(all_mse[[i]], mse_)
    all_r2[[i]] <- c(all_r2[[i]], r2_)
    }
    
  df_results <- data.frame(predictors = col_[1:k], mse = unlist(all_mse, use.names=FALSE), 
                           r2 = unlist(all_r2, use.names=FALSE))
  
  return(df_results)
  }

results_ts <- fun_model_run_preanalysis_pred(df_train = df_ts, params = params)
