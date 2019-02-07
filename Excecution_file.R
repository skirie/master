#### ------------------------------------------- ##
#### Excecution ##
#### ------------------------------------------- ##
  
#### ----------------------- ##
#### 1 - Load path, functions and packages ##
#### ----------------------- ##

  ## path ####
  mypath <- getwd()

  ## dp token ####
  token <- readRDS(paste0(mypath,"/token.rds"))
  
  ## functions ####
  source("Model_functions.R")
  
  ## Packages ####
  packages <- c("keras", "ggplot2", "Metrics", "httpuv", "rdrop2", "mlrMBO", "corrplot", "rgenoud", "betareg", "MASS")
  CheckPackages(packages) 
  use_condaenv("r-tensorflow")
  #### ####
  
#### ----------------------- ##
#### Check for preprocessed data  ##
#### ----------------------- ##
  
  CheckData()

#### ----------------------- ##
#### predictor pre analysis ##
#### ----------------------- ##
  
  df_night_model <-  df_night_model[, ]
  pred_analysis <- TargetPreAnalysisPredictors(df_train = df_night_model)
  df_train.1 <- pred_analysis[[1]]
  # save(pred_analysis, file = paste0(mypath, "/RData/results_pred_pre_analysis.RData"))
  # load(paste0(mypath, "/RData/results_pred_pre_analysis.RData"))
  
#### ----------------------- ##
#### Model Selection Respiration whole time span ##
#### ----------------------- ##
  
  results_resp_all_b <- TargetFunBO(df_train = df_train.1, path = mypath, opt.batch = T, ANN = "seq")
  
  # save(results_resp_all_b, file = paste0(mypath, "/RData/results_complete_", format(Sys.time(), "%d.%m"), ".RData"))
  # load(paste0(mypath, "/RData/results_complete_24.01.RData"))
  
#### ----------------------- ##
#### Bootstrap best model for error estimation ##
#### ----------------------- ##
  
  df_results_boot_r0.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis, 
                                              model_selection_results = results_resp_all_b, 
                                              prediction_data = df_pred_complete, 
                                              complete_data = df_merged, rep = 100)

  plot(df_merged$NEE[which(df_merged$dt %in% df_results_boot$dt)] ~ df_merged$dt[which(df_merged$dt %in% df_results_boot$dt)],
       type = "l", ylim = c(-30,30))  
  plot(df_merged$NEE_final2[which(df_merged$dt %in% df_results_boot$dt)] ~ df_merged$dt[which(df_merged$dt %in% df_results_boot$dt)],
       type = "l", ylim = c(-30,30))  
  lines(df_merged$NEE_final ~df_merged$dt, col = "red")
  
#### ----------------------- ##
#### Model Selection for an moving window of 4 years ##
#### ----------------------- ##
  
  df_train.1 <- cbind("dt" = df_night_model$dt, df_train.1)
  years_ <- unique(as.numeric(format(df_train.1$dt, "%Y")))
  results_pa <- vector("list", length(years_) - 3)
  results_ms <- vector("list", length(years_) - 3)
  
  for (i in 1:(length(years_) - 3)){
    window_ <- years_[i:(i + 3)]
    df_train.2 <- df_train.1[which(as.numeric(format(df_train.1$dt, "%Y")) %in% window_), ]
    
    #results_pa[[i]] <- TargetPreAnalysisPredictors(df_train = df_train.2)
    #df_train.3 <- results_pa[[i]][[1]]
    
    results_ms[[i]] <- TargetFunBO(df_train = df_train.2[,2:ncol(df_train.2)], path = mypath, opt.batch = T) 
  }
  
  save(results_ms, file = paste0(mypath, "/RData/results_movingwindow.RData"))
