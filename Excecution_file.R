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
  
  # CheckData()

#### ----------------------- ##
#### predictor pre analysis ##
#### ----------------------- ##
  
  pred_analysis_r1.1 <- TargetPreAnalysisPredictors(df_train = df_night_model)
  df_train.1 <- pred_analysis_r1.1[[1]]
  
  save(pred_analysis_r1.1, file = paste0(mypath, "/RData/results_pred_pre_analysis_r1.1.RData"))
  # load(paste0(mypath, "/RData/results_pred_pre_analysis_r0.1.RData"))
  
#### ----------------------- ##
#### Model Selection Respiration whole time span ##
#### ----------------------- ##
  
  results_resp_all_b_r1.1 <- TargetFunBO(df_train = df_train.1, path = mypath, opt.batch = T, ANN = "seq")

  save(results_resp_all_b_r1.1, file = paste0(mypath, "/RData/results_complete_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  # load(paste0(mypath, "/RData/results_complete_24.01.RData"))
  
#### ----------------------- ##
#### Bootstrap best model for error estimation ##
#### ----------------------- ##
  
  df_results_boot_r1.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_r1.1, 
                                              model_selection_results = results_resp_all_b_r1.1, 
                                              prediction_data = df_pred_complete, 
                                              complete_data = df_merged, rep = 100)
  
  
  save(df_results_boot_r1.1, file = paste0(mypath, "/RData/results_boots_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  
  summary(df_results_boot_r1.1[[2]])
  sum(df_results_boot_r1.1[[2]]$NEE, na.rm = T)
  sum(df_results_boot_r1.1[[2]]$NEE_final, na.rm = T)
  
  plot(df_results_boot_r1.1[[2]]$NEE[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)] ~ df_results_boot_r1.1[[2]]$dt[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)],
       type = "l", ylim = c(-30,30))  
  plot(df_results_boot_r1.1[[2]]$NEE_final[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)] ~ df_results_boot_r1.1[[2]]$dt[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)],
       type = "l", ylim = c(-30,30))  
  lines(df_results_boot_r1.1[[2]]$NEE_final ~ df_results_boot_r1.1[[2]]$dt, col = "red")
  
  summary(df_night_model) ## NEE_cor mit fast ausschließlich positiven Werten -> ANN wird kann nicht auf negative Werte trainiert werden.
  length(df_night_model$NEE_cor[df_night_model$NEE_cor < 0]) ## möglicherweise doch k-means? kurzer versuch notwendig.
  
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
