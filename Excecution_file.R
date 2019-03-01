#### ------------------------------------------- ##
#### Excecution ##
#### ------------------------------------------- ##
  
#### ----------------------- ##
#### 1 - Load path, functions and packages ##
#### ----------------------- ##

  ## path ####
  mypath <- getwd()

  ## dp token ####
  # token <- readRDS(paste0(mypath,"/token.rds"))
  
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
  
  pred_analysis_r1.1 <- TargetPreAnalysisPredictors(df_train = df_night_model, cluster = F, method_norm = "range_1_1")
  pred_analysis_r0.1 <- TargetPreAnalysisPredictors(df_train = df_night_model, cluster = F, method_norm = "range_0_1")
  pred_analysis_m0s1 <- TargetPreAnalysisPredictors(df_train = df_night_model, cluster = F, method_norm = "standarize")
  
  save(pred_analysis_r1.1, file = paste0(mypath, "/RData/results_pred_pre_analysis_r1.1.RData"))
  save(pred_analysis_r0.1, file = paste0(mypath, "/RData/results_pred_pre_analysis_r0.1.RData"))
  save(pred_analysis_m0s1, file = paste0(mypath, "/RData/results_pred_pre_analysis_m0s1.RData"))
  
  # load(paste0(mypath, "/RData/results_pred_pre_analysis_r0.1.RData"))
  
#### ----------------------- ##
#### Model Selection Respiration whole time span ##
#### ----------------------- ##
  
  results_resp_all_b_r1.1 <- TargetFunBO(df_train = pred_analysis_r1.1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                         cluster = F, method_norm = "range_1_1")
  results_resp_all_b_r0.1 <- TargetFunBO(df_train = pred_analysis_r0.1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                         cluster = F, method_norm = "range_0_1")
  results_resp_all_b_m0s1 <- TargetFunBO(df_train = pred_analysis_m0s1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                         cluster = F, method_norm = "standarize")
  
  save(results_resp_all_b_r1.1, file = paste0(mypath, "/RData/results_complete_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(results_resp_all_b_r0.1, file = paste0(mypath, "/RData/results_complete_r0.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(results_resp_all_b_m0s1, file = paste0(mypath, "/RData/results_complete_m0s1_", format(Sys.time(), "%d.%m"), ".RData"))
  # load(paste0(mypath, "/RData/results_complete_24.01.RData"))
  
#### ----------------------- ##
#### Bootstrap best model for error estimation ##
#### ----------------------- ##
  
  df_results_boot_r1.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_r1.1, 
                                              model_selection_results = results_resp_all_b_r1.1, 
                                              complete_data = df_merged, 
                                              rep = 100)
  df_results_boot_r0.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_r0.1, 
                                              model_selection_results = results_resp_all_b_r0.1, 
                                              complete_data = df_merged, 
                                              rep = 100)
  df_results_boot_m0s1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_m0s1, 
                                              model_selection_results = results_resp_all_b_m0s1, 
                                              complete_data = df_merged, 
                                              rep = 100)

  save(df_results_boot_r1.1, file =   paste0(mypath, "/RData/results_boots_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(df_results_boot_r0.1, file =   paste0(mypath, "/RData/results_boots_r0.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(df_results_boot_m0s1, file =   paste0(mypath, "/RData/results_boots_m0s1_", format(Sys.time(), "%d.%m"), ".RData"))
  
  pred_analysis_r0.1[[1]]
  # summary(df_results_boot_r1.1_c[[2]])
  # summary(df_results_boot_r1.1[[2]])
  # 
  # summary(df_results_boot_r0.1_c[[2]])
  # summary(df_results_boot_r0.1[[2]])
  # 
  # summary(df_results_boot_m0s1_c[[2]])
  # summary(df_results_boot_m0s1[[2]])
  # 
  # mean(df_results_boot_r1.1_c[[2]]$Re_final, na.rm = T)
  # mean(df_results_boot_r1.1[[2]]$Re_final, na.rm = T)
  # mean(df_results_boot_r1.1[[2]]$Re_gap_filled_95.conf, na.rm = T)
  # 
  # mean(df_results_boot_r0.1_c[[2]]$Re_final, na.rm = T)
  # mean(df_results_boot_r0.1[[2]]$Re_final, na.rm = T)
  # mean(df_results_boot_r0.1[[2]]$Re_gap_filled_95.conf, na.rm = T)
  # 
  # mean(df_results_boot_m0s1_c[[2]]$Re_final, na.rm = T)
  # mean(df_results_boot_m0s1[[2]]$Re_final, na.rm = T)
  # mean(df_results_boot_m0s1[[2]]$Re_gap_filled_95.conf, na.rm = T)
  
  # plot(df_results_boot_r1.1[[2]]$NEE[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)] ~ df_results_boot_r1.1[[2]]$dt[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)],
  #      type = "l", ylim = c(-30,30))  
  # plot(df_results_boot_r1.1[[2]]$NEE_final[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)] ~ df_results_boot_r1.1[[2]]$dt[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)],
  #      type = "l", ylim = c(-30,30))
  # plot(df_results_boot_r1.1[[2]]$Ts1[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)] ~ df_results_boot_r1.1[[2]]$dt[!is.na(df_results_boot_r1.1[[2]]$NEE_gap_filled)],
  #      type = "l", ylim = c(-30,30))  
  # lines(df_results_boot_r1.1[[2]]$NEE_final ~ df_results_boot_r1.1[[2]]$dt, col = "red")
  # 
  # plot(df_results_boot_r1.1[[2]]$NEE_cor[1:20000] ~ df_results_boot_r1.1[[2]]$dt[1:20000],
  #      type = "l", ylim = c(-30,30))  
  # plot(df_results_boot_r1.1[[2]]$NEE_gap_filled[1:20000] ~ df_results_boot_r1.1[[2]]$dt[1:20000],
  #      type = "l", ylim = c(-30,30)) 
  
#### ----------------------- ##
#### Calculate GPP ##
#### ----------------------- ##
  
  df_re_r1.1 <- df_results_boot_r1.1[[2]]
  df_re_r1.1$GPP <- NA
  df_re_r1.1$GPP[which(df_re_r1.1$flag_night == 1)] <- 0
  df_re_r1.1$GPP[which(df_re_r1.1$PPFDin < 5)] <- 0
  
  df_re_r1.1$GPP[which(df_re_r1.1$flag_night == 0 & df_re_r1.1$PPFDin > 5)] <- - 
    df_re_r1.1$NEE_measure[which(df_re_r1.1$flag_night == 0 & df_re_r1.1$PPFDin > 5)] + 
    df_re_r1.1$Re_final[which(df_re_r1.1$flag_night == 0 & df_re_r1.1$PPFDin > 5)]
  
  # summary(df_re_r1.1$GPP)
  
  df_re_r0.1 <- df_results_boot_r0.1[[2]]
  df_re_r0.1$GPP <- NA
  df_re_r0.1$GPP[which(df_re_r0.1$flag_night == 1)] <- 0
  df_re_r0.1$GPP[which(df_re_r0.1$PPFDin < 5)] <- 0
  
  df_re_r0.1$GPP[which(df_re_r0.1$flag_night == 0 & df_re_r0.1$PPFDin > 5)] <- - 
    df_re_r0.1$NEE_measure[which(df_re_r0.1$flag_night == 0 & df_re_r0.1$PPFDin > 5)] + 
    df_re_r0.1$Re_final[which(df_re_r0.1$flag_night == 0 & df_re_r0.1$PPFDin > 5)]
  
  # summary(df_re_r0.1$GPP)
  
  df_re_m0s1 <- df_results_boot_m0s1[[2]]
  df_re_m0s1$GPP <- NA
  df_re_m0s1$GPP[which(df_re_m0s1$flag_night == 1)] <- 0
  df_re_m0s1$GPP[which(df_re_m0s1$PPFDin < 5)] <- 0
  
  df_re_m0s1$GPP[which(df_re_m0s1$flag_night == 0 & df_re_m0s1$PPFDin > 5)] <- - 
    df_re_m0s1$NEE_measure[which(df_re_m0s1$flag_night == 0 & df_re_m0s1$PPFDin > 5)] + 
    df_re_m0s1$Re_final[which(df_re_m0s1$flag_night == 0 & df_re_m0s1$PPFDin > 5)]
  
  # summary(df_re_m0s1$GPP)

#### ----------------------- ##
#### Gap filling GPP ##
#### ----------------------- ##  
  df_re_r1.1_day <- df_re_r1.1[which(df_re_r1.1$flag_night == 0 & df_re_r1.1$PPFDin > 5), ]
  df_re_r0.1_day <- df_re_r0.1[which(df_re_r0.1$flag_night == 0 & df_re_r0.1$PPFDin > 5), ]
  df_re_m0s1_day <- df_re_m0s1[which(df_re_m0s1$flag_night == 0 & df_re_m0s1$PPFDin > 5), ]
  
  df_re_r1.1_day <- df_re_r1.1_day[-which(is.na(df_re_r1.1_day$GPP)), ]
  df_re_r0.1_day <- df_re_r0.1_day[-which(is.na(df_re_r0.1_day$GPP)), ]
  df_re_m0s1_day <- df_re_m0s1_day[-which(is.na(df_re_m0s1_day$GPP)), ]
  
#### ----------------------- ##
#### predictor pre analysis GPP ##
#### ----------------------- ##
  summary(df_re_r1.1_day)
  pred_analysis_gpp_r1.1 <- TargetPreAnalysisPredictors(df_train = df_re_r1.1_day, cluster = F, 
                                                        method_norm = "range_1_1", variable = "GPP")
  pred_analysis_gpp_r0.1 <- TargetPreAnalysisPredictors(df_train = df_re_r0.1_day, cluster = F, 
                                                        method_norm = "range_0_1", variable = "GPP")
  pred_analysis_gpp_m0s1 <- TargetPreAnalysisPredictors(df_train = df_re_m0s1_day, cluster = F, 
                                                        method_norm = "standarize", variable = "GPP")
  
  save(pred_analysis_gpp_r1.1, file = paste0(mypath, "/RData/results_pred_pre_analysis_gpp_r1.1.RData"))
  save(pred_analysis_gpp_r0.1, file = paste0(mypath, "/RData/results_pred_pre_analysis_gpp_r0.1.RData"))
  save(pred_analysis_gpp_m0s1, file = paste0(mypath, "/RData/results_pred_pre_analysis_gpp_m0s1.RData"))
  
  # load(paste0(mypath, "/RData/results_pred_pre_analysis_r0.1.RData"))
  
  #### ----------------------- ##
  #### Model Selection Respiration whole time span ##
  #### ----------------------- ##
  
  results_resp_all_b_gpp_r1.1 <- TargetFunBO(df_train = pred_analysis_gpp_r1.1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                             cluster = F, method_norm = "range_1_1", variable = "GPP")
  results_resp_all_b_gpp_r0.1 <- TargetFunBO(df_train = pred_analysis_gpp_r0.1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                             cluster = F, method_norm = "range_0_1", variable = "GPP")
  results_resp_all_b_gpp_m0s1 <- TargetFunBO(df_train = pred_analysis_gpp_m0s1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                             cluster = F, method_norm = "standarize", variable = "GPP")
  
  save(results_resp_all_b_gpp_r1.1, file = paste0(mypath, "/RData/results_complete_gpp_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(results_resp_all_b_gpp_r0.1, file = paste0(mypath, "/RData/results_complete_gpp_r0.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(results_resp_all_b_gpp_m0s1, file = paste0(mypath, "/RData/results_complete_gpp_m0s1_", format(Sys.time(), "%d.%m"), ".RData"))
  # load(paste0(mypath, "/RData/results_complete_24.01.RData"))
  
  #### ----------------------- ##
  #### Bootstrap best model for error estimation ##
  #### ----------------------- ##
  
  df_results_boot_gpp_r1.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_gpp_r1.1, 
                                                  model_selection_results = results_resp_all_b_gpp_r1.1, 
                                                  complete_data = df_re_r1.1, 
                                                  rep = 100)
  df_results_boot_gpp_r0.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_gpp_r0.1, 
                                                  model_selection_results = results_resp_all_b_gpp_r0.1, 
                                                  complete_data = df_re_r0.1, 
                                                  rep = 100)
  df_results_boot_gpp_m0s1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_gpp_m0s1, 
                                                  model_selection_results = results_resp_all_b_gpp_m0s1, 
                                                  complete_data = df_re_m0s1, 
                                                  rep = 100)
  
  
  save(df_results_boot_gpp_r1.1, file = paste0(mypath, "/RData/results_boots_gpp_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(df_results_boot_gpp_r0.1, file = paste0(mypath, "/RData/results_boots_gpp_r0.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(df_results_boot_gpp_m0s1, file = paste0(mypath, "/RData/results_boots_gpp_m0s1_", format(Sys.time(), "%d.%m"), ".RData"))
  
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
