#### ------------------------------------------- ##
#### Excecution ##
#### ------------------------------------------- ##
  
  # A detailed description of the installing procedure of Keras in R is available in the Thesis folder (Read_me.pdf) or "00_Installing_Keras.R"
  # This is the target file of the partitioning process. 
  # 
  # 
  # author: Ferdinand Briegel
  # last_update: 12.07.2019

#### ----------------------- ####
#### 1.1 - Load path, functions and packages ####
#### ----------------------- ####

  ## path ##
  mypath <- getwd()
  path <- substr(mypath, 1, nchar(mypath)-27)
  
  ### --- ### Please set here path for thesis folder ### ---- ###
  # path <- "O:/Master/SoSe_2019/Briegel-Ferdinand-2/Thesis_Ferdinand_Briegel/"
  
  ## functions ##
  source("05_Model_functions.R")
  
  ## Packages ##
  packages <- c("keras", "ggplot2", "Metrics", "httpuv", "rdrop2", "mlrMBO", "corrplot", "rgenoud", "betareg", "MASS")
  CheckPackages(packages) 
  use_condaenv("r-tensorflow")
  
  ## data ##
  load(paste0(path, "Data_and_Programming/master/RData/Rawdata/df_model.RData")) 
  # if not availabe, uncomment line 32 ("CheckData()") and run it. This will execeut the preprocess script
  #### ####
  
#### ----------------------- ####
#### 1.2 Check for preprocessed data and ustar correction ####
#### ----------------------- ####
  
  # CheckData()
  #### ####
  
  ## u* star correction (default of function is 0.19, has to be changed) 
  ustar_ <- 0.19
  
  list_corrected <- UStarCorrection(data= df_merged, ustar = ustar_)
  df_merged <- list_corrected[[1]]
  df_night_model <- list_corrected[[2]]
  
  rm(ustar_, list_corrected)
  
#### ----------------------- ####
#### 2.1 Respiration: Predictor pre analysis ####
#### ----------------------- ####
  
  ## unclusted data
  # pred_analysis_r1.1 <- TargetPreAnalysisPredictors(df_train = df_night_model, cluster = F, method_norm = "range_1_1")
  # pred_analysis_r0.1 <- TargetPreAnalysisPredictors(df_train = df_night_model, cluster = F, method_norm = "range_0_1")
  pred_analysis_m0s1 <- TargetPreAnalysisPredictors(df_train = df_night_model, cluster = F, method_norm = "standarize")
  
  # save(pred_analysis_r1.1, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_pred_pre_analysis_r1.1.RData"))
  # save(pred_analysis_r0.1, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_pred_pre_analysis_r0.1.RData"))
  save(pred_analysis_m0s1, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_pred_pre_analysis_m0s1.RData"))
  
  ## clustered data
  # pred_analysis_r1.1_c <- TargetPreAnalysisPredictors(df_train = df_night_model, cluster = T, method_norm = "range_1_1")
  # pred_analysis_r0.1_c <- TargetPreAnalysisPredictors(df_train = df_night_model, cluster = T, method_norm = "range_0_1")
  # pred_analysis_m0s1_c <- TargetPreAnalysisPredictors(df_train = df_night_model, cluster = T, method_norm = "standarize")
  
  # save(pred_analysis_r1.1_c, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_pred_pre_analysis_r1.1_c.RData"))
  # save(pred_analysis_r0.1_c, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_pred_pre_analysis_r0.1_c.RData"))
  # save(pred_analysis_m0s1_c, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_pred_pre_analysis_m0s1_c.RData"))
  #### ####
  
#### ----------------------- #####
#### 2.2 Respiration: Model Selection Respiration whole time span ####
#### ----------------------- ####
  
  ## unclusted data
  # results_resp_all_b_r1.1 <- TargetFunBO(df_train = pred_analysis_r1.1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
  #                                          cluster = F, method_norm = "range_1_1")
  # results_resp_all_b_r0.1 <- TargetFunBO(df_train = pred_analysis_r0.1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
  #                                          cluster = F, method_norm = "range_0_1")
  results_resp_all_b_m0s1 <- TargetFunBO(df_train = pred_analysis_m0s1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                           cluster = F, method_norm = "standarize")
  
  # save(results_resp_all_b_r1.1, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_complete_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  # save(results_resp_all_b_r0.1, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_complete_r0.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(results_resp_all_b_m0s1, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_complete_m0s1_", format(Sys.time(), "%d.%m"), ".RData"))
  
  ## clustered data
  # results_resp_all_b_r1.1_c <- TargetFunBO(df_train = pred_analysis_r1.1_c[[1]], path = mypath, opt.batch = T, ANN = "seq", 
  #                                        cluster = T, method_norm = "range_1_1")
  # results_resp_all_b_r0.1_c <- TargetFunBO(df_train = pred_analysis_r0.1_c[[1]], path = mypath, opt.batch = T, ANN = "seq", 
  #                                        cluster = T, method_norm = "range_0_1")
  # results_resp_all_b_m0s1_c <- TargetFunBO(df_train = pred_analysis_m0s1_c[[1]], path = mypath, opt.batch = T, ANN = "seq", 
  #                                        cluster = T, method_norm = "standarize")
  
  # save(results_resp_all_b_r1.1_c, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_complete_r1.1_c_", format(Sys.time(), "%d.%m"), ".RData"))
  # save(results_resp_all_b_r0.1_c, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_complete_r0.1_c_", format(Sys.time(), "%d.%m"), ".RData"))
  # save(results_resp_all_b_m0s1_c, file = paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_complete_m0s1_c_", format(Sys.time(), "%d.%m"), ".RData"))
  #### ####
  
#### ----------------------- ####
#### 2.3 Respiration: Bootstrap best model for error estimation ####
#### ----------------------- ####
  
  ## unclustered data
  # df_results_boot_r1.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_r1.1, 
  #                                             model_selection_results = results_resp_all_b_r1.1, 
  #                                             complete_data = df_merged, rep = 100)
  # df_results_boot_r0.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_r0.1, 
  #                                             model_selection_results = results_resp_all_b_r0.1, 
  #                                             complete_data = df_merged, rep = 100)
  df_results_boot_m0s1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_m0s1, 
                                              model_selection_results = results_resp_all_b_m0s1, 
                                              complete_data = df_merged, rep = 100)
  
  # save(df_results_boot_r1.1, file =   paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_boots_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  # save(df_results_boot_r0.1, file =   paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_boots_r0.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(df_results_boot_m0s1, file =   paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_boots_m0s1_", format(Sys.time(), "%d.%m"), ".RData"))
  
  ## clustered data
  # df_results_boot_r1.1_c <- BootstrapPrediction(pre_predictor_results = pred_analysis_r1.1_c, 
  #                                             model_selection_results = results_resp_all_b_r1.1_c, 
  #                                             complete_data = df_merged, rep = 100)
  # df_results_boot_r0.1_c <- BootstrapPrediction(pre_predictor_results = pred_analysis_r0.1_c, 
  #                                             model_selection_results = results_resp_all_b_r0.1_c, 
  #                                             complete_data = df_merged, rep = 100)
  # df_results_boot_m0s1_c <- BootstrapPrediction(pre_predictor_results = pred_analysis_m0s1_c, 
  #                                             model_selection_results = results_resp_all_b_m0s1_c, 
  #                                             complete_data = df_merged, rep = 100)
  
  # save(df_results_boot_r1.1_c, file =   paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_boots_r1.1_c_", format(Sys.time(), "%d.%m"), ".RData"))
  # save(df_results_boot_r0.1_c, file =   paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_boots_r0.1_c_", format(Sys.time(), "%d.%m"), ".RData"))
  # save(df_results_boot_m0s1_c, file =   paste0(path, "Data_and_Programming/master/RData/RE/Datapreparation_methods/results_boots_m0s1_c_", format(Sys.time(), "%d.%m"), ".RData"))
  #### ####
  
#### ----------------------- ####
#### 3.1 GPP: Calculate GPP ####
#### ----------------------- ####
  
  # df_re_r1.1 <- df_results_boot_r1.1[[2]]
  # df_re_r1.1$GPP <- NA
  # df_re_r1.1$GPP[which(df_re_r1.1$PPFDin < 5)] <- 0
  # 
  # df_re_r1.1$GPP[which(df_re_r1.1$PPFDin >= 5)] <- - 
  #   df_re_r1.1$NEE_measure[which(df_re_r1.1$PPFDin >= 5)] + 
  #   df_re_r1.1$Re_final[which(df_re_r1.1$PPFDin >= 5)]
  # 
  # # summary(df_re_r1.1$GPP)
  # 
  # df_re_r0.1 <- df_results_boot_r0.1[[2]]
  # df_re_r0.1$GPP <- NA
  # df_re_r0.1$GPP[which(df_re_r0.1$PPFDin < 5)] <- 0
  # 
  # df_re_r0.1$GPP[which(df_re_r0.1$PPFDin >= 5)] <- - 
  #   df_re_r0.1$NEE_measure[which(df_re_r0.1$PPFDin >= 5)] + 
  #   df_re_r0.1$Re_final[which(df_re_r0.1$PPFDin >= 5)]
  
  # summary(df_re_r0.1$GPP)
  
  df_re_m0s1 <- df_results_boot_m0s1[[2]]
  df_re_m0s1$GPP <- NA
  df_re_m0s1$GPP[which(df_re_m0s1$PPFDin < 5)] <- 0
  
  df_re_m0s1$GPP[which(df_re_m0s1$PPFDin >= 5)] <- - 
    df_re_m0s1$NEE_cor[which(df_re_m0s1$PPFDin >= 5)] + 
    df_re_m0s1$Re_final[which(df_re_m0s1$PPFDin >= 5)]
  
  # summary(df_re_m0s1)
  
  rm(df_results_boot_m0s1, df_results_boot_r0.1, df_results_boot_r1.1)
  #### ####
  
#### ----------------------- ####
#### 3.2 GPP: Gap filling GPP ####
#### ----------------------- #### 
  
  # df_re_r1.1_day <- df_re_r1.1[which(df_re_r1.1$PPFDin >= 5), ]
  # df_re_r0.1_day <- df_re_r0.1[which(df_re_r0.1$PPFDin >= 5), ]
  df_re_m0s1_day <- df_re_m0s1[which(df_re_m0s1$PPFDin >= 5), ]
  
  # df_re_r1.1_day <- df_re_r1.1_day[-which(is.na(df_re_r1.1_day$GPP)), ]
  # df_re_r0.1_day <- df_re_r0.1_day[-which(is.na(df_re_r0.1_day$GPP)), ]
  df_re_m0s1_day <- df_re_m0s1_day[-which(is.na(df_re_m0s1_day$GPP)), ]
  #### ####
  
#### ----------------------- ####
#### 3.3 GPP: Predictor pre analysis ####
#### ----------------------- ####

  # pred_analysis_gpp_r1.1 <- TargetPreAnalysisPredictors(df_train = df_re_r1.1_day, cluster = F, 
  #                                                       method_norm = "range_1_1", variable = "GPP")
  # pred_analysis_gpp_r0.1 <- TargetPreAnalysisPredictors(df_train = df_re_r0.1_day, cluster = F,
  #                                                       method_norm = "range_0_1", variable = "GPP")
  pred_analysis_gpp_m0s1 <- TargetPreAnalysisPredictors(df_train = df_re_m0s1_day, cluster = F, 
                                                        method_norm = "standarize", variable = "GPP")
  
  # save(pred_analysis_gpp_r1.1, file = paste0(path, "Data_and_Programming/master/RData/GPP/results_pred_pre_analysis_gpp_r1.1.RData"))
  # save(pred_analysis_gpp_r0.1, file = paste0(path, "Data_and_Programming/master/RData/GPP/results_pred_pre_analysis_gpp_r0.1.RData"))
  save(pred_analysis_gpp_m0s1, file = paste0(path, "Data_and_Programming/master/RData/GPP/results_pred_pre_analysis_gpp_m0s1.RData"))
  #### ####
  
#### ----------------------- ####
#### 3.4 GPP: Model Selection Respiration whole time span ####
#### ----------------------- ####
  
  # results_resp_all_b_gpp_r1.1 <- TargetFunBO(df_train = pred_analysis_gpp_r1.1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
  #                                            cluster = F, method_norm = "range_1_1", variable = "GPP")
  # results_resp_all_b_gpp_r0.1 <- TargetFunBO(df_train = pred_analysis_gpp_r0.1[[1]], path = mypath, opt.batch = T, ANN = "seq",
  #                                            cluster = F, method_norm = "range_0_1", variable = "GPP")
  results_resp_all_b_gpp_m0s1 <- TargetFunBO(df_train = pred_analysis_gpp_m0s1[[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                             cluster = F, method_norm = "standarize", variable = "GPP")
  
  # save(results_resp_all_b_gpp_r1.1, file = paste0(path, "Data_and_Programming/master/RData/GPP/results_complete_gpp_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  # save(results_resp_all_b_gpp_r0.1, file = paste0(path, "Data_and_Programming/master/RData/GPP/results_complete_gpp_r0.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(results_resp_all_b_gpp_m0s1, file = paste0(path, "Data_and_Programming/master/RData/GPP/results_complete_gpp_m0s1_", format(Sys.time(), "%d.%m"), ".RData"))
  #### ####
  
#### ----------------------- ####
#### 3.5 GPP: Bootstrap best model for error estimation ####
#### ----------------------- ####
  
  # df_results_boot_gpp_r1.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_gpp_r1.1, 
  #                                                 model_selection_results = results_resp_all_b_gpp_r1.1, 
  #                                                 complete_data = df_re_r1.1, 
  #                                                 rep = 100, variable = "GPP")
  # df_results_boot_gpp_r0.1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_gpp_r0.1,
  #                                                 model_selection_results = results_resp_all_b_gpp_r0.1,
  #                                                 complete_data = df_re_r0.1,
  #                                                 rep = 100, variable = "GPP")
  df_results_boot_gpp_m0s1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_gpp_m0s1, 
                                                  model_selection_results = results_resp_all_b_gpp_m0s1, 
                                                  complete_data = df_re_m0s1, 
                                                  rep = 100, variable = "GPP")
  
  # summary(df_results_boot_gpp_r1.1[[2]])
  # summary(df_results_boot_gpp_r0.1[[2]])
  # summary(df_results_boot_gpp_m0s1[[2]])
  # 
  # summary(df_results_boot_r1.1[[2]])
  # summary(df_results_boot_r0.1[[2]])
  # summary(df_results_boot_m0s1[[2]])
  # 
  # ## final gap filled NEE
  # df_results_boot_gpp_r1.1[[2]]$NEE_final <- df_results_boot_gpp_r1.1[[2]]$NEE_measure
  # df_results_boot_gpp_r1.1[[2]]$NEE_final[which(is.na(df_results_boot_gpp_r1.1[[2]]$NEE_final))] <- 
  #   - df_results_boot_gpp_r1.1[[2]]$GPP_final[which(is.na(df_results_boot_gpp_r1.1[[2]]$NEE_final))] + 
  #   df_results_boot_gpp_r1.1[[2]]$Re_final[which(is.na(df_results_boot_gpp_r1.1[[2]]$NEE_final))]
  # 
  # df_results_boot_gpp_r0.1[[2]]$NEE_final <- df_results_boot_gpp_r0.1[[2]]$NEE_measure
  # df_results_boot_gpp_r0.1[[2]]$NEE_final[which(is.na(df_results_boot_gpp_r0.1[[2]]$NEE_final))] <- 
  #   - df_results_boot_gpp_r0.1[[2]]$GPP_final[which(is.na(df_results_boot_gpp_r0.1[[2]]$NEE_final))] + 
  #   df_results_boot_gpp_r0.1[[2]]$Re_final[which(is.na(df_results_boot_gpp_r0.1[[2]]$NEE_final))]
  
  df_results_boot_gpp_m0s1[[2]]$NEE_final <- df_results_boot_gpp_m0s1[[2]]$NEE_cor
  df_results_boot_gpp_m0s1[[2]]$NEE_final[which(is.na(df_results_boot_gpp_m0s1[[2]]$NEE_final))] <- 
    - df_results_boot_gpp_m0s1[[2]]$GPP_final[which(is.na(df_results_boot_gpp_m0s1[[2]]$NEE_final))] + 
    df_results_boot_gpp_m0s1[[2]]$Re_final[which(is.na(df_results_boot_gpp_m0s1[[2]]$NEE_final))]
  
  # save(df_results_boot_gpp_r1.1, file = paste0(path, "Data_and_Programming/master/RData/GPP/results_boots_gpp_r1.1_", format(Sys.time(), "%d.%m"), ".RData"))
  # save(df_results_boot_gpp_r0.1, file = paste0(path, "Data_and_Programming/master/RData/GPP/results_boots_gpp_r0.1_", format(Sys.time(), "%d.%m"), ".RData"))
  save(df_results_boot_gpp_m0s1, file = paste0(path, "Data_and_Programming/master/RData/GPP/results_boots_gpp_m0s1_", format(Sys.time(), "%d.%m"), ".RData"))
  #### ####
  
#### ----------------------- ####
#### 4. Seasonality  ####
#### ----------------------- ####
  pred_analysis_season <- list()
  results_resp_all_season <- list()
  df_results_boot_season <- list()
  pred_analysis_gpp_season <- list()
  results_gpp_all_season <- list()
  df_results_boot_gpp_season <- list()
  
  ## loop for windows and months
  for (i in 1:12){
    ## extract season
    if (i == 1){
      df_mer <- df_merged[which(as.numeric(format(df_merged$dt,"%m")) %in% c(1, 2, 12)), ]
      df_nig <- df_night_model[which(as.numeric(format(df_night_model$dt,"%m")) %in% c(1, 2, 12)), ]
    } else if (i == 12){
      df_mer <- df_merged[which(as.numeric(format(df_merged$dt,"%m")) %in% c(1, 11, 12)), ]
      df_nig <- df_night_model[which(as.numeric(format(df_night_model$dt,"%m")) %in% c(1, 11, 12)), ]
    } else {
      df_mer <- df_merged[which(df_merged$month %in% c((i-1):(i+1))), ]
      df_nig <- df_night_model[which(as.numeric(format(df_night_model$dt,"%m")) %in% c((i-1):(i+1))), ]
    }

  #### Respiration-model ##
    ## Respiration: Predictor pre analysis
    pred_analysis_season[[i]] <- TargetPreAnalysisPredictors(df_train = df_nig, cluster = F, method_norm = "standarize")
    
    ## Respiration: Bayesian Optimization and predictor Analysis (model selection and predictor selection)
    results_resp_all_season[[i]] <- TargetFunBO(df_train = pred_analysis_season[[i]][[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                           cluster = F, method_norm = "standarize")
    
    ## Respiration: bootstrap
    df_results_boot_season[[i]] <- BootstrapPrediction(pre_predictor_results = pred_analysis_season[[i]], 
                                                model_selection_results = results_resp_all_season[[i]], 
                                                complete_data = df_mer, rep = 100, variable = "NEE_cor")
    
  #### GPP-model ##
    ## calculate GPP
    df_re_season <- df_results_boot_season[[i]][[2]]
    df_re_season$GPP <- NA
    df_re_season$GPP[which(df_re_season$PPFDin < 5)] <- 0
    
    df_re_season$GPP[which(df_re_season$PPFDin >= 5)] <-  
      - df_re_season$NEE_measure[which(df_re_season$PPFDin >= 5)] + 
      df_re_season$Re_final[which(df_re_season$PPFDin >= 5)]
    
    df_re_season_day <- df_re_season[which(df_re_season$PPFDin >= 5), ]
    df_re_season_day <- df_re_season_day[-which(is.na(df_re_season_day$GPP)), ]
    
    ## GPP: Predictor pre analysis
    pred_analysis_gpp_season[[i]] <- TargetPreAnalysisPredictors(df_train = df_re_season_day, cluster = F,
                                                                 method_norm = "standarize", variable = "GPP")
    
    ## GPP: model selection and predictor selection
    results_gpp_all_season[[i]] <- TargetFunBO(df_train = pred_analysis_gpp_season[[i]][[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                               cluster = F, method_norm = "standarize", variable = "GPP")
    
    ## GPP: bootstrap
    df_results_boot_gpp_season[[i]] <- BootstrapPrediction(pre_predictor_results = pred_analysis_gpp_season[[i]], 
                                                       model_selection_results = results_gpp_all_season[[i]], 
                                                       complete_data = df_re_season, rep = 100, variable = "GPP")
    
    ## NEE: final gap fill
    df_results_boot_gpp_season[[i]][[2]]$NEE_final <- df_results_boot_gpp_season[[i]][[2]]$NEE_measure
    df_results_boot_gpp_season[[i]][[2]]$NEE_final[which(is.na(df_results_boot_gpp_season[[i]][[2]]$NEE_final))] <- 
      - df_results_boot_gpp_season[[i]][[2]]$GPP_final[which(is.na(df_results_boot_gpp_season[[i]][[2]]$NEE_final))] + 
      df_results_boot_gpp_season[[i]][[2]]$Re_final[which(is.na(df_results_boot_gpp_season[[i]][[2]]$NEE_final))]
    
  }
  
  ## save data
  save(pred_analysis_season, results_resp_all_season, df_results_boot_season, pred_analysis_gpp_season, 
       results_gpp_all_season, df_results_boot_gpp_season, 
       file = paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Temporal_variability/results_full_season_", 
                     format(Sys.time(), "%d.%m"), ".RData"))
  
  #### ####
  
#### ----------------------- ####
#### 5. Moving Window over Years: Model Selection for an moving window of 4 years ####
#### ----------------------- ####

  years_ <- unique(as.numeric(format(df_merged$dt, "%Y")))
  years_ <- years_[-1]
  
  pred_analysis_year <- list()
  results_resp_all_year <- list()
  df_results_boot_year <- list()
  pred_analysis_gpp_year <- list()
  results_gpp_all_year <- list()
  df_results_boot_gpp_year <- list()
  
  ## loop for windows and years
  for (i in 1:(length(years_) - 1)){
    window_ <- years_[i:(i + 1)]
    
    if (i == 1){
      df_mer <- df_merged[which(as.numeric(format(df_merged$dt,"%Y")) %in% c(2001, window_)), ]
      df_nig <- df_night_model[which(as.numeric(format(df_night_model$dt,"%Y")) %in% c(2001, window_)), ]
    } else {
      df_mer <- df_merged[which(as.numeric(format(df_merged$dt,"%Y")) %in% window_), ]
      df_nig <- df_night_model[which(as.numeric(format(df_night_model$dt,"%Y")) %in% window_), ]
    }
    
  #### Respiration-model ##
    ## Respiration: Predictor pre analysis
    pred_analysis_year[[i]] <- TargetPreAnalysisPredictors(df_train = df_nig, cluster = F, method_norm = "standarize")

    
    # ## chosse predictors by hand -> save time
    # pred_model_re <- c("Ts1", "MS_mean", "airT", "RH", "LWin", "LWout", "h_last_precip", "precip_30d",
    #                 "year_ws_sin", "year_sa_sin", "day_sin", "NEE_cor")
    # 
    # pred_analysis_year[[1]] <- df_nig[, pred_model_re]
    
    ## Respiration: model selection and predictor selection
    results_resp_all_year[[i]] <- TargetFunBO(df_train = pred_analysis_year[[i]][[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                                cluster = F, method_norm = "standarize")
    
    ## Respiration: bootstrap
    df_results_boot_year[[i]] <- BootstrapPrediction(pre_predictor_results = pred_analysis_year[[i]], 
                                                       model_selection_results = results_resp_all_year[[i]], 
                                                      complete_data = df_mer, rep = 100, variable = "NEE_cor")
    
  #### GPP-model ##
    ## calculate GPP
    df_re_year <- df_results_boot_year[[i]][[2]]
    df_re_year$GPP <- NA
    df_re_year$GPP[which(df_re_year$PPFDin < 5)] <- 0
    
    df_re_year$GPP[which(df_re_year$PPFDin >= 5)] <- 
      - df_re_year$NEE_measure[which(df_re_year$PPFDin >= 5)] + 
      df_re_year$Re_final[which(df_re_year$PPFDin >= 5)]
    
    df_re_year_day <- df_re_year[which(df_re_year$PPFDin >= 5), ]
    df_re_year_day <- df_re_year_day[-which(is.na(df_re_year_day$GPP)), ]
    
    # GPP: Predictor pre analysis
    pred_analysis_gpp_year[[i]] <- TargetPreAnalysisPredictors(df_train = df_re_year_day, cluster = F,
                                                                 method_norm = "standarize", variable = "GPP")
    
    # ## chosse predictors by hand -> save time
    # pred_model_gpp <- c("Ts1", "Soil.moisture_main", "airT", "RH", "LWin", "LWout", "SWout", "PPFDin", 
    #                 "h_last_precip", "precip_30d", "year_ws_sin", "year_sa_sin", "day_sin", "GPP")
    # 
    # pred_analysis_gpp_year[[1]] <- df_re_year_day[, pred_model_gpp]
    
    ## GPP: model selection and predictor selection
    results_gpp_all_year[[i]] <- TargetFunBO(df_train = pred_analysis_gpp_year[[i]][[1]], path = mypath, opt.batch = T, ANN = "seq", 
                                               cluster = F, method_norm = "standarize", variable = "GPP")
    
    ## GPP: bootstrap
    df_results_boot_gpp_year[[i]] <- BootstrapPrediction(pre_predictor_results = pred_analysis_gpp_year[[i]], 
                                                           model_selection_results = results_gpp_all_year[[i]], 
                                                           complete_data = df_re_year, rep = 100, variable = "GPP")
    
    ## NEE: final gap fill
    df_results_boot_gpp_year[[i]][[2]]$NEE_final <- df_results_boot_gpp_year[[i]][[2]]$NEE_cor
    df_results_boot_gpp_year[[i]][[2]]$NEE_final[which(is.na(df_results_boot_gpp_year[[i]][[2]]$NEE_final))] <- 
      - df_results_boot_gpp_year[[i]][[2]]$GPP_final[which(is.na(df_results_boot_gpp_year[[i]][[2]]$NEE_final))] + 
      df_results_boot_gpp_year[[i]][[2]]$Re_final[which(is.na(df_results_boot_gpp_year[[i]][[2]]$NEE_final))]
  }
  
  ## save data
  save(pred_analysis_year, results_resp_all_year, df_results_boot_year, pred_analysis_gpp_year, results_gpp_all_year, df_results_boot_gpp_year, 
       file = paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Temporal_variability/results_full_year_", 
                     format(Sys.time(), "%d.%m"), ".RData"))

  #### ####

#### ----------------------- ####
#### 6. Fertilazation Effect ####
#### ----------------------- ####
  
  ## get pre-fertilazation data 
  df_mer_f <- df_merged[which(as.numeric(format(df_merged$dt,"%Y")) %in% c(2001:2006)), ]
  df_nig_f <- df_night_model[which(as.numeric(format(df_night_model$dt,"%Y")) %in% c(2001:2006)), ]
  
  df_mer_post_f <- df_merged
  df_mer_post_f$NEE_cor[which(as.numeric(format(df_mer_post_f$dt, "%Y")) %in% c(2007:2016))] <- NA
  # summary(df_mer_post_f)#$NEE_cor[which(as.numeric(format(df_mer_post_f$dt,"%Y")) %in% c(2007:2016))])
  
  df_nig_post_f <- df_night_model[which(as.numeric(format(df_night_model$dt,"%Y")) %in% c(2007:2016)), ]
  
#### Respiration Model ##
  ## Respiration: Predictor pre analysis
  pred_analysis_fert_m0s1 <- TargetPreAnalysisPredictors(df_train = df_nig_f, cluster = F, method_norm = "standarize")
  
  save(pred_analysis_fert_m0s1, 
       file = paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Fertilization/results_pred_pre_analysis_fert_m0s1.RData"))
  
  ## Respiration: Bayesian Optimization and predictor Analysis (model selection and predictor selection)
  results_resp_all_b_fert_m0s1 <- TargetFunBO(df_train = pred_analysis_fert_m0s1[[1]], path = mypath, 
                                              opt.batch = T, ANN = "seq", cluster = F, method_norm = "standarize")
  save(results_resp_all_b_fert_m0s1, 
       file = paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Fertilization/results_complete_fert_m0s1_", 
                     format(Sys.time(), "%d.%m"), ".RData"))
  
  ## Respiration: Bootstrap
  df_results_boot_fert_m0s1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_fert_m0s1, 
                                              model_selection_results = results_resp_all_b_fert_m0s1, 
                                              complete_data = df_mer_post_f, rep = 100)
  
  save(df_results_boot_fert_m0s1, 
       file = paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Fertilization/results_boots_fert_m0s1_", 
       format(Sys.time(), "%d.%m"), ".RData"))
  
#### GPP-model ##  
  ## GPP: Calculation
  df_re_m0s1 <- df_results_boot_fert_m0s1[[2]]
  df_re_m0s1$GPP <- NA
  df_re_m0s1$GPP[which(df_re_m0s1$PPFDin < 5)] <- 0
  
  df_re_m0s1$GPP[which(df_re_m0s1$PPFDin >= 5)] <- - 
    df_re_m0s1$NEE_cor[which(df_re_m0s1$PPFDin >= 5)] + 
    df_re_m0s1$Re_final[which(df_re_m0s1$PPFDin >= 5)]
  
  df_re_m0s1_day <- df_re_m0s1[which(df_re_m0s1$PPFDin >= 5), ]
  df_re_m0s1_day <- df_re_m0s1_day[-which(is.na(df_re_m0s1_day$GPP)), ]
  
  summary(df_re_m0s1)
  summary(df_re_m0s1$NEE_cor[which(df_re_m0s1$PPFDin >= 5)])
  
  ## GPP: Predictor pre analysis 
  pred_analysis_fert_gpp_m0s1 <- TargetPreAnalysisPredictors(df_train = df_re_m0s1_day, cluster = F, 
                                                             method_norm = "standarize", variable = "GPP")
  save(pred_analysis_fert_gpp_m0s1, 
       file = paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Fertilization/results_pred_pre_analysis_fert_gpp_m0s1.RData"))
  
  ## GPP: Bayesian Optimization and predictor Analysis (model selection and predictor selection)
  results_resp_all_b_fert_gpp_m0s1 <- TargetFunBO(df_train = pred_analysis_fert_gpp_m0s1[[1]], path = mypath, opt.batch = T, 
                                                  ANN = "seq", cluster = F, method_norm = "standarize", variable = "GPP")
  save(results_resp_all_b_fert_gpp_m0s1, 
       file = paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Fertilization/results_complete_fert_gpp_m0s1_", 
       format(Sys.time(), "%d.%m"), ".RData"))
  
  ## GPP: Bootstrap 
  df_results_boot_fert_gpp_m0s1 <- BootstrapPrediction(pre_predictor_results = pred_analysis_fert_gpp_m0s1, 
                                                       model_selection_results = results_resp_all_b_fert_gpp_m0s1, 
                                                       complete_data = df_re_m0s1, 
                                                       rep = 100, variable = "GPP")

  ## NEE: final gap fill
  df_results_boot_fert_gpp_m0s1[[2]]$NEE_final <- df_results_boot_fert_gpp_m0s1[[2]]$NEE_cor
  df_results_boot_fert_gpp_m0s1[[2]]$NEE_final[which(is.na(df_results_boot_fert_gpp_m0s1[[2]]$NEE_final))] <- 
    - df_results_boot_fert_gpp_m0s1[[2]]$GPP_final[which(is.na(df_results_boot_fert_gpp_m0s1[[2]]$NEE_final))] + 
    df_results_boot_fert_gpp_m0s1[[2]]$Re_final[which(is.na(df_results_boot_fert_gpp_m0s1[[2]]$NEE_final))]
  
  ## save data 
  save(df_results_boot_fert_gpp_m0s1, 
       file = paste0(path, "Data_and_Programming/master/RData/Temporal_variability_and_fertilization/Fertilization/results_boots_fert_gpp_m0s1_", 
       format(Sys.time(), "%d.%m"), ".RData"))
  