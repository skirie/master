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
  
  pred_analysis <- TargetPreAnalysisPredictors(df_train = df_night_model)  
  df_train.1 <- pred_analysis[[1]]
  
#### ----------------------- ##
#### Model Selection Respiration whole time span ##
#### ----------------------- ##
  
  results_resp_all <- TargetFunBO(df_train = df_train.1, path = mypath)
  
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
    
    results_ms[[i]] <- TargetFunBO(df_train = df_train.2[,2:ncol(df_train.2)], path = mypath) 
  }
  
  