#### ------------------------------------------- ##
#### Excecution ##
#### ------------------------------------------- ##
  
  ## path ####
  mypath <- getwd()
  
  ## dp token ####
  token <- readRDS(paste0(mypath,"/token.rds"))
  
#### ----------------------- ##
#### Load functions and packages ##
#### ----------------------- ##
  
  source("Model_functions.R")
  
  ## Packages ####
  packages <- c("keras", "ggplot2", "Metrics", "httpuv", "rdrop2", "mlrMBO", "corrplot", "rgenoud", "betareg", "MASS")
  CheckPackages(packages) 
  use_condaenv("r-tensorflow")
  #### ####
  
#### ----------------------- ##
#### Check for data preprocessing ##
#### ----------------------- ##
  
  CheckData()

#### ----------------------- ##
#### predictor pre analysis ##
#### ----------------------- ##
  
  pred_analysis <- TargetPreAnalysisPredictors(df_train = df_night_model)  
  df_train_1 <- pred_analysis[[1]]
  
#### ----------------------- ##
#### Model Selection Respiration whole time span ##
#### ----------------------- ##
  
  results_resp_all <- TargetFunBO(df_train = df_train_1, path = mypath)
  
  
  