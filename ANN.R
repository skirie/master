#### ------------------------------------------- ####
#### Modellierung von Treibhausgasen mittels Neuralen Netzwerken ####
#### ------------------------------------------- ####
  
#### -------------------------- ####
  ## Used Packages ####
#### -------------------------- ####
  
  ## Package Function ####
  check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  ## Packages ####
  packages <- c("keras", "ggplot2", "Metrics", "httpuv", "rdrop2", "mlrMBO", "corrplot", "rgenoud")
  check.packages(packages) 
  use_condaenv("r-tensorflow")
  
#### ------------------------------------- ####
  ## load data ####
#### ------------------------------------- ####
  
  ## load data ####
  ## path 
  #setwd("C:/Users/ferdinand.briegel/Desktop/05_Masterarbeit/Daten_und_Auswertung/Auswertung")
  mypath <- getwd()
  
  ## Flux Data ####
  df_raw <- read.csv(paste0(mypath, "/Daten/ANN_rawdata_ver2.csv"))
  
  ## Sun rise / set ####
  df_hel <- read.csv(paste0(mypath, "/Daten/sunsetrise.csv"),
                     header=T, sep=";")
  
  head(df_raw)
  summary(df_raw)
  
#### ------------------------------------- ####
  ## prepare/check data ####
#### ------------------------------------- ####  
  
  ## NaN to NA ####
  df_raw[df_raw == "NaN"] <- NA
  
  ## df_raw$RH has -Inf and Inf values ####
  summary(df_raw$RH)
  df_raw$RH[which(df_raw$RH > 100)] <- NA
  df_raw$RH[which(df_raw$RH < 0)] <- NA
  summary(df_raw$RH)
  
  ## shift time to PST
  df_raw[c(17:nrow(df_raw)),c(1:5)] <- df_raw[c(1:(nrow(df_raw)-16)),c(1:5)]
  df_raw[1:15,1] <- 2001
  df_raw[16,1] <- 2002
  df_raw[1:15,2] <- 12
  df_raw[16,2] <- 1
  df_raw[1:15,3] <- 31
  df_raw[16,3] <- 1
  df_raw[1:16,4] <- c(16, 17, 17, 18, 18, 19, 19, 20, 20, 21, 21, 22, 22, 23, 23, 0)
  df_raw[1:16,5] <- rep(c(30,0),8)
  
  ## Check for Colinearity
  M <- cor(df_raw[,7:29], use = "complete.obs")
  corrplot.mixed(M)
  df_raw <- df_raw[,-c(7,8,9,14,15,16)]
  
  ## Sun rise / set ####
  df_hel$Datum <- as.character(df_hel$Datum)
  df_hel$Sunrise <- as.character(df_hel$Sunrise)
  df_hel$Sunset <- as.character(df_hel$Sunset)
  
  df_hel_2 <- data.frame(df_hel, as.numeric(do.call(rbind, strsplit(df_hel$Datum,' '))[,1]))
  names(df_hel_2)[5] <- "day"
  
  df_hel_2 <- data.frame(df_hel_2, as.numeric(do.call(rbind, strsplit(df_hel$Sunset,':'))[,1]))
  df_hel_2 <- data.frame(df_hel_2, as.numeric(do.call(rbind, strsplit(df_hel$Sunset,':'))[,2]))
  names(df_hel_2)[6:7] <- paste("set", c("h","m"), sep = "_")
  
  df_hel_2 <- data.frame(df_hel_2, as.numeric(do.call(rbind, strsplit(df_hel$Sunrise,':'))[,1]))
  df_hel_2 <- data.frame(df_hel_2, as.numeric(do.call(rbind, strsplit(df_hel$Sunrise,':'))[,2]))
  names(df_hel_2)[8:9] <- paste("rise", c("h","m"), sep = "_")
  
  df_hel_2 <- df_hel_2[,-c(2:4)]
  
  ## flag night time data ####
  fun_flag_night <- function(df_flux, df_sun){
    hm_flux <- df_flux$hour * 60 + df_flux$min
    hm_sun_r <- df_sun$rise_h * 60 + df_sun$rise_m
    hm_sun_s <- df_sun$set_h * 60 + df_sun$set_m
    
    flag_ <- rep(0, nrow(df_flux))
    
    for (i in 1:12){
      w_m_raw <- which(df_flux$month == i) # which month
      w_m_sun <- which(df_sun$month == i) # which month
      
      for (j in 1:length(w_m_sun)){
        w_d_raw <- which(df_flux$day == j) # which day
        w_raw <- w_m_raw[which(w_m_raw %in% w_d_raw)] # match of month and day
        
        flag_[w_raw][hm_flux[w_raw] < hm_sun_r[w_m_sun][j] | hm_flux[w_raw] > hm_sun_s[w_m_sun][j]] <- 1
      }
    }
    return(flag_)
  }
  
  df_raw$flag_night <- fun_flag_night(df_flux = df_raw, df_sun = df_hel_2)
  
  rm(df_hel, fun_flag_night)
  
  ## First Plots - overview ####
  df_raw$dt <- paste0(df_raw$year,"-",df_raw$month, "-", df_raw$day, " ", df_raw$hour, ":", df_raw$min)
  df_raw$dt <- as.POSIXct(df_raw$dt, format="%Y-%m-%e %H:%M")
  
  #plot(df_raw$NEE_measure[1:17000] ~ df_raw$dt[1:17000], type = "l")
  
#### ------------------------------------- ####
  ## Prepare Data for Respiration Model ####
#### ------------------------------------- ####  
  
  ## Extract Night Data ####
  df_night <- df_raw[df_raw$flag_night == 1,-26]
  df_night <- df_night[-which(df_night$PPFDin > 5),]
  
  ## u* correction ####
  # Jassal et al. 2009: 0.19 | Krishnan et al. 2009: 0.16 | Jassal et al. 2010: 0.19 
  df_night$NEE_cor <- df_night$NEE_measure
  df_night$NEE_cor[df_night$ustar < 0.19] <- NA
  summary(df_night$NEE_measure)
  summary(df_night$NEE_cor)
  #percent_gaps <- sum(is.na(df_night$NEE_cor)) / nrow(df_night) ## 78.53 %
  
  ## data frame with NNE != NA ####
  # and without precip, pressure, lw, sw, co2
  df_night_model <-  df_night[!is.na(df_night$NEE_cor),c(9:10, 12:15, 19:22, 24)]
  df_night_pred <-  df_night[is.na(df_night$NEE_cor),c(9:10, 12:15, 19:22, 24)]
  
  summary(df_night_model)
  summary(df_night_pred)
  
  M <- cor(df_night_model, use = "complete.obs")
  corrplot.mixed(M)
  
  rm(M, df_hel_2)
#### ------------------------------------- ####
  ## First Dense layer ANN - a Baseline with all predictors - Crossvalidation
#### ------------------------------------- ####
  
  ## CV function ####
  fun_cross <- function(df_train, df_pred, k = 5, num_epochs = 100, optimizer = "rmsprop", lr = 1e-4, batch = 30, layer = 2, units = c(64,32)){
    
    ## NA to 0
    df_train[is.na(df_train)] <- 0
    df_pred[is.na(df_pred)] <- 0
    
    ## CV loop ####
    indices <- sample(1:nrow(df_train))
    folds <- cut(indices, breaks = k, labels = FALSE)
    
    all_mae_histories <- matrix(NA, nrow = k, ncol = num_epochs) 
    all_pred_base <- NULL
    
    ## Callback - early stopping ####
    callback_list <- list(callback_early_stopping(patience = 6))
    
    ## optimizer ####
    if (optimizer == "rmsprop"){
      optim_ <- optimizer_rmsprop(lr = lr)  
    } else if (optimizer == "adam"){
      optim_ <- optimizer_adam(lr = lr)
    }
    
    for (i in 1:k) {
      cat("processing fold #", i, "\n")
      
      ## Model ####
      if (layer == 1){
        model <- keras_model_sequential() %>% 
          layer_dense(units = units[1], activation = "relu", 
                      input_shape = dim(df_train)[[2]]-1) %>% 
          layer_dense(units = 1) 
      } else if (layer == 2){
        model <- keras_model_sequential() %>% 
          layer_dense(units = units[1], activation = "relu", 
                      input_shape = dim(df_train)[[2]]-1) %>% 
          layer_dense(units = units[2], activation = "relu") %>% 
          layer_dense(units = 1)
      } else if (layer == 3){
        model <- keras_model_sequential() %>% 
          layer_dense(units = units[1], activation = "relu", 
                      input_shape = dim(df_train)[[2]]-1) %>% 
          layer_dense(units = units[2], activation = "relu") %>% 
          layer_dense(units = units[3], activation = "relu") %>% 
          layer_dense(units = 1) 
      } else {
        return("only three layer possible. pls set layer from 1-3")
      }
      
      model %>% compile(
        optimizer = optim_, 
        loss = "mse", 
        metrics = c("mae"))
      
      # Prepare the validation data: data from partition # k
      val_indices <- which(folds == i, arr.ind = TRUE) 
      val_data <- as.matrix(df_train[, 1:18][val_indices,])
      val_targets <- as.array(df_train[, 19][val_indices])
      
      # Prepare the training data: data from all other partitions
      partial_train_data <- as.matrix(df_train[, 1:18][-val_indices,])
      partial_train_targets <- as.array(df_train[, 19][-val_indices])
      
      # normalize all data
      mins_data <- apply(partial_train_data, 2, min, na.rm = T)
      maxs_data <- apply(partial_train_data, 2, max, na.rm = T)
      mins_targets <- min(partial_train_targets, na.rm = T)
      maxs_targets <- max(partial_train_targets, na.rm = T)
      
      partial_train_data <- scale(partial_train_data, center = mins_data, 
                                  scale = maxs_data - mins_data)
      val_data <- scale(val_data, center = mins_data, 
                        scale = maxs_data - mins_data)
      partial_train_targets <- scale(partial_train_targets, center = mins_targets, 
                                     scale = maxs_targets - mins_targets)
      val_targets <- scale(val_targets, center = mins_targets, 
                           scale = maxs_targets - mins_targets)
      pred_data <- scale(df_pred[, 1:18], center = mins_data, 
                         scale = maxs_data - mins_data)
      
      history <- model %>% fit(
        partial_train_data, partial_train_targets,
        validation_data = list(val_data, val_targets),
        epochs = num_epochs, batch_size = batch, verbose = 2,
        callbacks = callback_list)
      
      # Evaluate the model on the validation data
      mae_history <- history$metrics$val_mean_absolute_error
      all_mae_histories[i,1:length(mae_history)] <- mae_history
      
      # predict 
      pred_base <- model %>% predict(pred_data)
      pred_base_ <- pred_base * (maxs_targets - mins_targets) + mins_targets
      all_pred_base <- cbind(all_pred_base, pred_base_)
    }
    
    return(list(all_mae_histories, all_pred_base))
  }
  
  ## First random try ####
  cv_64_32.16 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = 5, 
                           num_epochs = 100, batch = 16, layer = 2, units = c(64,32)) 
  cv_64_32.16 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = 10, 
                           num_epochs = 100, batch = 16, layer = 2, units = c(64,32)) 
  
  summary(cv_64_32.16[[2]])
  
  ## Resuls & Plot ####
  results_base_mae <- data.frame(
    epoch = seq(1:ncol(cv_64_32.16[[1]])),
    validation_mae = apply(cv_64_32.16[[1]], 2, mean))
  
  results_base_mean <- data.frame(
    epoch = seq(1:dim(cv_64_32.16[[2]])[1]),
    pred_ = apply(cv_64_32.16[[2]], 1, mean))
  


