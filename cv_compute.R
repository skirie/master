  
  ## settings ####
  batch <- 32
  k <- 5
  num_epochs <- 200
  
## Models with one layer - Batchsize 32 ####
  cv_8.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                          num_epochs = num_epochs, batch = batch, layer = 1, units = c(8))
  
  cv_16.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                          num_epochs = num_epochs, batch = batch, layer = 1, units = c(16))
  
  cv_32.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                          num_epochs = num_epochs, batch = batch, layer = 1, units = c(32))
  
  cv_48.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                          num_epochs = num_epochs, batch = batch, layer = 1, units = c(48))
  
  cv_64.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                          num_epochs = num_epochs, batch = batch, layer = 1, units = c(64))
  
  cv_80.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                          num_epochs = num_epochs, batch = batch, layer = 1, units = c(80))
  
  cv_96.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                          num_epochs = num_epochs, batch = batch, layer = 1, units = c(96))
  
## Models with two layers - batchsize 32 ####
  cv_8_4.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                         num_epochs = num_epochs, batch = batch, layer = 2, units = c(8,4))
  
  cv_16_8.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                         num_epochs = num_epochs, batch = batch, layer = 2, units = c(16,8))
  
  cv_32_16.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                         num_epochs = num_epochs, batch = batch, layer = 2, units = c(32,16))
  
  cv_48_24.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                         num_epochs = num_epochs, batch = batch, layer = 2, units = c(48,24))
  
  cv_64_32.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                         num_epochs = num_epochs, batch = batch, layer = 2, units = c(64,32))
  
  cv_80_40.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                         num_epochs = num_epochs, batch = batch, layer = 2, units = c(80,40))
  
  cv_96_48.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                         num_epochs = num_epochs, batch = batch, layer = 2, units = c(96,48))
  
  cv_16_16.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(16,16))
  
  cv_32_32.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(32,32))
  
  cv_64_64.32 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(64,64))
  
## Models with one layer - Batchsize 64 ####
  cv_8.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                       num_epochs = num_epochs, batch = batch, layer = 1, units = c(8))
  
  cv_16.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                        num_epochs = num_epochs, batch = batch, layer = 1, units = c(16))
  
  cv_32.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                        num_epochs = num_epochs, batch = batch, layer = 1, units = c(32))
  
  cv_48.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                        num_epochs = num_epochs, batch = batch, layer = 1, units = c(48))
  
  cv_64.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                        num_epochs = num_epochs, batch = batch, layer = 1, units = c(64))
  
  cv_80.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                        num_epochs = num_epochs, batch = batch, layer = 1, units = c(80))
  
  cv_96.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                        num_epochs = num_epochs, batch = batch, layer = 1, units = c(96))
  
## Models with two layers - batchsize 64 ####
  cv_8_4.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                         num_epochs = num_epochs, batch = batch, layer = 2, units = c(8,4))
  
  cv_16_8.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                          num_epochs = num_epochs, batch = batch, layer = 2, units = c(16,8))
  
  cv_32_16.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(32,16))
  
  cv_48_24.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(48,24))
  
  cv_64_32.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(64,32))
  
  cv_80_40.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(80,40))
  
  cv_96_48.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(96,48))
  
  cv_16_16.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(16,16))
  
  cv_32_32.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(32,32))
  
  cv_64_64.64 <- fun_cross(df_train = df_night_model, df_pred = df_night_pred, k = k, optimizer = "rmsprop",
                           num_epochs = num_epochs, batch = batch, layer = 2, units = c(64,64))
  
#### Data frames ####
  results_base_mean <- data.frame(
    epoch = seq(1:dim(cv_16.16[[2]])[1]),
    cv_16.16 = apply(cv_16.16[[2]], 1, mean),
    cv_32.16 = apply(cv_32.16[[2]], 1, mean),
    cv_64.32 = apply(cv_64.32[[2]], 1, mean),
    cv_64.16 = apply(cv_64.16[[2]], 1, mean),
    cv_32_16.8 = apply(cv_32_16.8[[2]], 1, mean),
    cv_32_16.16 = apply(cv_32_16.16[[2]], 1, mean)) 
  
  results_base_mae <- data.frame(
    epoch = seq(1:dim(cv_16.16[[1]])[2]),
    cv_16.16 = apply(cv_16.16[[1]], 2, mean),
    cv_32.16 = apply(cv_32.16[[1]], 2, mean),
    cv_64.32 = apply(cv_64.32[[1]], 2, mean),
    cv_64.16 = apply(cv_64.16[[1]], 2, mean),
    cv_32_16.8 = apply(cv_32_16.8[[1]], 2, mean),
    cv_32_16.16 = apply(cv_32_16.16[[1]], 2, mean)) 
  
  save(cv_64_32.8, cv_64_64.16, cv_64_64.32, cv_64_32_16.16, cv_64_32_16.32, cv_16.16, cv_32.16, 
       file = paste0(mypath, "/RData/cv_1.RData"))
  save(results_base_mean, results_base_mae, file = paste0(mypath, "/RData/cv_df_1.RData"))
  
  summary(results_base_mean)
  summary(results_base_mae)
  ggplot(results_base_mae, aes(x = epoch, y = cv_64_64.32)) + geom_smooth()  
  ggplot(results_base_mean, aes(x = epoch, y = cv_64_32.8)) + geom_line()  
  
