#### ------------------------------------- ####
## First recurrent ANN - a Baseline with all predictors - Crossvalidation
#### ------------------------------------- ####

## fun RNN  ####
fun_cross_rec <- function(df_train, k = 5, num_epochs = 100, optimizer = "rmsprop", lr = 1e-4, batch = 60, layer = 2, units = c(64,32)){
  ## NA to 0
  df_train[is.na(df_train)] <- 0
  
  ## Callback - early stopping ####
  callback_list <- list(callback_early_stopping(patience = 6))
  
  ## optimizer ####
  if (optimizer == "rmsprop"){
    optim_ <- optimizer_rmsprop(lr = lr)  
  } else if (optimizer == "adam"){
    optim_ <- optimizer_adam(lr = lr)
  }
  
  ## CV loop ####
  set.seed(5)
  indices <- sample(1:nrow(df_train))
  folds <- cut(indices, breaks = k, labels = FALSE)
  
  all_mae_histories <- NULL
  all_rmse <- NULL
  all_r2 <- NULL
  
  for (i in 1:k) {
    cat("processing fold #", i, "\n")
    
    ## Model ####
    if (layer == 1){
      model <- keras_model_sequential() %>% 
        layer_gru(units = units[1], input_shape = c(1, ncol(df_train)-1)) %>% 
        layer_dense(units = 1) 
    } else if (layer == 2){
      model <- keras_model_sequential() %>% 
        layer_gru(units = units[1], input_shape = c(1, ncol(df_train)-1),
                  return_sequences = T) %>% 
        layer_gru(units = units[2]) %>% 
        layer_dense(units = 1)
    } else if (layer == 3){
      model <- keras_model_sequential() %>% 
        layer_gru(units = units[1], input_shape = c(1, ncol(df_train)-1),
                  return_sequences = T) %>% 
        layer_gru(units = units[2], return_sequences = T) %>% 
        layer_gru(units = units[3]) %>% 
        layer_dense(units = 1) 
    } else {
      return("only three layer possible. pls set layer from 1-3")
    }
    
    model %>% compile(
      optimizer = optim_, 
      loss = "mse", 
      metrics = c("mae"))
    
    # Prepare the validation data: data from partition # k | Also needed for model build (input shape)
    val_indices <- which(folds == i, arr.ind = TRUE)
    set.seed(i*5)
    vt_indices <- sample(1:length(val_indices))
    vt_folds <- cut(vt_indices, breaks = 2, labels = FALSE)
    val_test_indices_2 <- which(vt_folds == 1, arr.ind = TRUE)
    
    ## normalize all data 
    df_part_train_scale <- df_train[-val_indices,]

    mins_data <- apply(df_part_train_scale, 2, min, na.rm = T)
    maxs_data <- apply(df_part_train_scale, 2, max, na.rm = T)

    df_train_ <- scale(df_train, center = mins_data, 
                       scale = maxs_data - mins_data)
    
    # Prepare the training and validation data: data from all other partitions
    partial_train_data <- array(df_train_[-val_indices, 1:ncol(df_train_)-1], 
                                dim = c(nrow(df_train_[-val_indices,]), 
                                        1, 
                                        ncol(df_train_)-1))
    
    partial_train_targets <- array(df_train_[-val_indices, ncol(df_train_)], 
                                   dim = nrow(df_train_[-val_indices,]))
    
    val_data <- array(df_train_[val_indices[val_test_indices_2], 1:ncol(df_train_)-1], 
                      dim = c(nrow(df_train_[val_indices[val_test_indices_2],]), 
                              1, 
                              ncol(df_train_)-1))
    
    val_targets <- array(df_train_[val_indices[val_test_indices_2], ncol(df_train_)], 
                         dim = nrow(df_train_[val_indices[val_test_indices_2],]))
    
    test_data <- array(df_train_[val_indices[-val_test_indices_2], 1:ncol(df_train_)-1], 
                           dim = c(nrow(df_train_[val_indices[-val_test_indices_2],]), 
                                   1, 
                                   ncol(df_train_)-1))
    
    test_targets <- as.numeric(df_train_[val_indices[-val_test_indices_2], ncol(df_train_)])

    history <- model %>% fit(
      partial_train_data, partial_train_targets,
      validation_data = list(val_data, val_targets),
      epochs = num_epochs, batch_size = batch, verbose = 0,
      callbacks = callback_list)
    
    # Evaluate the model on the validation data
    mae_history <- history$metrics$val_mean_absolute_error
    all_mae_histories <- rbind(all_mae_histories, mae_history)
    
    pred_test <- model %>% predict(test_data)
    rmse_ <- rmse(test_targets, pred_test)
    all_rmse <- rbind(all_rmse, rmse_)
    r2 <- cor(test_targets, pred_test) ^ 2
    all_r2 <- rbind(all_r2, r2)
  }
  return(list(all_mae_histories, all_rmse, all_r2))
}

df_rec_base <- fun_cross_rec(df_train = df_night_model, num_epochs = 200, optimizer = "adam", layer = 3, batch = 30, units = c(105,52,26))

