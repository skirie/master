## Target function

fun_cross_2 <- function(batchsize = 64, k = 20, epochs = 200, lr = 1e-4){
  ## Params ####
  params <- fun_params(batchsize = batchsize, k = k, epochs = epochs, lr = lr)
  models <- 
  df_results <- data.frame()
  
}

## Funktion Parameter ####
fun_params <- function(batchsize = 64, k = 20, epochs = 200, lr = 1e-4){
  params <- list()
  params[["batchsize"]] <- batchsize
  params[["k"]] <- k
  params[["epochs"]] <- epochs
  params[["optimizer"]] <- c("rmsprop", "adam")
  params[["lr"]] <- lr
}

## Function model build ####
fun_build_model <- function(layer, optimizer, units, lr){
  ## optimizer ####
  if (optimizer == "rmsprop"){
    optim_ <- optimizer_rmsprop(lr = lr)  
  } else if (optimizer == "adam"){
    optim_ <- optimizer_adam(lr = lr)
  }
  
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
}

## Funktion Modelldurchlauf ####
fun_model_run <- function(Nmin = 8, Nmax = 100, by_ = 12, layer = 2, params = params){
  # tagret data.frame
  rsq <- NULL
  rmse_ <- NULL
  
  # natrix with Nodes
  N <- seq(Nmin, Nmax, by_)
  N_ <- matrix(data = c(N, N*0.5, N*0.25), nrow = length(N), ncol = 3)
  
  # key 
  if (layer == 1){
    key <- c(paste0("N_", N_[,1]))
  } else if (layer == 2){
    key <- c(paste0("N_", N_[,1]), paste0("N_", N_[,1], "_", N_[,2]))
  } else if (layer == 3){
    key <- c(paste0("N_", N_[,1]), paste0("N_", N_[,1], "_", N_[,2]), paste0("N_", N_[,1], "_", N_[,2], "_", N_[,3])) 
  }
  
  for (l in 1:layer){
    for (i in 1:nrow(N_)){
      cv_ <- fun_cross(df_train = df_night_model, params = params, layer = layer, units = N_[i,1:l])
      rmse <- mean(cv_[[1]])
      r2_ <- mean(cv_[[2]])
    }
  }
}

## CV function ####
fun_model_compute <- function(df_train, params, layer = 2, units = c(64,32)){
  
  k <- params[["k"]]
  num_epochs <- params [["epochs"]]
  optimizer <- params[["optimizer"]]
  lr <- params[["lr"]]
  
  ## NA to 0
  df_train[is.na(df_train)] <- 0
  
  #all_mae_histories <- matrix(NA, nrow = k, ncol = num_epochs) 
  all_rmse <- NULL
  all_r2 <- NULL
  
  ## Callback - early stopping ####
  callback_list <- list(callback_early_stopping(patience = 6))
  
  ## Model
  model <- fun_build_model(layer = layer, optimizer = optimizer, units = units, lr = lr)
  
  ## Crossvalidation 2x k-fold crossvalidation
  for (j in 1:2){
    set.seed(j*5)
    indices <- sample(1:nrow(df_train))
    folds <- cut(indices, breaks = k, labels = FALSE)
    
    for (i in 1:k) {
      cat("processing fold #", i, "\n")
      
      # Prepare the validation and test data: data from partition # k
      val_test_indices <- which(folds == i, arr.ind = TRUE) 
      val_test_data <- as.matrix(df_train[, 1:18][val_test_indices,])
      val_test_targets <- as.array(df_train[, 19][val_test_indices])
      
      # split into validation and test data
      set.seed(i*5)
      vt_indices <- sample(1:nrow(val_test_data))
      vt_folds <- cut(vt_indices, breaks = 2, labels = FALSE)
      
      val_test_indices_2 <- which(vt_folds == 1, arr.ind = TRUE) 
      val_data <- as.matrix(val_test_data[val_test_indices_2,])
      val_targets <- as.array(val_test_targets[val_test_indices_2])
      test_data <- as.matrix(val_test_data[-val_test_indices_2,])
      test_targets <- as.array(val_test_targets[-val_test_indices_2])
      
      # Prepare the training data: data from all other partitions
      partial_train_data <- as.matrix(df_train[, 1:18][-val_test_indices,])
      partial_train_targets <- as.array(df_train[, 19][-val_test_indices])
      
      # normalize all data
      mins_data <- apply(partial_train_data, 2, min, na.rm = T)
      maxs_data <- apply(partial_train_data, 2, max, na.rm = T)
      mins_targets <- min(partial_train_targets, na.rm = T)
      maxs_targets <- max(partial_train_targets, na.rm = T)
      
      # predict = x
      partial_train_data <- scale(partial_train_data, center = mins_data, 
                                  scale = maxs_data - mins_data)
      val_data <- scale(val_data, center = mins_data, 
                        scale = maxs_data - mins_data)
      test_data <- scale(test_data, center = mins_data, 
                         scale = maxs_data - mins_data)
      
      # target = y
      partial_train_targets <- scale(partial_train_targets, center = mins_targets, 
                                     scale = maxs_targets - mins_targets)
      val_targets <- scale(val_targets, center = mins_targets, 
                           scale = maxs_targets - mins_targets)
      test_targets <- scale(test_targets, center = mins_targets, 
                           scale = maxs_targets - mins_targets)
      
      # fit model
      history <- model %>% fit(
        partial_train_data, partial_train_targets,
        validation_data = list(val_data, val_targets),
        epochs = num_epochs, batch_size = batch, verbose = 2,
        callbacks = callback_list)
      
      # Evaluate the model on the validation data
      #mae_history <- history$metrics$val_mean_absolute_error
      #all_mae_histories[i,1:length(mae_history)] <- mae_history
      
      # predict and scale back
      pred_test <- model %>% predict(test_data)
      rmse_ <- rmse(test_targets, pred_test)
      all_rmse <- cbind(all_rmse, rmse_)
      r2 <- cor(test_targets, pred_test) ^ 2
      all_r2 <- cbind(all_r2, r2)
    }
  }
  return(list(all_rmse, all_r2))
}

#### ####
