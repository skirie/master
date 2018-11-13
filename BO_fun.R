  fun_bo <- function(df_train, params, type = "full"){
    ## times cv repeated
    if (type == "pred"){
      times_cv <- 2
    } else {
      times_cv <- params[["times_cv"]]
    }
   
    ## params 
    k <- params[["k"]]
    num_epochs <- params[["epochs"]]
    optimizer <- params[["optimizer"]]
    lr <- params[["lr"]]
    layer <- params[["layer"]]
    layer_balance <- params[["layer_balance"]]
   
    ## bounds
    bounds <- list(layer = c(1L,3L), units = c(10L, 110L), batch = c(50L, 150L))
   
    ## set NA's to zero
    df_train[is.na(df_train)] <- 0
   
    ## empty vector
    all_rmse <- NULL
  
    nn_fit_bayes <- function(layer, units, batch) {
      units <- as.integer(rep(units, layer))
      if (layer > 1){
        units[2] <- as.integer(units[2]*layer_balance)
        if (layer == 3){
          units[3] <- as.integer(units[2]*layer_balance)
        }
      } 
      
      for (j in 1:times_cv){
        set.seed(j*5)
        indices <- sample(1:nrow(df_train))
        folds <- cut(indices, breaks = k, labels = FALSE)
        
        for (i in 1:k) {
          cat("processing fold #", paste0(j, ".", i), "\n")
          
          model <- fun_build_model(df_train = df_train, layer = layer, optimizer = optimizer, units = units, lr = lr)
          
          # Prepare the train, validation and test data: data from partition # k
          val_test_indices <- which(folds == i, arr.ind = TRUE)
          
          ## different datatype (array, matrix) for varying number of predictors 
          # -> indices are different (array <-> matrix)
          if (ncol(df_train) == 2){
            val_test_data <- as.array(df_train[val_test_indices, -ncol(df_train)])
            partial_train_data <- as.array(df_train[-val_test_indices, -ncol(df_train)])
            
            # min and max for mormalization
            mins_data <- min(partial_train_data, na.rm = T)
            maxs_data <- max(partial_train_data, na.rm = T)
          } else {
            val_test_data <- as.matrix(df_train[val_test_indices, -ncol(df_train)])
            partial_train_data <- as.matrix(df_train[-val_test_indices, -ncol(df_train)])
           
            # min and max for mormalization
            mins_data <- apply(partial_train_data, 2, min, na.rm = T)
            maxs_data <- apply(partial_train_data, 2, max, na.rm = T)
          }
          
          ## same for target data
          val_test_targets <- as.array(df_train[val_test_indices, ncol(df_train)])
          partial_train_targets <- as.array(df_train[-val_test_indices, ncol(df_train)])
          
          # normalize all data
          mins_targets <- min(partial_train_targets, na.rm = T)
          maxs_targets <- max(partial_train_targets, na.rm = T)
         
          # split combined val_test-data into separate validation and test data
          set.seed(i*5)
          vt_indices <- sample(1:nrow(val_test_data))
          vt_folds <- cut(vt_indices, breaks = 2, labels = FALSE)
          
          val_test_indices_2 <- which(vt_folds == 1, arr.ind = TRUE)
          
          # again split and different indices  (array <-> matrix)
          if (ncol(df_train) == 2){
            val_data <- val_test_data[val_test_indices_2]
            test_data <- val_test_data[-val_test_indices_2]
          } else {
            val_data <- val_test_data[val_test_indices_2,]
            test_data <- val_test_data[-val_test_indices_2,]
          }
          
          val_targets <- val_test_targets[val_test_indices_2]
          test_targets <- val_test_targets[-val_test_indices_2]
          
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
          model %>% fit(
            partial_train_data, partial_train_targets,
            validation_data = list(val_data, val_targets),
            epochs = num_epochs, batch_size = batch, verbose = 2,
            callbacks = list(callback_early_stopping(patience = 8)))
          
          # predict and scale back
          pred_test <- model %>% predict(test_data)
          rmse_ <- sqrt(mse(test_targets, pred_test))
          all_rmse <- rbind(all_rmse, rmse_)
          #r2 <- cor(test_targets, pred_test) ^ 2
          #all_r2 <- rbind(all_r2, r2)
        }
      }
      list(Score = -mean(all_rmse, na.rm = T), Pred = 0)
    }
    set.seed(8606)
    bo_search <- BayesianOptimization(nn_fit_bayes,
                                      bounds = bounds,
                                      init_points = 9,
                                      n_iter = 16,
                                      acq = "ei",
                                      eps = 0.01)
   
    return(bo_search)
  }
  
  params <- fun_params(layer = 3L, epochs = 150)
  bo_first <- fun_bo(df_train = df_night_model, params = params, type = "pred")
  