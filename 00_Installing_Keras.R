#### ------------------- ####
#### Installing Keras ####
#### ------------------- ####

  # Prior to this installations, Anaconda shoud be installed.

#### Keras ####
  ## download and load package
  install.packages("keras")
  library(keras)
  
  ## install keras
  install_keras()
  
#### Additional packages required ####
  ## function for installing them
  CheckPackages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  packages <- c("keras", "ggplot2", "Metrics", "httpuv", "rdrop2", "mlrMBO", "corrplot", "rgenoud", "betareg", "MASS")
  CheckPackages(packages) 
