#### ------------------------------------- ####
  ## Dropbox Link für Daten
#### ------------------------------------- ####
  
  ## Package Function ####
  check.packages <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  ## Usage ####
  packages <- c("httpuv", "rdrop2")
  check.packages(packages)
  
#### ------------------------------------- ####
  ## Daten einladen
#### ------------------------------------- ####
  
#### Daten einladen ####
  ## path 
  
  #setwd("E:/Uni/05_Masterarbeit/Auswertung") 
  mypath <- getwd()
  
#### Workspace speichern
  save.image(paste0(mypath, "/master/RData/save.RData"))

#### Verbindung zu dropbox aufbauen ####
  #token <- drop_auth()
  #saveRDS(token, file = "token.rds")
  token <- readRDS(paste0(mypath,"/master/token.rds"))
  
  ## Workspace runterladen ####
  drop_download('Master/R/save.RData', overwrite = T, dtoken = token)
  
  load("save.RData")
  ## upload WOrkspace ####
  drop_upload(paste0(mypath, "/master/RData/save.RData"), 'Master/R/', mode = "overwrite", dtoken = token)
  
  ## Script runterladen ####
  #drop_download('Master/R/Master_ANN.R', overwrite = T, dtoken = token)
  #file.edit('Master_ANN.R')
  #drop_upload('Master_ANN.R', 'Master/R/', mode = "overwrite", dtoken = token)
  
  
