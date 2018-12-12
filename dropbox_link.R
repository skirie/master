#### ------------------------------------- ####
  ## Dropbox Link für Daten
#### ------------------------------------- ####
  
#### Daten einladen ####
  ## path 
  mypath <- getwd()
  
#### Verbindung zu dropbox aufbauen ####
  token <- readRDS(paste0(mypath,"/token.rds"))
  
#### Workspace speichern
  save.image(paste0(mypath, "/RData/save.RData"))
  
  ## Workspace runterladen ####
  drop_download('Master/R/save.RData', paste0(mypath, "/RData/save.RData"), overwrite = T, dtoken = token)
  load(paste0(mypath, "/RData/save.RData"))
  
  ## upload WOrkspace ####
  drop_upload(paste0(mypath, "/RData/df_model.RData"), 'Master/R/', mode = "overwrite", dtoken = token)
  
  
  ls()
  