rm(list=ls())
library(data.table)
source(file = "models_function.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
      m_tafm <- tAFM(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("tafm_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_tafm$stud.params)
      
      assign(paste("tafm_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_tafm$kc.params)
      
      assign(paste("tafm_parms",dataset_list$Dataset[dataset],sep = "_"),m_tafm$overall.params)
      
      assign(paste("tafm_steps_time",dataset_list$Dataset[dataset],sep = "_"),m_tafm$steps_time)
      
      assign(paste("tafm_df",dataset_list$Dataset[dataset],sep = "_"),m_tafm$df)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished tAFM for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "modelFits_tAFM.RData")

#put together all student params for tAFM
all_dataframes <- sapply(.GlobalEnv, is.data.frame) 
all_std.parms <- all_dataframes[grepl("std", names(all_dataframes))]

tAFM_std.params <- do.call(rbind,mget(names(all_std.parms)[all_std.parms]))
write.csv(tAFM_std.params,"tAFM_std.params.csv",row.names = F)

all_kc.params <- all_dataframes[grepl("kc", names(all_dataframes))]
tAFM_kc.params <- do.call(rbind,mget(names(all_kc.params)[all_kc.params]))
write.csv(tAFM_kc.params,"tAFM_kc.params.csv",row.names = F)

all.params <- all_dataframes[grepl("afm_parms", names(all_dataframes))]
tAFM.params <- do.call(rbind,mget(names(all.params)[all.params]))
write.csv(tAFM.params,"tAFM.params.csv",row.names = F)

steps.time <- all_dataframes[grepl("steps", names(all_dataframes))]
tAFM.steps.time <- do.call(rbind,mget(names(steps.time)[steps.time]))
write.csv(tAFM.steps.time,"tAFM.steps_time.csv",row.names = F)