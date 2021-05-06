#for full model only

rm(list=ls())
library(data.table)
source(file = "models_function.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  tryCatch(
    {
      m_itafm <- itAFM(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("itafm_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_itafm$stud.params)
      
      assign(paste("itafm_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_itafm$kc.params)
      
      assign(paste("itafm_parms",dataset_list$Dataset[dataset],sep = "_"),m_itafm$overall.params)
      
      assign(paste("itafm_steps_time",dataset_list$Dataset[dataset],sep = "_"),m_itafm$steps_time)
      
      assign(paste("itafm_df",dataset_list$Dataset[dataset],sep = "_"),m_itafm$df)
      
      assign(paste("itafm_model",dataset_list$Dataset[dataset],sep = "_"),m_itafm$model)
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished itAFM for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "modelFits_itAFM.RData")

#put together all student params for itAFM
all_dataframes <- sapply(.GlobalEnv, is.data.frame)
all_std.parms <- all_dataframes[grepl("std", names(all_dataframes))]

itAFM_std.params <- do.call(rbind,mget(names(all_std.parms)[all_std.parms]))
write.csv(itAFM_std.params,"itAFM_std.params.csv",row.names = F)

all_kc.params <- all_dataframes[grepl("kc", names(all_dataframes))]
itAFM_kc.params <- do.call(rbind,mget(names(all_kc.params)[all_kc.params]))
write.csv(itAFM_kc.params,"itAFM_kc.params.csv",row.names = F)

all.params <- all_dataframes[grepl("itafm_parms", names(all_dataframes))]
itAFM.params <- do.call(rbind,mget(names(all.params)[all.params]))
write.csv(itAFM.params,"itAFM.params.csv",row.names = F)

steps.time <- all_dataframes[grepl("steps", names(all_dataframes))]
itAFM.steps.time <- do.call(rbind,mget(names(steps.time)[steps.time]))
write.csv(itAFM.steps.time,"itAFM.steps_time.csv",row.names = F)