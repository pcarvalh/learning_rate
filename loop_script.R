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

#clean environment
rm(list=ls())
source(file = "models_function.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
      m_iafm <- iAFM(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("iafm_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm$stud.params)
      
      assign(paste("iafm_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm$kc.params)
      
      assign(paste("iafm_parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm$overall.params)
      
      assign(paste("iafm_df",dataset_list$Dataset[dataset],sep = "_"),m_iafm$df)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished iAFM for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "modelFits_iAFM.RData")


#put together all student params for iAFM
all_dataframes <- sapply(.GlobalEnv, is.data.frame) 
all_std.parms <- all_dataframes[grepl("std", names(all_dataframes))]

iAFM_std.params <- do.call(rbind,mget(names(all_std.parms)[all_std.parms]))
write.csv(iAFM_std.params,"iAFM_std.params.csv",row.names = F)

all_kc.params <- all_dataframes[grepl("kc", names(all_dataframes))]
iAFM_kc.params <- do.call(rbind,mget(names(all_kc.params)[all_kc.params]))
write.csv(iAFM_kc.params,"iAFM_kc.params.csv",row.names = F)

all.params <- all_dataframes[grepl("iafm_parms", names(all_dataframes))]
iAFM.params <- do.call(rbind,mget(names(all.params)[all.params]))
write.csv(iAFM.params,"iAFM.params.csv",row.names = F)

#clean environment
rm(list=ls())
source(file = "models_function.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
      m_iafm_full <- iAFM_full(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("iafm_full_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_full$stud.params)
      
      assign(paste("iafm_full_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_full$kc.params)
      
      assign(paste("iafm_full_parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_full$overall.params)
      
      assign(paste("iafm_full_df",dataset_list$Dataset[dataset],sep = "_"),m_iafm_full$df)
      
      assign(paste("iafm_full_model",dataset_list$Dataset[dataset],sep = "_"),m_iafm_full$model)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished iAFM_full for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "modelFits_iAFM_full.RData")

#put together all student params for iAFM
all_dataframes <- sapply(.GlobalEnv, is.data.frame) 
all_std.parms <- all_dataframes[grepl("full_std", names(all_dataframes))]

iAFM_full_std.params <- do.call(rbind,mget(names(all_std.parms)[all_std.parms]))
write.csv(iAFM_full_std.params,"iAFM_full_std.params.csv",row.names = F)

all_kc.params <- all_dataframes[grepl("full_kc", names(all_dataframes))]
iAFM_full_kc.params <- do.call(rbind,mget(names(all_kc.params)[all_kc.params]))
write.csv(iAFM_full_kc.params,"iAFM_full_kc.params.csv",row.names = F)

all.params <- all_dataframes[grepl("iafm_full_parms", names(all_dataframes))]
iAFM_full.params <- do.call(rbind,mget(names(all.params)[all.params]))
write.csv(iAFM_full.params,"iAFM_full.params.csv",row.names = F)

#clean environment
rm(list=ls())
source(file = "models_function.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
      m_iafm <- AFM(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("afm_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm$stud.params)
      
      assign(paste("afm_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm$kc.params)
      
      assign(paste("afm_parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm$overall.params)
      
      assign(paste("afm_df",dataset_list$Dataset[dataset],sep = "_"),m_iafm$df)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished AFM for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "modelFits_AFM.RData")


#put together all student params for AFM
all_dataframes <- sapply(.GlobalEnv, is.data.frame) 
all_std.parms <- all_dataframes[grepl("std", names(all_dataframes))]

iAFM_std.params <- do.call(rbind,mget(names(all_std.parms)[all_std.parms]))
write.csv(iAFM_std.params,"AFM_std.params.csv",row.names = F)

all_kc.params <- all_dataframes[grepl("kc", names(all_dataframes))]
iAFM_kc.params <- do.call(rbind,mget(names(all_kc.params)[all_kc.params]))
write.csv(iAFM_kc.params,"AFM_kc.params.csv",row.names = F)

all.params <- all_dataframes[grepl("afm_parms", names(all_dataframes))]
iAFM.params <- do.call(rbind,mget(names(all.params)[all.params]))
write.csv(iAFM.params,"AFM.params.csv",row.names = F)