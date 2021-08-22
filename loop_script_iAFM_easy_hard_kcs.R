rm(list=ls())
library(data.table)
source(file = "models_function.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
      m_iafm_easy <- iAFM_easier(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("iafm_easy_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_easy$stud.params)
      
      assign(paste("iafm_easy_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_easy$kc.params)
      
      assign(paste("iafm_easy_parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_easy$overall.params)
      
      assign(paste("iafm_easy_df",dataset_list$Dataset[dataset],sep = "_"),m_iafm_easy$df)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished iAFM_easier for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "modelFits_iAFM_easier.RData")

#put together all student params for iAFM_res

all_dataframes <- sapply(.GlobalEnv, is.data.frame) 

all_std.parms <- all_dataframes[grepl("iafm_easy_std", names(all_dataframes))]
iAFM_easy_std.params <- do.call(rbind,mget(names(all_std.parms)[all_std.parms]))
write.csv(iAFM_easy_std.params,"iAFM_easy_std.params.csv",row.names = F)

all_kc.params <- all_dataframes[grepl("iafm_easy_kc", names(all_dataframes))]
iAFM_easy_kc.params <- do.call(rbind,mget(names(all_kc.params)[all_kc.params]))
write.csv(iAFM_easy_kc.params,"iAFM_easy_kc.params.csv",row.names = F)

all.params <- all_dataframes[grepl("iafm_easy_parms", names(all_dataframes))]
iAFM_easy.params <- do.call(rbind,mget(names(all.params)[all.params]))
write.csv(iAFM_easy.params,"iAFM_easy.params.csv",row.names = F)

rm(list=ls())
library(data.table)
source(file = "models_function.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
      m_iafm_hard <- iAFM_harder(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("iafm_hard_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_hard$stud.params)
      
      assign(paste("iafm_hard_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_hard$kc.params)
      
      assign(paste("iafm_hard_parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_hard$overall.params)
      
      assign(paste("iafm_hard_df",dataset_list$Dataset[dataset],sep = "_"),m_iafm_hard$df)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished iAFM_harder for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "modelFits_iAFM_harder.RData")

#put together all student params for iAFM_res

all_dataframes <- sapply(.GlobalEnv, is.data.frame) 

all_std.parms <- all_dataframes[grepl("iafm_hard_std", names(all_dataframes))]
iAFM_hard_std.params <- do.call(rbind,mget(names(all_std.parms)[all_std.parms]))
write.csv(iAFM_hard_std.params,"iAFM_hard_std.params.csv",row.names = F)

all_kc.params <- all_dataframes[grepl("iafm_hard_kc", names(all_dataframes))]
iAFM_hard_kc.params <- do.call(rbind,mget(names(all_kc.params)[all_kc.params]))
write.csv(iAFM_hard_kc.params,"iAFM_hard_kc.params.csv",row.names = F)

all.params <- all_dataframes[grepl("iafm_hard_parms", names(all_dataframes))]
iAFM_hard.params <- do.call(rbind,mget(names(all.params)[all.params]))
write.csv(iAFM_hard.params,"iAFM_hard.params.csv",row.names = F)
