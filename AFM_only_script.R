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

AFM_std.params <- do.call(rbind,mget(names(all_std.parms)[all_std.parms]))
write.csv(iAFM_std.params,"AFM_std.params.csv",row.names = F)

all_kc.params <- all_dataframes[grepl("kc", names(all_dataframes))]
iAFM_kc.params <- do.call(rbind,mget(names(all_kc.params)[all_kc.params]))
write.csv(iAFM_kc.params,"AFM_kc.params.csv",row.names = F)

all.params <- all_dataframes[grepl("afm_parms", names(all_dataframes))]
AFM.params <- do.call(rbind,mget(names(all.params)[all.params]))
write.csv(iAFM.params,"AFM.params.csv",row.names = F)
