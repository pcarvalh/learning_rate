rm(list=ls())
library(data.table)
source(file = "models_function.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
      m_iafm_res <- iAFM_restrict(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("iafm_res_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_res$stud.params)
      
      assign(paste("iafm_res_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_res$kc.params)
      
      assign(paste("iafm_res_parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_res$overall.params)
      
      assign(paste("iafm_res_df",dataset_list$Dataset[dataset],sep = "_"),m_iafm_res$df)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished iAFM_res for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "modelFits_iAFM_res.RData")


#put together all student params for iAFM_res

all_dataframes <- sapply(.GlobalEnv, is.data.frame) 

all_std.parms <- all_dataframes[grepl("iafm_res_std", names(all_dataframes))]
iAFM_res_std.params <- do.call(rbind,mget(names(all_std.parms)[all_std.parms]))
write.csv(iAFM_res_std.params,"iAFM_res_std.params.csv",row.names = F)

all_kc.params <- all_dataframes[grepl("iafm_res_kc", names(all_dataframes))]
iAFM_res_kc.params <- do.call(rbind,mget(names(all_kc.params)[all_kc.params]))
write.csv(iAFM_res_kc.params,"iAFM_res_kc.params.csv",row.names = F)

all.params <- all_dataframes[grepl("iafm_res_parms", names(all_dataframes))]
iAFM_res.params <- do.call(rbind,mget(names(all.params)[all.params]))
write.csv(iAFM_res.params,"iAFM_res.params.csv",row.names = F)
