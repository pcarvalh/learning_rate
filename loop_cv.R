#run iAFM CV
rm(list=ls())
library(data.table)
source(file = "models_function.R")
source(file = "Cross-validation.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
    #function(inputFile,folds,repeats,inputFile,kcm,response,opportunity,individual)
      cv_iafm <- cv(modelToFit= "iAFM",inputFile = dataset_list$StudentStep[dataset],folds=10,repeats=5,kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("rmse.vals",dataset_list$Dataset[dataset],sep = "_"),cv_iafm$rmse.vals)
      
      assign(paste("rmse.data.perc",dataset_list$Dataset[dataset],sep = "_"),cv_iafm$rmse.data.perc)
      
      assign(paste("mean.rmse",dataset_list$Dataset[dataset],sep = "_"),cv_iafm$mean.rmse)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished iAFM CV for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "cv_iAFM.RData")

#put together all student params for iAFM
all_dataframes <- sapply(.GlobalEnv, is.numeric) 

all_rmse.vals <- all_dataframes[grepl("rmse.vals", names(all_dataframes))]
iAFM_all_rmse.vals <- do.call(rbind,mget(names(all_rmse.vals)[all_rmse.vals]))
write.csv(iAFM_all_rmse.vals,"iAFM_all_rmse.vals.csv",row.names = F)

all_rmse.data.perc <- all_dataframes[grepl("rmse.data.perc", names(all_dataframes))]
iAFM_rmse.data.perc<- do.call(rbind,mget(names(all_rmse.data.perc)[all_rmse.data.perc]))
write.csv(iAFM_rmse.data.perc,"iAFM_rmse.data.perc.csv",row.names = F)

all.mean.rmse <- all_dataframes[grepl("mean.rmse", names(all_dataframes))]
iAFM.mean.rmse <- do.call(rbind,mget(names(all.mean.rmse)[all.mean.rmse]))
write.csv(iAFM.mean.rmse,"iAFM.mean.rmse.csv",row.names = F)

#run AFM CV

rm(list=ls())
library(data.table)
source(file = "models_function.R")
source(file = "Cross-validation.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
    #function(inputFile,folds,repeats,inputFile,kcm,response,opportunity,individual)
      cv_afm <- cv(modelToFit= "AFM",inputFile = dataset_list$StudentStep[dataset],folds=10,repeats=5,kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("rmse.vals",dataset_list$Dataset[dataset],sep = "_"),cv_afm$rmse.vals)
      
      assign(paste("rmse.data.perc",dataset_list$Dataset[dataset],sep = "_"),cv_afm$rmse.data.perc)
      
      assign(paste("mean.rmse",dataset_list$Dataset[dataset],sep = "_"),cv_afm$mean.rmse)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished AFM CV for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "cv_AFM.RData")

#put together all student params for AFM
all_dataframes <- sapply(.GlobalEnv, is.numeric) 

all_rmse.vals <- all_dataframes[grepl("rmse.vals", names(all_dataframes))]
AFM_all_rmse.vals <- do.call(rbind,mget(names(all_rmse.vals)[all_rmse.vals]))
write.csv(AFM_all_rmse.vals,"AFM_all_rmse.vals.csv",row.names = F)

all_rmse.data.perc <- all_dataframes[grepl("rmse.data.perc", names(all_dataframes))]
AFM_rmse.data.perc<- do.call(rbind,mget(names(all_rmse.data.perc)[all_rmse.data.perc]))
write.csv(AFM_rmse.data.perc,"AFM_rmse.data.perc.csv",row.names = F)

all.mean.rmse <- all_dataframes[grepl("mean.rmse", names(all_dataframes))]
AFM.mean.rmse <- do.call(rbind,mget(names(all.mean.rmse)[all.mean.rmse]))
write.csv(AFM.mean.rmse,"AFM.mean.rmse.csv",row.names = F)

#run tAFM CV
rm(list=ls())
library(data.table)
source(file = "models_function.R")
source(file = "Cross-validation.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  
  tryCatch(
    {
    #function(inputFile,folds,repeats,inputFile,kcm,response,opportunity,individual)
      cv_tafm <- cv(modelToFit= "tAFM",inputFile = dataset_list$StudentStep[dataset],folds=10,repeats=5,kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
      
      #output the KC model name
      assign(paste("rmse.vals",dataset_list$Dataset[dataset],sep = "_"),cv_tafm$rmse.vals)
      
      assign(paste("rmse.data.perc",dataset_list$Dataset[dataset],sep = "_"),cv_tafm$rmse.data.perc)
      
      assign(paste("mean.rmse",dataset_list$Dataset[dataset],sep = "_"),cv_tafm$mean.rmse)
      
    },
    error = function(e){
      message("* Caught an error on dataset ", dataset_list$Dataset[dataset])
      print(e)
    },
    finally = {
      print(paste("finished tAFM CV for",dataset_list$Dataset[dataset]))
    }
  )
}

save(list=ls(),file = "cv_tAFM.RData")

#put together all student params for tAFM
all_dataframes <- sapply(.GlobalEnv, is.numeric) 

all_rmse.vals <- all_dataframes[grepl("rmse.vals", names(all_dataframes))]
tAFM_all_rmse.vals <- do.call(rbind,mget(names(all_rmse.vals)[all_rmse.vals]))
write.csv(tAFM_all_rmse.vals,"tAFM_all_rmse.vals.csv",row.names = F)

all_rmse.data.perc <- all_dataframes[grepl("rmse.data.perc", names(all_dataframes))]
tAFM_rmse.data.perc<- do.call(rbind,mget(names(all_rmse.data.perc)[all_rmse.data.perc]))
write.csv(tAFM_rmse.data.perc,"tAFM_rmse.data.perc.csv",row.names = F)

all.mean.rmse <- all_dataframes[grepl("mean.rmse", names(all_dataframes))]
tAFM.mean.rmse <- do.call(rbind,mget(names(all.mean.rmse)[all.mean.rmse]))
write.csv(tAFM.mean.rmse,"tAFM.mean.rmse.csv",row.names = F)
