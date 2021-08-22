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
