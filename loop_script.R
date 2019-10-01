rm(list=ls())
library(data.table)
source(file = "models_function.R")
dataset_list <- fread("datasets.csv")

for(dataset in 1:nrow(dataset_list)){
  m_itafm <- itAFM(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
  
  #output the KC model name
  assign(paste("itafm_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_itafm$stud.params)
  
  assign(paste("itafm_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_itafm$kc.params)
  
  assign(paste("itafm_parms",dataset_list$Dataset[dataset],sep = "_"),m_itafm$overall.params)
  
  assign(paste("itafm_steps_time",dataset_list$Dataset[dataset],sep = "_"),m_itafm$steps_time)
  
  assign(paste("itafm_df",dataset_list$Dataset[dataset],sep = "_"),m_itafm$df)
  
  print(paste("finished itAFM for",dataset_list$Dataset[dataset]))
  
  m_iafm <- iAFM(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
  
  #output the KC model name
  assign(paste("iafm_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm$stud.params)
  
  assign(paste("iafm_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm$kc.params)
  
  assign(paste("iafm_parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm$overall.params)
  
  assign(paste("iafm_df",dataset_list$Dataset[dataset],sep = "_"),m_iafm$df)
  
  print(paste("finished iAFM for",dataset_list$Dataset[dataset]))
  
  m_iafm_full <- iAFM_full(dataset = dataset_list$StudentStep[dataset],kcm = dataset_list$KC[dataset],response = "First Attempt",opportunity = dataset_list$Opportunity[dataset],individual = "Anon Student Id")
  
  #output the KC model name
  assign(paste("iafm_full_std.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_full$stud.params)
  
  assign(paste("iafm_full_kc.parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_full$kc.params)
  
  assign(paste("iafm_full_parms",dataset_list$Dataset[dataset],sep = "_"),m_iafm_full$overall.params)
  
  assign(paste("iafm_full_df",dataset_list$Dataset[dataset],sep = "_"),m_iafm_full$df)
  
  print(paste("finished iAFM_full for",dataset_list$Dataset[dataset]))
  
}


save(list=ls(),file = "modelFits.RData")
