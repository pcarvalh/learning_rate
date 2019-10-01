#functions to run iAFM and itAFM and iAFM_full for datasets and export a) slope parameters, b) intercept parameters.

library(data.table)
library(lme4)
library(optimx)

# dataset <- "ds104_student_step_All_Data_218_2016_0406_071258"
# kcm <- "KC (Default2)"
# response <- "First Attempt" 
# opportunity <- "Opportunity (Default2)"
# individual <- "Anon Student Id"
# 
# dataset = "ds447_student_step"
# kcm = "KC (Article_Rule)"
# response = "First Attempt"
# opportunity = "Opportunity (Article_Rule)"
# individual = "Anon Student Id"
# 
# dataset = "ds406_student_step_ERQ_Effective_Computing_1367_2017_0319_025217_multiskill_converted-2"
# kcm = "KC (U3-Effective)"
# response = "First Attempt"
# opportunity = "Opportunity (U3-Effective)"
# individual = "Anon Student Id"

# dataset = "ds1899_student_step_All_Data_3646_2017_0301_011213"
# kcm = "KC (Main-KC7-split_renamed-PVfixed-models)"
# response = "First Attempt"
# opportunity = "Opportunity (Main-KC7-split_renamed-PVfixed-models)"
# individual = "Anon Student Id"

# dataset = "ds1935_student_step_All_Data_3680_2017_0228_022507"
# kcm = "KC (LFASearchBICModel1-PVfixed-models uploaded)"
# response = "First Attempt"
# opportunity = "Opportunity (LFASearchBICModel1-PVfixed-models uploaded)"
# individual = "Anon Student Id"

# dataset = dataset_list$StudentStep[5]
# kcm = dataset_list$KC[5]
# response = "First Attempt"
# opportunity = dataset_list$Opportunity[5]
# individual = "Anon Student Id"


iAFM <- function(dataset, kcm,response,opportunity,individual){
  df = suppressWarnings(fread(file=paste(getwd(),"/data/",dataset,".txt",sep=""),verbose = F)) #the file to import
  names(df) <- make.names(names(df)) #add the periods instead of spaces
  names(df)[which( colnames(df)==make.names(eval(kcm)) )] <- "KC" #replace the KC model name with "KC"
  names(df)[which( colnames(df)==make.names(eval(response)) )] <- "response" #replace the first attempt response name with "response"
  names(df)[which( colnames(df)==make.names(eval(opportunity)) )] <- "opportunity" #replace the opportunity name with "opportunity"
  names(df)[which( colnames(df)==make.names(eval(individual)) )] <- "individual" #replace the individualizing factor name with "individual"
  success <- ifelse(df$response=="correct",1,0) #recode response as 0 (incorrect) or 1 (correct)
  df$success <- success
  df$errorRate <- 1-success #add a success column
  df$opportunity <- df$opportunity-1
  
  rm(success)
  
  #check number of opportunities per KC
  #df <- as.data.table(df)
  #checks <- df[,.(number_opp = length(individual),max = max(opportunity)),by=.(individual,KC)]
  
  iafm.model <- suppressWarnings(glmer(success ~ (1|individual) + opportunity + (opportunity|individual) + (opportunity|KC) - 1, data=df, family=binomial(),control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))))
  
  #iafm.model2 <- suppressWarnings(glmer(success ~ opportunity + (opportunity|individual) + (opportunity|KC) - 1, data=df, family=binomial(),control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))))
  
  stud.params <- data.frame(cbind(row.names(ranef(iafm.model)$individual), ranef(iafm.model)$individual[,1], ranef(iafm.model)$individual[,3]) )
  stud.params <- cbind(Dataset = strsplit(dataset,split = "_")[[1]][1],Type="Student", stud.params)
  colnames(stud.params) <- c("Dataset","Type", "Student", "Intercept", "Slope")
  
  kc.params <- data.frame(cbind(row.names(ranef(iafm.model)$KC), ranef(iafm.model)$KC[,1], ranef(iafm.model)$KC[,2]) )
  kc.params <- cbind(Dataset = strsplit(dataset,split = "_")[[1]][1],Type="KC", kc.params)
  colnames(kc.params) <- c("Dataset","Type", "KC", "Intercept", "Slope")
  
  model_AIC <- AIC(iafm.model)
  model_BIC <- BIC(iafm.model)
  model_logLik <- as.numeric(logLik(iafm.model))
  #maineffect_intercept <- fixef(iafm.model)[[1]]
  maineffect_slope <- fixef(iafm.model)[[1]]
  
  overall.params <- data.frame(cbind(Dataset=strsplit(dataset,split = "_")[[1]][1],model_AIC,model_BIC,model_logLik,maineffect_slope))
  
  df$predicted_error_rate <- 1-predict(iafm.model,df,type="response",allow.new.levels=TRUE)
  
  return(list(stud.params=stud.params,kc.params=kc.params,overall.params=overall.params,df=df))
}

itAFM <- function(dataset, kcm,response,opportunity,individual){
  df = suppressWarnings(fread(file=paste(getwd(),"/data/",dataset,".txt",sep=""),verbose = F)) #the file to import
  time <- 'First Transaction Time'
  names(df) <- make.names(names(df)) #add the periods instead of spaces
  names(df)[which( colnames(df)==make.names(eval(kcm)) )] <- "KC" #replace the KC model name with "KC"
  names(df)[which( colnames(df)==make.names(eval(response)) )] <- "response" #replace the first attempt response name with "response"
  names(df)[which( colnames(df)==make.names(eval(opportunity)) )] <- "opportunity" #replace the opportunity name with "opportunity"
  names(df)[which( colnames(df)==make.names(eval(individual)) )] <- "individual" #replace the individualizing factor name with "individual"
  success <- ifelse(df$response=="correct",1,0) #recode response as 0 (incorrect) or 1 (correct)
  names(df)[which( colnames(df)==make.names(eval(time)) )] <- "time"
  df$success <- success
  df$errorRate <- 1-success #add a success column
  rm(success)
  
  #convert time column into time (conveniently)
  df$time<- as.POSIXct(df$time,format="%Y-%m-%d %H:%M:%S")
  
  #order things by student and KC
  df <- df[order(df$individual,df$KC,df$time)]
  
  #add new counter of "opportunity". Make sure that only real opportunities are counted, so remove nas for opportunities for KC that are empty for this KC model.
  setDT(df)[!is.na(df$opportunity), rec_opportunity := seq_len(.N), by=rleid(individual,KC)]
  
  #get time for first step in each KC for each student
  library(plyr)
  df <- ddply(.data = df,.variables = .(individual,KC),.fun = mutate,first_oppTime =  time[rec_opportunity==1])
  
  #df <- ddply(.data = df,.variables = .(individual,KC),.fun = mutate,first_oppTime =  time[opportunity==1])
  
  #calculate difference in time from first step
  df$timeDiff <- as.numeric(as.character(difftime(df$time,df$first_oppTime,units="secs")))
  df$timeDiff <- ifelse(is.na(df$timeDiff)&!is.na(df$rec_opportunity),0,df$timeDiff)
  #df$scaled_diff <- scale(df$timeDiff)
  
  #transform time series because of heteroskedasticity  using Box-Cox transformation.
  #library(forecast)
  #lambda.value <- BoxCox.lambda(df$timeDiff)
  #df$scaled_diff <- BoxCox(df$timeDiff,lambda=0.5)
  
  #df <- ddply(.data = df,.variables = .(individual,KC),.fun = mutate,scaled_diff = BoxCox(timeDiff,lambda=0.5))
  #df$scaled_diff <- ifelse(is.nan(df$scaled_diff),0,df$scaled_diff)
  
  #check number of opportunities per KC
  #df <- as.data.table(df)
  #checks4 <- df[,.(number_opp = length(individual),max = max(opportunity)),by=.(individual,KC)]
  
  #df <- df[!df$KC=="",]
  #df$timeDiff <- ifelse(df$rec_opportunity)
  
  #itafm.model <- suppressWarnings(glmer(success ~ (1|individual) + scaled_diff + (scaled_diff|individual) + (scaled_diff|KC) - 1, data=df[!is.na(df$rec_opportunity),], family=binomial(),control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))
  
  itafm.model <- suppressWarnings(glmer(success ~ (1|individual) + timeDiff + (timeDiff|individual) + (timeDiff|KC) - 1, data=df, family=binomial(),control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))))
  
  #itafm.model <- suppressWarnings(glmer(success ~ (1|individual) + (timeDiff|individual) + (timeDiff|KC) - 1, data=df, family=binomial(),control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE))))
  
  stud.params <- data.frame(cbind(row.names(ranef(itafm.model)$individual), ranef(itafm.model)$individual[,1], ranef(itafm.model)$individual[,3]) )
  stud.params <- cbind(Dataset = strsplit(dataset,split = "_")[[1]][1],Type="Student", stud.params)
  colnames(stud.params) <- c("Dataset","Type", "Student", "Intercept", "Slope")
  
  kc.params <- data.frame(cbind(row.names(ranef(itafm.model)$KC), ranef(itafm.model)$KC[,1], ranef(itafm.model)$KC[,2]) )
  kc.params <- cbind(Dataset = strsplit(dataset,split = "_")[[1]][1],Type="KC", kc.params)
  colnames(kc.params) <- c("Dataset","Type", "KC", "Intercept", "Slope")
  
  model_AIC <- AIC(itafm.model)
  model_BIC <- BIC(itafm.model)
  model_logLik <- as.numeric(logLik(itafm.model))
  #maineffect_intercept <- fixef(itafm.model)[[1]]
  maineffect_slope <- fixef(itafm.model)[[1]]
  
  overall.params <- data.frame(cbind(Dataset=strsplit(dataset,split = "_")[[1]][1],model_AIC,model_BIC,model_logLik,maineffect_slope))
  
  df$predicted_error_rate <- 1-predict(itafm.model,df,type="response",allow.new.levels=TRUE)
  
  df$time_opp <- round(df$timeDiff)
  
  df <- as.data.table(df)
  steps_time <- df[!time_opp==0,.(n_opp=max(opportunity),max_time_btw_opp=max(time_opp)),by=.(individual,KC)]
  steps_time$Dataset <- strsplit(dataset,split = "_")[[1]][1]
  
  return(list(stud.params=stud.params,kc.params=kc.params,overall.params=overall.params,df=df,steps_time=steps_time))
}

iAFM_full <- function(dataset, kcm,response,opportunity,individual){
  df = suppressWarnings(fread(file=paste(getwd(),"/data/",dataset,".txt",sep=""),verbose = F)) #the file to import
  time <- 'First Transaction Time'
  names(df) <- make.names(names(df)) #add the periods instead of spaces
  names(df)[which( colnames(df)==make.names(eval(kcm)) )] <- "KC" #replace the KC model name with "KC"
  names(df)[which( colnames(df)==make.names(eval(response)) )] <- "response" #replace the first attempt response name with "response"
  names(df)[which( colnames(df)==make.names(eval(opportunity)) )] <- "opportunity" #replace the opportunity name with "opportunity"
  names(df)[which( colnames(df)==make.names(eval(individual)) )] <- "individual" #replace the individualizing factor name with "individual"
  success <- ifelse(df$response=="correct",1,0) #recode response as 0 (incorrect) or 1 (correct)
  names(df)[which( colnames(df)==make.names(eval(time)) )] <- "time"
  df$success <- success
  df$errorRate <- 1-success #add a success column
  rm(success)
  
  #convert time column into time (conveniently)
  df$time<- as.POSIXct(df$time,format="%Y-%m-%d %H:%M:%S")
  
  #order things by student and KC
  df <- df[order(df$individual,df$KC,df$time)]
  
  #add new counter of "opportunity". Make sure that only real opportunities are counted, so remove nas for opportunities for KC that are empty for this KC model.
  setDT(df)[!is.na(df$opportunity), rec_opportunity := seq_len(.N), by=rleid(individual,KC)]
  
  #get time for first step in each KC for each student
  library(plyr)
  df <- ddply(.data = df,.variables = .(individual,KC),.fun = mutate,first_oppTime =  time[rec_opportunity==1])
  
  #df <- ddply(.data = df,.variables = .(individual,KC),.fun = mutate,first_oppTime =  time[opportunity==1])
  
  #calculate difference in time from first step
  df$timeDiff <- as.numeric(as.character(difftime(df$time,df$first_oppTime,units="secs")))
  df$timeDiff <- ifelse(is.na(df$timeDiff)&!is.na(df$rec_opportunity),0,df$timeDiff)
  #df$scaled_diff <- scale(df$timeDiff)
  
  #transform time series because of heteroskedasticity  using Box-Cox transformation.
  #library(forecast)
  #lambda.value <- BoxCox.lambda(df$timeDiff)
  #df$scaled_diff <- BoxCox(df$timeDiff,lambda=0.5)
  
 # iafm_full.model <- suppressWarnings(glmer(success ~ (1|individual) + opportunity + scaled_diff + ((scaled_diff+opportunity)|individual) + ((scaled_diff+opportunity)|KC) - 1, data=df, family=binomial(),control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))
  
  iafm_full.model <- suppressWarnings(glmer(success ~ (1|individual) + opportunity + timeDiff + ((timeDiff+opportunity)|individual) + ((timeDiff+opportunity)|KC) - 1, data=df, family=binomial(),control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))
  
  stud.params <- data.frame(cbind(row.names(ranef(iafm_full.model)$individual), ranef(iafm_full.model)$individual[,1], ranef(iafm_full.model)$individual[,2],ranef(iafm_full.model)$individual[,3]))
  stud.params <- cbind(Dataset = strsplit(dataset,split = "_")[[1]][1],Type="Student", stud.params)
  colnames(stud.params) <- c("Dataset","Type", "Student","Intercept","Slope_time", "Slope_opp")
  
  kc.params <- data.frame(cbind(row.names(ranef(iafm_full.model)$KC), ranef(iafm_full.model)$KC[,1], ranef(iafm_full.model)$KC[,2]),ranef(iafm_full.model)$KC[,3])
  kc.params <- cbind(Dataset = strsplit(dataset,split = "_")[[1]][1],Type="KC", kc.params)
  colnames(kc.params) <- c("Dataset","Type", "KC","Intercept","Slope_time","Slope_opp")
  
  model_AIC <- AIC(iafm_full.model)
  model_BIC <- BIC(iafm_full.model)
  model_logLik <- as.numeric(logLik(iafm_full.model))
  #maineffect_intercept <- fixef(iafm_full.model)[[1]]
  maineffect_slope <- fixef(iafm_full.model)[[1]]
  
  overall.params <- data.frame(cbind(Dataset=strsplit(dataset,split = "_")[[1]][1],model_AIC,model_BIC,model_logLik,maineffect_slope))
  
  df$predicted_error_rate <- 1-predict(iafm_full.model,df,type="response",allow.new.levels=TRUE)
  
  return(list(stud.params=stud.params,kc.params=kc.params,overall.params=overall.params,df=df,model=iafm_full.model))
}


