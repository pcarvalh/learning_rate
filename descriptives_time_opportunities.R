## Calculate max number of opportunities and total time spent (calendar) and actual time spent (not calendar)

dataset_list <- fread("datasets.csv")

# dataset <- dataset_list$StudentStep[1]
# kcm <- dataset_list$KC[1]
# response <- "First Attempt"[1]
# opportunity <- dataset_list$Opportunity[1]
# individual <- "Anon Student Id"[1]

library(data.table)
library(tidyverse)
library(plyr)

opp_time_descriptives <- function(dataset, kcm,response,opportunity,individual){
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
  df <- ddply(.data = df,.variables = .(individual,KC),.fun = mutate,first_oppTime =  time[rec_opportunity==1])
  
  #calculate difference in time from first step
  df$timeDiff <- as.numeric(as.character(difftime(df$time,df$first_oppTime,units="mins")))
  df$timeDiff <- ifelse(is.na(df$timeDiff)&!is.na(df$rec_opportunity),0,df$timeDiff)
  #df$scaled_diff <- scale(df$timeDiff)
  
  df$time_opp <- round(df$timeDiff)
  
  df <- as.data.table(df)
  df$Step.Duration..sec. <- ifelse(df$Step.Duration..sec.==".",0,df$Step.Duration..sec.)
  steps_time <- df[!KC=="",.(n_opp=max(opportunity),total_time=max(time_opp),time_studying=sum(as.numeric(as.character(Step.Duration..sec.)))/60),by=.(individual,KC)]
  
  steps_time$Dataset <- strsplit(dataset,split = "_")[[1]][1]
  
  return(steps_time)
}

for(i in 1:nrow(dataset_list)){
  if(!exists("descriptives_table")){
    descriptives_table <- opp_time_descriptives(dataset = dataset_list$StudentStep[i],kcm = dataset_list$KC[i],response = "First Attempt",opportunity = dataset_list$Opportunity[i],individual = "Anon Student Id") #opp_time_descriptives <- function(dataset, kcm,response,opportunity,individual)
  }else{
    temp_d <- opp_time_descriptives(dataset = dataset_list$StudentStep[i],kcm = dataset_list$KC[i],response = "First Attempt",opportunity = dataset_list$Opportunity[i],individual = "Anon Student Id") #opp_time_descriptives <- function(dataset, kcm,response,opportunity,individual)
    descriptives_table <- rbind(descriptives_table,temp_d)
    rm(temp_d)
  }
}

write.csv(descriptives_table,file="descriptives_table.csv",row.names = F)
