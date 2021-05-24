cv  <- function(inputFile,folds,repeats,inputFile,kcm,response,opportunity,individual){

# needed libraries
suppressMessages(library(ggplot2))
suppressMessages(library(caret))
suppressMessages(library(doSNOW))
suppressMessages(library(recipes))
suppressMessages(library(lme4))
suppressMessages(library(data.table))

# Cross-validation
inputFile = "ds104_student_step_All_Data_218_2016_0406_071258"
kcm <- "KC (Default2)"
response <- "First Attempt"
opportunity <- "Opportunity (Default2)"
individual <- "Anon Student Id"
folds =  10
repeats =  5

# get the model (for testing purposes only)
source("models_function.R")

m_out <- iAFM(dataset = inputFile, kcm = kcm, response = response, opportunity = opportunity, individual = individual)
fittedModel  <- m_out$model

#helper functions
#compute RMSE
RMSE <- function(error) { sqrt(mean(error^2)) }

#make k-fold cv
CV.KFold <- function(column, folds, repeats) {
  # This function ensures that the ratio of those
  # that survived and perished in each fold matches the overall training set. This
  # is known as stratified cross validation and generally provides better results.
  createMultiFolds(column, k = folds, times = repeats)
}

#make student stratified cv
CV.Stratified <- function(column, folds, repeats) {
  unique.values <- unique(column)
  folds.repeats = createMultiFolds(unique.values, k = folds, times = repeats)
  print(folds.repeats)
  #turn folds.repeats into row numbeer
  for (i in 1:length(folds.repeats)) {
    train.data.fold = list()
    unique.column.fold = folds.repeats[[i]]
    for (j in 1:length(unique.column.fold)) {
      value.in.train = unique.values[unique.column.fold[j]]
      train.data.fold = append(train.data.fold, which(column==value.in.train))
    }
    train.data.fold = unlist(train.data.fold)
    folds.repeats[[i]] = train.data.fold
    #CV.Stratified.folds = append(CV.Stratified.folds, train.data.fold)
  }
  folds.repeats
} 

# Load raw data

ds<- fread(file=paste(getwd(),"/data/",inputFile,".txt",sep=""),verbose = T,check.names=TRUE)

#read.table(inputFile, sep="\t", header=TRUE, quote="\"",comment.char = "",blank.lines.skip=TRUE)
#str(ds)

#process response column 
names(ds) <- make.names(names(ds)) #add the periods instead of spaces
names(ds)[which( colnames(ds)==make.names(eval(kcm)) )] <- "KC" #replace the KC model name with "KC"
names(ds)[which( colnames(ds)==make.names(eval(response)) )] <- "response" #replace the first attempt response name with "response"
names(ds)[which( colnames(ds)==make.names(eval(opportunity)) )] <- "opportunity" #replace the opportunity name with "opportunity"
names(ds)[which( colnames(ds)==make.names(eval(individual)) )] <- "individual" #replace the individualizing factor name with "individual"
success <- ifelse(ds$response=="correct",1,0) #recode response as 0 (incorrect) or 1 (correct)
ds$success <- success
ds$errorRate <- 1-success #add a success column
ds$opportunity <- ds$opportunity-1

ds <- ds[!is.na(ds$KC),]

ds$item.id <- paste(as.character(ds$Problem.Hierarchy), as.character(ds$Problem.Name), as.character(ds$Step.Name), sep=";")
#str(ds$item.id)

# Research has shown that 10-fold CV repeated 10 times is the best place to start,
# Leverage caret to create 100 total folds
set.seed(2348)
#cv.folds <- createMultiFolds(ds$First.Attempt, k = folds, times = repeats)
cv.folds <- CV.KFold(ds$success, folds, repeats)

#check stratification
#table(ds$First.Attempt)
#table(ds$First.Attempt[cv.folds[[1]]])
cv.student.folds <- CV.Stratified(ds$individual, folds, repeats)
cv.item.folds <- CV.Stratified(ds$item.id, folds, repeats)

preds.df <- data.frame()
rmse.vals = c()
rmse.data.perc = c()
# Set up doSNOW package for multi-core training. 
# NOTE - This works on Windows and Mac, unlike doMC
#cl <- makeCluster(6, type = "SOCK")
#registerDoSNOW(cl)

for (i in 1:(folds*repeats)){
  training<-NULL
  testing<-NULL
  training <- ds[cv.folds[[i]],]
  testing  <- ds[-cv.folds[[i]],]
  #modelingString = paste("fittedModel <- glmer(", formula, ", data=training, family= binomial(link = logit))", sep="")
  #eval(parse(text=modelingString))
  preds <- predict(fittedModel, newdata = testing, allow.new.levels = TRUE, type="response")
  preds.df <- rbind(preds.df, data.frame(Row=as.numeric(names(preds)), predValue=preds, row.names=NULL))
  if (i%%folds == 0) {
    preds.df.not.null <- subset(preds.df, !is.na(predValue))
    preds.df.not.null <- merge(preds.df.not.null,ds[,c("Row", "success")],by="Row")
    rmse.data.perc <- append(rmse.data.perc, nrow(preds.df.not.null)/nrow(preds.df))
    rmse.vals <- append(rmse.vals, RMSE(preds.df.not.null$predValue-preds.df.not.null$success))
    preds.df <- NULL
    preds.df.not.null <- NULL
  }
}

return(list(rmse.vals=rmse.vals,rmse.data.perc=rmse.data.perc,mean.rmse=mean(rmse.vals)))
}