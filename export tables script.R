#clean environment
rm(list=ls())

#load itAFM results
load("modelFits_itAFM.RData")

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

#load iAFM results
load("modelFits_iAFM.RData")

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

#load itAFM results
load("modelFits_itAFM_iAFM_full.RData")

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
