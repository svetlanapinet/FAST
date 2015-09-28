rm(list = ls())
setwd("/Volumes/SVETLANA/R/FAST")
library(jsonlite)
library(plyr)

data = read.table("ks_data2.txt", header = F, sep = "\t", quote = "", stringsAsFactors = F)

all = data.frame()
allabout = data.frame()

for (S in data[,1]){os = strsplit(data[S,3], '_')[[1]]
if (os[3] != "Navigator") {sj = data.frame(Subject = data[S,1], OS = os[2], OS_v = os[3], Navigator = os[5], Nav_v = os[6], Devicetype = os[8])}
else sj = data.frame(Subject = data[S,1], OS = os[2], OS_v = NA, Navigator = os[4], Nav_v = os[5], Devicetype = os[7])

A <- fromJSON(data[S,4])

# deal with last line
char = fromJSON(tail(A[,2],1))
char = data.frame(t(unlist(char)))
about = cbind(sj, char)
allabout = rbind.fill(allabout, about)

# keep only data
A = subset(A, training == F)

nc = nchar(A$rt)
rt2 = substr(A$rt, 2, nc-1)
as.numeric(unlist(strsplit(rt2, ","))) -> B
A[,c('RT1', 'RT2', 'RT3')] = matrix(B, ncol = 3, byrow = T)

A$IKI1 = A$RT2 - A$RT1
A$IKI2 = A$RT3 - A$RT2

dat = cbind(about, A)

all = rbind.fill(all, dat)}

all = subset(all, Subject != 32)
allabout = subset(allabout, Subject != 32)
allabout$age = as.numeric(as.character(allabout$age))
allabout$sound = as.character(allabout$sound)
allabout$sound = sapply(strsplit(allabout$sound, '_'), '[',2)
allabout$anyprob = as.character(allabout$anyprob)
allabout$anyprob = sapply(strsplit(allabout$anyprob, '_'), '[',2)


save(all, file = 'alldata.RData', row.names = F, col.names = T)
save(allabout, file = 'allabout.RData', row.names = F, col.names = T)


#### backup_1609 ####

rm(list = ls())
setwd("/Volumes/SVETLANA/R/FAST/20150920")
library(jsonlite)
library(plyr)

data = read.table("backup_20150920_221636_2.txt", header = F, sep = "\t", quote = "", stringsAsFactors = F, encoding = 'latin1')
# data = read.table("backup_20150916_150104.txt", header = F, sep = "\t", quote = "", stringsAsFactors = F)


# data = read.table("test.txt", header = F, sep = "\t", quote = "", stringsAsFactors = F)
# A = fromJSON(data[1,1])


# info = data[1:10,1:3]
# colnames(info)[1] = 'Subject'
# info[,2] = NULL
# info$d8 = v == 8


# Keep info about subjects and their OS/navigator
info = data.frame(Subject = data[,1])
os = strsplit(data[,3], '_')
v = sapply(os, function(x) length(x))

  # not linux users (for which we have info about their OS version)
d8 = unlist(os[v == 8])
l = length(d8)
notlin = data.frame(OS = d8[seq(2,l,by=8)], OS_v = d8[seq(3,l,by=8)], Navigator = d8[seq(5,l,by=8)], Nav_v = d8[seq(6,l,by=8)], Devicetype = d8[seq(8,l,by=8)])
notlin$which = which(v==8)

  #linux users
d7 = unlist(os[v == 7])
l = length(d7)
lin = data.frame(OS = d7[seq(2,l,by=7)], OS_v = NA, Navigator = d7[seq(4,l,by=7)], Nav_v = d7[seq(5,l,by=7)], Devicetype = d7[seq(7,l,by=7)])
lin$which = which(v==7)

info = merge(info,rbind(notlin,lin), by.x = 'Subject', by.y = 'which', all.x = T)

#which(is.na(info$OS))
# summary(info)



all = data.frame()
allabout = data.frame()
resp = data.frame()
training = data.frame()
subj_err = c()

# sub = 1:10


# 
for (S in data[,1])
{message <- try({
  A <- fromJSON(data[S,4])
  
  # deal with last line
  
#   if(length(A) == 19)  {resp = rbind.fill(resp, subset(A, trial_type == 'form', select = c('manual', 'sexe', 'age', 'anyprob', 'anywhich')))
#   A <- subset(A, , -c(manual, sexe, age, anyprob, anywhich))}
#   if(length(A) == 20)  {resp = rbind.fill(resp, subset(A, trial_type == 'form', select = c('manual', 'sexe', 'age', 'anyprob', 'anywhich', 'comefrom')))
#   A <- subset(A, , -c(manual, sexe, age, anyprob, anywhich, comefrom))}
  
  resp = rbind.fill(resp,A[A$trial_type == 'form', 15:length(A)])
  A <- A[, -(15:length(A))]

  resp[S,'Subject'] = S
  
  # isolate training
  training_sub = subset(A,training == T) # select = coln
  training = rbind.fill(training, data.frame(Subject = factor(S), training_sub, lengthTraining = nrow(training_sub)))
  
  # keep only data
  A = subset(A, training == F)
  
  # extract RT for each keypress
  nc = nchar(A$rt)
  rts = strsplit(substr(A$rt, 2, nc-1), ",")
  nel = sapply(rts, function(x) length(x))
  as.numeric(unlist(rts[nel == 3])) -> B
  A[,c('RT1', 'RT2', 'RT3')] = NA
  A[(nel == 3),c('RT1', 'RT2', 'RT3')] = matrix(B, ncol = 3, byrow = T)
  
  # calculate IKI
  A$IKI1 = A$RT2 - A$RT1
  A$IKI2 = A$RT3 - A$RT2
  
  A$Subject = factor(S)
  
  all = rbind.fill(all, A) #subset(A, select = colnall)
})

# store subjects with problems in importing JSON data
if (class(message) == 'try-error') subj_err = c(subj_err, S)

# show progression
print(S)}

# these are subjects with errors and which were not imported
print(subj_err)
print(length(subj_err))

# go back to these subjects at some point
S_err = c(151, 515, 516, 538) # + tests christelle/marieke

allabout = merge(info, resp) 

allabout = within(allabout,{ age = as.numeric(age)
manual = as.factor(manual)
sexe = as.factor(sexe)
anyprob = as.factor(anyprob)
comefrom = as.factor(comefrom)})


save(all, file = 'alldata.RData', row.names = F, col.names = T)
save(allabout, file = 'allabout.RData', row.names = F, col.names = T)
# save(probs, file = 'allprobs.RData', row.names = F, col.names = T)
# save(allabout, file = 'allabout.RData', row.names = F, col.names = T)


#### -----------
# one solution
for (S in data[sub,1])
{message <- try({
  A <- fromJSON(data[S,4])
  
  # deal with last line
  if (is.null(resp)) {resp = A[A$trial_type == 'form', 'responses']} else resp = paste(resp, A[A$trial_type == 'form', 'responses'], sep = ',')
  A$responses = NULL
  
  # char = fromJSON(tail(A[,2],1))
  # char = data.frame(t(unlist(char)))
  # about = cbind(sj, char)
  # allabout = rbind.fill(allabout, about)
  
  
  # isolate training
  training_sub = subset(A,training == T) # select = coln
  training = rbind.fill(training, data.frame(Subject = factor(S), training_sub, lengthTraining = nrow(training_sub)))
  
  # keep only data
  A = subset(A, training == F)
  
  # extract RT for each keypress
  nc = nchar(A$rt)
  rts = strsplit(substr(A$rt, 2, nc-1), ",")
  nel = sapply(rts, function(x) length(x))
  as.numeric(unlist(rts[nel == 3])) -> B
  A[,c('RT1', 'RT2', 'RT3')] = NA
  A[(nel == 3),c('RT1', 'RT2', 'RT3')] = matrix(B, ncol = 3, byrow = T)
  
  # calculate IKI
  A$IKI1 = A$RT2 - A$RT1
  A$IKI2 = A$RT3 - A$RT2
  
  A$Subject = factor(S)
  
  all = rbind.fill(all, A) #subset(A, select = colnall)
})

# store subjects with problems in importing JSON data
if (class(message) == 'try-error') subj_err = c(subj_err, S)

# show progression
print(S)}

# these are subjects with errors and which were not imported
print(subj_err)
print(length(subj_err))

# deal with responses to form (does not work, try something with gsub("[[:punct:]]", "", as.character(x)) maybe...)
probs = fromJSON(paste('[', resp, ']', sep = '')) 
allabout = rbind(info, probs) # this does not work yet...
