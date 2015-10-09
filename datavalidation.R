rm(list = ls())
setwd("/Volumes/SVETLANA/R/FAST/data/processed/20150929")
library(ggplot2)
library(diptest)
library(mclust)
library(fts)
load('alldata.RData')

suj = subset(all, Subject == sample(unique(all$Subject),1))
#116/452 (16) vs. 59/326/277 (8) vs. 577/74/286 (1)
RT = c(suj$RT1, suj$RT2, suj$RT3)
RTs = sort(RT)
d = c(0,diff(RTs))

data = data.frame(x = seq(1,length(d)), RT = na.exclude(RT), RTs = RTs, d = d)

ggplot(subset(data, d<30), aes(x = x, y = sort(d))) + geom_point()
ggplot(subset(data, d<15), aes(x = x, y = sort(d))) + geom_point()


# with modulo
suj = subset(all, Subject == sample(unique(all$Subject),1))
RT = c(suj$RT1, suj$RT2, suj$RT3)
data = data.frame(RT = RT, mod8 = RT %% 8, mod16 = RT %% 16)

ggplot(data, aes(mod8)) + geom_histogram(binwidth = 0.5) + geom_hline(yintercept = length(RT)/48, color = 'red')
# ggplot(data, aes(mod16)) + geom_histogram(binwidth = 0.5) + geom_hline(yintercept = length(RT)/48, color = 'red')

  #not that easy to characterize bimodality
# summary(Mclust(na.exclude(data$mod8)))
# summary(Mclust(na.exclude(data$mod16)))
# summary(densityMclust(na.exclude(data$mod16)))
# dip.test(na.exclude(data$mod16))

  # instead, have a criterion, according to the size of the vector
allmod = data.frame(suj = c(all$Subject, all$Subject, all$Subject), RT = c(all$RT1, all$RT2, all$RT3))
allmod <- remove.na.rows(allmod)
allmod$mod8 = allmod$RT %% 8
allmod$mod16 = allmod$RT %% 16

tab = table(allmod$suj, allmod$mod8)
arr = which(tab < 5, arr.ind = T)
sujpb = unique(arr[,1]) # length = 185

# with 5 subjects

suj = subset(all, Subject %in% sample(unique(all$Subject),5))

RT = c(suj$RT1, suj$RT2, suj$RT3)
RTs = sort(RT)
d = c(0,diff(RTs))

data = data.frame(RT = na.exclude(RT), RTs = RTs, d = d)

ggplot(data, aes(x = 1:length(data$RTs), y = RTs)) + geom_point()
subs = subset(data, RTs < 700 & RTs > 500)
ggplot(subs, aes(x = 1:length(subs$RTs), y = RTs)) + geom_point()
ggplot(data[500:700,], aes(x = 500:700, y = RTs)) + geom_point()

subs = subset(data, RTs < 800 & RTs >= 700)
ggplot(subs, aes(RTs)) + geom_histogram(binwidth = 1)
 

plot(sort(data$d), ylim = c(0,45))
ggplot(data, aes(x = 1:nrow(data), y = sort(d))) + geom_point()
ggplot(data, aes(x = 1:nrow(data), y = sort(d))) + geom_point() + scale_y_log10()


  # Keep subject info
suj = subset(all, Subject %in% sample(unique(all$Subject),3))

RT = c(suj$RT1, suj$RT2, suj$RT3)
sj = c(suj$Subject, suj$Subject, suj$Subject)

data = data.frame(Subject = sj, RT = RT)
data = subset(data, !is.na(RT))
data = data[order(data$RT),]
data$d = c(0,diff(data$RT))

subs = subset(data, RT < 900 & RT >= 600)
ggplot(subs, aes(x = 1:length(subs$RT), y = RT, color = factor(Subject))) + geom_point()
