rm(list = ls())
setwd("/Volumes/SVETLANA/R/FAST/20150920")
library(ggplot2)


load('allabout.RData')
S_out = subset(allabout$Subject, allabout$anyprob == 'prob_yes' | allabout$age < 18 |grepl("marieke|christelle",tolower(allabout$anywhich)))
S_out = c(S_out, c(151, 515, 516, 538)) # errors at importing, go back to them at some point

load('alldata.RData')

all = droplevels(subset(all, !Subject %in% S_out))

# colMeans(table(all$Subject, all$valid)/80)
# Acc = table(all$Subject, all$valid)/80
# S_acc = rownames(Acc)[Acc[,1] < 0.15]
# S_acc2 = rownames(Acc)[Acc[,1] < 0.10]
# 
# all = droplevels(subset(all, Subject %in% S_acc))

popmean = mean(all$RT1, na.rm = T)


# resampling subjects at every step
results = data.frame()
S = unique(all$Subject) #511 subjects
for (size in seq(1,50)){
sub = sample(S, size*10)
datasample = subset(all, Subject %in% sub)
m_sub = aggregate(RT1~ Subject, FUN = mean, data = datasample)
m = mean(m_sub[,2])
sd = sd(m_sub[,2])
results= rbind(results,c(size, m, sd))}

colnames(results) = c('Size', 'Mean', 'SD')

ggplot(results, aes(Mean, x = Size)) + geom_path() + geom_point() + 
  geom_hline(yintercept = popmean, color = 'red', lty = 2) + coord_cartesian(ylim = c(550,800))
ggplot(results, aes(SD, x = Size)) + geom_path()


# getting more subjects at every step
results2 = data.frame()
datasample = data.frame()
all2 = all
size = 10
while (size < 501) {
  S = unique(all2$Subject)
  sub = sample(S, 10)
  sample = subset(all2, Subject %in% sub)
  datasample = rbind(datasample, sample)
  all2 = droplevels(subset(all2, !Subject %in% sub))
  m_sub = aggregate(RT1~ Subject, FUN = mean, data = datasample)
  m = mean(m_sub[,2])
  sd = sd(m_sub[,2])
  results2= rbind(results2,c(size, m, sd))
  size = size + 10}

colnames(results2) = c('Size', 'Mean', 'SD')

ggplot(results2, aes(Mean, x = Size)) + geom_path() + geom_point() + 
  geom_hline(yintercept = popmean, color = 'red', lty = 2) + coord_cartesian(ylim = c(550,800))
ggplot(results2, aes(SD, x = Size)) + geom_path() + geom_point()
#


# keep all data from each sample (to draw boxplots/distributions)

results3 = data.frame()
all2 = all
sub_all = c()
size = 50
while (size < 501) {
  S = unique(all2$Subject)
  sub = sample(S, 50)
  sub_all = c(sub, sub_all)
  sample = subset(all, Subject %in% sub_all, select = c(Subject, RT1))
  sample$size = factor(size)
  results3 = rbind(results3, sample)
  all2 = droplevels(subset(all2, !Subject %in% sub))
  size = size + 50}

results3 = subset(results3, RT1 < 2000)
ggplot(results3, aes(x = size, y = RT1)) + geom_boxplot()
+ coord_cartesian(ylim = c(0,1500))

#
pdf('MeanbySamplesize_noresample.pdf')
dev.off()




# Code conditions
all$part = rep(seq(1,4), each = 20)
all$block = rep(seq(1,2), each = 40)

seq2 = c("iIm", "IiM")
seq3 = c("iaI", "IAi")
all$Hand = as.factor(ifelse(substr(all$finger_seq,1,1) == 'I', 'right', 'left'))
all$Seq = as.factor(ifelse(all$finger_seq %in% seq2 | all$finger_seq %in% seq3, 'v', 'c'))
all$Uncseq = as.factor(ifelse(all$finger_seq %in% seq2, '2', ifelse(all$finger_seq %in% seq3, '3', 'z')))

cond = subset(all, Seq != 'c', select = c('Subject', 'part', 'block', 'Uncseq'))
cond = unique(cond)
colnames(cond)[4] <- 'Unc'

all = merge(all, cond, all.x = T)
all$Uncseq = NULL

# Compute means by condition
cor = subset(all, valid == 1 & IKI1 > 0 & IKI2 >0)

