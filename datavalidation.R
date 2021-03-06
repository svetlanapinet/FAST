rm(list = ls())
setwd("/Volumes/SVETLANA/R/FAST/data/processed/20151020")
library(ggplot2)
library(diptest)
library(mclust)
library(fts)
load('alldata_541.RData') # Only people with no reported problem

##### Bootstrap validation #####


bootmean <- function (means, N) {sub = sample(means$Subject, N)
sample1 = subset(means, Subject %in% sub)
m1 = mean(sample1[,2])
return(m1)}

load('alldata_529_RT.RData')
means = aggregate(RT1 ~ Subject, FUN = mean, data = cor)

Ns = c(5, 10, 20, 50, 100, 200, 300, 400, 500)
Ms = c()
CIl = c()
CIu = c()
# N = 10
for (N in Ns) {
  b = c()
  while (length(b) < 1000) {b = c(b,bootmean(means,N))}
  mu = mean(b)
  ci = sd(b)/sqrt(N)
  ci_l = mu - ci
  ci_u = mu + ci
  Ms = c(Ms,mu)
  CIl = c(CIl,ci_l)
  CIu = c(CIu,ci_u)}

d = data.frame(Ns, Ms, CIl, CIu)
ggplot(d, aes(x = Ns, y = Ms)) + geom_path() + geom_point(size = 3) + geom_errorbar(aes(ymin = CIl, ymax = CIu)) + 
  scale_x_log10() + coord_cartesian(ylim = c(570,670)) + theme_bw() + xlab('Sample size') + ylab('Mean RT (ms)')

pdf('BootstrapRT.pdf', width = 7, height = 6)
dev.off()

# Distribution of means

means = aggregate(RT1 ~ Subject, FUN = mean, data = all)
ggplot(means, aes(RT1)) + geom_histogram(binwidth = 50)
tail(means[order(means$RT1),])
# S299, S244 are weird
s = subset(all, Subject == 299)
s = subset(all, Subject == 244)
  # some really long RT, probably unreported interruption?

ggplot(subset(all,RT1<5000), aes(RT1)) + geom_histogram(binwidth = 50)
tail(remove.na.rows(all[order(all$RT1),]),50)

# We can consider trials where RT1 < 3000 as a proxy for people actually being engaged in the task
all_3000 = subset(all, RT1 < 3000)
means = aggregate(RT1 ~ Subject, FUN = mean, data = all_3000)
ggplot(means, aes(RT1)) + geom_histogram(binwidth = 50)
all = all_3000


# Same with IKI

load('alldata_529_IKI.RData')
means = aggregate(IKI ~ Subject, FUN = mean, data = cor_long)

# iki = data.frame(Subject = rep(all$Subject,2), IKI = c(all$IKI1, all$IKI2))
# ggplot(subset(iki, IKI < 2000), aes(IKI)) + geom_histogram()
# means = aggregate(IKI ~ Subject, FUN = mean, data = iki)
# ggplot(means, aes(IKI)) + geom_histogram(binwidth = 5)

Ns = c(5, 10, 20, 50, 100, 200, 300, 400, 500)
Ms = c()
CIl = c()
CIu = c()
# N = 10
for (N in Ns) {
  b = c()
  while (length(b) < 1000) {b = c(b,bootmean(means,N))}
  mu = mean(b)
  ci = sd(b)/sqrt(N)
  ci_l = mu - ci
  ci_u = mu + ci
  Ms = c(Ms,mu)
  CIl = c(CIl,ci_l)
  CIu = c(CIu,ci_u)}

d = data.frame(Ns, Ms, CIl, CIu)
ggplot(d, aes(x = Ns, y = Ms)) + geom_path() + geom_point(size = 3) + geom_errorbar(aes(ymin = CIl, ymax = CIu)) + 
  coord_cartesian(ylim = c(200,250)) + scale_x_log10() + theme_bw() + xlab('Sample size') + ylab('Mean IKI (ms)')

pdf('BootstrapIKI.pdf', width = 7, height = 6)
dev.off()

##### Sampling rate #####

  # plot differences of ordered RTs
suj = subset(all, Subject == sample(unique(all$Subject),1))
#116/452 (16) vs. 59/326/277 (8) vs. 577/74/286 (1)
RT = c(suj$RT1, suj$RT2, suj$RT3)
RTs = sort(RT)
d = c(0,diff(RTs))

# data = data.frame(x = seq(1,length(d)), RT = na.exclude(RT), RTs = RTs, d = d)
# 
# ggplot(subset(data, d<30), aes(x = x, y = sort(d))) + geom_point()
# ggplot(subset(data, d<15), aes(x = x, y = sort(d))) + geom_point()

data = data.frame(RT = na.exclude(RT), RTs = RTs, d = d)
# plot(sort(data$d), ylim = c(0,45))
data2 = subset(data, d<30)
ggplot(data2, aes(x = 1:nrow(data2), y = sort(d))) + geom_point(alpha = 0.5) + xlab('') + ylab('RT Difference (ms)') + theme_bw() 

pdf('OrderedRT_4.pdf', width = 5, height = 5)
dev.off()


  # with modulo (8)
suj = subset(all, Subject == sample(unique(all$Subject),1))
suj = subset(all, Subject == 478) # 478 & 577
RT = c(suj$RT1, suj$RT2, suj$RT3)
data = data.frame(RT = RT, mod8 = as.factor(RT %% 8), mod16 = RT %% 16)

ggplot(data, aes(mod8)) + geom_histogram(binwidth = 0.5) + xlab('Remainder values') + ylab('Frequency') + theme_bw() + 
  scale_x_discrete(limits = c('0','1','2','3','4','5','6','7'))
chisq.test(table(data$mod8))$p.value

pdf('DistribChi2_4.pdf', width = 5, height = 5)
dev.off()



  # Let's have a criterion, according to the distribution of modulos (if homogeneous => sampling is too)
all$Subject = as.numeric(as.character(all$Subject))
allmod = data.frame(suj = c(all$Subject, all$Subject, all$Subject), RT = c(all$RT1, all$RT2, all$RT3))
allmod <- remove.na.rows(allmod)
allmod$mod8 = allmod$RT %% 8

tab = as.data.frame.matrix(table(allmod$suj, allmod$mod8))
results = data.frame(suj = rownames(tab))

# chi-square test on table mod8
for (s in 1:nrow(tab)) { results$test[s] = chisq.test(tab[s,])$p.value }
ggplot(results, aes(test))+geom_histogram(binwidth = 0.01)+geom_vline(xintercept = 0.05, color = 'red') # not great representation

length(which(results$test > 0.05)) # 165
length(which(results$test > 0.01)) # 208
length(which(results$test > 0.001)) # 243
length(which(results$test > 0.05/541)) # Bonferroni gives 276

# FDR gives:
fdr = subset(results, select = c('suj', 'test'))
fdr = fdr[order(fdr$test),]
fdr$s = seq(1:nrow(fdr))
fdr$k = (fdr$s*0.05 / 541)
fdr$crit = fdr$test < fdr$k
max(which(fdr$crit)) # 363 => then, reject 1:363 null hyp (67%)



# Is there a mode standing out for those subjects with a sampling bias?
results$YN = ifelse(results$test > 0.05/541, 0, 1)
results$mode = NA
for (s in 1:nrow(tab)) {
  if (results$YN[s] == 1) results$mode[s] = as.numeric(names(which.max(tab[s,])))}

ggplot(results, aes(mode))+geom_histogram()
  # answer is not really...


# Out of curiosity, compute root mean square on each subject's distribution of modulos
RMS <- function(x) {sqrt(mean(x^2))}

for (s in 1:nrow(tab)) { results$rms[s] = RMS(tab[s,])}
results$bon = as.factor(ifelse(results$test > 0.05/541, 0, 1))
results = merge(results, fdr[,c('suj','crit')], by = 'suj')
results$p5 = as.factor(ifelse(results$test > 0.05,0,1))
results$p1 = as.factor(ifelse(results$test > 0.01,0,1))
results$p10 = as.factor(ifelse(results$test > 0.001,0,1))

ggplot(results, aes(rms, fill = YN)) + geom_histogram()
ggplot(results, aes(rms, fill = crit)) + geom_histogram()
ggplot(results, aes(rms, fill = p5)) + geom_histogram()
ggplot(results, aes(rms, fill = p1)) + geom_histogram()
ggplot(results, aes(rms, fill = p10)) + geom_histogram()



# Do we see the same bias on IKI ?

all$Subject = as.numeric(as.character(all$Subject))
allmod = data.frame(suj = c(all$Subject, all$Subject), IKI = c(all$IKI1, all$IKI2))
allmod <- remove.na.rows(allmod)
allmod$mod8 = allmod$IKI %% 8

tab = as.data.frame.matrix(table(allmod$suj, allmod$mod8))
results = data.frame(suj = rownames(tab))

# chi-square test on table mod8 (if significant, then not uniform and sampling bias)
for (s in 1:nrow(tab)) { results$test[s] = chisq.test(tab[s,])$p.value }

ggplot(results, aes(test))+geom_histogram(binwidth = 0.01)+geom_vline(xintercept = 0.05, color = 'red') # not great representation

length(which(results$test > 0.05)) # 92
length(which(results$test > 0.01)) # 96
length(which(results$test > 0.001)) # 101
length(which(results$test > 0.05/541)) # Bonferroni gives 105
  # => even more than RTs!

# FDR gives:
fdr = data.frame(s = 1:541, p = sort(results$test))
fdr$k = (fdr$s*0.05 / 541)
fdr$crit = fdr$p < fdr$k
max(which(fdr$crit)) # 449/83%


# plot examples for IKI
suj = subset(all, Subject == sample(unique(all$Subject),1))
IKI = c(suj$IKI1, suj$IKI2)
data = data.frame(IKI = IKI, mod8 = IKI %% 8)

ggplot(data, aes(mod8)) + geom_histogram(binwidth = 0.5) + coord_cartesian(xlim = c(seq(0,8)))
chisq.test(table(data$mod8))$p.value
 




# -------------------
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
