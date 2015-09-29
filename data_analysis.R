rm(list = ls())
setwd("/Volumes/SVETLANA/R/FAST/data/processed/20150929")

#### FIRST LOOK AT DATA INFO ####
load('allabout.RData')

table(allabout$Navigator)
table(allabout$OS)

summary(allabout)
table(allabout$manual)
table(allabout$sexe)
summary(allabout$age)


# Exclude some subjects based on technical issues/problems
S_out = subset(allabout$Subject, allabout$anyprob == 'prob_yes' | allabout$age < 18 |grepl("marieke|christelle",tolower(allabout$anywhich)))
S_out = c(S_out, c(515)) # errors at importing, go back to them at some point



#### REARRANGE DATA ####
load('alldata.RData')

all = droplevels(subset(all, !Subject %in% S_out))

colMeans(table(all$Subject, all$valid)/80)
Acc = table(all$Subject, all$valid)/80
S_acc = rownames(Acc)[Acc[,1] < 0.15]
# S_acc2 = rownames(Acc)[Acc[,1] < 0.10]

all = droplevels(subset(all, Subject %in% S_acc))


### Rosenbaum's design includes:
# - Type of sequence (constant vs. varying) => Seq
# - Serial position of uncertainty (2nd vs. 3rd) => Unc
# - Hand of constant response (left vs. right) => Hand
# - Serial position of response (for IKI model only) => Pos


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


#### RT ####

rt = aggregate(RT1 ~ Subject + Hand + Seq + Unc, FUN = mean, data = cor)

rt = within(rt, {Seq = as.factor(Seq)
Hand = as.factor(Hand)
Unc = as.factor(Unc)
Subject = as.factor(Subject)})

# ANOVA
rt.aov = aov(RT1 ~ Hand * Seq * Unc + Error(Subject/(Seq * Unc)), data = rt)
summary(rt.aov)

rt.aov = aov(RT1 ~ Hand +  Seq * Unc + Error(Subject/(Seq * Unc)), data = rt)
summary(rt.aov) 


# Regression
library(lme4)

summary(lm(RT1 ~ Hand +  Seq * Unc, data = rt))

  # on means
A.lmer = lmer(RT1 ~ Hand +  Seq * Unc + (1|Subject), data = rt)
summary(A.lmer)

  # on all data
A.lmer = lmer(RT1 ~ Hand +  Seq * Unc + (1|Subject) + (1|finger_seq), data = cor)
summary(A.lmer)


#### IKI ####

m1 = aggregate(IKI1 ~ Subject + Hand + Seq + Unc, FUN = mean, data = cor)
m2 = aggregate(IKI2 ~ Subject + Hand + Seq + Unc, FUN = mean, data = cor)

colnames(m1)[5] = 'IKI'
colnames(m2)[5] = 'IKI'
m1$Pos = 2
m2$Pos = 3
m = rbind(m1,m2)
m = within(m, {Pos = as.factor(Pos)
Seq = as.factor(Seq)
Hand = as.factor(Hand)
Unc = as.factor(Unc)
Subject = as.factor(Subject)})


# ANOVA
iki.aov = aov(IKI ~ Hand + Seq * Unc * Pos + Error(Subject/(Seq * Unc * Pos)), data = m)
summary(iki.aov)

# Regression
summary(lm(IKI ~ Hand + Seq * Unc * Pos, data = m))
summary(lmer(IKI ~ Hand + Seq * Unc * Pos + (1|Subject), data = m))

# Mixed models

   # rearrange data
col = c('Subject', 'part', 'block', 'trial_index', 'trial_index_global', 'stimulus', 'key_seq', 'finger_seq', 'Hand', 'Seq', 'Unc')
IKI1 = subset(cor, select = c(col, 'IKI1'))
IKI2 = subset(cor, select = c(col, 'IKI2'))
colnames(IKI1)[12] = 'IKI'
colnames(IKI2)[12] = 'IKI'
IKI1$Pos = factor(1)
IKI2$Pos = factor(2)
cor_long = rbind(IKI1, IKI2)

cor_long = subset(cor_long, Subject %in% S_acc2) # criteria on accuracy (> 90%)

  # run models
iki.lmer = lmer(IKI ~ Hand + Seq * Unc * Pos + (1|Subject) + (1|finger_seq), data = cor_long)
summary(iki.lmer)

# Results
#                 Estimate Std. Error t value
# (Intercept)     256.944      5.653   45.46
# Handright        -5.011      7.625   -0.66
# Seqv            -20.423      2.251   -9.07
# Unc3            -26.794      1.529  -17.52
# Pos2            -36.784      1.531  -24.03
# Seqv:Unc3        12.427      2.717    4.57
# Seqv:Pos2        22.468      2.161   10.40
# Unc3:Pos2        44.510      2.162   20.59 ** interaction of interest
# Seqv:Unc3:Pos2  -18.875      3.049   -6.19

# Do we keep all these effects we don't really care about ??

# Add random slopes
# Add control variables (trials etc.)


###
# People effects
# Machine effects
# 

#### PLOT DATA ####

library(ggplot2)

ggplot(m, aes(y = IKI, x = Unc, color = Pos)) + geom_boxplot() 
ggplot(m, aes(y = IKI, x = Unc)) + geom_point() + facet_wrap(~Pos)
ggplot(m, aes(y = IKI, x = Pos, color = Unc)) + stat_summary(fun.y = 'mean', geom = 'point', size = 5) + stat_summary(fun.data = 'mean_cl_boot', geom = 'pointrange')
ggplot(m, aes(y = IKI, x = Unc, color = Pos)) + stat_summary(fun.y = 'mean', geom = 'point', size = 5) + stat_summary(fun.data = 'mean_cl_boot', geom = 'pointrange')

ggplot(m, aes(IKI)) + geom_histogram(binwidth = 5)
ggplot(cor_long, aes(IKI)) + geom_histogram(binwidth = 20)

qqnorm(cor_long$IKI)
qqline(cor_long$IKI)

qqnorm(-1/m$IKI)
qqline(-1/m$IKI)

qqnorm(log(cor_long$IKI))
qqline(log(cor_long$IKI))

sort(m$IKI)




#### -----------------
# Select some subjects based on accuracy or problems (not always applicable)
m_sub = subset(m, Subject %in% S_yes)
A.aov2 = aov(IKI ~ Condition * Pos + Error(Subject/(Condition*Pos)), data = m_sub)
summary(A.aov2)


m_sub2 = subset(m, Subject %in% S_acc)
A.aov3 = aov(IKI ~ Condition * Pos + Error(Subject/(Condition*Pos)), data = m_sub2)
summary(A.aov3)

aggregate(IKI ~ Condition + Pos, FUN = mean, m_sub2)


m_sub3 = subset(m, Subject %in% S_acc2)
A.aov4 = aov(IKI ~ Condition * Pos + Error(Subject/(Condition*Pos)), data = m_sub3)
summary(A.aov3)
