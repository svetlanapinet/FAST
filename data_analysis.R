rm(list = ls())
setwd("/Volumes/SVETLANA/R/FAST/data/processed/20151020")
library(ggplot2)

#### FIRST LOOK AT DATA INFO ####
load('allabout.RData')

# write.table(file = 'allabout.txt', allabout, row.names = F, col.names = T, sep = '\t')

table(allabout$Navigator)
table(allabout$OS)
table(allabout$Navigator, allabout$OS)
allabout = within(allabout, {
  Navigator = as.character(Navigator)
  Navigator2 = as.factor(ifelse(Navigator %in% c("Opera", "Chromium", "Iceweasel"), "Autres", Navigator))
  OS = as.character(OS)
  OS2 = as.factor(ifelse(OS %in% c("Linux", "Ubuntu", "Fedora"), "Linux", OS))})
table(allabout$Navigator2, allabout$OS2)

summary(allabout)
table(allabout$manual)
table(allabout$sexe)
table(allabout$sexe, allabout$manual)
summary(allabout$age)


# Exclude some subjects based on technical issues/problems
subset(allabout$anywhich, allabout$anyprob == 'prob_yes')
S_out = subset(allabout$Subject, allabout$anyprob == 'prob_yes' | allabout$age < 18 |grepl("marieke|christelle",tolower(allabout$anywhich)))
S_out = c(S_out, c(515, 58)) # errors at importing (christelle test) and other identified on excel sheet
  # how many out
length(subset(allabout$Subject, allabout$anyprob == 'prob_yes' | allabout$age < 18))

# Get participants info with final dataset
allabout = subset(allabout, !Subject %in% S_out)
allabout = within(allabout, {
  Navigator = as.character(Navigator)
  Navigator2 = as.factor(ifelse(Navigator %in% c("Opera", "Chromium", "Iceweasel"), "Autres", Navigator))
  OS = as.character(OS)
  OS2 = as.factor(ifelse(OS %in% c("Linux", "Ubuntu", "Fedora"), "Linux", OS))})

table(allabout$Navigator2, allabout$OS2)
table(allabout$OS2, allabout$OS_v)
table(allabout$Navigator2, allabout$Nav_v)
table(allabout$sexe)
table(allabout$manual)
table(allabout$sexe, allabout$manual)
summary(allabout$age)
table(allabout$comefrom)


#### REARRANGE DATA ####
load('alldata.RData')

all = droplevels(subset(all, !Subject %in% S_out))
# save(all, file = 'alldata_541.RData')
# all_541 = all

colMeans(table(all$Subject, all$valid)/80)
Acc = table(all$Subject, all$valid)/80
S_acc = rownames(Acc)[Acc[,1] < 0.15]
# S_acc2 = rownames(Acc)[Acc[,1] < 0.10]

all = droplevels(subset(all, Subject %in% S_acc))
# save(all, file = 'alldata_529.RData') # 541 participants & 85% Acc => 529

# load('alldata_529.Rdata')


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


    ## Criterion for RT & IKI

cor = subset(all, valid == 1 & IKI1 > 0 & IKI2 > 0) # only correct responses

# Distribution of RT
ggplot(cor, aes(RT1)) + geom_histogram(binwidth = 100) 

tail(sort(cor$RT1), 20) # three very extreme values (46389, 152510, 173653)

cor2 = subset(cor, RT1 < 30000)
ggplot(cor2, aes(RT1)) + geom_histogram(binwidth = 100) + scale_y_sqrt()

# cor2 = subset(cor, RT1 < 10000)
# ggplot(cor2, aes(RT1)) + geom_histogram(binwidth = 100) + scale_y_sqrt()

cor3 = subset(cor, RT1 < 3000)
ggplot(cor3, aes(RT1)) + geom_histogram(binwidth = 100) + scale_y_sqrt()


# Distribution of IKI
col = c('Subject', 'part', 'block', 'trial_index', 'trial_index_global', 'stimulus', 'key_seq', 'finger_seq', 'Hand', 'Seq', 'Unc')
IKI1 = subset(cor3, select = c(col, 'IKI1'))
IKI2 = subset(cor3, select = c(col, 'IKI2'))
colnames(IKI1)[12] = 'IKI'
colnames(IKI2)[12] = 'IKI'
IKI1$Pos = factor(1)
IKI2$Pos = factor(2)
cor_long = rbind(IKI1, IKI2)

ggplot(cor_long, aes(IKI)) + geom_histogram(binwidth = 20) + scale_y_sqrt()


# Final datasets

cor = subset(all, valid == 1 & IKI1 > 0 & IKI2 > 0)
cor = subset(cor, RT1 < 3000 & IKI1 < 1000 & IKI2 < 1000)

IKI1 = subset(cor, select = c(col, 'IKI1'))
IKI2 = subset(cor, select = c(col, 'IKI2'))
colnames(IKI1)[12] = 'IKI'
colnames(IKI2)[12] = 'IKI'
IKI1$Pos = factor(1)
IKI2$Pos = factor(2)
cor_long = rbind(IKI1, IKI2)

save(cor, file = 'alldata_529_RT.RData')
save(cor_long, file = 'alldata_529_IKI.RData')

# load('alldata_541.RData')
# nrow(cor)/nrow(all)


load('alldata_529_RT.RData')
load('alldata_529_IKI.RData')

#### ANOVAs ####

# RT 

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

ggplot(rt, aes(y = RT1, x = Unc)) + stat_summary(fun.y = 'mean', geom = 'point', size = 5) + stat_summary(fun.data = 'mean_cl_boot', geom = 'pointrange')
aggregate(RT1 ~ Unc, FUN = mean, data = rt)
aggregate(RT1 ~ Seq, FUN = mean, data = rt)
ggplot(rt, aes(y = RT1, x = Seq, color = Unc)) + stat_summary(fun.y = 'mean', geom = 'point', size = 5) + stat_summary(fun.data = 'mean_cl_boot', geom = 'pointrange')
aggregate(RT1 ~ Seq + Unc, FUN = mean, data = rt)



# IKI

m = aggregate(IKI ~ Subject + Hand + Seq + Unc + Pos, FUN = mean, data = cor_long)

m = within(m, {Pos = as.factor(Pos)
Seq = as.factor(Seq)
Hand = as.factor(Hand)
Unc = as.factor(Unc)
Subject = as.factor(Subject)})


# ANOVA
iki.aov = aov(IKI ~ Hand * Seq * Unc * Pos + Error(Subject/(Seq * Unc * Pos)), data = m)
summary(iki.aov)

iki.aov = aov(IKI ~ Hand + Seq * Unc * Pos + Error(Subject/(Seq * Unc * Pos)), data = m)
summary(iki.aov)

aggregate(IKI ~ Seq, FUN = mean, data = m)
aggregate(IKI ~ Pos, FUN = mean, data = m)
ggplot(m, aes(y = IKI, x = Pos, color = Unc)) + stat_summary(fun.y = 'mean', geom = 'point', size = 5) + stat_summary(fun.data = 'mean_cl_boot', geom = 'pointrange')
ggplot(m, aes(y = IKI, x = Pos, color = Seq)) + stat_summary(fun.y = 'mean', geom = 'point', size = 5) + stat_summary(fun.data = 'mean_cl_boot', geom = 'pointrange') + facet_wrap(~Unc)




#### Mixed models ####
# with RT<3000 criteria
library(lme4)
library(rms)
library(languageR)

  ### RTs

# on all data
A.lmer = lmer(RT1 ~ Hand +  Seq * Unc + (1|Subject) + (1|finger_seq), data = cor)
summary(A.lmer)

A.lmer2 = lmer(RT1 ~ Hand +  Seq * Unc + trial_index_global + (1|Subject) + (1|finger_seq), data = cor)
summary(A.lmer2)


# Results
#                     Estimate Std. Error t value
# (Intercept)         743.8150    22.5633   32.97
# Handright            17.8088    20.4523    0.87
# Seqv                 29.2709    17.0844    1.71
# Unc3               -125.3070    17.1104   -7.32
# trial_index_global   -0.7700     0.1607   -4.79
# Seqv:Unc3            -9.0013    24.0923   -0.37


# People effects
cor$Subject = as.numeric(as.character(cor$Subject))
cor2 = merge(cor, allabout, by = 'Subject')

m = aggregate(RT1 ~ Subject, FUN = mean, data = cor2)
m = merge(m, allabout)
ggplot(m, aes(x = age, y = RT1)) + geom_point()+ stat_smooth() 
  #breaking the effect of age in two parts seems reasonnable

A.lmer = lmer(RT1 ~ Hand +  Seq * Unc + trial_index_global + sexe + rcs(age,3) + manual + (1|Subject) + (1|finger_seq), data = cor2)
summary(A.lmer)
plotLMER.fnc(A.lmer)

# Results
#                     Estimate Std. Error t value
# (Intercept)         574.25957   56.54698   10.16
# Handright            -5.13205   13.63214   -0.38
# Seqv                 47.47307    4.28691   11.07
# Unc3               -118.46551    3.38003  -35.05
# Seqv:Unc3           -11.59974    5.44254   -2.13
# trial_index_global   -1.06978    0.03356  -31.88
# sexemale            -64.46650   13.63158   -4.73
# rcs(age, 3)age        3.67565    1.64669    2.23
# rcs(age, 3)age'       4.37365    2.08083    2.10
# manualright          27.09014   19.85717    1.36


# Machine effects

A.lmer2 = lmer(RT1 ~ Hand +  Seq * Unc + trial_index_global + OS2 + Navigator2 + (1|Subject) + (1|finger_seq), data = cor2)
summary(A.lmer2)
plotLMER.fnc(A.lmer2)

#                               Estimate Std. Error t value
# (Intercept)                  667.97121   64.43174   10.37
# Handright                     -3.36064   14.97596   -0.22
# Seqv                          47.44003    4.34772   10.91
# Unc3                        -119.58489    3.37308  -35.45
# Seqv:Unc3                    -10.82484    5.48627   -1.97
# trial_index_global            -1.07587    0.03352  -32.10
# OS2OS X                       78.81698   37.00379    2.13
# OS2Windows                    55.77929   32.35572    1.72
# Navigator2Chrome             -47.91682   71.32351   -0.67
# Navigator2Firefox             42.28480   69.48908    0.61
# Navigator2Internet Explorer   93.77576   73.76683    1.27
# Navigator2Safari             -48.02853   76.70075   -0.63


# People + machine 
A.lmer3 = lmer(RT1 ~ Hand +  Seq * Unc + trial_index_global + sexe + rcs(age,3) + manual + OS2 + Navigator2 + (1|Subject) + (1|finger_seq), data = cor2)
summary(A.lmer3)
plotLMER.fnc(A.lmer3)

A.lmer4 = lmer(RT1 ~ Seq * Unc + trial_index_global + sexe + rcs(age,3) + OS2 + Navigator2 + (1|Subject) + (1|finger_seq), data = cor2)
summary(A.lmer4)


  ### IKI

   # rearrange data
col = c('Subject', 'part', 'block', 'trial_index', 'trial_index_global', 'stimulus', 'key_seq', 'finger_seq', 'Hand', 'Seq', 'Unc')
IKI1 = subset(cor, select = c(col, 'IKI1'))
IKI2 = subset(cor, select = c(col, 'IKI2'))
colnames(IKI1)[12] = 'IKI'
colnames(IKI2)[12] = 'IKI'
IKI1$Pos = factor(1)
IKI2$Pos = factor(2)
cor_long = rbind(IKI1, IKI2)

  # run models
iki.lmer = lmer(IKI ~ Hand + Seq * Unc * Pos + trial_index_global + (1|Subject) + (1|finger_seq), data = cor_long)
summary(iki.lmer)

# Results
#                 Estimate Std. Error t value
# (Intercept)        281.2043     5.4747   51.36
# Handright           -4.0006     7.3851   -0.54
# Seqv               -18.9229     1.5618  -12.12
# Unc3               -29.1934     1.4888  -19.61
# Pos2               -37.4723     1.4902  -25.15
# Seqv:Unc3           11.4102     2.1507    5.31
# Seqv:Pos2           21.9627     2.1050   10.43
# Unc3:Pos2           45.8004     2.1030   21.78 ** interest of interest
# Seqv:Unc3:Pos2     -16.9328     2.9668   -5.71
# trial_index_global  -0.3077     0.0105  -29.30

# Do we keep all these effects we don't really care about ??
# Add random slopes ?


cor_long$Subject = as.numeric(as.character(cor_long$Subject))
cor_long2 = merge(cor_long, allabout)

# People effects
iki.lmer2 = lmer(IKI ~ Hand + Seq * Unc * Pos + trial_index_global + sexe + rcs(age,3) + manual + (1|Subject) + (1|finger_seq), data = cor_long2)
summary(iki.lmer2)

# Results
#                     Estimate Std. Error t value
# (Intercept)        210.99823   26.69244   7.905
# Handright           -8.05653    6.37505  -1.264
# Seqv               -19.10456    1.59838 -11.952
# Unc3               -29.04277    1.49460 -19.432
# Pos2               -37.64420    1.49601 -25.163
# Seqv:Unc3           11.56613    2.18120   5.303
# Seqv:Pos2           21.92569    2.11329  10.375
# Unc3:Pos2           45.63770    2.11123  21.617 **
# Seqv:Unc3:Pos2     -17.13993    2.97835  -5.755
# trial_index_global  -0.30742    0.01054 -29.174
# sexemale           -37.77269    6.44239  -5.863
# rcs(age, 3)age       1.53407    0.77826   1.971
# rcs(age, 3)age'      2.69067    0.98342   2.736
# manualright          9.93654    9.38496   1.059

# Machine effects
iki.lmer3 = lmer(IKI ~ Hand + Seq * Unc * Pos + trial_index_global + OS2 + Navigator2 + (1|Subject) + (1|finger_seq), data = cor_long2)
summary(iki.lmer3)

# Results
#                             Estimate Std. Error t value
# (Intercept)                 229.15544   32.07201   7.145
# Handright                    -5.09759    7.38206  -0.691
# Seqv                        -18.77189    1.55682 -12.058
# Unc3                        -29.03606    1.49098 -19.474
# Pos2                        -37.40957    1.49226 -25.069
# Seqv:Unc3                    11.24923    2.14848   5.236
# Seqv:Pos2                    21.84015    2.10791  10.361
# Unc3:Pos2                    45.82546    2.10597  21.760 **
# Seqv:Unc3:Pos2              -16.90957    2.97099  -5.692
# trial_index_global           -0.30606    0.01052 -29.093
# OS2OS X                      16.05957   18.43717   0.871
# OS2Windows                   10.76024   16.12159   0.667
# Navigator2Chrome             25.74897   35.53964   0.725
# Navigator2Firefox            45.56196   34.62581   1.316
# Navigator2Internet Explorer  63.37644   36.75686   1.724
# Navigator2Safari             24.50057   38.21857   0.641
# 

# People + machine 

iki.lmer3 = lmer(IKI ~ Hand + Seq * Unc * Pos + trial_index_global + sexe + rcs(age,3) + manual + OS2 + Navigator2 + (1|Subject) + (1|finger_seq), data = cor_long2)
summary(iki.lmer3)


#### PLOT DATA ####

library(ggplot2)
source('/Volumes/SVETLANA/R/CLAVSEM/functionsplotswithin.R')

load('alldata_529_IKI.RData')

m = summarySEwithin(cor_long, 'IKI', withinvars = c('Pos', 'Unc'), idvar = 'Subject')

ggplot(m, aes(x=Pos, y=IKI, group = Unc, color = Unc)) +
  geom_line(size = 2) + geom_errorbar(width=.1, aes(ymin=IKI-ci, ymax=IKI+ci), size = 2) +
  geom_point(shape=21, size=6, fill="white") +
  theme_bw() + theme(legend.position = c(0.9,0.9)) + coord_cartesian(ylim = c(208, 242)) + xlab('IKI Position') + ylab('IKI (ms)') + labs(colour = 'Uncertainty') 

pdf('Inter_UncPos.pdf', width = 6, height =  6)
dev.off()
#

m = aggregate(IKI ~ Subject + Hand + Seq + Unc + Pos, FUN = mean, data = cor_long)

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
