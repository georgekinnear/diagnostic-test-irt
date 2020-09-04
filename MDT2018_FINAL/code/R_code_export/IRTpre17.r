
# Set plot size (for jupyter notebook)
options(repr.plot.width  = 4, repr.plot.height = 3)

library(RColorBrewer)
suppressWarnings(source('h_mdt.R'))

library(ltm)
library(mirt)
library(xtable)
library(reshape2)
library(ggplot2)
theme_set(theme_bw())
library(ggrepel)
library(ggpubr)
source('f_IRT.R')

library(psych)
library(plyr)

## ----- Section 3.1 (Local independence) -----
# Determine number of respones in each question
# Observe frequency and check pairwise association
(dsc <- descript(mdt_clean[1:20]))

# Explore degree of pairwise associations
if (file.exists('../robject/IRT_cortest.Rdata')) {
  load('../robject/IRT_cortest.Rdata')
} else {
  cortest <- rcor.test(mdt_clean[1:20], method="kendall")
  save(cortest, file="../robject/IRT_cortest.Rdata")
}
cortest_sorted <- as.data.frame(cortest$p.values[order(-cortest$p.values[,'pvals']),])
cortest_sorted[,1:2] <- apply(cortest_sorted[,1:2], c(1,2), function (x) {
    group_pre17$Item[x]
})
# LaTeX: tab:IRT_cortest
print(xtable(head(cortest_sorted), digits=c(0,0,0,4), align=rep('c',4)), include.rownames=F)
subset(cortest_sorted, V1=='Q1' & V2=='Q15')
cortest$cor.mat['Q1','Q15']

# pdf('../fig/IRT_cor_pre17.pdf', width=7, height=6)
mycor_plot(cortest, group_pre17)
# dev.off()

## ----- Section 3.2 Dimensionality (factor analysis) -----
# pdf('../fig/EFA_scree.pdf', width=7, height=4)
fp_pre17 <- fa.parallel(mdt_clean[1:20], fa='fa')
# dev.off()

# 1 factor
(efa_pre17_1 <- fa(mdt_clean[1:20], nfactors = 1))

efa_pre17_1_load <- data.frame(Type=group_pre17$Type, MR1=efa_pre17_1$loadings[,1])
efa_pre17_1_load$sig[efa_pre17_1_load$MR1 > 0.25] <- "*"
# LaTeX: tab:EFA_pre17_1
print(xtable(efa_pre17_1_load[1:10,], digits=c(0,0,3,0), align=c('l','c','c','c')))
print(xtable(efa_pre17_1_load[11:20,], digits=c(0,0,3,0), align=c('l','c','c','c')))

# pdf("../fig/EFA_pre17_1.pdf", width=7, height=2)
ggplot(efa_pre17_1_load, aes(x=MR1, y=rep(0,20), color=Type)) + 
  geom_point() + 
  geom_label_repel(aes(label = rownames(efa_pre17_1_load)), show.legend=F) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  labs(x='Factor 1', 
       title='Standardised Loadings', 
       subtitle='Based upon correlation matrix')
# dev.off()

# 2 factor
(efa_pre17_2 <- fa(mdt_clean[1:20], nfactors = 2))

efa_pre17_2_load <- data.frame(Type=group_pre17$Type, MR1=efa_pre17_2$loadings[,1], MR2=efa_pre17_2$loadings[,2])
# pdf("../fig/EFA_pre17_2.pdf", width=7, height=5)
ggplot(efa_pre17_2_load, aes(x=MR1, y=MR2, color=Type)) + 
  geom_point() + 
  geom_label_repel(aes(label = rownames(efa_pre17_2_load)), show.legend=F) + 
  coord_fixed() + 
  scale_y_continuous(limits=c(-0.15,0.7)) + 
  scale_x_continuous(limits=c(-0.15,0.7)) + 
  labs(x='Factor 1', y='Factor 2', 
       title='Standardised Loadings', 
       subtitle='Based upon correlation matrix')
# dev.off()

efa_pre17_2_load[efa_pre17_2_load < 0.25] <- NA
# LaTeX: tab:EFA_pre17_2
print(xtable(efa_pre17_2_load[1:10,], digits=c(0,0,3,3), align=c('l','c','c','c')))
print(xtable(efa_pre17_2_load[11:20,], digits=c(0,0,3,3), align=c('l','c','c','c')))

## ----- Section 3.3 Model selection -----
# 1PL
coef_pre17_1PL <- suppressMessages(mirt(mdt_clean[1:20]*multi, 1, itemtype="gpcm", SE=T, gpcm_mats = mats, pars='values'))
con <- coef_pre17_1PL$parnum[coef_pre17_1PL$name=="a1"]
(fit_pre17_1PL <- suppressMessages(mirt(mdt_clean[1:20]*multi, 1, itemtype="gpcm", SE=T, gpcm_mats = mats, constrain=list(con))))

options(repr.plot.width  = 6, repr.plot.height = 6)
# pdf('../fig/IRT_pre17_1PL.pdf', width=7, onefile=F)
myggplot.mirt(fit_pre17_1PL, multi)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# Goodness-of-fit
if (file.exists('../robject/IRT_pre17_1PL_bootgof.Rdata'))  {
    load('../robject/IRT_pre17_1PL_bootgof.Rdata')
} else {
    # Approx. 6 mins (consider running on remote server)
    gof_pre17_1PL <- mybootgof.mirt(fit_pre17_1PL, mats, B=199, list(con))
    save(gof_pre17_1PL, file="../robject/IRT_pre17_1PL_bootgof.Rdata")
}
gof_pre17_1PL

df <- itemfit(fit_pre17_1PL)
df$sig <- cut(df$p.S_X2, c(0,0.001,0.01,0.05,0.1,1))
levels(df$sig) <- c('***','**','*','.','')
# LaTeX: tab:IRT_pre17_1PL_itemfit
print(xtable(df[1:10,], digits=c(0,0,1,0,3,0), align=c('l','l','r','r','c','l')), include.rownames=F)
print(xtable(df[11:20,], digits=c(0,0,1,0,3,0), align=c('l','l','r','r','c','l')), include.rownames=F)

# GPCM
(fit_pre17_GPCM <- suppressMessages(mirt(mdt_clean[1:20]*multi, 1, itemtype="gpcm", SE=T, gpcm_mats = mats)))

options(repr.plot.width  = 6, repr.plot.height = 6)
# pdf('../fig/IRT_pre17_GPCM.pdf', width=7, onefile=F)
myggplot.mirt(fit_pre17_GPCM, multi)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# Goodness-of-fit
if (file.exists('../robject/IRT_pre17_GPCM_bootgof.Rdata'))  {
    load('../robject/IRT_pre17_GPCM_bootgof.Rdata')
} else {
    # Approx. 8 mins 20 secs (consider running on remote server)
    gof_pre17_GPCM <- mybootgof.mirt(fit_pre17_GPCM, mats, B=199)
    save(gof_pre17_GPCM, file="../robject/IRT_pre17_GPCM_bootgof.Rdata")
}
gof_pre17_GPCM

df <- itemfit(fit_pre17_GPCM)
df$sig <- cut(df$p.S_X2, c(0,0.001,0.01,0.05,0.1,1))
levels(df$sig) <- c('***','**','*','.','')
# LaTeX: tab:IRT_pre17_GPCM_itemfit
print(xtable(df[1:10,], digits=c(0,0,1,0,3,0), align=c('l','l','r','r','c','l')), include.rownames=F)
print(xtable(df[11:20,], digits=c(0,0,1,0,3,0), align=c('l','l','r','r','c','l')), include.rownames=F)

# LaTeX: tab:IRT_anova
print(xtable(anova(fit_pre17_1PL, fit_pre17_GPCM), digits=c(rep(0,9), 4), align=rep("c",10)))

## ----- Section 3.4.1 Difficulty and discrimination -----
# pdf("../fig/IRT_pre17_GPCM_irf.pdf", width=7, height=6)
# myggplot.irf(fit_pre17_GPCM, multi)
# dev.off()

# LaTeX: tab:IRT_pre17_expscore
irf_pre17 <- myexpected.mirt(fit_pre17_GPCM, z=c(-2,0,2))/multi
irf_pre17[1,] <- irf_pre17[1,]-irf_pre17[2,]
irf_pre17[3,] <- irf_pre17[3,]-irf_pre17[2,]
irf_pre17 <- rbind(irf_pre17, irf_pre17[3,]-irf_pre17[1,])
print(xtable(addmargins(t(irf_pre17), margin=1), digits=c(0,2,2,2,2), align=rep('c',5)))

# LaTeX: tab:IRT_pre17_scoreprop
print(xtable(do.call(rbind, lapply(mdt_clean[c(2,11,19,20)], function (x) {
    x <- factor(x, levels=c('0','1.25','2.5','3.75','5'))
    prop.table(table(x)) * 100
})), digits=c(0,1,1,1,1,1)))

## ----- Section 3.4.2 information -----
# pdf("../fig/IRT_pre17_GPCM_info.pdf", width=7, height=6)
# myggplot.info(fit_pre17_GPCM, multi)
# dev.off()

# pdf("../fig/IRT_pre17_GPCM_stackinfo.pdf", width=7, height=6)
myggplot.stackinfo(fit_pre17_GPCM)
# dev.off()

# LaTeX: tab:IRT_pre17_summarytif
majorinfo_sum <- mysummaryinfo.mirt(fit_pre17_GPCM, type='major', each=F)
totalinfo_sum <- mysummaryinfo.mirt(fit_pre17_GPCM, type='total', each=F)
print(xtable(cbind(majorinfo_sum, totalinfo_sum[2:4]), digits=c(0,rep(2,7))))

# LaTeX: tab:IRT_pre17_summaryiif
majorinfo_each <- mysummaryinfo.mirt(fit_pre17_GPCM, type='major')
totalinfo_each <- mysummaryinfo.mirt(fit_pre17_GPCM, type='total')
print(xtable(cbind(majorinfo_each, totalinfo_each[2:4]), digits=c(0,rep(2,7))))

# LaTeX: tab:IRT_pre17_info
info_pre17 <- rbind.fill(lapply(1:20, function (x) {
    myareainfo.mirt(fit_pre17_GPCM, zrange=c(-2,2), which.items=x)[c('Info', 'TotalInfo')]
}))
info_pre17_major <- data.frame(group_pre17, 
                               MajorInfo=info_pre17$Info, 
                               Prop=prop.table(info_pre17$Info)*100)
info_pre17_total <- data.frame(group_pre17, 
                               TotalInfo=info_pre17$TotalInfo, 
                               Prop=prop.table(info_pre17$TotalInfo)*100)
print(xtable(arrange(info_pre17_major, Prop)), include.rownames=F)
print(xtable(arrange(info_pre17_total, Prop)), include.rownames=F)

## ----- Section 3.4.3 (Ability estimates) -----
eap_pre17 <- fscores(fit_pre17_GPCM, full.scores.SE = T)

dtvseap.df <- data.frame(DT=mdt_clean$Total, 
                         EAP=eap_pre17[,'F1'], 
                         EAP.lower=eap_pre17[,'F1'] - eap_pre17[,'SE_F1']*qnorm(0.975), 
                         EAP.upper=eap_pre17[,'F1'] + eap_pre17[,'SE_F1']*qnorm(0.975))
expvseap.df <- data.frame(Exp=rowSums(myexpected.mirt(fit_pre17_GPCM)/multi), EAP=seq(-5,5,length=201))
# pdf('../fig/IRT_pre17_EAP.pdf', width=7, height=5)
# Dummy fill and col used, for separating legends
ggplot(dtvseap.df, aes(EAP, DT, fill='EAP & CI,\ngiven responses')) +
    geom_segment(aes(x=EAP.lower, xend=EAP.upper, y=DT, yend=DT), colour='gray', show.legend=T) + 
    geom_line(data=expvseap.df, aes(EAP, Exp, col='Expected score,\ngiven ability')) + 
    geom_point(shape=1) + 
    scale_colour_manual(values='red', '') + 
    scale_fill_manual(values=1, '') + 
    coord_cartesian(xlim=c(-4,4), ylim=c(0,100)) + 
    labs(y='Diagnostic Test')
# dev.off()

sum(mdt_clean$Total==100)
max(eap_pre17[,'F1'])

## ----- Section 3.5 (Academic growth)-----
fit_14 <- suppressMessages(mirt(mdt14_clean[1:20]*multi, 1, itemtype="gpcm", SE=T, gpcm_mats = mats))

# Convert response pattern to ordinal for fscore
# Add missing category for Q4 (0.5 marks -> cat 1)
resp_13 <- sapply(mdt13_clean[,1:20], function (x) {
    as.numeric(factor(x))-1
})
resp_13[resp_13[,4]>0, 4] <- resp_13[resp_13[,4]>0, 4] + 1
resp_15 <- sapply(mdt15_clean[,1:20], function (x) {
    as.numeric(factor(x))-1
})
resp_15[resp_15[,4]>0, 4] <- resp_15[resp_15[,4]>0, 4] + 1
resp_16 <- sapply(mdt16_clean[,1:20], function (x) {
    as.numeric(factor(x))-1
})
resp_16[resp_16[,4]>0, 4] <- resp_16[resp_16[,4]>0, 4] + 1

eap_13 <- fscores(fit_14, method='EAP', response.pattern = resp_13)
eap_14 <- fscores(fit_14, method='EAP', response.pattern = extract.mirt(fit_14, 'data'))
eap_15 <- fscores(fit_14, method='EAP', response.pattern = resp_15)
eap_16 <- fscores(fit_14, method='EAP', response.pattern = resp_16)

joint_13 <- merge(mdt13_clean, demogr, by='AnonID', all.x=T)
joint_14 <- merge(mdt14_clean, demogr, by='AnonID', all.x=T)
joint_15 <- merge(mdt15_clean, demogr, by='AnonID', all.x=T)
joint_16 <- merge(mdt16_clean, demogr, by='AnonID', all.x=T)

joint_13$F1 <- eap_13[,'F1']
joint_13$SE_F1 <- eap_13[,'SE_F1']
joint_13$Year <- 2013
joint_14$F1 <- eap_14[,'F1']
joint_14$SE_F1 <- eap_14[,'SE_F1']
joint_14$Year <- 2014
joint_15$F1 <- eap_15[,'F1']
joint_15$SE_F1 <- eap_15[,'SE_F1']
joint_15$Year <- 2015
joint_16$F1 <- eap_16[,'F1']
joint_16$SE_F1 <- eap_16[,'SE_F1']
joint_16$Year <- 2016

eap.df <- rbind(joint_13, joint_14, joint_15, joint_16)
# pdf("../fig/GRTH_density.pdf", width=7, height=4)
ggplot(eap.df, aes(F1, fill=as.factor(Year), colour=as.factor(Year))) +
    geom_density(alpha=0.5) + 
    scale_x_continuous(limits=c(-3.5,3.5)) +
    labs(title="Density plot", 
         subtitle="Ability grouped by year of taking the test", 
         x="Ability", 
         fill='Year', colour='Year')
# dev.off()

eap.dsc <- rbind(describe(eap.df[eap.df$Year==2013,]$F1), 
                 describe(eap.df[eap.df$Year==2014,]$F1), 
                 describe(eap.df[eap.df$Year==2015,]$F1), 
                 describe(eap.df[eap.df$Year==2016,]$F1))[,c(2,3,4,8,9,11,12)]
rownames(eap.dsc) <- 2013:2016
print(xtable(eap.dsc, digits=c(0,0,2,2,2,2,2,2)))

ks1314 <- ks.test(eap_13[,'F1'], eap_14[,'F1'])
ks1415 <- ks.test(eap_14[,'F1'], eap_15[,'F1'])
ks1516 <- ks.test(eap_15[,'F1'], eap_16[,'F1'])
ks.df <- data.frame(statistic=c(ks1314$statistic, ks1415$statistic, ks1516$statistic), 
                    p.value=c(ks1314$p.value, ks1415$p.value, ks1516$p.value))
rownames(ks.df) <- c('13/14', '14/15', '15/16')
print(xtable(ks.df, digits=c(0,4,4)))

ks1314

ggplot(subset(eap.df, School=='Mathematics'), aes(F1, fill=as.factor(Year), colour=as.factor(Year))) +
    geom_density(alpha=0.5) + 
    scale_x_continuous(limits=c(-3.5,3.5)) +
    labs(title="Density plot", 
         subtitle="Ability grouped by year of taking the test", 
         x="Ability", 
         fill='Year', colour='Year')

ks.test(joint_13$F1[joint_13$School=='Mathematics'], joint_14$F1[joint_14$School=='Mathematics'])
ks.test(joint_14$F1[joint_14$School=='Mathematics'], joint_15$F1[joint_15$School=='Mathematics'])
ks.test(joint_15$F1[joint_15$School=='Mathematics'], joint_16$F1[joint_16$School=='Mathematics'])

anova(lm(F1 ~ as.factor(Year), data=subset(eap.df, School=='Mathematics' & (Year==2015 | Year==2016))))

anova(lm(F1 ~ as.factor(Year), data=subset(eap.df, Year==2015|Year==2016)))

library(plyr)
library(reshape2)
source('h_joindata.R')

?ks.test

eap.df <- data.frame(rbind(eap_13, eap_14, eap_15, eap_16)[,c('F1', 'SE_F1')])
eap.df <- cbind(eap.df, Year=c(rep(2013, nrow(eap_13)), 
                               rep(2014, nrow(eap_14)), 
                               rep(2015, nrow(eap_15)), 
                               rep(2016, nrow(eap_16))))
# pdf("../fig/GRTH_density.pdf", width=7, height=4)
ggplot(eap.df, aes(F1, fill=as.factor(Year), colour=as.factor(Year))) +
    geom_density(alpha=0.5) + 
    scale_x_continuous(limits=c(-3.5,3.5)) +
    labs(title="Density plot", 
         subtitle="Ability grouped by year of taking the test", 
         x="Ability", 
         fill='Year', colour='Year')
# dev.off()
