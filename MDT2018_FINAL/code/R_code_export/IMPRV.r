
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

source('h_gpcmcoef.R')

library(psych)
library(plyr)

## ----- Section 4.2.1 Local independence -----
# Explore degree of pairwise associations
if (file.exists('../robject/IMPRV_cortest17.Rdata')) {
  load('../robject/IMPRV_cortest17.Rdata')
} else {
  cortest17 <- rcor.test(mdt17_clean[1:20], method="kendall")
  save(cortest17, file="../robject/IMPRV_cortest17.Rdata")
}
cortest17_sorted <- as.data.frame(cortest17$p.values[order(-cortest17$p.values[,'pvals']),])
cortest17_sorted[,1:2] <- apply(cortest17_sorted[,1:2], c(1,2), function (x) {
    group_17$Item[x]
})
# LaTeX: tab:IMPRV_cortest
print(xtable(cortest17_sorted[1:6,], digits=c(0,0,0,4), align=rep('c',4)), include.rownames=F)

# cor.test(c(mdt13_clean[,1]), c(mdt13_clean[,15]), method='kendall')
# cor.test(c(mdt14_clean[,1]), c(mdt14_clean[,15]), method='kendall')
# cor.test(c(mdt15_clean[,1]), c(mdt15_clean[,15]), method='kendall')
# cor.test(c(mdt16_clean[,1]), c(mdt16_clean[,15]), method='kendall')
# cor.test(c(mdt17_clean[,1]), c(mdt17_clean[,15]), method='kendall')

# pdf('../fig/IMPRV_cor_17.pdf', width=7, height=6)
mycor_plot(cortest17, group_17)
# dev.off()

## ----- Section 4.2.2 Dimensionality -----
# pdf('../fig/IMPRV_EFA_scree.pdf', width=7, height=4)
fp_17 <- fa.parallel(mdt17_clean[1:20], fm='minres', fa='fa')
# dev.off()

# 1 factor
(efa_17_1 <- fa(mdt17_clean[1:20], nfactors = 1))

efa_17_1_load <- data.frame(Type=group_17$Type, MR1=efa_17_1$loadings[,1])
efa_17_1_load$sig[efa_17_1_load$MR1 > 0.25] <- "*"
# LaTeX: tab:IMPRV_EFA_17_1
print(xtable(efa_17_1_load[1:10,], digits=c(0,0,3,0), align=c('l','c','c','c')))
print(xtable(efa_17_1_load[11:20,], digits=c(0,0,3,0), align=c('l','c','c','c')))

# pdf("../fig/IMPRV_EFA_17_1.pdf", width=7, height=2)
ggplot(efa_17_1_load, aes(x=MR1, y=rep(0,20), color=Type)) + 
  geom_point() + 
  geom_label_repel(aes(label = rownames(efa_17_1_load)), show.legend=F) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  labs(x='Factor 1', 
       title='Standardised Loadings', 
       subtitle='Based upon correlation matrix')
# dev.off()

# 2 factor
(efa_17_2 <- fa(mdt17_clean[1:20], nfactors = 2))

efa_17_2_load <- data.frame(Type=group_17$Type, MR1=efa_17_2$loadings[,1], MR2=efa_17_2$loadings[,2])
# pdf("../fig/IMPRV_EFA_17_2.pdf", width=7, height=5)
ggplot(efa_17_2_load, aes(x=MR1, y=MR2, color=Type)) + 
  geom_point() + 
  geom_label_repel(aes(label = rownames(efa_17_2_load)), show.legend=F) + 
  coord_fixed() + 
  scale_y_continuous(limits=c(-0.15,0.75)) + 
  scale_x_continuous(limits=c(-0.15,0.75)) + 
  labs(x='Factor 1', y='Factor 2', 
       title='Standardised Loadings', 
       subtitle='Based upon correlation matrix')
# dev.off()

efa_17_2_load[efa_17_2_load < 0.25] <- NA
# LaTeX: tab:IMPRV_EFA_17_2
print(xtable(efa_17_2_load[1:10,], digits=c(0,0,3,3), align=c('l','c','c','c')))
print(xtable(efa_17_2_load[11:20,], digits=c(0,0,3,3), align=c('l','c','c','c')))

## ----- Section 4.3 Separate calibration -----
(fit_17 <- suppressMessages(mirt(round(mdt17_clean[1:20]*multi), 1, itemtype="gpcm", SE=T, gpcm_mats = mats17)))

options(repr.plot.width  = 6, repr.plot.height = 6)
# pdf('../fig/IMPRV_17.pdf', width=7, onefile=F)
myggplot.mirt(fit_17, multi, col=pal17)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

sameq <- group_17[!grepl("N", qnum2017),]
addedq <- group_17[grepl('N',qnum2017),]
removedq <- group_pre17[!(paste0('Q',1:20) %in% qnum2017[!grepl('N',qnum2017)]),]

a <- lapply(coef(fit_17)[as.character(sameq$Item)], function (x) {
    x[1, grep('d', colnames(x))]
})
b <- lapply(coef(fit_pre17_GPCM)[as.character(sameq$Item)], function (x) {
    x[1, grep('d', colnames(x))]
})

df <- data.frame(sameq[rep(seq_len(nrow(sameq)), sapply(a, length)),], 
                 DT17=unlist(a), Pre17=unlist(b))
# pdf('../fig/IMPRV_SC_lm.pdf', width=7, height=5)
ggplot(df, aes(Pre17, DT17)) + 
    geom_point(aes(col=Type)) + 
    geom_smooth(method='lm') + 
    coord_fixed(xlim=c(-4.5,4.5), ylim=c(-4.5,4.5)) + 
    labs(x='MDT2-GPCM', y='MDT3-GPCM')
# dev.off()

SC_lm <- lm(DT17 ~ Pre17, data=df)
summary(SC_lm)

coefs_SC <- mycoef.mirt(fit_17)

coefs_SC$d <- lapply(mycoef.mirt(fit_17)$d, function (x) {
                predict(SC_lm, data.frame(Pre17=x))
            })

options(repr.plot.width  = 6, repr.plot.height = 6)
# pdf('../fig/IMPRV_17_SC.pdf', width=7, onefile=F)
myggplot.mirt(fit_17, multi, coefs=coefs_SC, col=pal17)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

## ----- Section 4.4 FCIP -----
coef_b <- suppressMessages(mirt(round(mdt17_clean[1:20]*multi), 1, itemtyp="gpcm", gpcm_mats = mats17, pars="values"))
for (i in 1:nrow(sameq)) {
  coef_b[coef_b$item==as.character(sameq$Item[i]),]$value <- c(coef(fit_pre17_GPCM)[[as.character(sameq$Item[i])]])
  coef_b[coef_b$item==as.character(sameq$Item[i]),]$est <- FALSE
}

fit_17_fcip <- suppressMessages(mirt(round(mdt17_clean[1:20]*multi), 1, itemtyp="gpcm", gpcm_mats = mats17, pars=coef_b))

options(repr.plot.width  = 6, repr.plot.height = 6)
# pdf('../fig/IMPRV_17_FCIP.pdf', width=7, onefile=F)
myggplot.mirt(fit_17_fcip, multi, col=pal17)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

pal_cf <- palette()[c(21:25,2,4,8,11,12)]

z <- seq(-5,5,length=201)

y1 <- myinfo.mirt(fit_17_fcip)[,as.character(addedq$Item)]
y2 <- myinfo.mirt(fit_pre17_GPCM)[,as.character(removedq$Item)]
addedq_iif <- data.frame(z, y1)
removedq_iif <- data.frame(z, y2)
addedq_iif <- melt(addedq_iif, 'z')
removedq_iif <- melt(removedq_iif, 'z')
fcip_cf_iif <- rbind(addedq_iif, removedq_iif)

y3 <- myexpected.mirt(fit_17_fcip)[,as.character(addedq$Item)]/multi
y4 <- myexpected.mirt(fit_pre17_GPCM)[,as.character(removedq$Item)]/multi
addedq_irf <- data.frame(z, y3)
removedq_irf <- data.frame(z, y4)
addedq_irf <- melt(addedq_irf, 'z')
removedq_irf <- melt(removedq_irf, 'z')
fcip_cf_irf <- rbind(addedq_irf, removedq_irf)

options(repr.plot.width  = 6, repr.plot.height = 3)
# pdf('../fig/IMPRV_FCIP_cf.pdf', width=7, height=4, onefile=F)
p1 <- ggplot(fcip_cf_irf, aes(z, value, col=variable)) + 
    geom_line(aes(lty=variable), size=0.75) + 
    scale_colour_manual(name='', values=pal_cf) + 
    scale_linetype_manual(name='', values=rep(c(1,2),each=5)) + 
    coord_cartesian(xlim=c(-4,4), ylim=c(0,5)) +
    labs(title='Item Response Curves', x='Ability', y='Expected Score')
p2 <- ggplot(fcip_cf_iif, aes(z, value, col=variable)) + 
    geom_line(aes(lty=variable), size=0.75) + 
    scale_colour_manual(name='', values=pal_cf) + 
    scale_linetype_manual(name='', values=rep(c(1,2),each=5)) + 
    coord_cartesian(xlim=c(-4,4)) +
    labs(title='Item Information Curves', x='Ability', y='Information')
ggarrange(p1, p2, common.legend = T, legend='bottom')
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# LaTeX: tab:IMPRV_FCIP_expscore
irf_rm <- (myexpected.mirt(fit_pre17_GPCM, z=c(-2,0,2))[,as.character(removedq$Item)]/multi)[,c(2,5,1,3,4)]
irf_add <- (myexpected.mirt(fit_17_fcip, z=c(-2,0,2))[,as.character(addedq$Item)]/multi)[,c(1,5,2,3,4)]
irf_rm[1,] <- irf_rm[1,]-irf_rm[2,]
irf_rm[3,] <- irf_rm[3,]-irf_rm[2,]
irf_rm <- rbind(irf_rm, irf_rm[3,]-irf_rm[1,])
irf_add[1,] <- irf_add[1,]-irf_add[2,]
irf_add[3,] <- irf_add[3,]-irf_add[2,]
irf_add <- rbind(irf_add, irf_add[3,]-irf_add[1,])
print(xtable(rbind(addmargins(t(irf_add), margin=1), addmargins(t(irf_rm), margin=1)), 
             digits=c(0,2,2,2,2), align=rep('c',5)))

# LaTeX: tab:IMPRV_FCIP_summaryiif
majorinfo_each <- rbind(mysummaryinfo.mirt(fit_17_fcip, which.items = grep("N", qnum2017)[c(1,5,2,3,4)], type='major'), 
                        mysummaryinfo.mirt(fit_pre17_GPCM, which.items = c(4,12,2,8,11), type='major'))
totalinfo_each <- rbind(mysummaryinfo.mirt(fit_17_fcip, which.items = grep("N", qnum2017)[c(1,5,2,3,4)], type='total'), 
                        mysummaryinfo.mirt(fit_pre17_GPCM, which.items = c(4,12,2,8,11), type='total'))
print(xtable(cbind(majorinfo_each, totalinfo_each[2:4]), digits=c(0,rep(2,7))))

z <- seq(-5, 5, length=201)
fcip_cf_tif <- data.frame(z=z, 
                          Removed=rowSums(myinfo.mirt(fit_pre17_GPCM)[,c(2,4,8,11,12)]), 
                          Added=rowSums(myinfo.mirt(fit_17_fcip)[,c(3,7,10,11,12)]))
fcip_cf_tif <- melt(fcip_cf_tif, "z")
# pdf("../fig/IMPRV_FCIP_tifcf.pdf", width=7, height=4)
ggplot(fcip_cf_tif, aes(z, value, fill=variable)) + 
    geom_area(position="identity", alpha=0.5) + 
    geom_line(aes(col=variable)) + 
    coord_cartesian(xlim=c(-4,4)) + 
    labs(title='Test Information Curve', x='Ability', y='Information', fill='Question Set', colour='Question Set')
# dev.off()

# LaTeX: tab:IRT_pre17_summarytif
majorinfo_sum <- mysummaryinfo.mirt(fit_17_fcip, type='major', each=F)
totalinfo_sum <- mysummaryinfo.mirt(fit_17_fcip, type='total', each=F)
print(xtable(cbind(majorinfo_sum, totalinfo_sum[2:4]), digits=c(0,rep(2,7))))

# LaTeX: tab:IMPRV_FCIP_info
info_fcip <- rbind.fill(lapply(1:20, function (x) {
    myareainfo.mirt(fit_17_fcip, zrange=c(-2,2), which.items=x)[c('Info', 'TotalInfo')]
}))
info_pre17 <- myareainfo.mirt(fit_pre17_GPCM, zrange=c(-2,2))
info_fcip_major <- data.frame(group_17, 
                              MajorInfo=info_fcip$Info)
info_fcip_total <- data.frame(group_17, 
                              TotalInfo=info_fcip$TotalInfo)
info_fcip_major$Prop <- info_fcip_major$MajorInfo / info_pre17$Info * 100
info_fcip_total$Prop <- info_fcip_total$TotalInfo / info_pre17$TotalInfo * 100
print(xtable(arrange(info_fcip_major, Prop)), include.rownames=F)
print(xtable(arrange(info_fcip_total, Prop)), include.rownames=F)
# Add manually
sum(info_fcip_major$Prop)
sum(info_fcip_total$Prop)

eap_pre17 <- as.data.frame(fscores(fit_pre17_GPCM, method='EAP', full.scores.SE = T))
eap_17_fcip <- as.data.frame(fscores(fit_17_fcip, method='EAP', full.scores.SE = T))
eap_pre17$Test <- 'MDT2'
eap_17_fcip$Test <- 'MDT3'
eap.df <- rbind(eap_pre17, eap_17_fcip)
eap.df$Test <- factor(eap.df$Test, levels=c('MDT2', 'MDT3'))
# pdf("../fig/IMPRV_FCIP_eap.pdf", width=7, height=4)
ggplot(eap.df, aes(F1, colour=Test, fill=Test)) + 
    geom_density(alpha=0.5) + 
    scale_x_continuous(limits=c(-3.5,3.5)) +
    labs(title="Density plot", 
         subtitle="Ability grouped by test taken", 
         x="Ability")
# dev.off()

eap.dsc <- rbind(describe(eap.df[eap.df$Test=='MDT2',]$F1), 
                 describe(eap.df[eap.df$Test=='MDT3',]$F1))[,c(2,3,4,8,9,11,12)]
rownames(eap.dsc) <- c('MDT2', 'MDT3')
print(xtable(eap.dsc, digits=c(0,0,2,2,2,2,2,2)))

ks.test(eap_pre17[,'F1'], eap_17_fcip[,'F1'])

## ----- Section 4.5 Concurrent -----
mdt_cp <- as.data.frame(testEquatingData(list(mdt_clean[1:20], mdt17_clean[1:20])))

catg_cp <- lapply(mdt_cp, function (x) {sort(unique(x))})
mats_cp <- sapply(catg_cp, function (x) {
  as.matrix(round(x * multi))
})
pal_cp <- palette()[c(1,3,5,6,7,9,10,13:20,2,4,8,11,12,21:25)]

fit_17_cp <- suppressMessages(mirt(round(as.matrix(mdt_cp)*multi), 1, itemtyp="gpcm", gpcm_mats = mats_cp))

myggplot.mirt.cp <- function (x, multi = 1, zrange = c(-5, 5), z = seq(zrange[1], zrange[2], length = 201), coefs=NULL, find.avg = FALSE, labels = NULL, legend = FALSE, xlab="Ability", cx = "top", cy = NULL, ncol = 1, bty = "n", col = palette(), lty = 1, pch, lwd = 1, cex = par("cex"), ...) {
    nitems <- extract.mirt(x, 'nitems')
    z0 <- which(z==0)
    
    y1 <- myexpected.mirt(x, zrange, coefs=coefs)/multi
    y2 <- myinfo.mirt(x, zrange, coefs=coefs)
    
    irf.df <- data.frame(z, y1)
    iif.df <- data.frame(z, y2)
        
    irf.df <- melt(irf.df, 'z')
    iif.df <- melt(iif.df, 'z')
    
    trf.df <- data.frame(z, old=rowSums(y1[,c(1:20)]), new=rowSums(y1[,c(1:15,21:25)]))
    tif.df <- data.frame(z, old=rowSums(y2[,c(1:20)]), new=rowSums(y2[,c(1:15,21:25)]))
    
    trf.df <- melt(trf.df, 'z')
    tif.df <- melt(tif.df, 'z')

    p1 <- ggplot(irf.df, aes(z, value, col=variable)) + 
            geom_line(size=0.75) + 
            scale_color_manual(values=col, name='') +
            coord_cartesian(ylim=c(0,5), xlim=c(-4,4)) +
            labs(title='Item Response Curves', x='Ability', y='Expected Score')
    p2 <- ggplot(trf.df, aes(z, value, col=variable)) + 
            geom_line(size=0.75) +
            geom_point(data=subset(trf.df, z==0), aes(z, value, col=variable)) + 
#             geom_segment(aes(x=0, y=sums[z0], xend=0, yend=-5), size=0.25, linetype=3) +
#             geom_segment(aes(x=0, y=sums[z0], xend=-5, yend=sums[z0]), size=0.25, linetype=3) + 
            geom_label_repel(data=subset(trf.df, z==0), label=paste0('(',0,',',round(subset(trf.df, z==0)$value,2),')'), hjust=-0.25, vjust=1, show.legend=F) + 
            coord_cartesian(ylim=c(0,100), xlim=c(-4,4)) +
            labs(title='Test Response Curve', x='Ability', y='Expected Total Score', col='MDT') + 
            geom_label(aes(3.3, 19, label='MDT2', col=factor('old'))) + 
            geom_label(aes(3.3, 5, label='MDT3', col=factor('new')))
    p3 <- ggplot(iif.df, aes(z, value, col=variable)) +
            geom_line(size=0.75) +
            scale_color_manual(values=col) + 
            scale_y_continuous(limits=c(0, NA)) +
            coord_cartesian(xlim=c(-4,4)) +
            labs(title='Item Information Curves', x='Ability', y='Information')
    p4 <- ggplot(tif.df, aes(z, value, col=variable)) +
            geom_line(size=0.75) + 
            scale_y_continuous(limits=c(0, NA)) +
            coord_cartesian(xlim=c(-4,4)) +
            labs(title='Test Information Curve', x='Ability', y='Information', col='MDT') + 
            geom_label(aes(3.3, 6.5, label='MDT2', col=factor('old'))) + 
            geom_label(aes(3.3, 5.5, label='MDT3', col=factor('new')))

    ggarrange(p1, p2, p3, p4, ncol=2, nrow=2, common.legend=T, legend='bottom', align='v')
}

options(repr.plot.width  = 6, repr.plot.height = 6)
# pdf('../fig/IMPRV_17_CP.pdf', width=7, onefile=F)
myggplot.mirt.cp(fit_17_cp, multi, col=pal_cp)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

z <- seq(-5,5,length=201)

y1 <- myinfo.mirt(fit_17_cp)[,16:20]
y2 <- myinfo.mirt(fit_17_cp)[,21:25]
oldtest_iif <- data.frame(z, y1)
newtest_iif <- data.frame(z, y2)
oldtest_iif <- melt(oldtest_iif, 'z')
newtest_iif <- melt(newtest_iif, 'z')
cp_iif <- rbind(newtest_iif, oldtest_iif)

y3 <- myexpected.mirt(fit_17_cp)[,16:20]/multi
y4 <- myexpected.mirt(fit_17_cp)[,21:25]/multi
oldtest_irf <- data.frame(z, y3)
newtest_irf <- data.frame(z, y4)
oldtest_irf <- melt(oldtest_irf, 'z')
newtest_irf <- melt(newtest_irf, 'z')
cp_irf <- rbind(newtest_irf, oldtest_irf)

options(repr.plot.width  = 6, repr.plot.height = 3)
# pdf('../fig/IMPRV_CP_cf.pdf', width=7, height=4, onefile=F)
p1 <- ggplot(cp_irf, aes(z, value, col=variable)) + 
    geom_line(aes(lty=variable), size=0.75) + 
    scale_colour_manual(name='', values=pal_cf) + 
    scale_linetype_manual(name='', values=rep(c(1,2),each=5)) + 
    coord_cartesian(xlim=c(-4,4), ylim=c(0,5)) +
    labs(title='Item Response Curves', x='Ability', y='Expected Score')
p2 <- ggplot(cp_iif, aes(z, value, col=variable)) + 
    geom_line(aes(lty=variable), size=0.75) + 
    scale_colour_manual(name='', values=pal_cf) + 
    scale_linetype_manual(name='', values=rep(c(1,2),each=5)) + 
    coord_cartesian(xlim=c(-4,4)) +
    labs(title='Item Information Curves', x='Ability', y='Information')
ggarrange(p1, p2, common.legend = T, legend='bottom')
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# LaTeX: tab:IMPRV_CP_info
info_cp <- rbind.fill(lapply(1:25, function (x) {
    myareainfo.mirt(fit_17_cp, zrange=c(-2,2), which.items=x)[c('Info', 'TotalInfo')]
}))
group_17_cp <- rbind(group_pre17[-c(2,4,8,11,12),], group_pre17[c(2,4,8,11,12),], group_17[c(3,7,10,11,12),])
info_fcip_major <- data.frame(group_17_cp, 
                              MajorInfo=info_cp$Info, 
                              Prop=prop.table(info_cp$Info)*100)
info_fcip_total <- data.frame(group_17_cp, 
                              TotalInfo=info_cp$TotalInfo, 
                              Prop=prop.table(info_cp$TotalInfo)*100)
print(xtable(arrange(info_fcip_major, Prop)), include.rownames=F)
print(xtable(arrange(info_fcip_total, Prop)), include.rownames=F)

eap.df <- as.data.frame(fscores(fit_17_cp, method='EAP', full.scores.SE=T))
eap.df$Test <- rep(c('MDT2', 'MDT3'), c(nrow(mdt_clean), nrow(mdt17_clean)))
# eap.df$Test <- factor(eap.df$Year, levels=c('MDT2', 'MDT3')) # reorder factors
# pdf("../fig/IMPRV_CP_eap.pdf", width=7, height=4)
ggplot(eap.df, aes(F1, colour=Test, fill=Test)) + 
    geom_density(alpha=0.5) + 
    scale_x_continuous(limits=c(-3.5,3.5)) +
    labs(title="Density plot", 
         subtitle="Ability grouped by test taken", 
         x="Ability")
# dev.off()

eap.dsc <- rbind(describe(eap.df[eap.df$Test=='MDT2',]$F1), 
                 describe(eap.df[eap.df$Test=='MDT3',]$F1))[,c(2,3,4,8,9,11,12)]
rownames(eap.dsc) <- c('MDT2', 'MDT3')
print(xtable(eap.dsc, digits=c(0,0,2,2,2,2,2,2)))
