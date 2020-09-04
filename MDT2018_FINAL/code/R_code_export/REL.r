
# Set plot size (for jupyter notebook)
options(repr.plot.width  = 4, repr.plot.height = 3)

library(RColorBrewer)
suppressWarnings(source('h_mdt.R'))

library(plyr)
library(reshape2)
source('h_joindata.R')

library(ggplot2)
theme_set(theme_bw())
library(scales)
library(ggpubr)
library(xtable)
source('f_REL.R')

library(mirt)
source('h_gpcmcoef.R')

joint_pre17 <- merge(joint_pre17, eap_pre17, by='AnonID', all.x=T)
joint_17 <- merge(joint_17, eap_17, by='AnonID', all.x=T)

joint_pre17_py <- joint_pre17
joint_pre17_py$School <- as.numeric(joint_pre17$School)
joint_pre17_py$Region <- as.numeric(joint_pre17$Region)
joint_17_py <- joint_17
joint_17_py$School <- as.numeric(joint_17$School)
joint_17_py$Region <- as.numeric(joint_17$Region)

for (i in 1:nrow(joint_pre17)) {
    if (is.na(joint_pre17$Y1Fs[i])) {
        joint_pre17_py$Y1Fs.bin[i] <- NA_character_
    } else if (joint_pre17$Y1Fs[i]<=1) {
        joint_pre17_py$Y1Fs.bin[i] <- 'Not-At-Risk'
    } else {
        joint_pre17_py$Y1Fs.bin[i] <- 'At-Risk'
    }
}
for (i in 1:nrow(joint_17)) {
    if (is.na(joint_17$Y1Fs[i])) {
        joint_17_py$Y1Fs.bin[i] <- NA_character_
    } else if (joint_17$Y1Fs[i]<=1) {
        joint_17_py$Y1Fs.bin[i] <- 'Not-At-Risk'
    } else {
        joint_17_py$Y1Fs.bin[i] <- 'At-Risk'
    }
}

# For Python use
write.csv(joint_pre17_py, file='../robject/joint_pre17.csv', row.names=F)
write.csv(joint_17_py, file='../robject/joint_17.csv', row.names=F)

## ----- Section 5.2 DT/ability vs outcomes -----
rsq1 <- summary(lm(ILA~Total, data=joint_pre17))$r.squared
rsq2 <- summary(lm(CAP~Total, data=joint_pre17))$r.squared
rsq3 <- summary(lm(PPS~Total, data=joint_pre17))$r.squared
rsq4 <- summary(lm(ILA~Total, data=joint_17))$r.squared
rsq5 <- summary(lm(CAP~Total, data=joint_17))$r.squared
rsq6 <- summary(lm(PPS~Total, data=joint_17))$r.squared
p1 <- ggplot(joint_pre17, aes(Total, ILA)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=20, y=98, label=paste0('R^2=',round(rsq1,4)), size=3) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT2')
p2 <- ggplot(joint_pre17, aes(Total, CAP)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=20, y=98, label=paste0('R^2=',round(rsq2,4)), size=3) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT2')
p3 <- ggplot(joint_pre17, aes(Total, PPS)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=20, y=98, label=paste0('R^2=',round(rsq3,4)), size=3) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT2')
p4 <- ggplot(joint_17, aes(Total, ILA)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=20, y=98, label=paste0('R^2=',round(rsq4,4)), size=3) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT3')
p5 <- ggplot(joint_17, aes(Total, CAP)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=20, y=98, label=paste0('R^2=',round(rsq5,4)), size=3) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT3')
p6 <- ggplot(joint_17, aes(Total, PPS)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=20, y=98, label=paste0('R^2=',round(rsq6,4)), size=3) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT3')
options(repr.plot.width  = 8, repr.plot.height = 6)
# pdf('../fig/REL_scatter_DT.pdf', width=7, height=5, onefile=F)
ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

rsq1 <- summary(lm(ILA~F1, data=joint_pre17))$r.squared
rsq2 <- summary(lm(CAP~F1, data=joint_pre17))$r.squared
rsq3 <- summary(lm(PPS~F1, data=joint_pre17))$r.squared
rsq4 <- summary(lm(ILA~F1, data=joint_17))$r.squared
rsq5 <- summary(lm(CAP~F1, data=joint_17))$r.squared
rsq6 <- summary(lm(PPS~F1, data=joint_17))$r.squared
p1 <- ggplot(joint_pre17, aes(F1, ILA)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=-1.5, y=98, label=paste0('R^2=',round(rsq1,4)), size=3) + 
    coord_cartesian(xlim=c(-2.5,2.5), ylim=c(0,100)) + 
    labs(x='Ability (MDT2)')
p2 <- ggplot(joint_pre17, aes(F1, CAP)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=-1.5, y=98, label=paste0('R^2=',round(rsq2,4)), size=3) + 
    coord_cartesian(xlim=c(-2.5,2.5), ylim=c(0,100)) + 
    labs(x='Ability (MDT2)')
p3 <- ggplot(joint_pre17, aes(F1, PPS)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=-1.5, y=98, label=paste0('R^2=',round(rsq3,4)), size=3) + 
    coord_cartesian(xlim=c(-2.5,2.5), ylim=c(0,100)) + 
    labs(x='Ability (MDT2)')
p4 <- ggplot(joint_17, aes(F1, ILA)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=-1.5, y=98, label=paste0('R^2=',round(rsq4,4)), size=3) + 
    coord_cartesian(xlim=c(-2.5,2.5), ylim=c(0,100)) + 
    labs(x='Ability (MDT3)')
p5 <- ggplot(joint_17, aes(F1, CAP)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=-1.5, y=98, label=paste0('R^2=',round(rsq5,4)), size=3) + 
    coord_cartesian(xlim=c(-2.5,2.5), ylim=c(0,100)) + 
    labs(x='Ability (MDT3)')
p6 <- ggplot(joint_17, aes(F1, PPS)) + 
    geom_point(size=0.5) + 
    geom_smooth(method='lm') + 
    annotate('text', x=-1.5, y=98, label=paste0('R^2=',round(rsq6,4)), size=3) + 
    coord_cartesian(xlim=c(-2.5,2.5), ylim=c(0,100)) + 
    labs(x='Ability (MDT3)')
options(repr.plot.width  = 8, repr.plot.height = 6)
# pdf('../fig/REL_scatter_Ability.pdf', width=7, height=5, onefile=F)
ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# LaTeX: tab:REL_ILAGrade_vs_MDT
df <- subset(joint_pre17, select=c('AnonID', 'Total', 'ILA.Grade'))
df <- df[!is.na(df$ILA.Grade),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p1 <- stack_prop_tiles(df, MDTInterval, ILA.Grade, xlab='MDT2 (2013-2016)', fac='ILA Grade')

df <- subset(joint_17, select=c('AnonID', 'Total', 'ILA.Grade'))
df <- df[!is.na(df$ILA.Grade),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p2 <- stack_prop_tiles(df, MDTInterval, ILA.Grade, xlab='MDT3 (2017)', fac='ILA Grade')

p <- ggarrange(p1, p2, common.legend=T, legend='bottom')

options(repr.plot.width  = 6, repr.plot.height = 3)
# pdf('../fig/REL_ILAGrade_vs_MDT.pdf', width=7, height=4.5)
annotate_figure(p, top=text_grob('Introduction to Linear Algebra', face='bold', size=15))
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# LaTeX: tab:REL_CAPGrade_vs_MDT
df <- subset(joint_pre17, select=c('AnonID', 'Total', 'CAP.Grade'))
df <- df[!is.na(df$CAP.Grade),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p1 <- stack_prop_tiles(df, MDTInterval, CAP.Grade, xlab='MDT2 (2013-2016)', fac='CAP Grade')

df <- subset(joint_17, select=c('AnonID', 'Total', 'CAP.Grade'))
df <- df[!is.na(df$CAP.Grade),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p2 <- stack_prop_tiles(df, MDTInterval, CAP.Grade, xlab='MDT3 (2017)', fac='CAP Grade')

p <- ggarrange(p1, p2, common.legend=T, legend='bottom')

options(repr.plot.width  = 6, repr.plot.height = 3)
# pdf('../fig/REL_CAPGrade_vs_MDT.pdf', width=7, height=4.5)
annotate_figure(p, top=text_grob('Calculus and its Applications', face='bold', size=15))
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# LaTeX: tab:REL_PPSGrade_vs_MDT
df <- subset(joint_pre17, select=c('AnonID', 'Total', 'PPS.Grade'))
df <- df[!is.na(df$PPS.Grade),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p1 <- stack_prop_tiles(df, MDTInterval, PPS.Grade, xlab='MDT2 (2013-2016)', fac='PPS Grade')

df <- subset(joint_17, select=c('AnonID', 'Total', 'PPS.Grade'))
df <- df[!is.na(df$PPS.Grade),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p2 <- stack_prop_tiles(df, MDTInterval, PPS.Grade, xlab='MDT3 (2017)', fac='PPS Grade')

p <- ggarrange(p1, p2, common.legend=T, legend='bottom')

options(repr.plot.width  = 6, repr.plot.height = 3)
# pdf('../fig/REL_PPSGrade_vs_MDT.pdf', width=7, height=4.5)
annotate_figure(p, top=text_grob('Proofs and Problem Solving', face='bold', size=15))
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# LaTeX : tab:REL_Y1Fs_vs_MDT_5070
df <- subset(joint_pre17, select=c('AnonID', 'Total', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p1 <- stack_prop_tiles(df, MDTInterval, Y1Fs, xlab='MDT2 (2013-2016)', fac='Number of Courses with Grade D or below')

df <- subset(joint_17, select=c('AnonID', 'Total', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p2 <- stack_prop_tiles(df, MDTInterval, Y1Fs, xlab='MDT3 (2017)', fac='Number of Courses with Grade D or below')

p <- ggarrange(p1, p2, common.legend=T, legend='bottom')

options(repr.plot.width  = 6, repr.plot.height = 3)
# pdf('../fig/REL_Y1Fs_vs_MDT_5070.pdf', width=7, height=4.5)
annotate_figure(p, top=text_grob('Three Year 1 Courses', face='bold', size=15))
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# LaTeX : tab:REL_Y1Fs_vs_MDT_maths
df <- subset(joint_pre17, School=='Mathematics', select=c('AnonID', 'Total', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p1 <- stack_prop_tiles(df, MDTInterval, Y1Fs, xlab='MDT2 (2013-2016)', fac='Number of Courses with Grade D or below')

df <- subset(joint_17, School=='Mathematics', select=c('AnonID', 'Total', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]
df$MDTInterval <- cut(df$Total, c(0,50,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p2 <- stack_prop_tiles(df, MDTInterval, Y1Fs, xlab='MDT3 (2017)', fac='Number of Courses with Grade D or below')

p <- ggarrange(p1, p2, common.legend=T, legend='bottom')

options(repr.plot.width  = 6, repr.plot.height = 3)
# pdf('../fig/REL_Y1Fs_vs_MDT_maths.pdf', width=7, height=4.5)
annotate_figure(p, top=text_grob('Three Year 1 Courses (Maths Students)', face='bold', size=15))
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# LaTeX : tab:REL_Y1Fs_vs_MDT_506070
df <- subset(joint_pre17, select=c('AnonID', 'Total', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]
df$MDTInterval <- cut(df$Total, c(0,50,60,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p1 <- stack_prop_tiles(df, MDTInterval, Y1Fs, xlab='MDT2 (2013-2016)', fac='Number of Courses with Grade D or below')

df <- subset(joint_17, select=c('AnonID', 'Total', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]
df$MDTInterval <- cut(df$Total, c(0,50,60,70,100))
levels(df$MDTInterval) <- c(levels(df$MDTInterval), 'Unknown')
df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
p2 <- stack_prop_tiles(df, MDTInterval, Y1Fs, xlab='MDT3 (2017)', fac='Number of Courses with Grade D or below')

p <- ggarrange(p1, p2, common.legend=T, legend='bottom')

options(repr.plot.width  = 6, repr.plot.height = 3)
# pdf('../fig/REL_Y1Fs_vs_MDT_506070.pdf', width=7, height=4.5)
annotate_figure(p, top=text_grob('Three Year 1 Courses', face='bold', size=15))
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

## ----- Section 5.3 Qual vs outcomes -----
df <- subset(joint_17, select=c('AnonID', 'Category', 'Region', 'ILA', 'CAP', 'PPS', 'AAC', 'APPS'))
df <- df[rowSums(is.na(df[4:8]))!=5,]
# LaTeX: tab:REL_freq_qual
print(xtable(t(addmargins(table(df$Category))), digits=rep(0,6)), include.rownames=F)

df$Category[grep('Cat', df$Category)] <- 'Known'
print(xtable(addmargins(table(df$Category, df$Region)), digits=rep(0,6)))

# LaTeX: tab:REL_ILAGrade_vs_Qual
df <- subset(joint_17, select=c('AnonID', 'Category', 'ILA.Grade'))
df <- df[!is.na(df$ILA.Grade),]
p1 <- stack_prop_tiles(df, Category, ILA.Grade, title='ILA', xlab='Category (2017)', fac='Grade')

# LaTeX: tab:REL_CAPGrade_vs_Qual
df <- subset(joint_17, select=c('AnonID', 'Category', 'CAP.Grade'))
df <- df[!is.na(df$CAP.Grade),]
p2 <- stack_prop_tiles(df, Category, CAP.Grade, title='CAP', xlab='Category (2017)', fac='Grade')

# LaTeX: tab:REL_PPSGrade_vs_Qual
df <- subset(joint_17, select=c('AnonID', 'Category', 'PPS.Grade'))
df <- df[!is.na(df$PPS.Grade),]
p3 <- stack_prop_tiles(df, Category, PPS.Grade, title='PPS', xlab='Category (2017)', fac='Grade')

options(repr.plot.width  = 8, repr.plot.height = 3)
# pdf('../fig/REL_Grade_vs_Qual.pdf', width=7, height=3.5, onefile=F)
ggarrange(p1, p2, p3, ncol=3, common.legend=T, legend='bottom')
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

# LaTeX: tab:REL_Y1Fs_vs_Qual
df <- subset(joint_17, select=c('AnonID', 'Category', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]

# pdf('../fig/REL_Y1Fs_vs_Qual.pdf', width=4.5, height=3.5)
stack_prop_tiles(df, Category, Y1Fs, title='Three Year 1 Courses', xlab='Category (2017)', fac='Number of\nCourses with\nGrade D\nor below')
# dev.off()

# LaTeX: tab:REL_Y1Fs_vs_Qual_maths
df <- subset(joint_17, School=='Mathematics', select=c('AnonID', 'Category', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]

# pdf('../fig/REL_Y1Fs_vs_Qual_maths.pdf', width=4.5, height=3.5)
stack_prop_tiles(df, Category, Y1Fs, title='Three Year 1 Courses (Maths Students)', xlab='Category (2017)', fac='Number of\nCourses with\nGrade D\nor below')
# dev.off()

## ----- Section 5.3.2 DT vs Qual -----

# LaTeX: tab:REL_MDT_vs_Qual
df <- subset(joint_17, select=c('AnonID', 'Total', 'Category', 'ILA', 'CAP', 'PPS'))
df <- df[rowSums(is.na(df[4:6]))!=3,]
df$MDTInterval <- cut(df$Total, c(0,50,60,70,100))
# df$MDTInterval <- factor(as.character(df$MDTInterval), levels=c(rev(levels(df$MDTInterval)), 'Unknown'))
# df$MDTInterval[is.na(df$MDTInterval)] <- 'Unknown'
df$MDTInterval <- factor(as.character(df$MDTInterval), levels=rev(levels(df$MDTInterval)))

# pdf('../fig/REL_MDT_vs_Qual.pdf', width=4.5, height=3.5)
stack_prop_tiles(df, Category, MDTInterval, xlab='Category (2017)', fac='MDT3')
# dev.off()

## ----- Section 5.4 Combining two -----

# LaTeX: tab:REL_Y1Fs_vs_NewCat_all
df <- subset(joint_17, select=c('AnonID', 'Total', 'Category', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]
df$NewCat <- 'Unknown'
for (i in 1:nrow(df)) {
    if (df$Category[i]=='Cat1') {
        if (is.na(df$Total[i])) {
            df$NewCat[i] <- 'Cat1-'
        } else if (df$Total[i]<=70) {
            df$NewCat[i] <- 'Cat1-'
        } else {
            df$NewCat[i] <- 'Cat1+'
        }
    } else if (df$Category[i]=='Cat2') {
        if (is.na(df$Total[i])) {
            df$NewCat[i] <- 'Cat2-'
        } else if (df$Total[i]<=60) {
            df$NewCat[i] <- 'Cat2-'
        } else {
            df$NewCat[i] <- 'Cat2+'
        }
    } else if (df$Category[i]=='Cat3') {
        if (is.na(df$Total[i])) {
            df$NewCat[i] <- 'Cat3+'
        } else if (df$Total[i]<=70) {
            df$NewCat[i] <- 'Cat3-'
        } else {
            df$NewCat[i] <- 'Cat3+'
        }
    } else {
        if (is.na(df$Total[i])) {
            df$NewCat[i] <- 'Unknown'
        } else if (df$Total[i]<=60) {
            df$NewCat[i] <- 'Unknown'
        } else {
            df$NewCat[i] <- 'Unknown'
        }
    }
}

options(repr.plot.width  = 8, repr.plot.height = 3)
# pdf('../fig/REL_Y1Fs_vs_NewCat_all.pdf', width=7, height=3.5)
stack_prop_tiles(df, NewCat, Y1Fs, title='Three Year 1 Courses', xlab='Detailed Category (2017)', fac='Number of\nCourses with\nGrade D\nor below')
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

## ----- Section 5.4 Combining two -----

# LaTeX: tab:REL_Y1Fs_vs_NewCat
df <- subset(joint_17, select=c('AnonID', 'Total', 'Category', 'Y1Fs'))
df <- df[!is.na(df$Y1Fs),]
df$NewCat <- 'Unknown'
for (i in 1:nrow(df)) {
    if (df$Category[i]=='Cat1') {
        if (is.na(df$Total[i])) {
            df$NewCat[i] <- 'Cat-'
        } else if (df$Total[i]<=70) {
            df$NewCat[i] <- 'Cat-'
        } else {
            df$NewCat[i] <- 'Cat+'
        }
    } else if (df$Category[i]=='Cat2') {
        if (is.na(df$Total[i])) {
            df$NewCat[i] <- 'Cat-'
        } else if (df$Total[i]<=60) {
            df$NewCat[i] <- 'Cat-'
        } else {
            df$NewCat[i] <- 'Cat+'
        }
    } else if (df$Category[i]=='Cat3') {
        if (is.na(df$Total[i])) {
            df$NewCat[i] <- 'Cat+'
        } else if (df$Total[i]<=70) {
            df$NewCat[i] <- 'Cat-'
        } else {
            df$NewCat[i] <- 'Cat+'
        }
    } else {
        if (is.na(df$Total[i])) {
            df$NewCat[i] <- 'Unknown'
        } else if (df$Total[i]<=60) {
            df$NewCat[i] <- 'Cat-'
        } else {
            df$NewCat[i] <- 'Cat+'
        }
    }
}

# pdf('../fig/REL_Y1Fs_vs_NewCat.pdf', width=4.5, height=3.5)
stack_prop_tiles(df, NewCat, Y1Fs, title='Three Year 1 Courses', xlab='Refined Category (2017)', fac='Number of\nCourses with\nGrade D\nor below')
# dev.off()

# Previous cell must be run! 
for (i in 1:nrow(df)) {
    if (df$NewCat[i] == 'Cat-') {
        df$Predicted[i] <- 'Pred.Pos'
    } else if (df$NewCat[i] == 'Cat+') {
        df$Predicted[i] <- 'Pred.Neg'
    } else {
        df$Predicted[i] <- NA_character_
    }
}
for (i in 1:nrow(df)) {
    if (df$Y1Fs[i] <= 1) {
        df$True[i] <- 'True.Neg'
    } else {
        df$True[i] <- 'True.Pos'
    }
}
df$True <- factor(df$True, levels=c('True.Pos','True.Neg'))
df$Predicted <- factor(df$Predicted, levels=c('Pred.Pos','Pred.Neg'))
cm_NewCat <- table(df$True, df$Predicted)
print(xtable(cm_NewCat, align=c('r','c','c')))

## ----- Section 5.5 Extra maths courses -----
df <- subset(joint_pre17, School=='Mathematics', select=c('Total','ILA','ILA.MfP1'))
df <- df[complete.cases(df),]
p1 <- ggplot(df, aes(Total, ILA)) + 
    geom_point(aes(col=ILA.MfP1), size=0.5) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT2 (2013-2016)', col='Also take MfP1?')

df <- subset(joint_pre17, School=='Mathematics', select=c('Total','CAP','CAP.MfP1'))
df <- df[complete.cases(df),]
p2 <- ggplot(df, aes(Total, CAP)) + 
    geom_point(aes(col=CAP.MfP1), size=0.5) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT2 (2013-2016)', col='Also take MfP1?')

df <- subset(joint_pre17, School=='Mathematics', select=c('Total','PPS','PPS.MfP1'))
df <- df[complete.cases(df),]
p3 <- ggplot(df, aes(Total, PPS)) + 
    geom_point(aes(col=PPS.MfP1), size=0.5) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT2 (2013-2016)', col='Also take MfP1?')

df <- subset(joint_17, School=='Mathematics', select=c('Total','ILA','ILA.MfP1'))
df <- df[complete.cases(df),]
p4 <- ggplot(df, aes(Total, ILA)) + 
    geom_point(aes(col=ILA.MfP1), size=0.5) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT3 (2017)', col='Also take MfP1?')

df <- subset(joint_17, School=='Mathematics', select=c('Total','CAP','CAP.MfP1'))
df <- df[complete.cases(df),]
p5 <- ggplot(df, aes(Total, CAP)) + 
    geom_point(aes(col=CAP.MfP1), size=0.5) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT3 (2017)', col='Also take MfP1?')

df <- subset(joint_17, School=='Mathematics', select=c('Total','PPS','PPS.MfP1'))
df <- df[complete.cases(df),]
p6 <- ggplot(df, aes(Total, PPS)) + 
    geom_point(aes(col=PPS.MfP1), size=0.5) + 
    coord_cartesian(xlim=c(0,100), ylim=c(0,100)) + 
    labs(x='MDT3 (2017)', col='Also take MfP1?')

# pdf('../fig/REL_scatter_MfP1.pdf', width=7, height=5, onefile=F)
ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2, common.legend=T, legend='bottom')
# dev.off()

p1 <- split_boxplot(joint_pre17, 'ILA', xlab='MDT2 (2013-2016)', latex=T)
p2 <- split_boxplot(joint_pre17, 'CAP', xlab='MDT2 (2013-2016)', latex=T)
p3 <- split_boxplot(joint_pre17, 'PPS', xlab='MDT2 (2013-2016)', latex=T)
p4 <- split_boxplot(joint_17, 'ILA', xlab='MDT3 (2017)', latex=T)
p5 <- split_boxplot(joint_17, 'CAP', xlab='MDT3 (2017)', latex=T)
p6 <- split_boxplot(joint_17, 'PPS', xlab='MDT3 (2017)', latex=T)
options(repr.plot.width  = 8, repr.plot.height = 6)
# pdf('../fig/REL_splitbox.pdf', width=7, height=5.5, onefile=F)
ggarrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2, common.legend=T, legend='bottom')
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

## ----- Something else -----

options(repr.plot.width  = 6, repr.plot.height = 3)
a <- c(0,50,60,70,100)
stack_prop_bar(joint_pre17, 1, a, plots=F)
stack_prop_bar(joint_17, 1, a, plots=F)
# pdf('../fig/REL_stackpropbar.pdf', width=7, height=3, onefile=F)
p1 <- stack_prop_bar(joint_pre17, 1, a)
p2 <- stack_prop_bar(joint_17, 1, a)
ggarrange(p1, p2, common.legend = T)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)

joint_17 <- merge(joint_17, entryquals[c('AnonID', 'Qual', 'Category')], by='AnonID', all.x=T)
joint_17$Qual[which(is.na(joint_17$Qual))] <- 'Unknown'
joint_17$Category[which(is.na(joint_17$Category))] <- 'Unknown'

df <- joint_17[, c('AnonID', 'Total', 'Category', 'ILA')]
df <- df[!is.na(df$ILA),]
df$Interval <- cut(df$Total, c(0,50,60,70,100))

levels(df$Interval) <- c(levels(df$Interval), 'Unknown')
df$Interval[is.na(df$Interval)] <- 'Unknown'

addmargins(df.count <- table(Interval=df$Interval, Category=df$Category))

df.count <- table(Interval=df$Interval[df$Interval!='Unknown'], Category=df$Category[df$Interval!='Unknown'])
df.freq <- data.frame(df.count)
df.prop <- data.frame(prop.table(df.count, margin=2))
df <- merge(df.freq, df.prop, by=c("Interval", "Category"))
df$Interval <- factor(as.character(df$Interval), rev(levels(df$Interval)))
df <- arrange(df, Category, rev(Interval))
df <- ddply(df, .(Category), transform, pos = cumsum(Freq.y) - (0.5 * Freq.y))

ggplot(df, aes(Category, Freq.y, fill=Interval)) +
    geom_bar(stat='identity', position='fill') +
    geom_text(data=df, aes(Category, pos, label=paste0(round(Freq.y * 100, 1), "%")), size=4) +
    scale_fill_brewer(palette='RdYlGn', direction=-1) + 
    scale_y_continuous(labels=percent_format()) + 
    labs(x='Category', y='Proportion')

df1 <- joint_17[, c('AnonID', 'Y1Fs', 'Qual', 'Category', 'School')]
# df1 <- df1[df1$School=='Mathematics',]
dfib <- df1[df1$Qual=='IB',]
dfalv <- df1[df1$Qual=='A-Level',]
dfsqa <- df1[df1$Qual=='SQA',]
dfna <- df1[df1$Qual=='Unknown',]

dfib <- dfib[complete.cases(dfib),]
dfalv <- dfalv[complete.cases(dfalv),]
dfsqa <- dfsqa[complete.cases(dfsqa),]
dfna <- dfna[complete.cases(dfna),]

dfib.count <- table(Y1Fs=dfib$Y1Fs, Category=dfib$Category)
dfalv.count <- table(Y1Fs=dfalv$Y1Fs, Category=dfalv$Category)
dfsqa.count <- table(Y1Fs=dfsqa$Y1Fs, Category=dfsqa$Category)
dfna.count <- table(Y1Fs=dfna$Y1Fs, Category=dfna$Category)

dfib.prop <- data.frame(prop.table(dfib.count, margin=2))
dfalv.prop <- data.frame(prop.table(dfalv.count, margin=2))
dfsqa.prop <- data.frame(prop.table(dfsqa.count, margin=2))
dfna.prop <- data.frame(prop.table(dfna.count, margin=2))

dfib <- arrange(dfib.prop, Category, rev(Y1Fs))
dfalv <- arrange(dfalv.prop, Category, rev(Y1Fs))
dfsqa <- arrange(dfsqa.prop, Category, rev(Y1Fs))
dfna <- arrange(dfna.prop, Category, rev(Y1Fs))

dfib <- ddply(dfib, .(Category), transform, pos = cumsum(Freq) - (0.5 * Freq))
dfalv <- ddply(dfalv, .(Category), transform, pos = cumsum(Freq) - (0.5 * Freq))
dfsqa <- ddply(dfsqa, .(Category), transform, pos = cumsum(Freq) - (0.5 * Freq))
dfna <- ddply(dfna, .(Category), transform, pos = cumsum(Freq) - (0.5 * Freq))

addmargins(dfib.count)
addmargins(dfalv.count)
addmargins(dfsqa.count)
addmargins(dfna.count)

pib <- ggplot(dfib, aes(Category, Freq, fill=Y1Fs)) +
    geom_bar(stat='identity', position='fill') +
    geom_text(data=dfib, aes(Category, pos, label=paste0(round(Freq * 100, 1), "%")), size=4) +
    scale_fill_brewer(palette='RdYlGn', direction=-1) + 
    scale_y_continuous(labels=percent_format()) + 
    labs(x='IB', y='Proportion')
palv <- ggplot(dfalv, aes(Category, Freq, fill=Y1Fs)) +
    geom_bar(stat='identity', position='fill') +
    geom_text(data=dfalv, aes(Category, pos, label=paste0(round(Freq * 100, 1), "%")), size=4) +
    scale_fill_brewer(palette='RdYlGn', direction=-1) + 
    scale_y_continuous(labels=percent_format()) + 
    labs(x='A-Level/AS-Level', y='Proportion')
psqa <- ggplot(dfsqa, aes(Category, Freq, fill=Y1Fs)) +
    geom_bar(stat='identity', position='fill') +
    geom_text(data=dfsqa, aes(Category, pos, label=paste0(round(Freq * 100, 1), "%")), size=4) +
    scale_fill_brewer(palette='RdYlGn', direction=-1) + 
    scale_y_continuous(labels=percent_format()) + 
    labs(x='Higher/AH', y='Proportion')
pna <- ggplot(dfna, aes(Category, Freq, fill=Y1Fs)) +
    geom_bar(stat='identity', position='fill') +
    geom_text(data=dfna, aes(Category, pos, label=paste0(round(Freq * 100, 1), "%")), size=4) +
    scale_fill_brewer(palette='RdYlGn', direction=-1) + 
    scale_y_continuous(labels=percent_format()) + 
    labs(x='Unknown', y='Proportion')
options(repr.plot.width  = 8, repr.plot.height = 3)
ggarrange(palv, pib, psqa, pna, ncol=4, common.legend = T)
options(repr.plot.width  = 4, repr.plot.height = 3)

df1 <- joint_17[, c('AnonID', 'Y1Fs', 'Category', 'Total', 'School')]
df1 <- df1[!is.na(df1$Y1Fs),]
# df1 <- df1[df1$School=='Mathematics',]
# df1$Category[df1$Category=='Cat2' & (df1$Total < 60 | is.na(df1$Total))] <- 'Cat2-'
# df1$Category[df1$Category=='Cat2' & (df1$Total > 60 & !is.na(df1$Total))] <- 'Cat2+'
# df1$Category[df1$Category=='Unknown' & (df1$Total < 60 | is.na(df1$Total))] <- 'Cat1'
# df1$Category[df1$Category=='Unknown' & (df1$Total > 60 & !is.na(df1$Total))] <- 'Cat3'
# df1$Category[df1$Category=='Unknown'] <- 'Cat3'

df2 <- df1

# df1$Category[df1$Category=='Cat1' & (df1$Total < 70 | is.na(df1$Total))] <- 'Cat1-'
# df1$Category[df1$Category=='Cat1' & (df1$Total > 70 & !is.na(df1$Total))] <- 'Cat1+'
# df1$Category[df1$Category=='Cat3' & (df1$Total < 50 | is.na(df1$Total))] <- 'Cat3-'
# df1$Category[df1$Category=='Cat3' & (df1$Total > 50 & !is.na(df1$Total))] <- 'Cat3+'

df2$Category[df1$Category %in% c('Cat1','Cat2-')] <- 'Cat1/2-'
df2$Category[df1$Category %in% c('Cat3','Cat2+')] <- 'Cat2+/3'

df1.count <- table(Y1Fs=df1$Y1Fs, Category=df1$Category)
df1.prop <- data.frame(prop.table(df1.count, margin=2))
df1 <- arrange(df1.prop, Category, rev(Y1Fs))
df1 <- ddply(df1, .(Category), transform, pos = cumsum(Freq) - (0.5 * Freq))

df2.count <- table(Y1Fs=df2$Y1Fs, Category=df2$Category)
df2.prop <- data.frame(prop.table(df2.count, margin=2))
df2 <- arrange(df2.prop, Category, rev(Y1Fs))
df2 <- ddply(df2, .(Category), transform, pos = cumsum(Freq) - (0.5 * Freq))

addmargins(df1.count)
addmargins(df2.count)
p1 <- ggplot(df1, aes(Category, Freq, fill=Y1Fs)) +
    geom_bar(stat='identity', position='fill', width=rep(colSums(prop.table(df1.count)), each=4)/max(colSums(prop.table(df1.count)))) +
    geom_text(data=df1, aes(Category, pos, label=paste0(round(Freq * 100, 1), "%")), size=4) +
    scale_fill_brewer(palette='RdYlGn', direction=-1) + 
    scale_y_continuous(labels=percent_format()) + 
    labs(x='Category', y='Proportion')
p2 <- ggplot(df2, aes(Category, Freq, fill=Y1Fs)) +
    geom_bar(stat='identity', position='fill') +
    geom_text(data=df2, aes(Category, pos, label=paste0(round(Freq * 100, 1), "%")), size=4) +
    scale_fill_brewer(palette='RdYlGn', direction=-1) + 
    scale_y_continuous(labels=percent_format()) + 
    labs(x='Category', y='Proportion')
options(repr.plot.width  = 6, repr.plot.height = 3)
ggarrange(p1, p2, ncol=2, common.legend = T)
options(repr.plot.width  = 4, repr.plot.height = 3)

df1 <- joint_17[, c('AnonID', 'Y1Fs', 'Category')]
df1 <- df1[complete.cases(df1),]
data1.count <- table(Y1Fs=df1$Y1Fs, Category=df1$Category)
df1.freq <- data.frame(data1.count)
df1.prop <- data.frame(prop.table(data1.count, margin=2))
df1 <- merge(df1.freq, df1.prop, by=c("Y1Fs", "Category"))

df1 <- arrange(df1, Category, rev(Y1Fs))
df1 <- ddply(df1, .(Category), transform, pos = cumsum(Freq.y) - (0.5 * Freq.y))
df1

table(joint_17$Region[joint_17$Category=='Unknown' & !is.na(joint_17$ILA)])
