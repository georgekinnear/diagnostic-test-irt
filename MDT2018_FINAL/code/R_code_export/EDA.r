
# Set plot size (for jupyter notebook)
options(repr.plot.width  = 4, repr.plot.height = 3)

library(RColorBrewer)
suppressWarnings(source('h_mdt.R'))

library(xtable)
library(ggplot2)
theme_set(theme_bw())
library(ggpubr)
library(plyr)

# Self defined functions for generating tables and histogram
tableGen <- function (v) {  
  sum.table.v <- c(summary(v), sd(v))
  sum.table.v <- data.frame(sum.table.v)
  rownames(sum.table.v) <- c("Min.","1st Qu.","Median","Mean",
                             "3rd Qu.","Max","Stand. Dev.")
  colnames(sum.table.v) <- "Value"
  return(sum.table.v)
}

histplot <- function (df, col=NULL, title='Histogram of Data') {
  col <- if (is.null(col))
    'Blues'
  else col
  df.shift <- df
  df.shift$Total[df.shift$Total == 100] <- 99
  ggplot(df.shift, aes(x=Total)) + 
    geom_histogram(aes(fill=..count..), bins=22, col="black", closed="right") + 
    scale_x_continuous(name = "Total score (0-100)",
                       breaks = seq(0, 100, 20), 
                       limits = c(-3,105)
                       ) + 
    scale_fill_gradient2(high=brewer.pal(5,col)) + 
    labs(title=title, subtitle=paste0("Total scores frequency: ", nrow(df), " students"))
}

## ----- Section 1.1 -----
df <- data.frame(Label = paste0('N',1:20), group_17)
# LaTeX: tab:EDA_groups
print(xtable(df, align=c('l','c','l','c')), include.rownames = F)

## ----- Section 1.2.1 -----
# LaTeX: tab:EDA_dsc_17
print(xtable(tableGen(mdt17$Total), digits=c(0,2)))

# pdf("../fig/EDA_hist_17.pdf", width=7, height=4)
histplot(mdt17fac, title='Raw Data')
# dev.off()

## ----- Section 1.2.2 -----
# LaTeX: tab:EDA_difficulty
print(xtable(t(colMeans(mdt17[1:10])), align=rep('c',11)))
print(xtable(t(colMeans(mdt17[11:20])), align=rep('c',11)))

# LaTeX: tab:EDA_count_clean_legacy
print(xtable(data.frame(No.=c(nrow(mdt17fac), 
                              nrow(mdt17fac_nodup), 
                              nrow(mdt17fac)-nrow(mdt17_clean_legacy), 
                              nrow(mdt17_rm_legacy), 
                              nrow(mdt17_clean_legacy)))))

# LaTeX: tab:EDA_dsc_17clean_legacy
print(xtable(tableGen(mdt17_clean_legacy$Total), digits=c(0,2)))

# pdf("../fig/EDA_hist_17clean_legacy.pdf", width=7, height=4)
histplot(mdt17_clean_legacy, 'Oranges', title='Data after Cleaning (Legacy)')
# dev.off()

## ----- Section 1.2.3 -----
# LaTeX: tab:EDA_validresp_legacy
print(xtable(t(as.data.frame(table(Valid.Response=rowSums(mdt17fac_nodup[row.names(mdt17_rm_legacy),]!='-')))), 
             align=c('r', rep('c',12))), 
      include.colnames=F)

# Factor version scores of the removed student
# Sorted by number of '-'
arrange(mdt17fac_nodup[row.names(mdt17_rm_legacy),], rowSums(mdt17fac_nodup[row.names(mdt17_rm_legacy),]!='-'))

# LaTeX: tab:EDA_count_clean
print(xtable(data.frame(No.=c(nrow(mdt17fac), 
                              nrow(mdt17fac_nodup), 
                              nrow(mdt17fac)-nrow(mdt17_clean), 
                              nrow(mdt17_rm), 
                              nrow(mdt17_clean)))))

# LaTeX: tab:EDA_dsc_17clean
print(xtable(tableGen(mdt17_clean$Total), digits=c(0,2)))

# pdf("../fig/EDA_hist_17clean.pdf", width=7, height=4)
histplot(mdt17_clean, 'Greens', title='Data after Cleaning')
# dev.off()

# LaTeX: tab:EDA_validresp
print(xtable(t(as.data.frame(table(Valid.Response=rowSums(mdt17fac_nodup[row.names(mdt17_rm),]!='-')))), 
             align=c('r', rep('c',8))), 
      include.colnames=F)

arrange(mdt17fac_nodup[row.names(mdt17_rm),], rowSums(mdt17fac_nodup[row.names(mdt17_rm),]!='-'))

## ----- Section 1.2.4 -----
# Confusion matrix
TP <- min(nrow(mdt17_rm), nrow(mdt17_rm_legacy))
FN <- nrow(mdt17_rm) - TP
FP <- nrow(mdt17_rm_legacy) - TP
TN <- nrow(mdt17_clean) - FP
cm <- as.table(matrix(c(TP, FP, FN, TN), ncol=2))
rownames(cm) <- c('Removed by new method', 'Kept by new method')
colnames(cm) <- c('Removed by old method', 'Kept by old method')
# LaTeX: tab:EDA_cm
print(xtable(addmargins(cm), digits=rep(0,4), align=c('r', rep('c',3))))

(ACC <- (TP+TN)/(TP+FN+FP+TN))

## ----- Section 1.3 -----
# LaTeX: tab:EDA_dsc_13to16_clean
tab.13to16 <- cbind(tableGen(mdt13_clean$Total), 
                   tableGen(mdt14_clean$Total), 
                   tableGen(mdt15_clean$Total), 
                   tableGen(mdt16_clean$Total))
colnames(tab.13to16) <- paste0('MDT2 (', 2013:2016, ')')
print(xtable(tab.13to16, digits=c(0,rep(2,4))))

p1 <- histplot(mdt13_clean, 'RdPu', title='MDT2 (2013)')
p2 <- histplot(mdt14_clean, 'GnBu', title='MDT2 (2014)')
p3 <- histplot(mdt15_clean, 'YlGn', title='MDT2 (2015)')
p4 <- histplot(mdt16_clean, 'Purples', title='MDT2 (2016)')
options(repr.plot.width  = 6, repr.plot.height = 6)
# pdf('../fig/EDA_hist_13to16_clean.pdf', width=7, height=6, onefile=F)
ggarrange(p1, p2, p3, p4)
# dev.off()
options(repr.plot.width  = 4, repr.plot.height = 3)
