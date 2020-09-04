source('header.R')

library(ltm)
library(xtable)

source('IRT_myfunc.R')

# -----

# Determine number of respones in each question
# remember to check with original questions
# Observe frequency and check pairwise association
dsc <- descript(mdt_clean[4:23])

# Create credit-category conversion list
catg <- list()
for (i in 1:20) {
  catg[[i]] <- as.numeric(names(dsc$perc[[i]]))
}

mdt_catg <- mycr2catg(mdt_clean[4:23])

# Explore degree of pairwise associations
if (file.exists('../robject/IRT_cortest.Rdata')) {
  load('../robject/IRT_cortest.Rdata')
} else {
  cortest <- rcor.test(mdt_clean[4:23], method="kendall")
  save(cortest, file="../robject/IRT_cortest.Rdata")
}
cortest_sorted <- cortest$p.values[order(-cortest$p.values[,'pvals']),]
# LaTeX: tab:IRT_cortest
print(xtable(cortest_sorted[1:4,], digits=c(0,0,0,4), align=rep('c',4)), include.rownames=F)

# ----- Fit GPCM model -----

# Set number of iterations to 300 allows estimation to converge
con <- list(iter.qN = 300)

# ---

# Assuming discrimination = 1
if (file.exists('../robject/IRT_rasch.Rdata'))  {
  load('../robject/IRT_rasch.Rdata')
} else {
  fit_rasch <- gpcm(mdt_clean[4:23], constraint="rasch", start.val="random", control=con)
  gof_rasch <- myGoF.gpcm(fit_rasch)
  eap_rasch <- factor.scores(fit_rasch, method="EAP", resp.patterns=mdt_catg)
  save(fit_rasch, gof_rasch, eap_rasch, file="../robject/IRT_rasch.Rdata")
}

pdf("../fig/IRT_GPCM_Rasch.pdf", width=7)
par(mfrow=c(2,2))
# Plot ICC (IRF)
myplot.gpcm(fit_rasch, col=palette(), lwd=2, legend=T, cx="bottomright", cex=0.45)
# Plot expected total score (TRF)
myplot.gpcm(fit_rasch, items=0, find.avg=T, col=palette(), lwd=2)
# Plot IIC
plot(fit_rasch, type="IIC", col=palette(), lwd=2, legend=T, cex=0.45, cx="topright")
# Plot test information curve
plot(fit_rasch, type="IIC", items=0, col=palette(), lwd=2)
dev.off()

# ---

# Assuming fix discrimination
fit_1PL <- gpcm(mdt_clean[4:23], constraint="1PL", start.val="random", control=con)

# ---

# Without constraint
if (file.exists('../robject/IRT_gpcm.Rdata')) {
  load('../robject/IRT_gpcm.Rdata')
} else {
  fit_gpcm <- gpcm(mdt_clean[4:23], constraint="gpcm", start.val="random", control=con)
  gof_gpcm <- myGoF.gpcm(fit_gpcm)
  # Calculate expected a posterior
  eap_gpcm <- factor.scores(fit_gpcm, method="EAP", resp.patterns=mdt_catg)
  save(fit_gpcm, gof_gpcm, eap_gpcm, file="../robject/IRT_gpcm.Rdata")
}

summary(fit_gpcm)

pdf("../fig/IRT_GPCM_GPCM.pdf", width=7)
par(mfrow=c(2,2))
myplot.gpcm(fit_gpcm, col=palette(), lwd=2, legend=T, cx="bottomright", cex=0.45)
myplot.gpcm(fit_gpcm, items=0, find.avg=T, col=palette(), lwd=2)
plot(fit_gpcm, type="IIC", col=palette(), lwd=2, legend=T, cex=0.45, cx="topright")
plot(fit_gpcm, type="IIC", items=0, col=palette(), lwd=2)
dev.off()

# --- 

mdt.aov <- anova(fit_rasch, fit_gpcm)
myprint.xtable.aov.gpcm(mdt.aov)

# -----

mdt.fitted <- mycatg2cr(eap_gpcm$score.dat[order(eap_gpcm$score.dat$z1),])

# Plot of theoretical and empirical TRF
pdf("../fig/IRT_resid.pdf")
myplot.resid.gpcm(fit_gpcm, items=0, lwd=2)
par(mfrow=c(2,2))
for (i in 1:20) {
  myplot.resid.gpcm(fit_gpcm, items=i, bins=30, lwd=2)
}
dev.off()

# -----

pvals <- matrix(numeric(20*40), ncol=20)

pb <- txtProgressBar(min = 0, max = 20*40, style = 3)
for (i in 1:20) {
  for (b in (length(catg[[i]])+1):40) {
    pvals[b,i] <- myplot.resid.gpcm(fit_gpcm, items=i, bins=b, plot=F)
    setTxtProgressBar(pb, (i-1)*40+b)
  }
}
close(pb)

for (i in 1:20)
  print(c(length(catg[[i]]), order(-pvals[,i])[1:10]))

# ---

gof <- GoF.gpcm(fit_gpcm, simulate.p.value = F)

margins(fit_gpcm)

margins(fit, type="three")

fit_tmp <- gpcm(mdt[6:10], constraint="gpcm", start.val="random")
plot(fit_tmp)
myGoF.gpcm(fit_tmp)


margins(fit_tmp)
rcor.test(mdt[6:9])
factor.scores(fit_tmp)
fitted(fit_tmp) #nested by factor.scores
margins(fit_tmp, type="three")

vals <- plot(fit_gpcm, type = "IIC", items = 0, plot = FALSE)
plot(vals[, "z"], 1 / sqrt(vals[, "test.info"]), type = "l", lwd = 2,
     xlab = "Ability", ylab = "Standard Error",
     main = "Standard Error of Measurement")

a <- margins(fit_gpcm, type="three-way")

myplot.gpcm(fit_gpcm, items=9, find.avg=T, col=palette(), lwd=2)

fit_tmp <- gpcm(mdt_clean[3+c(1,3,5,7,9,10,14,15,16,17,18,19,20)], constraint="gpcm", start.val="random", control=con)
myplot.gpcm(fit_tmp, items=1, col=palette(), lwd=2)
par(mfrow=c(2,1))
plot(fit_tmp, items=5)
plot(fit_gpcm, items=9)
