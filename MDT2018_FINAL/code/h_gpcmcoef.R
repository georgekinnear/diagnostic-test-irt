## Header of coefficients from each GPCM for further analysis 
## Codes are extracted from MIRT.R and IMPRV.R

fit_pre17_GPCM <- suppressMessages(mirt(mdt_clean[1:20]*multi, 1, itemtype="gpcm", gpcm_mats = mats))
fit_14 <- suppressMessages(mirt(mdt14_clean[1:20]*multi, 1, itemtype="gpcm", gpcm_mats = mats))
fit_17 <- suppressMessages(mirt(round(mdt17_clean[1:20]*multi), 1, itemtype="gpcm", gpcm_mats = mats17))
fit_18 <- suppressMessages(mirt(round(mdt18_clean[1:20]*multi), 1, itemtype="gpcm", gpcm_mats = mats18))
fit_MDT3 <- suppressMessages(mirt(round(MDT3clean[1:20]*multi), 1, itemtype="gpcm", gpcm_mats = matsMDT3))

eap_pre17 <- data.frame(AnonID=mdt_clean$AnonID, fscores(fit_pre17_GPCM, method='EAP', full.scores.SE=T))
eap_17 <- data.frame(AnonID=mdt17_clean$AnonID, fscores(fit_17, method='EAP', full.scores.SE=T))
eap_18 <- data.frame(AnonID=mdt18_clean$AnonID, fscores(fit_18, method='EAP', full.scores.SE=T))
eap_MDT3 <- data.frame(AnonID=MDT3clean$AnonID, fscores(fit_MDT3, method='EAP', full.scores.SE=T))
