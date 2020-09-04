## ----- Set Working Directory -----
#setwd("/Volumes/s1680642/MDT/MDT2018/code")
#setwd("C:/Users/gkinnear/ownCloud/Research/2017 Diagnostic Test/PTAS Project Summer 2018/MDT2018_FINAL/code")
#library(tidyverse)

## ----- Load Diagnostic Test 2013-2016 Data -----
mdt <- read.csv('../data/ANON_DiagTest13to16_new.csv', header=T)
#Rearrange columns
mdt <- mdt[, c(grep("Q", colnames(mdt)), which(colnames(mdt)=="Total"), which(colnames(mdt)=="AnonID"))]
mdt <- mdt[complete.cases(mdt),]

# Ordering data frame so that higher total score is placed above lower total score where exam numbers are duplicated
mdt <- mdt[order(mdt$AnonID, -mdt$Total),]
# Takes only the upper row of consecutive duplicated rows
mdt <- mdt[!duplicated(mdt$AnonID),]

# # Remove students correctly answer less than 3 quetions and score below 10
# mdt_omit <- (mdt$Total <= 10) + (rowSums(mdt[6:25]!=0) < 3)
# mdt <- mdt[mdt_omit < 2, ]
# rm(mdt_omit)

# Eliminating students who did not score enough on the 5 easiest questions 
# (based on raw scores) of the second half of the test (11,12,13,16,17)
mdt_easy <- mdt[, c(11,12,13,16,17)]
# No. of zeros in those five questions
no_of_zeros <- rowSums(mdt_easy==0)
# Exclude people getting more than 3 zeros and <= 30 points in total
mdt_clean <- mdt[-which(no_of_zeros>3 & mdt$Total<=30),]
mdt_rm <- mdt[which(no_of_zeros>3 & mdt$Total<=30),]

# Taxonomy
group_pre17 <- data.frame(Item=colnames(mdt[1:20]), Type=c("A", "A", "B", "A", "A", "A", "B", "A", "A", "B", "A", "A", "A", "B", "A", "A", "A", "A", "B", "B"))


## ----- Load Diagnostic Test 2013 Data -----
mdt13 <- read.csv('../data/ANON_DiagTest_13-14.csv', header=T)
mdt13 <- mdt13[complete.cases(mdt13),]
mdt13 <- mdt13[order(mdt13$AnonID, -mdt13$Total),]
mdt13 <- mdt13[!duplicated(mdt13$AnonID),]
mdt13_easy <- mdt13[, paste0("Q", c(11,12,13,16,17))]
no_of_zeros13 <- rowSums(mdt13_easy==0)
mdt13_clean <- mdt13[-which(no_of_zeros13>3 & mdt13$Total<=30),]
mdt13_rm <- mdt13[which(no_of_zeros13>3 & mdt13$Total<=30),]


## ----- Load Diagnostic Test 2014 Data -----
mdt14 <- read.csv('../data/ANON_DiagTest_14-15.csv', header=T)
mdt14 <- mdt14[complete.cases(mdt14),]
mdt14 <- mdt14[order(mdt14$AnonID, -mdt14$Total),]
mdt14 <- mdt14[!duplicated(mdt14$AnonID),]
mdt14_easy <- mdt14[, paste0("Q", c(11,12,13,16,17))]
no_of_zeros14 <- rowSums(mdt14_easy==0)
mdt14_clean <- mdt14[-which(no_of_zeros14>3 & mdt14$Total<=30),]
mdt14_rm <- mdt14[which(no_of_zeros14>3 & mdt14$Total<=30),]


## ----- Load Diagnostic Test 2015 Data -----
mdt15 <- read.csv('../data/ANON_DiagTest_15-16.csv', header=T)
mdt15 <- mdt15[complete.cases(mdt15),]
mdt15 <- mdt15[order(mdt15$AnonID, -mdt15$Total),]
mdt15 <- mdt15[!duplicated(mdt15$AnonID),]
mdt15_easy <- mdt15[, paste0("Q", c(11,12,13,16,17))]
no_of_zeros15 <- rowSums(mdt15_easy==0)
mdt15_clean <- mdt15[-which(no_of_zeros15>3 & mdt15$Total<=30),]
mdt15_rm <- mdt15[which(no_of_zeros15>3 & mdt15$Total<=30),]


## ----- Load Diagnostic Test 2016 Data -----
mdt16 <- read.csv('../data/ANON_DiagTest_16-17.csv', header=T)
#Rearrange columns
mdt16 <- mdt16[, c(grep("Q", colnames(mdt16)), which(colnames(mdt16)==c("Total", "AnonID")))]
mdt16 <- mdt16[complete.cases(mdt16),]
mdt16 <- mdt16[order(mdt16$AnonID, -mdt16$Total),]
mdt16 <- mdt16[!duplicated(mdt16$AnonID),]
mdt16_easy <- mdt16[, paste0("Q", c(11,12,13,16,17))]
no_of_zeros16 <- rowSums(mdt16_easy==0)
mdt16_clean <- mdt16[-which(no_of_zeros16>3 & mdt16$Total<=30),]
mdt16_rm <- mdt16[which(no_of_zeros16>3 & mdt16$Total<=30),]


## ----- Load Diagnostic Test 2017 Data -----
mdt17fac <- read.csv('../data/ANON_DiagTest17regraded.csv', header=T)

# Remove row of means, rename questions, re-count levels
mdt17fac <- mdt17fac[-nrow(mdt17fac),]
colnames(mdt17fac)[5:25] <- c("Total", paste0("N", 1:20))
for (i in 1:20) {
  mdt17fac[, paste0("N", i)] <- factor(mdt17fac[, paste0("N", i)])
}

# Rearrange columns
mdt17fac <- mdt17fac[, c(grep("N", colnames(mdt17fac)), which(colnames(mdt17fac)==c("Total", "AnonID")))]

# Rename questions using old question number
qnum2017 <- c("Q1", "Q3", "N3", "Q5", "Q6", "Q7", "N7", "Q9", "Q10", "N10", "N11", "N12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20")
colnames(mdt17fac)[1:20] <- qnum2017

# Ordering data frame so that higher total score is placed above lower total score where exam numbers are duplicated
mdt17fac <- mdt17fac[order(mdt17fac$AnonID, -mdt17fac$Total),]
# Takes only the upper row of consecutive duplicated rows
mdt17fac_nodup <- mdt17fac[!duplicated(mdt17fac$AnonID),]

# Numeric version
mdt17 <- mdt17fac_nodup
mdt17[mdt17 == "-"] <- "0.00"
for (i in 1:20) {
  s <- mdt17[, i]
  mdt17[, i] <- as.numeric(levels(s))[s]
}

mdt17_easy <- mdt17[, c(11,12,18,16,17)]
no_of_zeros17_legacy <- rowSums(mdt17_easy==0)
no_of_zeros17 <- rowSums(mdt17fac_nodup[, c(11,12,18,16,17)]=='-')
mdt17_clean_legacy <- mdt17[-which(no_of_zeros17_legacy>3 & mdt17$Total<=30),]
mdt17_rm_legacy <- mdt17[which(no_of_zeros17_legacy>3 & mdt17$Total<=30),]
mdt17_clean <- mdt17[-which(no_of_zeros17>3 & mdt17$Total<=30),]
mdt17_rm <- mdt17[which(no_of_zeros17>3 & mdt17$Total<=30),]

group_17 <- data.frame(Item=colnames(mdt17_clean[1:20]), Type=c("A", "B", "A", "A", "A", "B", "B", "A", "B", "B", "C", "A", "A", "B", "A", "A", "A", "A", "B", "B"))



## ----- Load Diagnostic Test 2018 Data -----
mdt18fac <- read.csv('../data/ANON_DiagTest18A.csv', header=T)

# Remove row of means, rename questions, re-count levels
mdt18fac <- mdt18fac[-nrow(mdt18fac),]
colnames(mdt18fac)[5:25] <- c("Total", paste0("N", 1:20))
for (i in 1:20) {
  mdt18fac[, paste0("N", i)] <- factor(mdt18fac[, paste0("N", i)])
}

# Rearrange columns
mdt18fac <- mdt18fac[, c(grep("N", colnames(mdt18fac)), which(colnames(mdt18fac)==c("Total", "AnonID")))]

# Rename questions using old question number
# defined above qnum2017 <- c("Q1", "Q3", "N3", "Q5", "Q6", "Q7", "N7", "Q9", "Q10", "N10", "N11", "N12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20")
colnames(mdt18fac)[1:20] <- qnum2017

# Ordering data frame so that higher total score is placed above lower total score where exam numbers are duplicated
mdt18fac <- mdt18fac[order(mdt18fac$AnonID, -mdt18fac$Total),]
# Takes only the upper row of consecutive duplicated rows
mdt18fac_nodup <- mdt18fac[!duplicated(mdt18fac$AnonID),]

# Numeric version
mdt18 <- mdt18fac_nodup
mdt18[mdt18 == "-"] <- "0.00"
for (i in 1:20) {
  s <- mdt18[, i]
  mdt18[, i] <- as.numeric(levels(s))[s]
}

mdt18_easy <- mdt18[, c(11,12,18,16,17)]
no_of_zeros18_legacy <- rowSums(mdt18_easy==0)
no_of_zeros18 <- rowSums(mdt18fac_nodup[, c(11,12,18,16,17)]=='-')
mdt18_clean_legacy <- mdt18[-which(no_of_zeros18_legacy>3 & mdt18$Total<=30),]
mdt18_rm_legacy <- mdt18[which(no_of_zeros18_legacy>3 & mdt18$Total<=30),]
mdt18_clean <- mdt18[-which(no_of_zeros18>3 & mdt18$Total<=30),]
mdt18_rm <- mdt18[which(no_of_zeros18>3 & mdt18$Total<=30),]

group_18 <- data.frame(Item=colnames(mdt18_clean[1:20]), Type=c("A", "B", "A", "A", "A", "B", "B", "A", "B", "B", "C", "A", "A", "B", "A", "A", "A", "A", "B", "B"))

MDT3 = rbind(mdt17,mdt18)
MDT3clean = rbind(mdt17_clean,mdt18_clean)


## ----- List of categories -----
# Determine number of respones in each question
# catg <- list()
# for (i in 1:20) {
#     catg[[i]] <- sort(unique(mdt[,i]))
# }
# catg17 <- list()
# for (i in 1:20) {
#     catg17[[i]] <- sort(unique(mdt17[,i]))
# }

catg <- lapply(mdt[1:20], function (x) {sort(unique(x))})
catg17 <- lapply(mdt17[1:20], function (x) {sort(unique(x))})
catg18 <- lapply(mdt18[1:20], function (x) {sort(unique(x))})
catgMDT3 <- lapply(MDT3[1:20], function (x) {sort(unique(x))})


## ----- Matrices of scoring functions for mirt -----
multi <- 8
mats <- sapply(catg, "*", multi)
mats <- sapply(mats, as.matrix)
mats17 <- sapply(catg17, function (x) {
  as.matrix(round(x * multi))
})
mats18 <- sapply(catg18, function (x) {
  as.matrix(round(x * multi))
})
matsMDT3 <- sapply(catgMDT3, function (x) {
  as.matrix(round(x * multi))
})


## ----- Define colour palette -----
palette("default")
cc<-palette()
mypalette <- c(cc[c(1,3,5,6,7,8)],brewer.pal(12, "Paired")[-c(4,5)], brewer.pal(8, "Dark2")[-c(2,4,5,6)], brewer.pal(9, "Set1")[8], brewer.pal(12, "Set3")[c(1,8,10,12)])
palette(mypalette)
# Rearrange order of colours for Diagnostic Test 2017
pal17 <- palette()[c(1,3,21,5,6,7,22,9,10,23,24,25,13:20)]


## ----- Remove Useless Objects -----
rm(mdt_easy, mdt13_easy, mdt14_easy, mdt15_easy, mdt16_easy, mdt17_easy, no_of_zeros, no_of_zeros13, no_of_zeros14, no_of_zeros15, no_of_zeros16, no_of_zeros17)
