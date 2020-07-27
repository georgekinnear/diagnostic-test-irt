# take all possible samples of 32 out of 36 questions for 2018 data,
# rerun and record a statistic for each,
# show result wouldn't change much with other selections

# load packages ----------------------------------------------------------------

library(tidyverse)
library(mirt)      # For IRT analysis
library(bench)     # To measure time

# load data --------------------------------------------------------------------

eth_2018_32 <- read_csv("data/2018-q32/all-2018.csv")
eth_2018_36 <- read_csv("data/2018-q36/all.csv")
eth_2019 <- read_csv("data/2019-q32/all-2019.csv")

# identify all possible samples of 32 out of 36 questions for 2018 data --------

# each row of the following is one possible sample of 32 questions out of 36
questions    <- paste0("A", 1:36)
combinations <- combn(questions, 32)
combinations <- t(combinations)

# rerun and record a statistic for each ----------------------------------------

#for (i in 1:nrow(combinations)){
beg_time <- Sys.time()
  for (i in 1:5){
    curent_sample <- eth_2018_36 %>% select(combinations[i, ])
    fit_2pl_2018 <- mirt(
      data = curent_sample,  # use current sample
      model = 1,             # number of factors to extract
      itemtype = "2PL",      # 2 parameter logistic model
      SE = TRUE              # estimate standard errors
    )
  }
end_time <- Sys.time()

end_time-beg_time # Time difference of 8.399646 secs

83996.46/60/60    # Could take ~day to run, but parallelizable
