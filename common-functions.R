# Take a
tidy_mirt_coefs <- function(x){
  x %>%
    # melt the list element
    melt() %>%
    # convert to a tibble
    as_tibble() %>%
    # convert factors to characters
    mutate(across(where(is.factor), as.character)) %>%
    # only focus on rows where X2 is a or b (discrimination or difficulty)
    filter(X2 %in% c("a", "b")) %>%
    # in X1, relabel par (parameter) as est (estimate)
    mutate(X1 = if_else(X1 == "par", "est", X1)) %>%
    # unite columns X2 and X1 into a new column called var separated by _
    unite(X2, X1, col = "var", sep = "_") %>%
    # turn into a wider data frame
    pivot_wider(names_from = var, values_from = value)
}

# Given a table of results (columns are items and rows are students) that may include partial marks for items,
# rescore the items to be consecutive integers
recategorise_marks <- function(data, MAX_MARKS = NULL){
  MARK_CATEGORIES <- lapply(apply(data, 2, unique), sort)
  MAX <- unlist(lapply(MARK_CATEGORIES,max))
  if(is.null(MAX_MARKS)){
    MAX_MARKS <- MAX
  }else{
    if(any(MAX_MARKS < MAX))
      stop("There is a mark that is greater than the specified max.")
  }
  data_recat <- data
  for(item in 1:ncol(data)){
    for(subj in 1:nrow(data)){
      if(is.na(data[subj,item])) {
        data_recat[subj,item] <- NA
      } else {
        data_recat[subj,item] <- which(MARK_CATEGORIES[[item]] %in% data[subj,item])
      }
    }
  }
  return(list(data_recat = data_recat, MARK_CATEGORIES = MARK_CATEGORIES, MAX_MARKS = MAX_MARKS))
}
