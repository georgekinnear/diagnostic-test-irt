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
