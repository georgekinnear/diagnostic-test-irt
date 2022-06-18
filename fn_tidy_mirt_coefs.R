tidy_mirt_coefs <- function(x){
  x %>%
    # remove the GroupPars results produced by mirt, retaining only the item parameters
    purrr::list_modify("GroupPars" = NULL) %>%
    # go through the list and make each entry into a tibble
    purrr::map(as_tibble, rownames = "var") %>%
    # put each of the tibbles into long form
    purrr::map(pivot_longer, cols = !var, names_to = "par") %>%
    # flatten the list into a single tibble
    bind_rows(.id = "Question") %>%
    # tweak the variable names: relabel par (parameter) as est (estimate)
    mutate(var = if_else(var == "par", "est", var)) %>%
    # turn into a wider data frame
    pivot_wider(names_from = "var", values_from = "value")
}
