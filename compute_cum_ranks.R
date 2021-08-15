compute_cum_ranks <- function(data) {
  f_rank <- function(data) {
    f1 <- function(x) {
      frankv(x, order = , ties.method = "first")
    }
    # browser()
    df <- select_if(data, ~ is.numeric(.x))
    # df
    df_name <- names(df)
    #
    new_df <- as.data.frame(sapply(df, f1, simplify = FALSE))
    # print(new_df)
    colnames(new_df) <- paste0("rank_", df_name)
    data.frame(data, new_df)
  }
  
  data %>%
    group_by(DateRep) %>%
    tidyr::nest() -> data2
  
  data2$data <- lapply(data2$data, f_rank)
  data2 <- tidyr::unnest(data2)
  data2 <- ungroup(data2)
  return(data2)
}
