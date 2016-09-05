rbindList <- function(df_list){
  len <- length(df_list)
  if (len <= 1) return(df_list)
  result <- rbind(df_list[[1]], df_list[[2]])
  for (i in seq_len(len)[-(1:2)]){
    result <- rbind(result, df_list[[i]])
  }
  result
}