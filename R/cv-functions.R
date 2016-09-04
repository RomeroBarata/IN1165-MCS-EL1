createStratifiedPartitions <- function(y, folds = 10, repeats = 5){
  if (is.data.frame(y)) y <- unlist(y, use.names = FALSE)
  classes_dist <- table(y)
  stratified_partitions <- list()
  for (i in seq_len(repeats)){
    partition <- vector(mode = "numeric", length = length(y))
    for(j in seq_along(classes_dist)){
      max_sample <- ceiling(classes_dist[j] / folds) * folds
      folds_idx <- rep(1:folds, length.out = max_sample)
      class_partition <- sample(folds_idx)[1:classes_dist[j]]
      class_id <- names(classes_dist)[j]
      partition[y == class_id] <- class_partition
    }
    stratified_partitions[[paste("rep", i, sep = "-")]] <- partition
  }
  stratified_partitions
}

cvTrain <- function(data, method, method_args = list(), 
                    folds, repeats, cores, ...){
  partitions <- createStratifiedPartitions(data[[ncol(data)]], 
                                           folds = folds, 
                                           repeats = repeats)
  results <- parallel::mcMap(train, 
                             data = list(data), 
                             method = list(method), 
                             method_args = list(method_args), 
                             partition = partitions, 
                             cores = cores, 
                             mc.cores = cores, 
                             ...)
}

train <- function(data, method, method_args = list(), partition, cores, ...){
  folds <- length(unique(partition))
  for (i in seq_len(folds)){
    training <- data[partition != i, ]
    model <- do.call(method, c(list(data = training), method_args))
    
    testing <- data[partition == i, -ncol(data)]
    predictions <- predict(model, testing)
    
    y <- unlist(data[partition == i, ncol(data)], use.names = FALSE)
    
    # Average individual accuracy
    individual_acc <- vapply(1:ncol(predictions), 
                             function(j) mean(predictions[, j] == y), 
                             numeric(1))
    avg_individual_acc <- mean(individual_acc)
    # Average parwise diversity measures
    pairwise_combs <- combn(ncol(predictions), 2, simplify = FALSE)
    f <- function(cols){
      oracle_matrix <- oracleMatrix(predictions[, cols], y)
      pairwiseMeasures(oracle_matrix)
    }
    pairwise_measures <- vapply(pairwise_combs, f, numeric(5))
    avg_pairwise_measures <- rowMeans(pairwise_measures)
    # Non-pairwise measures
    non_pairwise_measures <- nonPairwiseMeasures(predictions, y)
    # Ensemble error
    maj_vote <- apply(predictions, 1, function(x) names(which.max(table(x))))
    ensemble_error <- 1 - mean(maj_vote == y)
    
    if (i == 1){
      results <- rbind(c(avg_ind_acc = avg_individual_acc, 
                         avg_pairwise_measures, non_pairwise_measures, 
                         ensemble_error = ensemble_error))
    } else{
      results <- rbind(results, 
                       c(avg_ind_acc = avg_individual_acc, 
                         avg_pairwise_measures, non_pairwise_measures, 
                         ensemble_error = ensemble_error))
    }
  }
  results
}