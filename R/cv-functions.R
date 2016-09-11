createStratifiedPartition <- function(y, folds = 10){
  if (is.data.frame(y)) y <- unlist(y, use.names = FALSE)
  classes_dist <- table(y)

  partition <- vector(mode = "numeric", length = length(y))
  for(i in seq_along(classes_dist)){
    max_sample <- ceiling(classes_dist[i] / folds) * folds
    folds_idx <- rep(1:folds, length.out = max_sample)
    class_partition <- sample(folds_idx)[1:classes_dist[i]]
    class_id <- names(classes_dist)[i]
    partition[y == class_id] <- class_partition
  }
  partition
}

cvTrain <- function(data, method, method_args = list(), 
                    folds, repeats, cores, seed = NULL, ...){
  if (!is.null(seed)) set.seed(seed)
  partitions <- replicate(repeats, 
                          createStratifiedPartition(data[[ncol(data)]], folds), 
                          simplify = FALSE)
  
  results <- parallel::mcMap(train, 
                             data = list(data), 
                             method = list(method), 
                             method_args = list(method_args), 
                             partition = partitions, 
                             cores = list(cores), 
                             mc.cores = cores)
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
    ensemble_acc <- mean(maj_vote == y)
    
    # Assemble results
    if (i == 1){
      results <- rbind(c(avg_ind_acc = avg_individual_acc, 
                         ensemble_acc = ensemble_acc, 
                         avg_pairwise_measures, non_pairwise_measures))
    } else{
      results <- rbind(results, 
                       c(avg_ind_acc = avg_individual_acc, 
                         ensemble_acc = ensemble_acc, 
                         avg_pairwise_measures, non_pairwise_measures))
    }
  }
  results
}