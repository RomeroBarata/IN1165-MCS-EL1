#' Generate bootstrap samples.
#' 
#' \code{generateBootstrapSamples} returns a list containing the indices of 
#' bootstrap samples.
#' 
#' Given a number of examples \code{m} and a number of bootstrap samples 
#' \code{b}, the function returns a list of \code{b} elements where each of 
#' them is a vector of length \code{m} containing the examples' indices.
#' 
#' @param m Number of examples in the data set.
#' @param b Number of bootstrap samples.
#' @return A list with \code{b} elements, each containing the indices of a 
#'  bootstrap sample.

generateBootstrapSamples <- function(m, b){
  replicate(b, sample(m, size = m, replace = TRUE), simplify = FALSE)
}

#' Generate an ensemble of trees through the bagging algorithm.
#' 
#' \code{bagging} generates a tree ensemble using the bagging strategy, where 
#' each tree in the ensemble is trained with a boostrap sample from the 
#' original data set.
#' 
#' The base decision tree is the one available in the \code{rpart} package.
#' 
#' @param data A data frame containing the predictors and the outcome. The 
#'  outcome must be a binary factor and the last column.
#' @param L Number of trees in the ensemble.
#' @param cores Number of cores for parallel processing. If running on Windows 
#'  \code{cores} must be left as 1.
#' @return A list containing the trained trees, the trees' predictions, and 
#'  the out-of-bag error of the ensemble.

bagging <- function(data, L = 100, cores = 1){
  # In order to use the formula interface in the rpart function
  names(data)[ncol(data)] <- "Class"
  
  # Generate the bootstrap samples
  m <- nrow(data)  # Number of examples
  bootstrap_samples <- generateBootstrapSamples(m, L)
  
  trainDecisionTree <- function(bootstrap_sample){
    rpart::rpart(Class ~ ., data = data, subset = bootstrap_sample, 
                 method = "class", parms = list(split = "information"))
  }
  bagging_models <- parallel::mclapply(bootstrap_samples, 
                                       trainDecisionTree, mc.cores = cores)
  
  makePreds <- function(model, x, bootstrap_sample){
    preds <- rep(NA, nrow(x))
    preds[-bootstrap_sample] <- predict(model, x[-bootstrap_sample, ], type = "class")
    preds
  }
  preds <- parallel::mcMap(makePreds, 
                           bagging_models, 
                           list(subset(data, select = -Class)), 
                           bootstrap_samples, 
                           mc.cores = cores)
  lvls <- levels(data$Class)
  preds <- lapply(preds, function(x) ifelse(x == 1, lvls[1],
                                            ifelse(x == 2, lvls[2], NA)))
  preds <- matrix(unlist(preds, use.names = FALSE), ncol = L)
  colnames(preds) <- paste("Tree", 1:L)

  majorityVote <- function(x){
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    names(which.max(table(x)))
  }
  oob_preds <- apply(preds, 1, majorityVote)
  oob_error <- 1 - mean(data$Class == oob_preds, na.rm = TRUE)
  list(bagging_models = bagging_models, models_predictions = preds, 
       oob_error = oob_error)
}