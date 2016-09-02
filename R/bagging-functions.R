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

#' Compute the Out-of-Bag examples.
#' 
#' \code{oobExamples} returns a matrix of logicals identifying for each example 
#' which bootstrap sample they did not participate.
#' 
#' The resulting matrix can be used to compute the out-of-bag error of a 
#' bagging procedure.
#' 
#' @inheritParams generateBootstrapSamples
#' @param bootstrap_samples A list where each element contains the indices of 
#'  a bootstrap sample.
#' @return A matrix of logicals where each row represents an example and each
#'  column represents a bootstrap sample. The i,j-th entry is \code{TRUE} if 
#'  the i-th example is not present in the j-th bootstrap sample, and 
#'  \code{FALSE} otherwise.

oobExamples <- function(m, bootstrap_samples){
  vapply(bootstrap_samples, function(x) !(1:m %in% x), logical(m))
}

bagging <- function(data, L = 100, oob_error = FALSE, cores = 1){
  # In order to use the formula interface in the rpart function
  names(data)[ncol(data)] <- "Class"
  
  m <- nrow(data)  # Number of examples
  bss <- generateBootstrapSamples(m, L)
  
  trainDecisionTree <- function(x){
    rpart::rpart(Class ~ ., data = data, subset = x, 
                 method = "class", parms = list(split = "information"))
  }
  bagging_models <- parallel::mclapply(bss, trainDecisionTree, mc.cores = cores)
  
  if (oob_error == TRUE) 
    preds <- oobPreds(bagging_models, subset(data, select = -Class), bss, cores)
  
  error <- 1 - mean(subset(data, select = Class) == preds)
  
  list(bagging_models = bagging_models, oob_error = error)
}

oobPreds <- function(bagging_models, data_preds, bootstrap_samples, cores){
  m <- nrow(data_preds)  # Number of examples
  oob_matrix <- oobExamples(m, bootstrap_samples)
  oob_models_list <- apply(oob_matrix, 1, function(x) which(x))
  
  majorityVoting <- function(i){
    example <- data_preds[i, ]
    models_idx <- oob_models_list[[i]]
    preds <-sapply(bagging_models[models_idx], 
                   function(model) predict(model, example, type = "class"))
    names(which.max(table(preds)))
  }
  
  unlist(parallel::mclapply(1:m, majorityVoting, mc.cores = cores))
}