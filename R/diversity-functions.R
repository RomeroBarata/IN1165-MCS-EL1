oracleMatrix <- function(predictions, ground_truth){
  # idx <- complete.cases(predictions)
  # predictions <- predictions[idx, ]
  d1 <- predictions[, 1] == ground_truth
  d2 <- predictions[, 2] == ground_truth
  
  a <- sum(d1 & d2)
  b <- sum(d1 & !d2)
  c <- sum(!d1 & d2)
  d <- sum(!(d1 | d2))
  
  matrix(c(a, b, c, d), byrow = TRUE, nrow = 2, ncol = 2) / nrow(predictions)
}

correlation <- function(a, b, c, d){
  (a * d - b * c) / sqrt((a + b) * (c + d) * (a + c) * (b + d))
}

qStatistic <- function(a, b, c, d){
  (a * d - b * c) / (a * d + b * c)
}

interraterAgreement <- function(a, b, c, d){
  (2 * (a * d - b * c)) / ((a + b) * (b + d) + (a + c) * (c + d))
}

disagreementMeasure <- function(b, c){
  b + c 
}

doubleFaultMeasure <- function(d){
  d
}

pairwiseMeasures <- function(oracle_matrix){
  a <- oracle_matrix[1, 1]
  b <- oracle_matrix[1, 2]
  c <- oracle_matrix[2, 1]
  d <- oracle_matrix[2, 2]
  
  c(correlation = correlation(a, b, c, d), 
    Q = qStatistic(a, b, c, d), 
    binKappa = interraterAgreement(a, b, c, d), 
    disagreement = disagreementMeasure(b, c), 
    doubleFault = doubleFaultMeasure(d))
}

entropyMeasure <- function(predictions, ground_truth){
  f <- function(i){
    preds <- predictions[i, ] == ground_truth[i]
    preds <- preds[!is.na(preds)]
    a <- sum(preds)
    b <- length(preds) - a
    min(a, b)
  }
  total <- sum(sapply(1:nrow(predictions), f))
  (1 / nrow(predictions)) * (2 / (ncol(predictions) - 1)) * total
}

KW <- function(predictions, ground_truth){
  f <- function(i){
    preds <- predictions[i, ] == ground_truth[i]
    preds <- preds[!is.na(preds)]
    a <- sum(preds)
    b <- length(preds) - a
    a * b
  }
  total <- sum(sapply(1:nrow(predictions), f))
  (1 / (nrow(predictions) * (ncol(predictions)^2))) * total
}

nonPairwiseInterraterAgreement <- function(predictions, ground_truth){
  f <- function(j){
    preds <- predictions[, j] == ground_truth
    mean(preds, na.rm = TRUE)
  }
  L <- ncol(predictions)
  p_hat <- mean(sapply(1:L, f))
  1 - (L * KW(predictions, ground_truth)) / ((L - 1) * p_hat * (1 - p_hat))
}

difficultMeasure <- function(predictions, ground_truth){
  N <- nrow(predictions)
  L <- ncol(predictions)
  num_correct <- vapply(1:N, 
                        function(i) sum(predictions[i, ] == ground_truth), 
                        numeric(1))
  var(num_correct / L)
}

omegaMeasure <- function(predictions , ground_truth){
  N <- nrow(predictions)
  L <- ncol(predictions)
  num_correct <- vapply(1:N, 
                        function(i) sum(predictions[i, ] == ground_truth), 
                        numeric(1))
  num_correct <- num_correct[num_correct > 0 & num_correct < L]
  correct_distribution <- table(factor(num_correct, levels = 1:(L - 1)))
  
  total <- 0
  if ((L %% 2) == 0){
    a <- (L / 2):2
    b <- 1:(L / 2)
    idx <- c(a, b)
    for (i in 1:(L - 1)){
      total <- total + (1 / idx[i]) * correct_distribution[i]
    }
  } else{
    a <- floor(L / 2):1
    b <- 1:floor(L / 2)
    idx <- c(a, b)
    for (i in 1:(L - 1)){
      total <- total + (1 / idx[i]) * correct_distribution[i]
    }
  }
  total / N
}

nonPairwiseMeasures <- function(predictions, ground_truth){
  c(entropy = entropyMeasure(predictions, ground_truth), 
    KW = KW(predictions, ground_truth), 
    kappa = nonPairwiseInterraterAgreement(predictions, ground_truth), 
    difficultMeasure = difficultMeasure(predictions, ground_truth), 
    omega = omegaMeasure(predictions, ground_truth))
}