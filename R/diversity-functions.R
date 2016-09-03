oracleMatrix <- function(x){
  x <- x[complete.cases(x), ]
  d1 <- x[, 1] == x[, 3]
  d2 <- x[, 2] == x[, 3]
  
  a <- sum(d1 & d2)
  b <- sum(d1 & !d2)
  c <- sum(!d1 & d2)
  d <- sum(!(d1 | d2))
  
  matrix(c(a, b, c, d), byrow = TRUE, nrow = 2, ncol = 2) / nrow(x)
}

correlation <- function(a, b, c, d){
  (a * d - b * c) / sqrt((a + b) * (c + d) * (a + c) * (b + d))
}

qStatistic <- function(a, b, c, d){
  (a * d - b * c) / (a * d + b * c)
}

interraterAgreement <- function(a, b, c, d){
  (2 * (a * d - b * c)) / ((a + b) * (c + d) * (a + c) * (b + d))
}

disagreementMeasure <- function(b, c){
  b + c 
}

doubleFaultMeasure <- function(d){
  d
}