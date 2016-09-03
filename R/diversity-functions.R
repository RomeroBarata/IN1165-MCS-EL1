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