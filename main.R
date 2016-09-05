## Load packages -------------------------------
needs::needs(readr, ggplot2)

## Constants -----------------------------------
DATA_PATH <- "data"
R_PATH <- "R"
CORES <- 2  # For parallel processing
SEED <- 14563  # For a fair comparison between models
FOLDS <- 5
REPEATS <- 2

## Source files --------------------------------
source(file.path(R_PATH, "bagging-functions.R"))
source(file.path(R_PATH, "cv-functions.R"))
source(file.path(R_PATH, "diversity-functions.R"))
source(file.path(R_PATH, "util-functions.R"))

## Read data into the workspace ----------------
ca_data <- read_csv(file.path(DATA_PATH, "crx.csv"), 
                    col_names = FALSE, 
                    col_types = cols(
                      X11 = col_double(), 
                      X14 = col_double(), 
                      X16 = col_factor(c("+", "-"))
                    ), 
                    na = "?")
# If left as character columns, any values that weren't present when training 
# a tree won't be recognized by them when predicting.
ca_data[] <- lapply(ca_data, 
                    function(x) if (is.character(x)) as.factor(x) else x)

## Cross-validation process --------------------
L_range <- seq(10, 100, by = 10)
results <- vector(mode = "list", length = length(L_range))
for (i in seq_along(L_range)){
  results[[i]] <- cvTrain(ca_data, 
                          method = "bagging", 
                          method_args = list(L = L_range[i], cores = CORES), 
                          folds = FOLDS, repeats = REPEATS, 
                          cores = CORES, seed = SEED)
}

## Assemble the results ------------------------
f <- function(i){
  rslt <- rbind(results[[i]][[1]], results[[i]][[2]])
  rslt <- rbind(colMeans(rslt))
  cbind(as.data.frame(rslt), data.frame(L = L_range[i]))
}
results <- lapply(1:length(results), f)
results <- rbindList(results)
results_tidy <- tidyr::gather(results, measure, value, -L)
results_tidy[["measure"]] <- factor(results_tidy[["measure"]], 
                                    levels = c("avg_ind_acc", "ensemble_acc", 
                                               "correlation", "Q", 
                                               "binKappa", "disagreement", 
                                               "doubleFault", "entropy", 
                                               "KW", "kappa", 
                                               "difficultMeasure", "omega.1"), 
                                    labels = c("Avg. Ind. Acc.", 
                                               "Ensemble Acc.", "Correlation", 
                                               "Q", "Pairwise Kappa", 
                                               "Disagreement", "Double-Fault", 
                                               "Entropy", "KW", 
                                               "Non-Pairwise Kappa", 
                                               "Measure of Difficulty", 
                                               "Omega"))

## Plot the results ------------------------------
my_theme <- theme_bw()
p <- ggplot(results_tidy, aes(x = L, y = value, colour = measure)) +
  geom_line() + geom_point() + my_theme + ylab("Value") + 
  scale_colour_discrete(name = "Measure")
pply <- plotly::ggplotly(p)
