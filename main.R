## Load packages -------------------------------
needs::needs(readr)

## Constants -----------------------------------
DATA_PATH <- "data"
R_PATH <- "R"
CORES <- 2  # For parallel processing

## Source files --------------------------------
source(file.path(R_PATH, "bagging-functions.R"))
source(file.path(R_PATH, "cv-functions.R"))
source(file.path(R_PATH, "diversity-functions.R"))

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