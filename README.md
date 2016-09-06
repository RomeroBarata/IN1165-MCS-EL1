# IN1165 MCS EL1
First exercise list for **IN1165 Multiple Classifier Systems**.

## How to run it?
Running the script `main.R` should do all the work. It can be done from the `R` console with the command `source("main.R")` (this directory should be your working directory). Script configuration is also in `main.R`. You can change the range of `L` values used by the `bagging` algorithm, the cross-validation parameters, and the number of cores for parallel processing. If you are a Windows user please change the number of cores to 1. The final results will be stored in the variables `results` (table) and `pl` (graph).

## Organization
The data set utilised for the experimentation is in the `data` folder. All the implemented functions are logically organised into scripts inside the `R` folder. The `figures` folder contains images for the report. A report containing my answers for the first exercise list is in the file `report.html`.
