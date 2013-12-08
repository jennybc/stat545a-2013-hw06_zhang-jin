## one script to rule them all

## clean out any previous work
outputs <- c("data/abalone_clean.csv",
             list.files("Graphs", pattern = "*.png$", full.names = TRUE))
file.remove(outputs)

## run my scripts
source("code/style.R") # Not important, just make RPubs file beautiful
source("HW6_part1.R")
source("HW6_part2.R")