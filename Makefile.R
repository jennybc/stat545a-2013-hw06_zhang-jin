## one script to rule them all

## clean out any previous work
outputs <- c("abalone_clean.csv",list.files(pattern = "*.png$"))
file.remove(outputs)

## run my scripts
source("style.R") # Not important, just make RPubs file beautiful
source("HW6_part1.R")
source("HW6_part2.R")