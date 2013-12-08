## one script to rule them all

## clean out any previous work
outputs <- c("data/abalone_clean.csv",
             list.files("graphs", pattern = "*.png$", full.names = TRUE))
file.remove(outputs)

## run my scripts
source("code/HW6_part1.R")
source("code/HW6_part2.R")