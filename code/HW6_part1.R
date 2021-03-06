library(ggplot2)
library(plyr)
# The data 
Data = read.csv("data/abalone.csv")  # Import Data

str(Data) # Structure of the Data

# There are 4 different measures for weight i.e. Whole.weight, Shucked.weight, Viscera.weight and Shell.weight. And Whole.weight should be the easiist one to measure. So I drop out all the other measures. 
Data =  subset(Data, select= -c(Shucked.weight,Viscera.weight,Shell.weight))

# Plot number of abalone with different rinng. 
ggplot(Data, aes(x = Rings, fill = Sex)) +
  geom_bar(binwidth = 1, color = "blue", origin = min(Data$Rings), position = "identity") +
  scale_x_continuous(name = "Rings", breaks = seq(0, 30, by = 2)) +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer("Location", type = "qual", palette = 3) +
  ylab("Number of abalone") +
  ggtitle("Number of abalone with different Rings") + facet_wrap(~Sex, ncol=3) 

ggsave(file.path("graphs", "Number of abalone with different Rings.png"))
# From the graph above, we can see that the range of the Rings is from 1 to 29, which maybe too much to measure. In reality, people may not require so detaied catogory. So I group abalones with less than 6 rings (<7.5 years old), from 6 to 13 rings (7.5 to 14.5 years old) and more than 13 rings (>14.5 years old), indicating young, adult and old abalones correspondingly, and lable them as 1,2,3.
theBreaks <- c(0, 6, 13, 30)
gData <-
  data.frame(Data,
             AgeCodes = cut(Data$Rings, breaks = theBreaks, labels = FALSE),
             Age = cut(Data$Rings, breaks = theBreaks))
str(gData)

# Roughly plot the graph and get little tast about data
ggplot(gData, aes(x = Height, y = factor(Rings), colour= factor(Sex) )) +
  geom_jitter(position = position_jitter(width = .3))  +
  geom_point() + ggtitle("Original Data: Height vs Rings") +
  scale_colour_brewer(type="seq",palette="Set1") 

ggsave(file.path("graphs", "Original Data: Height vs Rings.png"))
# From the graph above we can see that there are some outliers in Female. I want to get rid of them.
jData = subset(gData, Height <0.4)
ggplot(jData, aes(x = Height, y = factor(Rings), colour= factor(Sex) ))+
  geom_jitter(position = position_jitter(width = .1))  + geom_point() +
  ggtitle("Reduced Data: Height vs Rings") +
  scale_colour_brewer(type="seq",palette="Set1") 

ggsave(file.path("graphs", "Reduced Data: Height vs Rings.png"))


# reorder data according to Sex and Rings
Order_Data = arrange(jData, Sex, Rings, Length)
write.table(Order_Data,
            file.path("data", "abalone_clean.csv"), 
            sep = ",", row.names = FALSE)





