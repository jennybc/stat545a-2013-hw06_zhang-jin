library(ggplot2)
library(plyr)
library(nnet)
library(MASS)
library(gridExtra)
library(lattice)
library(RColorBrewer)

Data = read.csv("abalone_clean.csv")  # Import Data
head(Data)
# Number of abalone of different Sex in different Age group.
with(Data, table(Sex, Age))

# First= of all, I want to test whether Observation data like Height, Whole.weight can be helpful to devide the abalone to different Age group. Logistice regression is applied to do this analysis.
ggplot(Data, aes(x = Whole.weight, y=Height)) + geom_point(aes(colour = Rings)) +scale_colour_gradient(low = "purple") + stat_smooth(colour="red") + ggtitle("Whole.weight vs Height")

ggplot(Data, aes(x = Length, y=Height)) + geom_point(aes(colour = Rings)) +scale_colour_gradient(low = "purple") + stat_smooth(colour="red") + ggtitle("Length vs Height")

ggplot(Data, aes(x =Length , y=Diameter)) + geom_point(aes(colour = Rings)) +scale_colour_gradient(low = "purple") + stat_smooth(colour="red") + ggtitle("Length vs Diameter")

ggsave("Whole.weight vs Height.png")
ggsave("Length vs Height.png")
ggsave("WLength vs Diameter.png")

# From above graphs, it is obvious that Length, Height and Diameter are relatively linear correlated. In fact, it is natural to think that the bigger the abalone, the haeavier they are, so I want ot check whether it is perfectly correlated between Whole.weight and Volume. 
# Construct Volum and Whole.weight dataset

yData = ddply(Data, ~Sex+Age, summarize, Volume = Length * Diameter * Height,  Whole.weight = Whole.weight,Rings = Rings)
# Draw the point and regression lines
cols <- c("Linear"="#f04546","Cubic"="#3591d1")
ggplot(data = yData, aes(x = Whole.weight, y = Volume)) + 
  geom_smooth(method = "lm",aes(colour = "Linear"),lwd = 1 )+ 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3),aes( colour = "Cubic"),lwd = 1)+
  geom_point(alpha = 1/10,colour = "purple") + 
  scale_colour_manual("Type of Regression",values = cols) + 
  ggtitle("Volume vs Whole.weight") + coord_flip() + 
  scale_y_continuous(name = "Volume", breaks = seq(0, 0.15, by = 0.03))
ggsave("Volume vs Whole.weight.png")
# From the graph above, it can be seen that Cubic regression and Linear regression is almost same in the begining, but diverge when Volume is large, which means that Volume and Whole.weight is highly correlated for small abalone.

# Why not just use Whole.weight to predict the Rings of abalone? 
# Calculate mean and variance of Whole.weight in different Sex.
with(Data, do.call(rbind, tapply(Whole.weight, Sex, function(x) c(M = mean(x), SD = sd(x)))))
# It seems that F amd M are more heavier than I. Does that mean I group do not have large rings?
ggplot(data=Data, aes(x = Rings,group=Sex,colour=Sex)) + geom_density() + ggtitle("Density of Rings of each Sex")
# Sex I group abalone is yougner than F and M group, but it do have old abalone with Sex I.
ggsave("Density of Rings of each Sex.png")

ggplot(data = Data, aes(x = factor(Age), y = Whole.weight)) + geom_boxplot(outlier.colour = "purple", outlier.size = 3,aes(fill = Sex)) + facet_wrap(~Sex, ncol=3) + ggtitle("Whole.weight vs Age for different Sex") + xlab("Age")
ggsave("Whole.weight vs Age for different Sex.png")
# Compare to M and F, it seems that abalone with sex I have less weight. 

# Fit Multinomial Regression Model
logit <- multinom(Age ~  Whole.weight, data = yData)
summary(logit) 
pp = fitted(logit)
pred<-predict(logit)
table(pred,Data$Age)

# Construct a new dataset with information above.
newdata = with(yData, data.frame(Weight = Whole.weight,Sex = Sex, Pred_Prob = pp, Age = Age,Pred_Age = pred))
head(newdata)
ggplot(newdata, aes(x = Pred_Age, y=Age, colour = factor(Pred_Age==Age))) +
  geom_point(position = "jitter") +
  scale_colour_manual("Age Group", values = c("2", "3"), labels = c("Wong Classification","Right Classification")) +
  ggtitle("Classification") 
ggsave("Classification.png")

# Plot the Predicted Probability for different age group
cols <- c("Young"="blue","Adult"="#f04546", "Old" = "green")
ggplot(newdata, aes(x= Weight)) + 
  geom_line(aes(y = Pred_Prob.1, colour = "Young")) + 
  geom_line(aes(y = Pred_Prob.2, colour = "Adult")) +
  geom_line(aes(y = Pred_Prob.3, colour = "Old")) + 
  ylab("Predicted Probability") +  facet_wrap(~Sex, ncol=3) +
  scale_colour_manual("Age Group", values = cols) 
ggsave("Predicted Probability.png")
# From the graph, we can tell that prediction for Young gropu should be good,but it may difficult to distinguish Old and Adult when Weight is heavy.

# It seems that more Sex I abalone is mis-classfied to Young and Adult group, but more Sex F abalone is mis-classfied to Old grope. And the biggest problem is in Old group. One reason maybe that we adopt linear combinaiton of varaibles in multinomial regression instead. 

# How about using Linear Discriminant Analysis?
# Only using Whole.weight 
LDA1 = lda(Age ~ Whole.weight, yData)
pred1 = predict(LDA1)$class
newdataL1 = with(yData, data.frame(Weight = Whole.weight,Sex = Sex,Age = Age, Pred_Age1 = pred1))
Data_misL1 = subset(newdataL1, Age != Pred_Age1)

# Using both Whole.weight and Volume.
LDA2 = lda(Age ~ ., yData)
pred2 = predict(LDA2)$class
newdataL2 = with(yData, data.frame(Weight = Whole.weight,Sex = Sex,Age = Age, Pred_Age2 = pred2))
Data_misL2 = subset(newdataL2, Age != Pred_Age2)

p1 = ggplot(Data_misL1, aes(x = Age,fill = Sex)) + geom_bar(binwidth = 1/2,color = "darkgrey") + 
  ylab("Number of mis-classfied data") + 
  ggtitle(" Using Whole.weight only") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer("Sex", type = "qual", palette = 3)
p2 = ggplot(Data_misL2, aes(x = Age,fill = Sex)) + geom_bar(binwidth = 1/2,color = "darkgrey") + 
  ylab("Number of mis-classfied data") + 
  ggtitle("Using Whole.weight & Volume") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer("Sex", type = "qual", palette = 3)
grid.arrange(p1, p2, main = "Mis Classified Data for Different Sex__LDA")
ggsave("Mis Classified Data for Different Sex__LDA.png")
# From above analysis, we find out that even though Whole.weight is highly correlated to Volume, but Volume have significant influence on the classification problem. Does this mean Volume is more important to do classification?

# Only use Volume
LDA3 = lda(Age ~ Volume, yData)
pred3 = predict(LDA3)$class
newdataL3 = with(yData, data.frame(Weight = Whole.weight,Sex = Sex,Age = Age, Pred_Age3 = pred3))
Data_misL3 = subset(newdataL3, Age != Pred_Age3)


ggplot(newdataL2, aes(x = Pred_Age2, y=Age, colour = factor(Pred_Age2==Age))) +
  geom_point(position = "jitter") + 
  scale_colour_manual("Age Group", values = c("2", "3"), labels = c("Wong Classification","Right Classification")) +
  ggtitle("Classification using Whole.weight & Volume")
ggsave("Classification using Whole.weight & Volume.png")
ggplot(newdataL3, aes(x = Pred_Age3, y=Age, colour = factor(Pred_Age3==Age))) +
  geom_point(position = "jitter") + 
  scale_colour_manual("Age Group", values = c("2", "3"), labels = c("Wong Classification","Right Classification")) +
  ggtitle("Classification Using Volume")
ggsave("Classification Using Volume.png")

# The answer is NO. Using Volume and Whole.weight seperately will not give a good classification.

# Little conlusion: Using LDA is more efficient to classify the data then multinomial regression. It is sure that using observations ie, Whole.weight and Volume together, we can already classify the avalone into different Age group well.

# Then, which variable is more important, Length, Diameter, Height, or Whole.weight? We do not want to consider Volume now because this measure can not be obtained directly. So we will go back to above four easily measured variables.
# New data frame which record the minimum value and maximum value of each variable.
jData <- ddply(Data, ~ Sex + Rings, function(x) {
  jLevels <- c("min", "max")
  Length = range(x$Length)
  Height = range(x$Height)
  Diameter = range(x$Diameter)
  Whole.weight = range(x$Whole.weight)
  return(data.frame(Length, Height, Diameter, Whole.weight, stat = jLevels))
})
# Plot the graph 
p_Height = ggplot(jData, aes(x = Rings,y = Height, group = stat,colour = stat)) + geom_line() + geom_point() + facet_wrap(~Sex, ncol=3) + ggtitle("Height: Min vs Min") + theme(legend.position = "none")
p_Length = ggplot(jData, aes(x = Rings,y = Length, group = stat,colour = stat)) + geom_line() + geom_point() + facet_wrap(~Sex, ncol=3) + ggtitle("Length: Min vs Max")  + theme(legend.position = "none")
p_Diameter = ggplot(jData, aes(x = Rings,y = Diameter, group = stat,colour = stat)) + geom_line() + geom_point() + facet_wrap(~Sex, ncol=3) + ggtitle("Diameter: Min vs Max") + theme(legend.position = "none")
p_Whole.weight = ggplot(jData, aes(x = Rings,y = Whole.weight, group = stat,colour = stat)) + geom_line() + geom_point() + facet_wrap(~Sex, ncol=3) + ggtitle("Whole.weight: Min vs Max") 

tmp <- ggplot_gtable(ggplot_build(p_Whole.weight))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
legend <- tmp$grobs[[leg]]

grid.arrange(arrangeGrob(p_Whole.weight+theme(legend.position="none"),p_Length,p_Height,p_Diameter))
ggsave("Min vs Max within each Observed Varialbes.png")

# Try the similar thing in xyplot
px_Height = xyplot(Height ~  Rings| Sex, jData, group=stat, auto.key =  list(columns = 2,x = 0.35, y = 0.85, corner = c(0, 1)), type=c("p", "l"),par.settings = list(superpose.line = list(col = c("red", "blue")), superpose.symbol = list(col = c("red", "blue"))), grid = "h")
grid.arrange(arrangeGrob(p_Height+xlab(""),px_Height))
ggsave("ggplot vs xyplot.png")

# From the graph above, we can see that Whole.weight and Height share the similar trend of range, while Length and Diameter share similar trend of range. Does it mean Whole.wieght is more close related to Height, and Length is more related to Diameter?

# Regression.
# Height
jFun <- function(x) {
  name = c()
  Coefs = c()
  for(i in 1:4 ){
  temp = c()
  temp <- rbind(temp,as.numeric(coef(lm(x[,i+1] ~ Rings, x))))
  Coefs = rbind(Coefs, temp)
  name = rbind(name, names(x[i+1]))
  }
  return(data.frame(Intercept = Coefs[,1],Slope = Coefs[,2] ,Variable = name))
}
jCoefs <- ddply(Data, ~ Sex, jFun)

foo <- ddply(jCoefs, ~ Variable, function(x) {
    lerange <- c(which.min(x$Intercept), which.max(x$Intercept))
    cbind(x[lerange, c("Variable", "Sex", "Intercept", "Slope")], stat=c("min_slope", "max_slope"))
 })

# From above table, it shows that Sex I have min_slope for all Variables, and Sex F have max_slope for all variables.
# Plot the regression line of Height
ggplot(subset(Data, Sex %in% c("F","I")), aes(x = Rings, y = Height) ) +   facet_wrap(~Sex, ncol=2) + 
  geom_smooth(method = "lm", colour = "purple") + 
  geom_point(aes(colour = factor(Age))) +
  ggtitle("Max_slope vs Min_slope of Height") + 
  scale_colour_manual("Age Group",values = c("4","3","5"), labels = c("Young","Adult","Old")) 
ggsave("Max_slope vs Min_slope of Height.png")

# Plot the regression line of Diameter
ggplot(subset(Data, Sex %in% c("F","I")), aes(x = Rings, y = Diameter)) +   facet_wrap(~Sex, ncol=2) + 
  geom_smooth(method = "lm", colour = "purple") + 
  geom_point(aes(colour = factor(Age))) +
  ggtitle("Max_slope vs Min_slope of Diameter") + 
  scale_colour_manual("Age Group",values = c("4","3","5"), labels = c("Young","Adult","Old")) 
ggsave("Max_slope vs Min_slope of Diameter.png")

# Plot the regressio line of Length
ggplot(subset(Data, Sex %in% c("F","I")), aes(x = Rings, y = Length)) +   facet_wrap(~Sex, ncol=2) + 
  geom_smooth(method = "lm", colour = "purple") + 
  geom_point(aes(colour = factor(Age))) +
  ggtitle("Max_slope vs Min_slope of Length") + 
  scale_colour_manual("Age Group",values = c("4","3","5"), labels = c("Young","Adult","Old")) 
ggsave("Max_slope vs Min_slope of Length.png")

# Plot the regression line of Whole.weight
ggplot(subset(Data, Sex %in% c("F","I")), aes(x = Rings, y = Whole.weight)) +   facet_wrap(~Sex, ncol=2) + 
  geom_smooth(method = "lm", colour = "purple") + 
  geom_point(aes(colour = factor(Age))) +
  ggtitle("Max_slope vs Min_slope of Whole.weight") + 
  scale_colour_manual("Age Group",values = c("4","3","5"), labels = c("Young","Adult","Old")) 
ggsave("Max_slope vs Min_slope of Whole.weight.png")


