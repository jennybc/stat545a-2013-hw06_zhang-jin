stat545a-2013-hw06_zhang-jin
============================

STAT545A Assignment

Introduction 
Abalone, an excellent source of iron and pantothenic acid, is a nutritious food resource and farming in Australia, America and East Asia. 100 grams of abalone yields more than 20% recommended daily intake of these nutrients. Meanwhile, the nutrients found most abundant in abalone are selenium and phosphorous, which are microelements needy for human beings (Health Benefit Of, 2011). The nutrition in abalone is related to its age. The first 5 years of abalone are considered to be growing period, during which a fast growing rate is guaranteed with one inch longer per year. After 6 years, it turns into mature period, with the size and growing rate of abalones relatively stable. And there is more nutritious after abalones mature (Royal Hawaiian Seafood).

The economic value of abalone is positively correlated with its age. Therefore, to detect the age of abalone accurately is important for both farmers and customers to determine its price. ?However, the current technology to decide the age is quite costly and inefficient. Farmers usually cut the shells and count the rings through microscopes to estimate the abalone¡¯s age. This complex method increases the cost and limits its popularity. Our target is to find out the best indicators to forecast the rings, then the age of abalones. 

Description of Dataset
In this project, the data set ¡°Abalone¡± is obtained from UCI Machine Learning Repository (1995). The data set contains physical measurements of 4177 abalones recorded in December 1995 by Marine Research Laboratories Taroona, Department of Primary Industry and Fisheries, Tasmania, Australia. There are nine variables, namely, Sex, Length, Diameter, and Height, Whole weight, Shucked weight, Viscera weight, Shell weight and Rings.The variable ¡°Rings¡± is linearly related to the age of an abalone, as age equals to number of rings plus 1.5.