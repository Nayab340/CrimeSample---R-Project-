View(crimesample)

install.packages("tidyverse")

library(tidyverse)

dim(crimesample)

#removing missing values 
colSums(is.na(crimesample))


colSums(is.na(crimesample))

crimesample <- crimesample[ ,!(names(crimesample) %in%drops)]

crimesample <- na.omit(crimesample)

#check for duplicates 

crimesample <- crimesample[!duplicated(df$id), ]


#rounding decimals to 2d.p
#$ dollar sign used to indicate a specific column in the data 

crimesample$growth <- round(crimesample$growth,digit=2)
crimesample$Latitude <- round(crimesample$Latitude,digit=2)
crimesample$Longitude <- round(crimesample$Longitude,digit=2)

#check for outliers using boxplots 

library(ggplot2)

ggplot(crimesample, aes(x=Beat, y=District)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1)+ scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0,50))



ggplot(crimesample, aes(x=District, y=Ward)) + geom_point()+ scale_y_continuous(labels=scales::comma)+coord_cartesian(ylim = c(0, 300))

#scatterplot
data(crimesample)
barplot(crimesample$Ward, names.arg=crimesample$District, main="Bar Plot", xlab="District", ylab="Ward", col="red", xlim = c(0, 30))


#barplot
data(crimesample)
plot(crimesample$District, crimesample$Ward, main="Scatter Plot", xlab="District", ylab="Ward", col="blue",pch=19)

#line plot

x <- seq(0,2*pi, length.out=100)
y <- sin(x)
plot(x, y, type="l", main="Line Plot", xlab="x", ylab="sin(x)", col="purple")

#barplot 
data(crimesample)
barplot(crimesample$District, names.arg=crimesample$Ward, main="Bar Plot", xlab="Ward", ylab="District", col="red", xlim = c(0, 100))

#2
barplot(crimesample$Community.Area, names.arg=crimesample$Ward, main="Bar Plot", xlab="Ward", ylab="Community.Area", col="red", xlim = c(0, 100))


#histograms

data(crimesample)
hist(crimesample$District, main="Histogram", xlab="District", ylab="Frequency", col="orange")

hist(crimesample$Ward, main="Histogram", xlab="Ward", ylab="Frequency", col="orange")

#pie chart

counts <- table(crimesample$Primary.Type)
pie(counts, main="Pie Chart - Cylinder Distribution", labels=names(counts), col=c("red", "green", "blue"))


counts <- table(crimesample$Primary.Type)
pie(counts, main="Pie Chart - Primary Type", labels=names(counts), col = rainbow(length(counts)))

#biovariate analysis

ggplot(crimesample, aes(x=District, y=Ward)) + geom_point()+ scale_y_continuous(labels=scales::comma)+coord_cartesian(ylim = c(0, 300))



#boxplot 

data(crimesample)
boxplot(Beat ~ Primary.Type, data=crimesample, main="Box Plot", xlab="Primary Type", ylab="Beat")


#correlation 

cor(crimesample$Ward.Area,crimesample$District)

ggplot(crimesample, aes(x = crimesample$Primary.Type))+geom_bar()+coord_flip()

ggplot(crimesample, aes(x=Beat, y=Primary.Type)) +geom_boxplot(outlier.colour = "red", outlier.shape=1)+scale_x_continuous(labels=scales::comma)

ggplot(crimesample, aes(x= Location.Description, y= )) 























