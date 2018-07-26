# Many of the codes will be reused in the future and I am just a motal who keeps 
# forgetting. It is a better idea to write out those code clearly, save them and 
# reuse them in the future.

setwd("C:/Users/leo/Documents/Teaching/Statistics/Teaching plans/Descriptional Statistics")
ExampleData <- read.csv('ExampleData.csv', header = TRUE)

#This chunk is to draw a pie chart
library(dplyr)
library(ggplot2)
data <- ExampleData
data <- data %>% 
  group_by(GENDER) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(GENDER))
data$label <- scales::percent(data$per)
ggplot(data=data)+
  geom_bar(aes(x="", y=per, fill=GENDER), stat="identity", width = 1)+
  coord_polar("y", start=0)+
  theme_void()+
  geom_text(aes(x=1, y = cumsum(per) - per/2, label=label))

#This chunk is to draw a bar graph
# bar graph with vertical axis 'Frequency'
library(dplyr)
library(ggplot2)
data <- ExampleData
ggplot(data, aes(x = GENDER)) +
     geom_bar(stat = 'count', width = 0.3,  fill = 'steelblue') +
     ylab('Frequency') 
# bar graph with vertical axis 'Relative frequency'
data <- ExampleData
data <- data %>% 
  group_by(GENDER) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(per=`n`/sum(`n`)) %>% 
  arrange(desc(GENDER))
data$label <- scales::percent(data$per)
ggplot(data, aes(x = GENDER, y = per)) +
       geom_bar(stat = 'identity', width = 0.3, fill = 'steelblue')+
       ylab('Relative frequency')
#Histogram with vertical axis frequency
library(ggplot2)
library(scales)
ggplot(data = ExampleData, aes(ExampleData$FINAL)) + 
  geom_histogram(breaks=seq(32.5, 102.5, by = 5), 
                 col="black", 
                 fill=" white", 
                 alpha = .2) +
  labs(x="FINAL", y="Frequency")+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text = element_text(size = 10,face="bold" ))
#Histogram with vertical axis relative frequency
ggplot(data = ExampleData, aes(ExampleData$FINAL)) + 
  geom_histogram(aes(y = ..density..*5), breaks=seq(32.5, 102.5, by = 5), 
                 col="black", 
                 fill=" white", 
                 alpha = .2) +
  labs(x="FINAL", y="Relative frequency") +
  scale_y_continuous(labels = percent_format())+
  theme(axis.title=element_text(size=14,face="bold"), 
        axis.text = element_text(size = 10,face="bold" ))
# Histogram with bin width 1
library(ggplot2)
library(scales)
ggplot(data = ExampleData, aes(ExampleData$FINAL)) + 
  geom_histogram(aes(y = ..density..), breaks=seq(40, 100, by = 1), 
                 col="black", 
                 fill=" white", 
                 alpha = .2) +
  labs(x="FINAL", y="Relative frequency") +
  scale_y_continuous(labels = percent_format())+
  theme(axis.title=element_text(size=14,face="bold"), 
        axis.text = element_text(size = 10,face="bold" ))

final <- ExampleData$FINAL
sort(final)
# Smoothed density curve
library(ggplot2)
ggplot(data = ExampleData, aes(FINAL)) +
  geom_density(color = 'darkblue', size = 1) +
  xlim(c(30, 110))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

# smooth Cumulative relative frequency curve
# use kernel density
library(ggplot2)
smooth_cdf <- function(data, kernel_lenghth, sample_size = 100){
  #decide the sample values
  point_interval = diff(range(data))/sample_size
  points <- rep(0, sample_size + 1)
  least = min(data)
  for (i in 1: length(points)){
    points[i] <- least + (i-1.5)* point_interval
  }
  #Calculate kernel density
  kernel_density <- rep(0, length(points))
  for (i in 1:length(points)){
    kernel_density[i] <- 
      sum(data > points[i] - kernel_lenghth & data <= points[i] + kernel_lenghth )/(2*kernel_lenghth*length(data)) 
   }
  #Calculate the cumulative density
  cumulative_density <- rep(0, length(kernel_density))
  for (i in 1:length(kernel_density)){
    cumulative_density[i] <- sum(kernel_density[1:i]) * point_interval
  }
  # draw the scatterplot and the corve
  DataFrame <- data.frame(d = points, c = cumulative_density)
  ggplot(DataFrame, aes(x = d, y = c)) + geom_point(alpha = 0) + 
    geom_smooth(method='auto', se=FALSE, fullrange=TRUE)
}
 
smooth_cdf(ExampleData$FINAL, kernel_lenghth = 2) +
  theme(axis.title=element_text(size=14,face="bold"), 
        axis.text = element_text(size = 10,face="bold" )) +
  ylab('Cumulative relative frequency')+
  xlab('FINAL')+ geom_smooth(size = 1.5)

# A symmetric distribution
library(ggplot2)
p1 <- ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = dnorm, size = 1)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_text(size = 14, face = 'bold'))+
  xlab('Symmetric')
# A right-skewed function
p2 <- ggplot(data.frame(x = c(0, 6)), aes(x)) + stat_function(fun = function(x) x*exp(-x), size = 1)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_text(size = 14, face = 'bold'))+
  xlab('Right-skewed')
# A left-skewed function
p3 <- ggplot(data.frame(x = c(-6, 0)), aes(x)) + stat_function(fun = function(x) -x*exp(x), size = 1)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_text(size = 14, face = 'bold'))+
  xlab('Left-skewed')
      
# put the above three graphs in one figure
library(gridExtra)
grid.arrange(p2, p1, p3, nrow = 1)

# A cumulative distribution with skewness
data <- rgamma(10000, shape = 2, scale = 1)
smooth_cdf(data, kernel_lenghth = 0.2, sample_size = 1000) +
       theme(axis.title=element_text(size=14,face="bold"), 
        axis.text = element_text(size = 10,face="bold" )) +
       ylab('Cumulative relative frequency')+
       xlab('X')+
       xlim(c(0.1, 7))

# distribution of two clusters and a gap
library(ggplot2)
library(scales)
data1 <- rnorm(100, mean = -1, sd = 0.2)
data2 <- rnorm(100, mean = 1, sd = 0.2)
two_cluster_data <- c(data1, data2)
Data_frame_two_cluster <- data.frame(two_cluster_data)
ggplot(data = Data_frame_two_cluster, aes(two_cluster_data)) + 
  geom_histogram(breaks=seq(-2, 2, by = 0.2), 
                 col="black", 
                 fill=" white", 
                 alpha = .2) +
  labs(x="X", y="")+
  theme(axis.title.x=element_text(size=14,face="bold"),
        axis.text.x = element_text(size = 10,face="bold" ),
        axis.title.y =element_blank(),
        axis.text.y = element_blank())

# Outliers
library(ggplot2)
library(scales)
data1 <- rnorm(5, mean = -1, sd = 0.05)
data2 <- rnorm(200, mean = 1, sd = 0.2)
two_cluster_data <- c(data1, data2)
Data_frame_two_cluster <- data.frame(two_cluster_data)
ggplot(data = Data_frame_two_cluster, aes(two_cluster_data)) + 
  geom_histogram(breaks=seq(-2, 2, by = 0.2), 
                 col="black", 
                 fill=" white", 
                 alpha = .2) +
  labs(x="X", y="")+
  theme(axis.title.x=element_text(size=14,face="bold"),
        axis.text.x = element_text(size = 10,face="bold" ),
        axis.title.y =element_blank(),
        axis.text.y = element_blank())

# Five number summary of students in class 23
class23data <- subset(ExampleData, ExampleData$CLASS == 23)
summary(class23data$FINAL)
boxplot(class23data$FINAL)
summary(ExampleData$FINAL)
# boxplot
library(ggplot2)
helper <- rep('FINAL', nrow(ExampleData))
boxplot.data <- data.frame(ExampleData$FINAL, helper)
ggplot(ExampleData, aes(x = helper, y = FINAL))+
  geom_boxplot(outlier.size = 5)+ coord_flip()+
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x=element_text(size=16,face="bold"),
        axis.text.x = element_text(size = 12,face="bold" ))
# Boxplot of two distributions
library(ggplot2)
ExampleData$CLASS <- as.factor(ExampleData$CLASS)
ggplot(ExampleData, aes(x = CLASS, y = FINAL))+
  geom_boxplot(outlier.size = 5)+ coord_flip()+
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text = element_text(size = 12,face="bold" ))
# Back-to-back stemplot
library(aplpack)
Class23 <- subset(ExampleData, ExampleData$CLASS == 23)$FINAL
Class24 <- subset(ExampleData, ExampleData$CLASS == 24)$FINAL
stem.leaf.backback(Class23,Class24)
