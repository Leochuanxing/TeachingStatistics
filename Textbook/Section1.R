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
# stem-plot
library(aplpack)
stem.leaf(ExampleData$FINAL)
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

#Normal distribution
library(ggrepel)
p1 <- ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = dnorm, size = 1)+
  stat_function(fun = function(x){0}, size = 1) +
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = 14, face = 'bold'),
        axis.text.x = element_text(size = 12, face = 'bold'),
        axis.ticks.x = element_line(size=1,color='black'),
        axis.ticks.length = unit(-0.1, 'cm'))+
        ylim(0,0.43)
# add special tick mark
p2 <- p1 +scale_x_continuous(breaks=c(-3, -2, -1, 0, 1, 2, 3), 
                       labels = c(expression(mu - 3 *sigma), expression(mu - 2 *sigma),expression(mu - sigma), expression(mu),
                                  expression(mu + sigma), expression(mu + 2 *sigma), expression(mu + 3 *sigma)))
# add annotation
p3 <- p2  + annotate("text", x=3.2, y=0.3, label = "f(x)==frac(1, sqrt(2*pi*sigma))~plain(e)^{frac(-(x-mu)^2, 2*sigma^2)}",
                   parse=TRUE, cex = 9) 
   
p3

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

# side-by-side bar graph
library(reshape2)
library(ggplot2)
library(scales)
library(dplyr)
data <- ExampleData
data <- data %>% 
  group_by( BASKETBALL,GENDER) %>% 
  summarize(n = n())
sum_by_basket <- data %>% summarize(total = sum(n))
data <- data %>% ungroup()
per <- data$n/26
data$Percentage <- scales::percent(per)
ggplot(data, aes(BASKETBALL, Percentage, fill=GENDER)) + 
  geom_bar(position="dodge",stat="identity", width = 0.6)+
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text = element_text(size = 12,face="bold" ))

# stacked bar graph
library(reshape2)
library(ggplot2)
library(scales)
library(dplyr)
data <- ExampleData
data <- data %>% 
  group_by( BASKETBALL,GENDER) %>% 
  summarize(n = n())
sum_by_basket <- data %>% summarize(total = sum(n))
data <- data %>% ungroup()
per <- data$n/26
data$Percentage <- scales::percent(per)
ggplot(data, aes(BASKETBALL, Percentage, fill=GENDER)) + 
  geom_bar(position="stack",stat="identity", width = 0.5)+
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text = element_text(size = 12,face="bold" ))

# scatter plot
setwd("C:/Users/leo/Documents/Teaching/Statistics/Teaching plans/Descriptional Statistics")
Scores <- read.csv('Scores.csv')
library(ggplot2)
ggplot(Scores, aes(x=Calculus, y=Physics)) + 
  geom_point(size = 3)+
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text = element_text(size = 12,face="bold" ))

# Visualizing the residuals
model <- lm(Scores$Physics ~ Scores$Calculus)
Scores$predicted <- predict(model)
Scores$residuals <- residuals(model)
  # take a quick look at the data
  head(Scores)
  #plot the actual and predicted values
  ggplot(Scores, aes(x = Calculus, y = Physics))+ # set up the canvas
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  geom_segment(aes(xend = Calculus, yend = predicted)) +
  geom_point(size = 3)+
  geom_point(aes(y = predicted), size = 3, color = 'red', shape = 1)+  # Add the predicted values
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text = element_text(size = 12,face="bold" ))
# Least-squares regression line
  ggplot(Scores, aes(x = Calculus, y = Physics)) + geom_point()+
    geom_point(size = 3)+
    geom_smooth(method = 'lm', size = 2, clolor = 'blue', se = FALSE)+
    theme(axis.title=element_text(size=16,face="bold"),
          axis.text = element_text(size = 12,face="bold"))
# Summary of the above least squares regression line
  summary(model)
# Residual plot
  ggplot(Scores, aes(x=Calculus, y= residuals)) +
    geom_point(size = 3)+
    theme(axis.title=element_text(size=16,face="bold"),
          axis.text = element_text(size = 12,face="bold"))
  
  
# model 2
library(ggplot2)
model2 <- lm(ExampleData$FINAL~ExampleData$MID )
ResidualFrame2 <- data.frame(MID = ExampleData$MID, Residuals = model2$residuals)
ggplot(ResidualFrame2, aes(x = MID, y = Residuals)) + 
  geom_point()+  theme(axis.title=element_text(size=16,face="bold"),
                       axis.text = element_text(size = 12,face="bold"))
ggplot(ExampleData, aes(x = MID, y = FINAL)) + geom_point()+
  geom_point(size = 3)+
  geom_smooth(method = 'lm', size = 1, clolor = 'blue', se = FALSE)+
  theme(axis.title=element_text(size=16,face="bold"),
        axis.text = element_text(size = 12,face="bold"))
summary(model2)

# Uniform distribution
library(ggplot2)
ggplot(data.frame(x = c(0, 1)), aes(x)) + stat_function(fun = function(x) 1, size = 1)+
  xlab('Uniform distribution') +    
  theme(axis.title=element_text(size=16,face="bold"),
      axis.text = element_text(size = 12,face="bold"))

# Show the different sampling distribution of different sampling method
#Histogram with vertical axis frequency
library(ggplot2)
library(scales)
samples <- rnorm(100, mean = 10, sd = 2)
df_samples = data.frame(samples)
ggplot(data = df_samples, aes(samples)) + 
  geom_dotplot(binwidth = 0.5, 
                 col="black", 
                 fill="blue",
               alpha = 0.6,
               dotsize = 0.8) +
  labs(x="Sampling distribution of SRS", y = "")+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text = element_text(size=8,face="bold")+
          xlim(c(5, 16)))

samples <- rnorm(100, mean = 10, sd = 1)
df_samples = data.frame(samples)
ggplot(data = df_samples, aes(samples)) + 
  geom_dotplot(binwidth = 0.5, 
               col="black", 
               fill="blue",
               alpha = 0.6,
               dotsize = 0.6) +
  labs(x="Sampling distribution of Stratified Random Samples", y = "")+
  theme(axis.title=element_text(size=14,face="bold"),
        axis.text = element_blank())+
  scale_x_continuous(limits = c(5, 15))

# Sampling diStribuiton

###1, Generate 100 samples of size 25 from a normal distribution N(0, 5)

samples <- matrix(NA, nrow=100, ncol=25) # creat an empty matrix
#sample
for (i in 1:nrow(samples)){
  samples[i,] <- rnorm(25, 0, 5)
}
# calculate the sample mean 
sample_means <- rowMeans(samples)
#draw a dot plot to show the sampling distribution
library(ggplot2)
df_sample_means = data.frame(sample_means)
ggplot(data = df_sample_means, aes(sample_means)) + 
  geom_dotplot(binwidth = 0.5, 
               col="black", 
               fill="blue",
               alpha = 0.6,
               dotsize = 0.4) +
  labs(x="Sampling distribution of 100 samples of size 25 ", y = "")+
  theme(axis.title.x =element_text(size=14,face="bold"),
        axis.text.x = element_text(size=8,face="bold"),
        axis.text.y = element_blank())
round(samples[1, ], digits = 3)
round(mean(samples[1,]), digits = 3)

# 100 samples from a population of 50 black balls and 150 white balls with sample size 20

samples <- matrix(NA, nrow = 100, ncol = 20)

for (i in 1: nrow(samples)){
  samples[i, ] <- sample(1:200, 20, replace = F)
}

count <- function(vector, lower, upper){
  n = 0
  for (i in 1:length(vector)){
    if (vector[i]>= lower & vector[i] <= upper ){
      n <- n +1
    }
  }
  n
}

p_hats <- rep(0, 100)
for (i in 1:100){
  p_hats[i] <-  count(samples[i, ], 0, 50)/20
}

df_p_hats <- data.frame(p_hats)
ggplot(data = df_p_hats, aes(p_hats)) + 
  geom_dotplot(binwidth = 0.01, 
               col="black", 
               fill="blue",
               alpha = 0.6,
               dotsize = 1.5) +
  labs(x="", y = "")+
  theme(axis.title.x =element_text(size=14,face="bold"),
        axis.text.x = element_text(size=8,face="bold"),
        axis.text.y = element_blank())

# Demonstration of Central Limit theorem.
library(ggplot2)
help("rgamma")
p2 <- ggplot(data.frame(x = c(0, 6)), aes(x)) + stat_function(fun = function(x) exp(-x), size = 1)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.x = element_text(size = 14, face = 'bold'))+
  xlab('X')
p2 + annotate("text", x=4, y=0.6, label = "f(x)==e^{-x}",
              parse=TRUE, cex = 9) 


# 100 samples with size 10, 
Sampling_Distribution <- function(n, n_samples, dot_size = 0.5,
                                  bin_width = 0.05){
  
  samples_means <- rep(NA, n_samples)
  for (i in 1:n_samples) {
    samples_means[i] <- mean(rgamma(n, 1, 1))
  }
  
  df_samples_means <- data.frame(samples_means)
  ggplot(data = df_samples_means, aes(samples_means)) + 
    geom_dotplot(binwidth = bin_width, 
                 col="black", 
                 fill="blue",
                 alpha = 0.6,
                 dotsize = dot_size) +
    labs(x="", y = "")+
    theme(axis.title.x =element_text(size=14,face="bold"),
          axis.text.x = element_text(size=16,face="bold"),
          axis.text.y = element_blank())
  
}

p5 <- Sampling_Distribution(n = 5, n_samples = 500, 
                            dot_size = 0.3, bin_width = 0.1)
p5

p10 <- Sampling_Distribution(n = 10, n_samples = 500, 
                            dot_size = 0.3, bin_width = 0.07)
p10


p20 <- Sampling_Distribution(n = 20, n_samples = 500, 
                             dot_size = 0.3, bin_width = 0.05)
p20

p40 <- Sampling_Distribution(n = 40, n_samples = 500, 
                             dot_size = 0.3, bin_width = 0.035)
p40

p80 <- Sampling_Distribution(n = 80, n_samples = 500, 
                             dot_size = 0.3, bin_width = 0.025)
p80

p160 <- Sampling_Distribution(n = 160, n_samples = 500, 
                             dot_size = 0.3, bin_width = 0.0175)
p160


# Display the Student's t distributions with various
# degrees of freedom and compare to the normal distribution

x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3",  "df=30", "normal")

plot(x, hx, type="l", lty=1, lwd = 2, xlab="x value",
     ylab="Density", main="")

for (i in 1:3){
  lines(x, dt(x,degf[i]),lty = i+1, lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(2, 3, 4, 1), col=colors)

#Confidence intervals
library(ggplot2)
p1 <- ggplot(data.frame(x = c(-3, 3)), aes(x)) + stat_function(fun = dnorm, size = 1)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.x = element_text(size = 14, face = 'bold'))+
  xlab('Symmetric')
p1

sample_points <- rnorm(500, 0, 1)
df_sample_points <- data.frame(sample_points)
 ggplot(df_sample_points, aes(sample_points))+
  geom_dotplot(binwidth = 0.2, 
               col="black", 
               fill="blue",
               alpha = 0.6,
               dotsize = 0.45) +
  #geom_density(stat = "density", position = "identity")+
  labs(x="", y = "")+
  theme(axis.title.x =element_text(size=14,face="bold"),
        axis.text = element_blank())+
        scale_x_continuous(limits = c(-3, 3))

 # Sampling distribution on significance test
 library(ggplot2)
ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = dnorm, size = 1)+
   stat_function(fun = function(x){0}, size = 1) +
   theme(axis.title.y=element_blank(),
         axis.ticks=element_blank(),
         axis.title.x = element_blank(),
         axis.text.x = element_blank())+
  ylim(-0.05, 0.43)+
  annotate(geom = 'text', label =c("paste(p[0])", "Desicion~is~based~on~this", "assumed~sampling~distribution"), x =c(0, 1.5, 1.5), y =c(0, 0.3, 0.2), hjust =c(0, 0, 0), 
           vjust = c(1.2, 0, -1), parse = TRUE)

ggplot(data.frame(x = c(-5, 5)), aes(x)) + stat_function(fun = dnorm, size = 1)+
  stat_function(fun = function(x){0}, size = 1) +
  theme(axis.title.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank())+
  ylim(-0.05, 0.43)+
  annotate(geom = 'text', label =c("paste(p[1])", "Samples~are~drawn~from~this", "real~sampling~distribution"), x =c(0, 1.5, 1.5), y =c(0, 0.3, 0.2), hjust =c(0, 0, 0), 
           vjust = c(1.2, 0, -1), parse = TRUE)
  
# inference for regression line
library(rgl)

# chi-square distribution
library(ggplot2)
ggplot(data.frame(x = c(0, 28)), aes(x)) + stat_function(fun = function(x) dchisq(x, df = 1), size = 1)+
  stat_function(fun = function(x){0}, size = 1) +
  stat_function(fun = function(x) dchisq(x, df = 4), size = 1, colour = "green")+
  stat_function(fun = function(x) dchisq(x, df = 8), size = 1, colour = "red")+
  stat_function(fun = function(x) dchisq(x, df = 16), size = 1, colour = "blue")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())+
  ylim(0,0.22)+
  annotate(geom = 'text', label =c("df=1", "df=4", "df=8", "df=16"),
           x =c(2, 5, 9, 17), y =c(0.21, 0.15, 0.1, 0.075)
            )
