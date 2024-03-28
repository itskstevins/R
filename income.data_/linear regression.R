library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
income.data <- read.csv("D:/kstevins/Documents/datascience/R/income.data_/income.data.csv")
#summary(income.data)
#To check wherther the dependent variable (happiness) follows a normal distribution
#hist(income.data$happiness)
#plot(happiness ~ income, data = income.data)
heart.data <- read.csv("D:\\kstevins\\Documents\\datascience\\R\\heart.data_\\heart.data.csv")
#cor(heart.data$biking, heart.data$smoking)
#The correlation between biking and smoking is small (0.015 is only a 1.5% correlation), 
#so we can include both parameters in our model.
#hist(heart.data$heart.disease)
#linearity
#plot(heart.disease ~ biking, data = heart.data)
#plot(heart.disease~ smoking, data = heart.data)
#To perform a simple linear regression analysis and check the results, 
#you need to run two lines of code. The first line of code makes the linear model,
#and the second line prints out the summary of the model:
income.lm <- lm(happiness ~ income, data = income.data)
#summary(income.lm)

heart.lm <- lm(heart.disease ~ biking + smoking , data = heart.data)
#summary(heart.lm)
#par(mfrow=c(1,1))
#plot(income.lm)


#Visualization
#library(ggplot2)
#income.graph<-ggplot(income.data, aes(x=income, y=happiness))+
#  geom_point()

#income.graph <- income.graph + geom_smooth(method="lm", col="red")
#income.graph <- income.graph +
 # stat_regline_equation(label.x = 3, label.y = 7)

#income.graph <- income.graph +
 # theme_bw() +
  #labs(title = "Reported happiness as a function of income",
#       x = "Income (x$10,000)",
 #      y = "Happiness score (0 to 10)")

#income.graph






plotting.data<-expand.grid(
  biking = seq(min(heart.data$biking), max(heart.data$biking), length.out=30),
  smoking=c(min(heart.data$smoking), mean(heart.data$smoking), max(heart.data$smoking)))

plotting.data$predicted.y <- predict.lm(heart.lm, newdata=plotting.data)

plotting.data$smoking <- round(plotting.data$smoking, digits = 2)
plotting.data$smoking <- as.factor(plotting.data$smoking)
#plot original data

heart.plot <- ggplot(heart.data, aes(x=biking, y=heart.disease)) +
  geom_point()

heart.plot <- heart.plot +
  geom_line(data=plotting.data, aes(x=biking, y=predicted.y, color=smoking), size=1.25)
heart.plot <-
  heart.plot +
  theme_bw() +
  labs(title = "Rates of heart disease (% of population) \n as a function of biking to work and smoking",
       x = "Biking to work (% of population)",
       y = "Heart disease (% of population)",
       color = "Smoking \n (% of population)") + 
  annotate(geom="text", x=30, y=1.75, label=" = 15 + (-0.2*biking) + (0.178*smoking)")

heart.plot

