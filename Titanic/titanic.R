setwd('/Users/Ryota/Documents/R/Titanic')
dat <- read.csv("titanic3.csv")

survivor <- sum(data$survived)
dead <- nrow(dat) - survivor
bars <- c(survivor,dead)
deadper <- round((dead/(survivor+dead))*100,digits = 1)
deadper <- paste(as.character(deadper),"% Died")

male <- sum(dat$sex == "male")
female <- sum(dat$sex == "female") 

sex = c(male,female)

barplot(bars,names.arg = c("survived","dead"),col = c("blue","red"),main = deadper)
barplot(sex,names.arg = c("male","female"),col = c("blue","red"),main = "sex",ylim = c(0,1000))

age <- dat$age
h <- hist(age)

fare <-dat$fare
f <-hist(fare,breaks = "FD")

dat.lm <- lm(dat$survived ~ age+pclass+isMale,data = dat)
summary(dat.lm)
