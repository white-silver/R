setwd('/Users/Ryota/Documents/proj')
dat <- read.csv("_result.csv")
dat.lm <- lm(ten ~ one, data = dat)
m <- mean(dat$ten)
s <- sd(dat$ten)
h0 <- m + 3*s
h1 <- m + 2*s
h2 <- m - 2*s
h3 <- m + s
h4 <- m - s
plot(dat$one,dat$ten,xlab = "Cycle 1",ylab = "Cycle 10")
abline(dat.lm,col = "red",lwd = 4)
abline(h = m,col = "red")
abline(h = h0)
abline(h = h1)
abline(h = h2)
abline(h = h3)
abline(h = h4)
summary(dat.lm)

hist(dat$one,breaks = "FD")
hist(dat$ten,breaks = "FD")


shapiro.test(dat$one)
shapiro.test(dat$ten)