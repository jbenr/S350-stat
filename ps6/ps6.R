## 2 trosset 7.7 ex1
#a
a1 <- read.table(url("http://mypage.iu.edu/~mtrosset/StatInfeR/Data/sample771.dat"))
a1v <- c(a1[,1],a1[,2],a1[,3],a1[,4],a1[,5])
plot(ecdf(a1v),main="ECDF")
#b
mean1 <- mean(a1v)
var1 <- var(a1v)
sd1 <- sqrt(var1)
print(mean1)
print(var1)
#c
med1 <- median(a1v)
iq1 <- qnorm(0.75, mean=mean1, sd=sd1) - qnorm(0.25, mean=mean1, sd=sd1)
print(med1)
print(iq1)
#e
boxplot(a1v,main="boxplot")
#f
qqnorm(a1v,main="normal probability plot")
#g
plot(density(a1v),main="kernal density estimate")
#h
h1 = "I do not think that this sample was drawn from a normal distribtion because it looks skewed to the left."
print(h1)

## 3 trosset 7.7 ex2
#a
a2 <- read.table(url("http://mypage.iu.edu/~mtrosset/StatInfeR/Data/pulses.dat"),fill = TRUE)
a2v <- c(a2[,1],a2[,2],a2[,3],a2[,4],a2[,5],a2[,6],a2[,7],a2[,8],a2[,9],a2[,10])
a2v = a2v[!is.na(a2v)]
plot(ecdf(a2v),main="ECDF")
#b
mean2 <- mean(a2v)
var2 <- var(a2v)
sd2 <- sqrt(var2)
print(mean2)
print(var2)
#c
med2 <- median(a2v)
iq2 <- qnorm(0.75, mean=mean2, sd=sd2) - qnorm(0.25, mean=mean2, sd=sd2)
print(med2)
print(iq2)
#e
boxplot(a2v,main="boxplot")
#f
qqnorm(a2v,main="normal probability plot")
#g
plot(density(a2v),main="kernal density estimate")
#h
h2 = "I think that this data looks relatively normal with some outliers and a slight skew to the left. The basic structure of the bell curve, including the location of the mode and median, makes it seem slightly normal."
print(h2)

