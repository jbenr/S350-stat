## Ben Reichert
## Problem Set 10

# 1 Attached

# 2
# I think the simpler explanantion for decreased performance following praise is that performance varies, no matter
# what the pilots are told. A key topic in Chapter 15 is the regression effect, which is essentially the tendency of
# data to regress towards the mean in the case of pilots testing very well initially. 

# 3
# a) The prediction of 98 wins again is likely too high because the average wins of an MLB team is 81 with a standard
# deviation of 11.7, meaning getting anyhthing over 92-93 wins is extremely unlikely.

# b)
t <- 30
n <- 162
m <- 81
sd <- 11.7
corr <- 0.54
r <- 0.54

m1 <- 98
wins <- m*(1-r) + r*m1
cat("Predicted wins:",wins,"\n")

# c)
# In every set of data there are some outliers. It is almost expected that there will be some inconsistency,
# hence the 95% confidence interval mechanism. However, every team is still statistically expected to stay within
# the 95% of data. Outliers like winning 96 games are stastically very unlikely. Although it happens most seasons
# to at least one team, the individual probability of that happening to a given team is very low.

# 4.  Trosset chapter 15.7 exercise 8.
# a) I think it would be unfair to replace Jill's Test 2 score with her Test 1 score because Test 2 was clearly harder
# than Test 1, so while an 80 on Test 1 is just above the average and within the standard deviation, an 80 on Test 2
# is way above the average and beyond the standard deviation. I would advise the professor gives Jill a 70 on Test 2
# because she did 1/2 of the standard deviation better than the ave on Test 1, so why not give her the same deal on
# Test 2.

# b) I like this suggestion because it is statistically consistent with how Jack would most likely perform in relation
# to the rest of the class. I would advise the professor assigns the score of 85. 


# 5.  Trosset chapter 15.7 exercise 5, parts (a), (b), and (c).
x <- c(69,64,65,63,65,62,65,64,66,59,62)
y <- c(71,68,66,67,70,71,70,73,72,65,66)
n <- length(x)
sumx <- sum(x)
sumy <- sum(y)
sumxy <- sum(x*y)
sumx2 <- sum(x^2)
sumy2 <- sum(y^2)
# a)
pears <- ( (n*sumxy) - (sumx*sumy) )/sqrt( (n*sumx2 - sumx^2) * (n*sumy2 - sumy*sumy) )
cat("Coefficient of determination:",pears^2,"\n")

# b)
print(summary(lm(y~x)))
# The P-value of 0.07442 > 0.05 thus we fail to reject the null hypothesis.
# We conclude that the data does not provide convincing evidence to conclude
# sister's height influences brother's.

# c)
m <- 0.5909
sd <- 0.2929
n <- 11
err <- qnorm(0.95)*sd/sqrt(n)
cat("90% confidence interval: (",m-err,",",m+err,")")
