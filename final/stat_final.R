### Ben Reichert
### Final


## 1
# a) 
girls <- c(rep(0, 23236), rep(1,58529), rep(2,53908), rep(3, 18770))
n <- length(girls)
mean <- mean(girls)
var <- var(girls)
# mean = 1.441665
# variance = 0.7905698

# b)
t <- t.test(girls)
# 95% confidence interval of (1.437230, 1.446099)

# c)
# width/2 = Z * sd(n)/sqrt(n)
# 0.02/2 = 1.96 * sd(n) / sqrt(n)
# 0.01 = 1.96 * sd(n) / sqrt(n)
# sqrt(n) = 1.96 * sd(n) / 0.01
# n = (1.96 * sd(n) / 0.01)
n <- (qnorm(0.975)*sd(girls)/0.01)^2
# Number of families to sample (n) when we want 95% confidence and width of 0.02: n = 30370 (rounded up to nearest whole family)


## 2 ##
# a)
# We need to compare the means of two independent groups of data, so we either do students t-test or welch's t-test. 
# Because the standard deviations are not the same we use Welch's t-test.
# Even though the samples are right skewed, they are large enough so that this will not matter much when doing Welch's.

# b)
t <- (16.8 - 24.3) / sqrt( 15.9^2/592 + 17.3^2/154 )
# T-statistic = -4.87
p <- 2*pt(-abs(t),df=225)
# P-value = 0.000002

# c)
m <- 16.8-24.3
diff <- qnorm(0.975)*sqrt( 15.9^2/592 + 17.3^2/154 )
min <- m-diff
max <- m+diff
# Confidence interval: (-10.51764, -4.482365)


## 3 ##
pears <- read.table("PearsonLee.txt", header=TRUE)

# a)
lin <- lm(childHeight ~ parentHeight, data = pears)
# Linear regression formula: y = 0.221x + 51.497
# Where y is the predicted child's height and x is parent's height.
library(ggplot2)
print(plot(pears[,1], pears[,2], xlab="parentHeight", ylab="childHeight"))
abline(a=51.497, b=0.221, col=2)

# b)
# linearity
pears.df <- data.frame(pears,
                      fitted = fitted.values(lin),
                      residuals = residuals(lin))
print(ggplot(pears.df, aes(x=parentHeight, y=residuals)) + geom_point() + geom_smooth())
# this data looks relatively linear. The regression line on the graph follows an upward trend of
# taller parents produce taller children on average, which is what we would expect. However, the risidual
# graph shaded region does not entirely contain the line y=0, we see an upward trend. Because we see
# an upward trend in the residuals it is hard to confirm that this data is in fact linear. 

# independence
# the two sets of data are not independent. The height of the child is affected by
# the height their parent, and is thus not independent of it, or is dependent on it.

# homoskedasticity
print(ggplot(pears.df, aes(x = parentHeight, y = abs(residuals))) + geom_point() + geom_smooth())
# Equal variance of errors seems to hold. The magnitude of residuals (blue line) holds constant.

# normality
print(ggplot(pears.df, aes(sample = residuals)) + stat_qq())
# the normal QQ plot is very straight so its safe to say the residuals are normal.

# c)
# linear model for dads
dads <- subset(pears, parentGender == "Father")
lin <- lm(childHeight ~ parentHeight, data = dads)
print(summary(lin))
# y = 0.41304x + 37.88347
dads.df <- data.frame(dads,
                       fitted = fitted.values(lin),
                       residuals = residuals(lin))
print(ggplot(dads.df, aes(x=parentHeight, y=residuals)) + geom_point() + geom_smooth())
print(ggplot(dads.df, aes(x = parentHeight, y = abs(residuals))) + geom_point() + geom_smooth())
print(ggplot(dads.df, aes(sample = residuals)) + stat_qq())
# the assumptions of linear regression are much easier to confirm with the dads data alone.

moms <- subset(pears, parentGender == "Mother")
lin <- lm(childHeight ~ parentHeight, data = moms)
print(summary(lin))
# y = 0.5431x + 32.0195
moms.df <- data.frame(moms,
                      fitted = fitted.values(lin),
                      residuals = residuals(lin))
print(ggplot(moms.df, aes(x=parentHeight, y=residuals)) + geom_point() + geom_smooth())
print(ggplot(moms.df, aes(x = parentHeight, y = abs(residuals))) + geom_point() + geom_smooth())
print(ggplot(moms.df, aes(sample = residuals)) + stat_qq())
# the assumptions for linear regression are also much easier to confirm with the moms data by itself.


## 4 ##
birds <- read.table("flowersandbirds.txt", header=TRUE)

# a)
m <- mean(birds[,2])
# The mean is 44.64, which is much lower than 57.
# Null: the mean of the difference between flowerGuess and birdGuess is 0.
# Alternative: the mean of the difference between flowerGuess and birdGuess is not 0.
print(t.test(birds[,2], birds[,3], paired = TRUE))
# Test used: Paired t-test
# Test statistic t = 0.58631
# P-value = 0.5594
# Substantive conclusion: since p > 0.05, we fail to reject the null and resort to the conclusion that diff of means = 0.

# b)
print(plot(birds[,2], birds[,3], main="flowerGuess v birdGuess", xlab="flowerGuess", ylab="birdGuess"))
# The graph above makes the data look like a strong positive linear relationship between flowerGuess and birdGuess.

# c)
heads <- subset(birds, Group == "Heads")
tails <- subset(birds, Group == "Tails")
head <- heads[,2]-heads[,3]
tail <- tails[,2]-tails[,3]
# Null: expected value for 'head' and 'tail' data sets above are the same 
# Alternative: expected value for diff in flowerGuess and birdGuess for heads is stastically different from
# the expected value for diff in flowerGuess and birdGuess for tails. 
print(t.test(head,tail))
# Test used: Welch's two-sample t-test
# t = 1.475
# df = 54.284
# p-value = 0.146
# Conclusion: because p-value > 0.05, we fail to reject the null and assert that the expected values for differences
# in flowerGuess and birdGuess between the heads population and tails population are the same.


## 5 ##
iowa <- read.table("IowaHouses.txt", header=TRUE)
price <- iowa[,1]
logprice <- log(price)
cond <- iowa[,2]

# a)
lin <- lm(logprice ~ cond, data = iowa)
# Formula: y = -0.127 + 12.824
plot(cond, logprice)
abline(a = 12.824, b = -0.127, col=2)
print(summary(lin))
# Since p-value: 0.04132 < 0.05, we reject the null hypothesis and assert the alternative that house price
# is dependent on house condition.

# b)
boxplot(logprice~cond,data=iowa, main="House Condition and Price",
        xlab="Condition", ylab="log(Price)")
# Referencing the boxplot, it appears that
cond <- as.factor(cond)
lin <- lm(logprice ~ cond, data = iowa)
print(anova(lin))
# The p-value of the variance analysis: 0.1481 > 0.05 so we fail to reject the null hypothesis and conclude
# that we do not have enough evidence to conclude that price depends on condition.

# c)
# (a) The problems with doing linear regression here is that a house can be in good condition, yet smaller and at a worse location.
# And a house can be in bad condition, but much larger and better location. Obviously the homes that are larger and in better
# locales will sell for more than a smaller house that's in better condition. So the problem with this linear model (a) is that
# we cannot accurately use house condition as a numerical predictor. We are not confident that the 4 assumptions of linear
# regression are met, namely the Homoskedasticity or normalty of errors.
# (b) The problem with doing variance analysis here is that we don't know if we can check off all of the boxes for assumptions
# of the ANOVA test. For one, it is hard to say if the population follows a normal trend. Also, hard to assert that the sample data
# is independent of itself, as real estate is often a bubble sort of a market that follows local trends of price. 

# I think despite both of their flaws that the ANOVA variance test is the best to run on this data. I think that for one, the variance
# of real estate prices is a better statistic to study because, especially in real estate, there is going to be a significant amount
# of variance for a number of reasons. The ANOVA test builds that into the strategy of analysis whereas the linear regression technique
# does not. I also think that it is more valuable to separate the house conditions into seperate categories, as the ANOVA test does,
# instead of making it numerical, as linear regression does. The condition of a house on a scale of 1-10 seems much more subjective
# than objective, it should be in a category and not serve as an actual value of measurement. Plus, the assumptions of the ANOVA test
# are slightly more easily met, as we don't have to assume that the data is linear... which it does not look like it is. I also personally
# agree more with the results of the ANOVA test than the linear regression model. 






