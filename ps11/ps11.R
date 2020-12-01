# Ben Reichert
# Problem Set 11
library(tidyverse)
library(moderndive)
### 1
angst <- read.table("examanxiety.txt", header = TRUE, sep = "", dec = ".")

# a)
wom <- subset(angst, Gender == 'Female')
men <- subset(angst, Gender == 'Male')
print(t.test(wom[,4],men[,4]))
# Null: gender has no impact on anxiety levels.
# Alternative: gender has an impact on anxiety levels.
# Because our p-value: 0.7424 > 0.05, we fail to reject the null hypothesis that gender has no impact on anxiety levels.

# b)
x <- angst[,4]
y <- angst[,3]
anxiety <- lm(y ~ x, data = angst)
print(get_regression_table(anxiety))
core <- cor(x,y)
b <- core * sd(y) / sd(x)
a <- mean(y) - b * mean(x)
# The regression formula is y = -0.73x + 111, where x is anxiety level and y is exam score.
# This means that for every anxiety level your exam score goes down 0.73.

# c)
plot(x, y, pch = 16, cex = 1, col = "purple", main = "Anxiety and Test Scores", xlab = "Anxiety Level (0-100)", ylab = "Test Score (0-100)")
abline(a, b, col="red")
# i) Linearity is met because the trend of the graph follows a linear pattern, which is that as anxiety increases test scores decrease.
# ii) Independence is not met because it seems that the two variables impact the outcome of the other. 
err <- rep(0,length(x))
for(i in 1:length(x)) {
  err[i] <- y[i] - (-.73*x[i]+111)
}
plot(x, err, pch = 16, cex = 1, col = "red", main = "Anxiety & Test Scores Regression Error", xlab = "Anxiety Level (0-100)", ylab = "Error")
# iii) Equal variance of errors looks like it is met according to the graph of error along the regression line. It is not perfectly
# equal, but the variances trend in the same direction.
# iiii) Normality of errors is not met because the error along the regression line graph does not look normal at all. It is very skewed.

### 2
emp <- read.table("GameEmpathy.txt", header = TRUE, sep = "", dec = ".")

neut <- subset(emp, game.type == "neutral")
gta <- subset(emp, game.type == "GTA")
half <- subset(emp, game.type == "HalfLife")

# i) neutral
m_neut <- ave(neut[,4])
mn <- m_neut[1]

# ii) half-life
m_half <- ave(half[,4])
mh <- m_half[1]

# iii) GTA
m_gta <- ave(gta[,4])
mg <- m_gta[1]

gt <- emp[,2]
em <- emp[,4]
boxplot(em~gt,data=emp, main="Game and Empathy Levels",
        xlab="Game", ylab="Empathy level")

# Referencing the boxplot, it appears that the GTA game data has the highest variance yet a higher average empathy level than
# the neutral game. The half-life game data had the highest mean and lowest variance. 
anova = aov(em~gt,data=emp) # analysis of variance
summary(anova)
# The p-value of the variance analysis: 0 < 0.05 so we conclude that the means of the variances are statistically different,
# leading us to conclude that the type of game has an impact on average empathy.

# b)
# i) neutral
print(t.test(neut[,4],neut[,3]))
# Null: there is no significant difference between 
# Alternative: gender has an impact on anxiety levels.
# Because our p-value: 0.7424 > 0.05, we fail to reject the null hypothesis that gender has no impact on anxiety levels.

# ii) half-life
print(t.test(half[,4],half[,3]))
# Null: there is no significant impact of gender on anxiety levels
# Alternative: gender has an impact on anxiety levels
# Because our p-value: 0.01 < 0.05, we reject the null hypothesis and assert the alternative that, for half-life gamers,
# gender has an impact on anxiety levels. 

# iii) GTA
print(t.test(gta[,4],gta[,3]))
# Null: there is no significant impact that gender has on anxiety levels 
# Alternative: gender has an impact on anxiety levels
# Because p-value: 0.0003 < 0.05, we reject the null hypothesis and take the alternative that, for GTA gamers,
# gender definitively has an impact on anxiety levels. 





