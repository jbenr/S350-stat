## Problem Set 9
## Ben Reichert

## 1
cat("1.\n ")
# a)
cat("a) This data should be analyzed using a one-sample t-test because there are two sets of data,\n",
    "we are taking into account the difference between the two, which is in fact one set of data.\n",
    "And for analyzing one sample population or set of data we use the one-sample t-test.\n ")
# b)
decafm <- 53
decafsd <- 19
cofm <- 41.5
cofsd <- 17
n <- 10

delta.hat <- decafm - cofm
se <- sqrt(decafsd*decafsd/n + cofsd*cofsd/n)
t.welch = delta.hat/se
nu = ( (decafsd^2/n) + (cofsd^2/n) )^2 / ( ((decafsd^2)/n)^2/(n-1) + ((cofsd^2/n)^2)/(n-1) )
p.value <- 2*(1 - pt(abs(t.welch), df = nu))
cat("b) When people with type-2 diabetes consume dates with cofee...\n",
    "Null hypothesis: Glycemic index is not affected.\n",
    "Alternative hypothesis: Glycemic index is different when coffee is consumed.\n",
    "t-statistic =",t.welch,", P-value =",p.value,"\n ")
# c)
difm <- 11.5
difsd <- 21
lower <- difm - qt(.975, df = n-1)* difsd/sqrt(n)
upper <- difm + qt(.975, df = n-1)* difsd/sqrt(n)
#q <-qt(0.975, df=nu)
#lower <- delta.hat-q*se
#upper <- delta.hat+q*se
cat("c) 95% confidence interval: (",lower,",",upper,")\n ")
# d)
cat("d) I think it is only safe to say that within this sample set (of people with type-2 diabetes)\n",
    "we can conclude that on average dates will produce a similar glycemic index with or without coffee.\n",
    "However, I do NOT think it would be correct to state that on average, no matter what the sample\n",
    "population might be, that dates would produce the same glycemic index with or without coffee.\n\n")

## 2 Trosset 11.4 Problem Set C, parts 1-3
cat("2.\n ")
# 1
# a)
cat("a) Experimental unit: Cholesterol level\n ")
# b)
cat("b) Experimental units drawn from 2 different populations:\n",
    "Type A: urgent, aggressive, and ambitious heavy, middle aged men.\n",
    "Type B: noncompetitive, more relaxed, and less hurried heavy, middle aged men.\n",
    "20 units drawn from each population.\n",
    "2-sample problem.\n ")
# c
cat("c) Only one measurement was taken on each experimental unit, cholesterol level.\n ")
# d
typea <- c(233,291, 312, 250, 246, 197, 268, 224, 239,239,
           254,276, 234, 181, 248, 252, 202, 218, 212,325)
typeb <- c(344,185, 263, 246, 224, 212, 188, 250, 148,169,
           226,175, 242, 252, 153, 183, 137, 202, 194,213)
typec <-typea-typeb
print(t.test(typea, typeb))
cat("d) Parameters of interest:\n",
    "Type A mean =",mean(typea),"\n",
    "Type B mean =",mean(typeb),"\n",
    "Delta hat =",mean(typea)-mean(typeb),"\n ")
# e
cat("e) For heavy middle aged men...\n",
    "Null hypothesis: Cholesterol levels remain the same despite personality differences.\n",
    "Alternative hypothesis: More aggressive, higher intensity individuals have higher cholesterol\n",
    "levels and thus a higher chance coronary heart disease.\n ")
# 2
plot(density(typea))
lines(density(typeb), col="red")
cat("2) After seeing the density plots for the two sets of data it is safe to say that neither of them\n",
    "are normally distributed.\n ")
# 3
typeam <- mean(typea)
typeasd <- sd(typea)
typebm <- mean(typeb)
typebsd <- sd(typeb)
na <- length(typea)
nb <- length(typeb)

delta.hat2 <- typeam - typebm
se2 <- sqrt(typeam*typeasd/na + typebm*typebsd/nb)
t.welch2 = delta.hat2/se2
nu2 = ( (typeasd^2/na) + (typebsd^2/nb) )^2 / ( ((typeasd^2)/na)^2/(na-1) + ((typebsd^2/nb)^2)/(nb-1) )
p.value2 <- 2*(1 - pt(abs(t.welch2), df = nu2))
cat("3)\n ",
    "a) P-value = 0.01481\n ",
    "If we adopt a significance level of 5% we should not reject the null hypothesis.\n  ")
difm <- mean(typec)
difsd <- sd(typec)
lower <- difm - qt(.95, df = n-1)* difsd/sqrt(n)
upper <- difm + qt(.95, df = n-1)* difsd/sqrt(n)
cat("b) 90% confidence interval: (",lower,",",upper,")\n")

## 3 Trosset 11.4 Problem Set D, parts 1-4
norm <- c(4.1, 6.3, 7.8, 8.5, 8.9, 10.4, 11.5, 12, 13.8, 17.6, 24.3, 37.2)
diab <- c(11.5,12.1,16.1,17.8,24,28.8,33.9,40.7,51.3,56.2,61.7,69.2)
cat("3.\n ")
# 1
plot(density(norm))
lines(density(diab), col="red")
cat("1) No, according to their graphs, these data sets do not look like symmetric distributions.\n",
    "They are lop sided.")
# 2
normlog<-log(norm)
diablog<-log(diab)
plot(density(normlog))
lines(density(diablog), col="red")
normsr<-sqrt(norm)
diabsr<-sqrt(diab)
plot(density(normsr))
lines(density(diabsr), col="red")
cat("2) The transformed measurements look slightly more symmetric. Especially the natural logarithm\n",
    "transformation of the normal control group.\n ")
# 3
normlog<-log(norm)
diablog<-log(diab)
plot(density(normlog))
lines(density(diablog), col="red")
cat("3) The transformed measurements do look to be relatively normal, their graphs form near spot on\n",
    "bell curves.\n ")
# 4
cat("4)\n ")
print(t.test(normlog, diablog))
cat(" Because the p-value is 0.0009776 which is < 0.05, it is safe to reject the null hypothesis and\n",
    "accept the alternative that the measurements of urinary beta-thromboglobulin excretion is conclusively\n",
    "higher in people with diabetes-12.")
