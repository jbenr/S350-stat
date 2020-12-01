### 1 - Trosset ex 8.4.4
m1 = 300
sd1 = 30
n1= 20
m120 = 300*n1
sd120 = sqrt(n1)*sd1
p1 <- 1-pnorm(6300, mean=m120, sd=sd120)
cat("1.","\n"
    ,"Probability that 20 packs lasts >= 105 hrs (6300 minutes): ",p1,"\n\n")

### 2
#a
e2 = (-2*0.3)+(-1*0.6)+(12*0.1)
#b
var2 = ( ((-2)^2*0.3) + ((-1^2)*0.6) + ((12^2)*0.1) ) - 0
#c
n2 = 3
ebar2 = e2/n2
#d
varbar2 = var2/n2
#e
sd2 = sqrt(varbar2)/10
ee2 <- 1 - pnorm(0.5, mean=ebar2, sd=sd2)
  
cat("2.","\n",
    "a) E(X) =",e2,"= 0\n",
    "b) Var(X) =",var2,"\n",
    "c) E(Xbar) =",ebar2,"= 0\n",
    "d) Var(Xbar) =",varbar2,"\n",
    "e) P(Xbar > 0.5) =",ee2,"\n\n")

### 3
households <- c(rep(1,27), rep(2,34), rep(3,16), rep(4, 13), rep(5, 6), rep(6,3), 7)
#a
a3 = sum(households)/length(households)
#b
b3 = sd(households)
#c
c3 = b3*sqrt(length(households)-1)/length(households)
#d
d3 = pnorm(0.5/b3, mean=a3, sd=b3)-pnorm(-0.5/b3, mean=a3, sd=b3)
#e
sd3 <- sd(households)
m3<- mean(households)
n3<- length(households)
err3 <- qnorm(0.975)*sqrt(sd3/n3)

cat("3.","\n",
    "a) Mean =",a3,"\n",
    "b) SD =",b3,"\n",
    "c) Error =",c3,"\n",
    "d) P(|EX| < 0.5) =",d3,"\n",
    "e) Confidence interval: (",m3-err3,",",m3+err3,")","\n",
    "The 95% confidence interval of # of people per household based off of the simple random sample is","\n",
    "within 2 and 3, so yes we can reasonably assume that the average household size is between 2 and 3.","\n\n")

### 4
p4 <- 0.58
n4 <- 1009
err4 <- qnorm(0.975)*sqrt(p4*(1-p4)/n4)
cat("4.","\n",
    "Confidence interval: (",p4-err4,",",p4+err4,")",
    "\n\n")

### 5
cat("5.","\n",
    "True, because 570 / 600 is",570/600,", this question is really asking will 95% of the confidence\n",
    "intervals contain 50%. The answer is yes because that is what the 95% confidence interval is deciphering,\n",
    "it is saying we are 95% confident that this 0.5 chance event will show in the interval.")
