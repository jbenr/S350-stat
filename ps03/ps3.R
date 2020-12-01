cat("Problem Set 3","\n")
cat("Ben Reichert\n\n")

### 1 ###
a1 = 1 - (4/5 * 3/4)
a1a = (4/5 * 1/4) + 1/5
cat("1.\n", "a.) P(Selecting Pink Ranger) =", a1, "\n")
b1 = (2/5 * 3/4) + (3/5 * 2/4) + (2/5 * 1/4)
b1a = 1 - (3/5 * 2/4)
cat(" b.) P(Selecting at least one female) =", b1, "\n")
c1 = (1/2 * 3/4) + (1/2 * 1/4)
cat(" c.) P(Selecting Pink Ranger | At least one is female) =", c1, "\n")
d1 = "These events are not independent. I know this because if they were independent
 P(Selecting  Pink Ranger) and P(Selecting Pink Ranger | At least one is female) would
 be equal, but the conditional probability is more likely. Thus, the initial condition
 that at least one female is selected impacts the likelihood of the Pink Ranger being
 selected. This contradicts the definition of independent events, the events are not
 independent."
cat(" d.)", d1, "\n\n")

### 2 ###
a2 = (2+1)/4
cat("2.\n", "a.) P(X ≤ 2) = F(2) =", a2, "\n")
b2 = 1 - a2
cat(" b.) P(X > 2) = 1 - P(X ≤ 2) = 1 - F(2) =", b2, "\n")
c2 = ((2.5+1)/4) - (0.5/2)
cat(" c.) P(0.5 < X ≤ 2.5) = P(X ≤ 2.5) - P(X ≤ 0.5) = F(2.5) - F(0.5) =", c2, "\n")
d2 = (1+1)/4
cat(" d.) P(X = 1) =", d2, "\n")
e2 = 1.4
cat(" e.) F(q) = 0.6, so q =", e2, "\n\n")

### 3 ###
pmf3 = "PMF f(X):
     0.1 - S ∈ {1,6}
     0.4 - S ∈ {3,4}"
cdf3 = "CDF F(X):
       0 - S < 1
     0.1 - 1 ≤ S < 3
     0.5 - 3 ≤ S < 4
     0.9 - 4 ≤ S < 6
     1.0 - S ≥ 6"
ev3 = 0.1*1 + 0.1*6 + 0.4*3 + 0.4*4
var3 = 0.1*(1-ev3)^2 + 0.4*(3-ev3)^2 + 0.4*(4-ev3)^2 + 0.1*(6-ev3)^2
sd3 = sqrt(var3)
cat("3.\n","a.)",pmf3,"\n b.)",cdf3,"\n c.) E(X) =",ev3,
    "\n d.) Var(X) =",var3,"\n e.) σ(X) =",sd3,"\n\n")

### 4 ###
func4 <- function(x) {
  result <- (7-x)/20
  return(result)
}
pmf4 = paste("    ",func4(1),"- x = 1\n    ",func4(2),"- x = 2\n     ",func4(3),"- x = 3\n    ",
             func4(4),"- x = 4\n     ",func4(5),"- x = 5\n    ","   0 - x = 6")
cdf4 = paste("       0 - 1 < x\n","    ",func4(1),"- 1 ≤ x < 2\n    ",func4(2)+func4(1),
             "- 2 ≤ x < 3\n    ",func4(2)+func4(1)+func4(3),"- 3 ≤ x < 4\n     ",
             func4(4)+func4(2)+func4(1)+func4(3),"- 4 ≤ x < 5\n       ",
             func4(4)+func4(2)+func4(1)+func4(3)+func4(5),
             "- 5 ≤ x < 6\n    ","   0 - x ≥ 6")
ev4 = 0.3*1 + 0.25*2 + 0.2*3 + 0.15*4 + 0.1*5
var4 = 0.1*(1-ev4)^2 + 0.4*(3-ev4)^2 + 0.4*(4-ev4)^2 + 0.1*(6-ev4)^2
sd4 = sqrt(var4)
cat("4.\n","a.) PMF f(X):\n",pmf4,"\n b.) CDF F(x)\n",cdf4,"\n c.) E(X) =",ev4,
    "\n d.) Var(X) =",var4,"\n e.) σ(X) =",sd4,"\n\n")

### 5 ###
a51 = 0.3 * 0.3
a52 = 2 * 0.7 * 0.3
a53 = 0.7 * 0.7
cat("5.\n","a.) P(X = 0) =", a51,"\n     P(X = 1) =",a52,"\n     P(X = 2) =",a53, "\n")
cat(" b.) CDF F(y):\n", "       0 - y < 0\n    ", a51, "- 0 ≤ y < 1\n    ",
    a52+a51, "- 1 ≤ y < 2\n       ", a53+a52+a51, "- 2 ≤ y < 3\n", "       0 - y ≥ 3\n")
ev5 = 1*.42 + 2*.49
cat(" c.) E(X) =",ev5)

