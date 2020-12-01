### 1
cat("1.\n ")
# a
ex1 <- 720/4
cat("a) EX =",ex1,"\n ")
# b
b1 <- 1 - pbinom(236, 720, 0.25)
cat("b) 1 - pbinom(236, 720, 0.25) is the correct way, giving us:",b1,"\n ")
# c
cat("c) The  significance  probability  is  (very  small).   This  means  a\n",
    "number of  correct  answers  this  high  would  be  (very  surprising)\n",
    "if the students were just randomly guessing.  The data is thus (compatible)\n",
    "with the null hypothesis.  We have (strong evidence) in favor of the\n",
    "alternative hypothesis that students are doing better than random guessing.\n\n")

### 2
cat("2.\n ")
ANESpilot <- read.table("ANESpilot.txt", header = TRUE)
# a
cat("a) p -> (probability a randomly selected person from this population\n",
    "would give Clinton a higher FT score than Trump)\n",
    "Null Hypothesis: p < 0.5, because Trump won the election, it is safe to assume\n",
    "that Trump would have a higher 'FT score'.\n",
    "Alternative Hypothesis: p >= 0.5, where Hilary was generally more approved of.\n ")
# b
cat("b) if the null hypothesis is true, X has a Binomial(1163, 0.5) distribution.",
    "\n ")
# c
hillary <- sum(ANESpilot$ClintonFT > ANESpilot$TrumpFT)
donald <- sum(ANESpilot$TrumpFT > ANESpilot$ClintonFT)
hprob <- hillary/(hillary+donald)
cat("c) # of respondants who rated Hillary > Trump:",hillary,"\n",
    "and # of respondants who rated Trump > Hillary:",donald,"\n ")
# d
hvar <- var(ANESpilot$ClintonFT)
hmean <- mean(ANESpilot$ClintonFT)
tvar <- var(ANESpilot$TrumpFT)
tmean <- mean(ANESpilot$TrumpFT)
pop <- length(ANESpilot$TrumpFT)
hp <- hvar/pop
tp <- tvar/pop

vhat <- ( (hp+tp)^2 ) / ( ( (hp*hp)/(pop-1) )+( (tp*tp)/(pop-1) ) )
qt <- qt(0.975, vhat)
tw <- (hmean-tmean)/(sqrt((hvar/pop)+(tvar/pop)))
p <- 2*pt(-tw,qt)
cat("d) P-value:",p,"\n ")
# e
cat("e) The data provides evidence that Hillary generally gets higher\n",
    "feeling thermometer scores than Donald. Hillary's mean score is",hmean,"\n",
    "while Donald's mean score is",tmean,"\n\n")

### 3
n3 <- 61
m3 <- 6.5
sd3 <- 12
cat("3.\n ")
# a
cat("a) Null hypothesis: that the mean change in test scores is not positive.\n",
    "Alternative hypothesis: that the mean change in test scores is positive.\n ")
# b
t <- m3 / (sd3/sqrt(n3))
cat("b) T =",t,"\n ")
# c
p3 <- 1-pnorm(t)
cat("c) One tailed P-value:",p3,"\n ")
# d
cat("d) Because the P-value is significantly less than 0.05, we can reject\n",
    "the null hypothesis that the mean change in test score is not positive,\n",
    "thus we can support the alternative hypothesis that the mean change in\n",
    "test scores is positive.\n ")
# e
cat("e) I do not believe that the study provides evidence that live reggae music\n",
    "improves test scores. It supports the claim that studying math while listening\n",
    "to reggae for 2 and a half hours improves test scores, but for all we know studying\n",
    "math while not listening to reggae would have improved them more, causing us to\n",
    "conclude that reggae music is a detriment to math test scores. ")