cat("Ben Reichert (collaboration with Rozheen Nersisyan)\n")
cat("Problem Set 5\n\n")

# 1
a1 = pnorm(0, mean=-5, sd=10)
b1 = 1-pnorm(5, mean=-5, sd=10)
c1 = pnorm(7, mean=-5, sd=10) - pnorm(-3, mean=-5, sd=10)
d1 = pnorm(5, mean=-5, sd=10) - pnorm(-15, mean=-5, sd=10)
e1 = (1-pnorm(5, mean=-5, sd=10)) + pnorm(1, mean=-5, sd=10)

cat("1.\n",
    "a) P(X < 0) =",a1,"\n",
    "b) P(X > 5) =",b1,"\n",
    "c) P(−3 < X < 7) =",c1,"\n",
    "d) P(|X + 5| < 5) =",d1,"\n",
    "e) P(|X - 3| > 2) =",e1,"\n\n")

# 2
m12 = 1
v12 = 9
m22 = 3
v22 = 16
am2 = m12 + m22
av2 = v12 + v22
bm2 = -m22
bv2 = v22
cm2 = m12 - m22
cv2 = v12 + v22
dm2 = 2*(m12)
dv2 = (2^2)*(v12)
em2 = dm2 - 2*(m22)
ev2 = dv2 + (2^2)*(v22)

cat("2.\n",
    "a) Mean =",am2,", Var =",av2,"\n",
    "b) Mean =",bm2,", Var =",bv2,"\n",
    "c) Mean =",cm2,", Var =",cv2,"\n",
    "d) Mean =",dm2,", Var =",dv2,"\n",
    "e) Mean =",em2,", Var =",ev2,"\n\n")

# 3
ev3 = 525
sd3 = 110
a3 = 1-pnorm(600, mean=ev3, sd=sd3)
biq13 = qnorm(0.75, mean=ev3, sd=sd3)
biq23 = qnorm(0.25, mean=ev3, sd=sd3)
brange3 = biq13 - biq23
bev3 = ev3*2
bv3 = (sd3^2) + (sd3^2)
c3 = 1-pnorm(1200, mean=bev3, sd=sqrt(bv3))
dev3 = (ev3+ev3)/2
dv3 = bv3/(2^2)
e3 = 1-pnorm(600, mean=dev3, sd=sqrt(dv3))

cat("3.\n",
    "a) P(X1 > 600) =",a3,"\n",
    "b) Yes, S is an (approximately) normal random variable because adding\n two Normal random variables will produce a Normal random variable.\n",
    "E(S) =",bev3,", Var(S) =",bv3,"\n",
    "c) P(S > 1200) =",c3,"\n",
    "d) Yes, A is an (approximately) normal random variable because adding\n two Normal random variables and dividing by 2 will produce a Normal random\n variable.\n",
    "E(A) =",dev3,", Var(A) =",dv3,"\n",
    "e) P(A > 600) =",e3,"\n\n")

# 4
a4 = (1/30)+(2/30)+(3/30)+(4/30)+(5/30)
set4 <- c(1/30, 3/30, 6/30, 10/30, 15/30, 21/30, 28/30, 36/30, 45/30, 55/30,
          66/30, 78/30, 91/30, 105/30, 120/30, 136/30, 153/30, 171/30, 190/30,
          (21/3),(22/3)+(21/60),(23/3)+(43/60),(24/3)+(66/60),(25/3)+(90/60),(26/3)+(115/60),
          (27/3)+(141/60),(28/3)+(168/60),(29/3)+(196/60),(30/3)+(225/60),(31/3)+(255/60),(32/3)+(286/60),
          (33/3)+(318/60),(34/4)+(351/60),(35/3)+(385/60),(36/3)+(420/3),(37/3)+(456/60),(38/3)+(493/60),(39/3)+(531/60),
          (40/3)+(570/60)+(40/3)+(570/60)+1,(40/3)+(570/60)+2,(40/3)+(570/60)+3,(40/3)+(570/60)+4,(40/3)+(570/60)+5,(40/3)+(570/60)+6,
          (40/3)+(570/60)+7,(40/3)+(570/60)+8,(40/3)+(570/60)+9,(40/3)+(570/60)+10,(40/3)+(570/60)+11,(40/3)+(570/60)+12,(40/3)+(570/60)+13,
          (40/3)+(570/60)+14,(40/3)+(570/60)+15,(40/3)+(570/60)+16,(40/3)+(570/60)+17,(40/3)+(570/60)+18,(40/3)+(570/60)+19,(40/3)+(570/60)+20)
m4 = mean(set4)
sd4 = sd(set4)
b4 = qnorm(0.75, mean=m4, sd=sd4) - qnorm(0.25, mean=m4, sd=sd4)

cat("4.\n","a) Since y(5) =",a4,", the median is at y = 5.\n",
    "b) Interquartile range =",b4,"\n",
    "c) Skewed to the right because the median and mean are both closer to the\n lower quartile.\n\n")

# 5
m5 = 120
sd5 = 20
a5 = pnorm(135, mean=m5, sd=sd5) - pnorm(115, mean=m5, sd=sd5)
b5 = 1 - (pnorm(160, mean=m5, sd=sd5))^10
fq3 = qnorm(0.25, mean=m5, sd=sd5)
tq3 = qnorm(0.75, mean=m5, sd=sd5)

cat("5.\n","a) P(115 < BPM < 135) =",a5,"\n",
    "b) At least one out of ten P(BPM > 160) =",b5,"\n",
    "c) First Quartile:",fq3,", Third Quartile:",tq3)
