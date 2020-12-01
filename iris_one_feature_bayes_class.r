# create a bayes classifier for the iris data using a single feature: the petal length

# since the feature is continuous and we only know how to do discrete probability, we will quantize
# the feature into 4 different levels

# we estimate our class-conditional probability distributions by counting, for each class
# the number of examples we have at each level.  We write Q[c,v] = P(x = v | C = c)

# our prior is just "uniform"  (1/3,1/3,1/3) since 50 examples of each class

# we maximize p(C = c)P(x | C = c) over the class c to find our classification
# (this is what the Bayes classifier does)



data(iris3)   # (different form of same data) 3 dim array,  iris3[50 examples, 4 attributes, 3 classes]
petal_len = 4

bk = c(0,.5,1.,1.5,2.,2.5)  # the breakpoints we will used in quantizing the petal_len variable
values = length(bk)-1;

iris3[,petal_len,] = cut(iris3[,petal_len,],breaks=bk,labels=1:values)   # substitute the  quantized values

x = iris3[,petal_len,];    # just to simplify going forward ...  x[i,c] is the quantized value



Q = array(0,dim=c(3,values));	# Q[c,v] is P(x = v | class c)
for (c in 1:3) {
   for (v in 1:values) {
      Q[c,v] = sum(x[,c] == v)/50   # estimate probs by usual counting
   }
}



prior = rep(1/3,3);  # prior not very important in this example since data chosen to have 50 examples from each class
error = 0;     # will count the # of errors our Bayes classifier makes


# do the classification of the flowers

for (c in 1:3) {   # for each class 
    for (i in 1:50) {    # for each flower ...
    	p = prior;
	v = x[i,c];
	p = p*Q[,v];
	p = p/sum(p)  # make them into probs
	cat("true class: ",c,"bc says: ",which.max(p),"\n");
	if (c != which.max(p)) error = error+1;
    }
}

cat("errors = ", error, "\n");
