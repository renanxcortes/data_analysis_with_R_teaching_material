# NORMAL

r = 10000;
n = 9;

M = matrix(0,n,r);
Xbar = rep(0,r);

for (i in 1:r)
{
  M[,i] = rnorm(n,0,1);
}

for (i in 1:r)
{
  Xbar[i] = mean(M[,i]);
}

hist(Xbar,breaks=20);
mean(Xbar)
sd(Xbar)



# UNIFORME

r = 10000;
n = 9

M = matrix(0,n,r);
Xbar = rep(0,r);

for (i in 1:r)
{
  M[,i] = runif(n,0,1);
}

for (i in 1:r)
{
  Xbar[i] = mean(M[,i]);
}

hist(Xbar,breaks=20);




# EXPONENCIAL

r = 100000;
n = 200;

M = matrix(0,n,r);
Xbar = rep(0,r);

for (i in 1:r)
{
  M[,i] = rexp(n,1);
}

for (i in 1:r)
{
  Xbar[i] = mean(M[,i]);
}

hist(Xbar,breaks=50);




