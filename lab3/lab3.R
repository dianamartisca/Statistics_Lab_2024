density_exponential = function(lambda, n, a) {
  x = seq(0, a, n)
  y = dexp(x, lambda)
  plot(x, y, type = 'l')
}

density_exponential(1,0.1,10)

density_gamma = function(alpha, lambda, n, a) {
  x = seq(0, a, n)
  y = dgamma(x, alpha, lambda)
  plot(x, y, type = 'l')
}

density_gamma(10,1,0.1,10)

density_student = function(r, n, a) {
  x = seq(-a, a, n)
  y = dt(x, r)
  plot(x, y, type = 'l')
}

density_student(30,0.1,10)

density_norm = function(miu, sigma, n, a) {
  x = seq(-a, a, n)
  y = dnorm(x, miu, sigma)
  plot(x, y, type = 'l')
}

density_norm(0,0.1,0.01,1)

#ii
LLN_Poisson = function(lambda, n) {
  sum = 0
  for(i in 1:n) {
    u = rpois(1, lambda)
    sum = sum + u
  }
  return (sum/n);
}

LLN_Poisson(3,10000)

LLN_Poisson_var = function(lambda,n) {
  return(mean(rpois(n,lambda)));
}
LLN_Poisson_var(3,10000)

LLN_Gamma = function(alfa,lambda,n){
  return(mean(rgamma(n,alfa,lambda)));
}
LLN_Gamma(12,4,10000)

#ii.1
LLN_Exp = function(lambda,n){
  return(mean(rexp(n,lambda)));
}
LLN_Exp(3,10000)

LLN_Binom = function(m,p,n){
  return(mean(rbinom(n,m,p)));
}
LLN_Binom(10,0.5,10000)

#ii.2
LLN_Student = function(r,n){
  return(mean(rt(n,r)));
}

ex2 = function(){
  for(n in 10^(3:6))
    for(r in 2:5)
      cat('estimarea cu n = ',n, 'si r = ',r,'este',LLN_Student(r,n),'\n')
}
ex2()

#iii
CLT_Poisson = function(lambda, n, N, z) {
  expectation = lambda
  st_dev = lambda
  upper_bound = z * st_dev/sqrt(n) + expectation
  sum = 0
  for(i in 1:N) {
    x_n = mean(rpois(n, lambda))
    if(x_n <= upper_bound) {
      sum = sum + 1
    }
  }
  return(sum/N)
}
CLT_Poisson(3,300,10000,0)

pnorm(1)
CLT_Poisson(1,300,10000,1)
pnorm(1.5)
CLT_Poisson(1,300,10000,1.5)
pnorm(2)
CLT_Poisson(1,300,10000,2)

#iii.1
CLT_Exp = function(lambda, n, N, z) {
  expectation = 1/lambda
  st_dev = 1/lambda
  upper_bound = z * st_dev/sqrt(n) + expectation
  sum = 0
  for(i in 1:N) {
    x_n = mean(rexp(n, lambda))
    if(x_n <= upper_bound) {
      sum = sum + 1
    }
  }
  return(sum/N)
}
CLT_Exp(2,30,10000,1)

#iii.2
CLT_Gamma = function(alpha, lambda, n, N, z) {
  expectation = alpha/lambda
  st_dev = sqrt(alpha)/lambda
  upper_bound = z * st_dev/sqrt(n) + expectation
  sum = 0
  for(i in 1:N) {
    x_n = mean(rgamma(n, alpha, lambda))
    if(x_n <= upper_bound) {
      sum = sum + 1
    }
  }
  return(sum/N)
}
CLT_Gamma(3,2,30,10000,1)

#iv
barplot(dbinom(0:100,100,0.3))

binomial_probability = function(n, p, k) {
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q = (k + 0.5-expectation)/standard_deviation;
  return(1 - pnorm(q));
}
binomial_probability(50,0.3,10)

#iv.1
binomial_probability2 = function(n, p, k) {
  expectation = n*p;
  variance = n*p*(1 - p);
  standard_deviation = sqrt(variance);
  q = (k - 0.5-expectation)/standard_deviation;
  return(pnorm(q));
}
binomial_probability2(50,0.3,11)+binomial_probability(50,0.3,10)
