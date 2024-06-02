#B1
volum_tor <- function(R, r, n) {
  N_C <- 0
  for (i in 1:n) {
    x <- runif(1, -R-r, R+r)
    y <- runif(1, -R-r, R+r)
    z <- runif(1, -r, r)
    if ((z*z + (sqrt(x*x + y*y) - R)*(sqrt(x*x + y*y) - R)) < r*r) {
      N_C <- N_C + 1
    }
  }
  volum_cub <- (2 * (R + r))*(2 * (R + r)) * (2 * r)
  return (volum_cub * (N_C / n))
}

volum_exact <- 2 * pi*pi * 10 * 3*3
volum_estimat<-volum_tor(10,3,10000)
abs_err=abs(volum_estimat-volum_exact)
rel_err=abs_err/volum_exact
volum_exact
volum_estimat
rel_err

volum_exact <- 2 * pi*pi * 10 * 3*3
volum_estimat<-volum_tor(10,3,20000)
abs_err=abs(volum_estimat-volum_exact)
rel_err=abs_err/volum_exact
volum_exact
volum_estimat
rel_err

volum_exact <- 2 * pi*pi * 10 * 3*3
volum_estimat<-volum_tor(10,3,50000)
abs_err=abs(volum_estimat-volum_exact)
rel_err=abs_err/volum_exact
volum_exact
volum_estimat
rel_err

#B2
arie_triunghi <- function(n) {
  x1 <- 0
  y1 <- 0
  x2 <- 2
  y2 <- 0
  x3 <- 1.2
  y3 <- 2.4
  a <- min(x1, x2, x3)
  b <- max(x1, x2, x3)
  c <- min(y1, y2, y3)
  d <- max(y1, y2, y3)
  N_C <- 0
  for (i in 1:n) {
    x <- runif(1, a, b)
    y <- runif(1, c, d)
    if (y >= 0 && y <= 2 * x && y <= 6 - 3 * x) {
      N_C <- N_C + 1
    }
  }
  arie_dreptunghi <- (b - a) * (d - c)
  return(arie_dreptunghi * (N_C / n))
}
arie_exacta<-(1/2*2*2.4)
arie_estimata<-arie_triunghi(20000)
abs_err=abs(arie_estimata-arie_exacta)
rel_err=abs_err/arie_exacta
arie_exacta
arie_estimata
rel_err

#B3.a
a=function(N){
  s=0
  for(i in 1:N){
    x=runif(1,-1,1)
    s=s+((2*x-1)/(x*x-x-6))
  }
  return((1-(-1))*s/N)
}

estimat<-a(10000)
exact<-log(3)-log(2)
estimat
exact

#B3.b
b=function(N,a,b){
  s=0
  for(i in 1:N){
    x=runif(1,a,b)
    s=s+((x+4)/((x-3)^1/3))
  }
  return((b-a)*s/N)
}

estimat<-b(10000,4,11)
exact<-61.3
estimat
exact

#B3.c
c=function(N,a,b){
  s=0
  for(i in 1:N){
    x=runif(1,a,b)
    s=s+(x*exp(-x^2))
  }
  return((b-a)*s/N)
}

estimat<-c(100000,0,100)
exact<-1/2
estimat
exact

#B4.a
numar_ani<-function(n,p,q,initial,final) {
  nr=0
  curent=initial
  while(curent<final)
  {
    curent=curent+rbinom(1,n,p)-rbinom(1,curent,q)
    nr=nr+1
  }
  return (nr)
}

numar_ani(1000,0.25,0.01,10000,15000)

#B4.b
probabilitate<-function(n,p,q,initial,final,nr_luni) {
  set.seed(123)
  nr_sim<-100
  rez<-numeric(nr_sim)
  for(i in 1:nr_sim)
  {
    curent<-initial
    for(luni in 1:nr_luni)
      curent<-curent+rbinom(1,n,p)-rbinom(1,curent,q)
    rez[i]<-curent
  }
  prob<-sum(rez>=final)/nr_sim
  return (prob)
}

probabilitate(1000,0.25,0.01,10000,15000,(40*12+10))

#B4.c
probabilitate_2<-function(n,p,q,initial,final,nr_luni,err) {
  p <- probabilitate(n,p,q,initial,final,nr_luni)
  nr_sim <- 100
  while (TRUE) {
    rez <- numeric(nr_sim)
    for (i in 1:nr_sim) {
      curent <- initial
      for (luni in 1:nr_luni) 
        curent<-curent+rbinom(1,n,p)-rbinom(1,curent,q)
      rez[i]<-curent
    }
    prob<-sum(rez>=final)/nr_sim
    if (abs(prob-p) <= err) 
      break
    nr_sim <- nr_sim * 2
  }
  return (prob)
}

probabilitate_2(1000,0.25,0.01,10000,15000,(40*12+10),0.01)

