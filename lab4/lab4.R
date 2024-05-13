#1
disc_area = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    if(x*x + y*y <= 1)
      N_C = N_C + 1;
  }
  return(4*N_C/N);
}

disc_area(10000)
abs_err=abs(disc_area(20000)-pi)
rel_err=abs_err/pi
abs_err
rel_err

sphere_vol = function(N) {
  N_C = 0;
  for(i in 1:N) {
    x = runif(1, -1, 1);
    y = runif(1, -1, 1);
    z=runif(1,-1,1);
    if(x*x + y*y +z*z<= 1)
      N_C = N_C + 1;
  }
  return(8*N_C/N);
}

sphere_vol(10000)
abs_err=abs(sphere_vol(20000)-4*pi/3)
rel_err=abs_err/(4*pi/3)
abs_err
rel_err

#2
MC_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = runif(1, 0, 10);
    sum = sum + exp(-u*u/2);
  }
  return(10*sum/N);
}

MC_integr_average= function(k, N) {
  estimates = vector();
  for(i in 1:k)
    estimates[i] = MC_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}

MC_integr_average(30,20000)
MC_integr_average(30,50000)

MC_improved_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 1);
    sum = sum + exp(-u*u)/exp(-u);
  }
  return(sum/N);
}

MC_improved_integration(30)

MC_imprvd_integr_average= function(k, N) {
  estimates = 0;
  for(i in 1:k)
    estimates[i] = MC_improved_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}

MC_imprvd_integr_average(30,20000)
MC_imprvd_integr_average(30,50000)

#3
Nr_days = function() {
  nr_days = 1;
  last_errors = c(27, 31);
  nr_errors = 27;
  while(nr_errors > 0) {
    lambda = min(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

MC_nr_days = function(N) {
  s = 0;
  for(i in 1:N)
    s = s + Nr_days();
  return(s/N);
}

MC_nr_days(10000)

Nr_days = function() {
  nr_days = 2;
  last_errors = c(13, 15, 9);
  nr_errors = 13;
  while(nr_errors > 0) {
    lambda = min(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1:2]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

MC_nr_days = function(N) {
  s = 0;
  for(i in 1:N)
    s = s + Nr_days();
  return(s/N);
}

MC_nr_days(10000)

Nr_days = function() {
  nr_days = 2;
  last_errors = c(18, 22, 28);
  nr_errors = 18;
  while(nr_errors > 0) {
    lambda = min(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1:2]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

MC_nr_days_21 = function(N) {
  s = 0;
  for(i in 1:N) {
    if(Nr_days() > 21)
    s = s + 1;
  }
  return(s/N);
}

MC_nr_days_21(5000)

 alfa = 1 - 0.95
 z = qnorm(alfa/2)
 epsilon = 0.01
 p = 0.246
 N_min = p*(1 - p)*(z/epsilon)^2
 N_min
 MC_nr_days_21(N_min + 1)


#1.2
arie=function(N){
  N_C=0
  for(i in 1:N){
    x=runif(1,0,2)
    y=runif(1,0,2)
    if(y<=-2*x^2+5*x-2 && y>=0){
      N_C=N_C+1
    }
  }
  return(4*N_C/N)
}
arie(10000)
abs_err=abs(arie(10000)-27/24)
rel_err=abs_err/(27/24)
rel_err

#2.1.b
b=function(N){
  s=0
  for(i in 1:N){
    x=runif(1,1,4)
    s=s+exp(x)
  }
  return((4-1)*s/N)
}

b(10000)
abs_err=abs(b(10000)-51.87987)
rel_err=abs_err/51.87987
abs_err
rel_err

#2.1.d
d=function(N,a,b){
  s=0
  for(i in 1:N){
    x=runif(1,a,b)
    s=s+1/(4*x^2-1)
  }
  return((b-a)*s/N)
}

d(10000,1,50)
abs_err=abs(d(10000,1,50)-log(3/4))
rel_err=abs_err/log(3/4)
abs_err
rel_err

#2.2
MC_improved_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 3);
    sum = sum + exp(-2*u^2)/(3*exp(-3*u));
  }
  return(sum/N);
}

MC_improved_integration(50000)
abs_err=abs(MC_improved_integration(50000)-sqrt(pi/8))
rel_err=abs_err/sqrt(pi/8)
abs_err
rel_err

MC_imprvd_integr_average= function(k, N) {
  estimates = 0;
  for(i in 1:k)
    estimates[i] = MC_improved_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}

MC_imprvd_integr_average(30,50000)

#3.2
service = function() {
  u = runif(1, 0, 1)
  if (u < 3/4)
    return(rexp(1, 4))
  else
    return(rexp(1, 12))
}

MC_service = function(N) {
  sum = 0
  for (i in 1:N)
    sum = sum + service()
  return(sum / N)
}

MC_service(10000)

#4.1
prob=function(N){
  c=0
  for(i in 1:N){
    x=rgeom(1,0.3)
    y=rgeom(1,0.5)
    if(x<y^2){
      c=c+1
    }
  }
  return(c/N)
}

prob(1000)

alfa=1-0.95
z=qnorm(alfa/2)
epsilon=0.005
p=0.269
N_min=p*(1-p)*(z/epsilon)^2
N_min
prob(30000)