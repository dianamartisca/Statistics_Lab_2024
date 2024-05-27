selection_mean=function(filename) {
  x = scan(filename);
  m = mean(x)
}
selection_mean("history.txt")

#ii.1

zconfidence_interval=function(n,sample_mean,alfa,sigma)
{
  critical_z=qnorm(1-alfa/2,0,1)
  a=sample_mean-critical_z*sigma/sqrt(n)
  b=sample_mean+critical_z*sigma/sqrt(n)
  interval=c(a,b)
  interval
}
zconfidence_interval(100,20,0.1,sqrt(9))

#ii.2
zconfidence_interval(25,67.53,0.1,10)

#ii.3
zconfidence_interval(50,5,0.05,0.5)

#ii.4
zconfidence_interval(100,1280,0.01,140)

#ii.5
zconfidence_interval(35,60,0.1,5)
zconfidence_interval(35,60,0.05,5)
zconfidence_interval(35,60,0.01,5)

#ii.6
zconfidence_interval=function(filename,alfa,sigma)
{
  x = scan(filename)
  n=length(x)
  sample_mean = mean(x)
  critical_z=qnorm(1-alfa/2,0,1)
  a=sample_mean-critical_z*sigma/sqrt(n)
  b=sample_mean+critical_z*sigma/sqrt(n)
  interval=c(a,b)
  interval
}
zconfidence_interval("history.txt",0.05,5)



#iii.1
t_conf_interval=function(n,sample_mean,alfa,s)
{
  se=s/sqrt(n)
  critical_t=qt(1-alfa/2,n-1)
  a=sample_mean-critical_t*se
  b=sample_mean+critical_t*se
  interval=c(a,b)
  interval
}
t_conf_interval(60,3.3,0.05,0.4)

#iii.2
t_conf_interval(196,44.65,0.01,sqrt(2.25))

#iii.3a
t_conf_interval(49,12,0.01,1.75)
t_conf_interval(49,12,0.05,1.75)

#iii.3b
t_conf_interval(49,13.5,0.05,1.25)

#iii.4
t_conf_interval=function(filename,alfa)
{
  x=scan(filename)
  n=length(x)
  sample_mean=mean(x)
  s=sd(x)
  se=s/sqrt(n)
  critical_t=qt(1-alfa/2,n-1)
  a=sample_mean-critical_t*se
  b=sample_mean+critical_t*se
  interval=c(a,b)
  interval
}
t_conf_interval("history.txt",0.05)
t_conf_interval("history.txt",0.01)

#iii.5
sample=c(12,11,12,10,11,12,13,12,11,11,13,14,10)
n=length(sample)
sample_mean=mean(sample)
s=sd(sample)
t_conf_interval(n,sample_mean,0.1,s)
t_conf_interval(n,sample_mean,0.05,s)
t_conf_interval(n,sample_mean,0.01,s)



#iv.1
test_proportion=function(alfa,n,succese,p0,tip)
{
  p_prim=succese/n
  z_score=(p_prim-p0)/sqrt(p0*(1-p0)/n)
  if(tip=='r')
    critical_z=qnorm(1-alfa)
  if(tip=='l')
    critical_z=qnorm(alfa)
  if(tip=='s')
    critical_z=qnorm(1-alfa/2)
  return(c(z_score,critical_z))
}
result=test_proportion(0.01,100,63,0.6,'r')
result

#iv.2
result=test_proportion(0.05,150,20,0.1,'l')
result