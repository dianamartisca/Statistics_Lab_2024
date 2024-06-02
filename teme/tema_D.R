#D1
zconfidence_interval=function(filename,alfa,sigma)
{
  tablou = read.csv(filename, header=T)
  x= tablou[['probabilitati']]
  n=length(x)
  sample_mean = mean(x)
  critical_z=qnorm(1-alfa/2,0,1)
  a=sample_mean-critical_z*sigma/sqrt(n)
  b=sample_mean+critical_z*sigma/sqrt(n)
  interval=c(a,b)
  interval
}
zconfidence_interval("probabilitati.csv",0.05,sqrt(92.16))
zconfidence_interval("probabilitati.csv",0.01,sqrt(92.16))

#D2
t_conf_interval=function(filename,alfa)
{
  tablou = read.csv(filename, header=T)
  x= tablou[['statistica']]
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
t_conf_interval("statistica.csv",0.05)
t_conf_interval("statistica.csv",0.01)

#D3
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
result=test_proportion(0.01,100,14,0.85,'r')
result
result=test_proportion(0.05,100,14,0.85,'r')
result
