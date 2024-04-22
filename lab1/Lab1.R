x=seq(0.001,10,length=200)
y=log2(x)
plot(x,y,type='l',main='grafic')

n = 20
x = seq(0,n,1) # x cont¸ine valorile dela 0 la 20
y = dbinom(x, n, 0.4)
barplot(y, space = 0)

dispersie = function (x, p) {
  media = sum(p*x);
  dispersie = sum(p*(x - media)^2);
  return (dispersie)
  }
y = c(23, 32, 31, 27, 27, 33, 25, 21)
q = c(1/8, 1/16, 1/8, 1/16, 1/8, 1/16, 1/8, 5/16)
dispersie(y, q)

vector_sqrt = function(x) {
  for(i in 1:length(x)) {
    if(x[i] > 0)
      x[i] = sqrt(x[i])
    else
      x[i] = sqrt(-x[i])
  }
}

#ex1
ex1=function(x)
{
  n=length(x)
  max=min=suma=x[1]
  k=0
  for(i in 2:n) {
    max=max(max,x[i])
    min=min(min,x[i])
    suma=suma+x[i]
    if(x[i]>=40)
      k=k+1
  }
  return (c(max,min,suma/n,suma,min/max,k,(1-k/n)*100))
}

#ex2
ex2=function(x)
{
  n=length(x)
  a=x/sum(x)
  b=(x-min(x))/max(x)
  c=vector()
  d=vector()
  for(k in 1:(n-1))
  {
    c[k]=sum(x[1:k])/sum(x[(k+1):n])
    d[k]=min(x[1:k])/max(x[(k+1):n])
  }
  print(a)
  print(b)
  print(c)
  print(d)
}

#ex4
ex4=function(n,p)
{
  x=0:n
  y=(dbinom(x,n,p))
  barplot(y, space = 1, col='red')
}

#ex9
ex9=function(n,p)
{
  x=0:(n-1)
  y=dgeom(x,p)
  barplot(y, space = 1, col='blue')
}

#ex10
ex10=function(n,p)
{
  x=0:(n-1)
  y=dpois(x,p)
  barplot(y, space = 1, col='green')
}