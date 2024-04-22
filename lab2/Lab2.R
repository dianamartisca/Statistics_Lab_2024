
#ex 1.3
tablou=read.csv("life_expect.csv",header = T)
male=tablou[['male']]
female=tablou[['female']]
i1=seq(min(male),max(male),(max(male)-min(male))/7)
i2=seq(min(female),max(female),(max(female)-min(female))/7)
hist(male,breaks =i1,freq=F, col="blue",main="Speranta viata-Male")
hist(female,breaks =i2,freq=F, col="red",main="Speranta viata-Female")

#ex 3.1

sample= c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)
outliers_mean=function(x){
  outliers=vector()
  m=mean(x)
  s=sd(x)
  j=0
  for (i in 1:length(x)) {
    if(x[i]<=(m-2*s)|x[i]>=(m+2*s)){
      j=j+1
      outliers[j]=x[i]
    }
  }
  return(outliers)
}
outliers_mean(sample)

#ex 3.2

outliers_iqr=function(x){
  q_1=as.vector(quantile(x))[2]
  q_3=as.vector(quantile(x))[4]
  iqr=q_3-q_1
  outliers=vector()
  j=0
  for (i in 1:length(x)) 
    if((x[i]<(q_1-1.5*iqr))|(x[i]>(q_3+1.5*iqr))){
      j=j+1
      outliers[j]=x[i]
    }
  return(outliers)
}
outliers_iqr(sample)

#ex 3.3

sam=scan("sample2.txt")
summary(sam)
outliers_mean(sam)
outliers_iqr(sam)





