library()
library(stats)
install.packages("optrees")
library(optrees)
setwd("C:\\Users\\ADMIN\\Desktop\\acads")
install.packages("ggplot2")
library(ggplot2)
library(factoextra)
dv=read.csv("loctns.csv")
dv
dv=dv[,-1]
dv=dv[-1,]
head(dv)
dv1=as.matrix(dv)
temp=data.frame(s=seq(1:4),x=c(0,10,12,15),y=c(0,11,9,8))
temp=temp[,-1]
temp
temp1=get_dist(dv)
temp1=as.matrix(temp1)
d=temp1
temp1
10^0.5
head(dv1)
df <- USArrests
df <- scale(df)
d=get_dist(dv)
head(dv)
a=data.frame("x"=c(1,4,7),"y"=c(2,8,12))
row.names(a)=c("a","b","c")
d1=dist(dv)

d=get_dist(dv)
d
d=as.matrix(d)
d
d[16,16]
d[6,9]
library(gtools)
#16,15,135
10
13
14
7
11
1
12
15

#14,11,16
#home-15-13-12-3-1-9-4-8-2-6-home
#13-7-3-10-5-14-11
# home-6-9-4-8-2-1-12-15-home






b=c(4,8,9,3,12,13,15)

b=c(2,6,9,4,8)
ftr=g[24,]

combi=c(5,7,10,11,14,13)
combi
nrow(l2)
l1=permutations(n=7,r=7,v=b,repeats.allowed = F)
l2=permutations(n=7,r=7,v=b,repeats.allowed = F)
tail(l1)
library(dplyr)
cmb=merge(x=l2,y=l1,by=NULL)
nrow(cmb)
ncol(cmb)
tail(cmb)
g=data.frame("X1"=c(16),"x2"=c(6),l1,"pre"=c(2),"end"=c(16))
g=data.frame("X1"=c(16),l1,"end"=c(16))


tj=nrow(g)

rt=list()

for(j in 1:tj)
{
hu=as.double(g[j,])
lk=length(hu)-1
su=0
for(i in 1:lk)
{
  x=hu[i]
  y=hu[i+1]
  su=su +round(d[x,y])
  
}
rt=append(rt,su)
}
g$dis=rt
g$dis=as.double(g$dis)



g[which(g$dis==min(g$dis)),]

g[81,]
g[3177,]
g[14,]
tail(ju)
g[3177,]
g[393,]
g[263,]


#1357 10 12 
d[16,11]+d[11,1]+d[1,12]+d[12,3]+d[3,7]+d[7,13]+d[13,14]+d[14,5]+d[5,10]+d[10,15]+d[15,16]
d[11,3]+d[3,7]+d[7,13]+d[13,14]+d[14,5]+d[5,10]+d[10,16]+d[11,16]

d[16,15]+d[15,13]+d[13,2]+d[2,12]+d[12,3]+d[3,8]+d[8,4]+d[4,9]+ d[9,1]+d[1,6]+d[6,16]
#100-10-5-14-7-11-100
#home-11-3-7 -13 -14-5-10-homerobot 1)
fr=g[2,]
fr=as.double(fr)
#r1:home- 11-1 -12 - 3-  7- 13- 14-  5- 10- 15-home (r1:0,2,r2:.1)

typeof(fr[1])
#100-15-13-12-3-1-9-4-8-2-6-100
#home-16-11-10-5-14-7-3-12-13-15-1-home
#16,15,13
#14,11,16
#37677 16 11   15   10  5 14 13  7  3 12      1  16 368
d[12,2]+d[2,13]+d[13,15]
d[12,13]+d[13,2]+d[2,15]
311-d[11,1]-d[11,16]+d[1,6]+d[6,16]
#694832
d[16,6]+d[6,9]+d[9,4]+d[4,8]+d[8,3]+d[3,2]+d[2,16]
print("Testing is done")
