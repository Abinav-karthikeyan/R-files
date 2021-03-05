setwd("C:\\Users\\ADMIN\\Desktop\\acads")

zt<- read.csv("zomato.csv",nrows=4000)
zt[1,]
str(zt)
write.csv(zt,"C:\\Users\\ADMIN\\Desktop\\acads\\zomres7.csv")
k=read.csv("zomres5.csv")
k=k[-1,]
str(k)
zs[1,]
head(zt)
train[1,]

zt=zt[,c(3,4,5,6,7,10,12,13,16)]

#filtering 1
str(zt)
zs=zt[,c(2,3,4,5,8,9)]
library(ggplot2)
ggplot(data.frame(zs$listed_in.type.), aes(x=zs$listed_in.type.)) +
  geom_bar()
head(zs)
cln=c("online","table","rating","votes","cost","type")
colnames(zs)=cln
colnames(zs)
str(zs)
levels(xs$)

?randomForest
zs$cost=as.numeric(levels(zs$cost))[zs$cost]
val=mean(na.omit(zs$cost))
val
zs$cost[is.na(zs$cost)]=val
library(rpart)
str(zs)
hist(zs$cost)
hist(zs$rating)
zs$rating=as.factor(zs$rating)




jri=zs$rating
jri=gsub("/.*","",jri)
str(jri)
nrow(zs1)
head(zs)
zs$ratings=as.numeric(jri)
val1=median(na.omit(zs$ratings))
val1
zs$ratings[is.na(zs$ratings)]=val1

str(zs)
library(tree)
zs=zs[,-3]
sample=sample.split(zs,SplitRatio=0.91)

train=subset(zs,sample==TRUE)
test=subset(zs,sample==FALSE)
is.na.data.frame(train)
dr=data.frame(train$votes,train$cost,train$online,train$ratings,train$type.1,train$table)
str(dr)
x <- rpart(ratings~.,train,
           control = list(minsplit = 5, maxdepth =8, xval = 6))
summary(x)f
sfknwgklnsngvfefewgv ewgve
?helpText
head(train)
levels(train$type)
levels(test$type)
d=data.frame("online"=input$onl,"table"=input$tble,"votes"=input$vote,"cost"=input$cst,"type"="Buffet")
x
xt=tree(train.ratings~.-train.votes,dr,method=T)
plot(x)
text(x)
plotcp(x)
library(rpart.plot)
c=cor.test(train$votes,train$ratings)
str(train)
rpart.plot(x)
plot(x)
text(x)
p1=predict(x,train)
head(test)
p2=predict(x,test)

head(p1)
train$temp=p1
tail(train,8)
length(p1)
zs$treepred=p1
acc=list()
a1=mean((p1-train$ratings)^2)
a2=mean((p2-test$ratings)^2)
a1=a1^0.5
a2=a2^0.5
a2
str(train)
library(caret)
dmy <- dummyVars(" ~ .", data = dr)
trsf <- data.frame(predict(dmy, newdata = dr))
trsf

x <- rpart(train.ratings~.+train.online.Yes,trsf,method="anova", 
           control = list(minsplit = 15, maxdepth = 20, xval = 10))
x
str(dr)

p=predict.train(ba,newdata=dr,type ="raw")
p2=pre
p=as.nu
head(p)
t=table(p,dr$train.ratings)
m=mean((dr$train.ratings-p)^2)
m

ct=cv.tree(xt)
pct=prune.rpart(x,0.1)
pct

is.na(dr$train.ratings)
str(p)
p=as.numeric(levels(p))[p]

dr$train.ratings[is.na(dr$train.ratings)]=3.5

library(randomForest)
head(dr)
is.na(train$ratings)
ncol(train)
colnames(dr)
head(train)
ba= randomForest(ratings~.,data=train[,1:6])


is.na(dr$train.ratings)
px=predict(x,dr[,1:6])         
dr$treepred=px
ba
train$ratings[is.na(train$ratings)]=3.5
plot(ba)
p3=predict(ba,train[,1:6])
a3=mean((p3-train$ratings)^2)
a4=mean((p4-test$ratings)^2)
p4=predict(ba,test)
a3^0.5
a4^0.5
dr$predi=p
head(dr)
str(dr)
ba$
which.min(ba$mse)
dr=na.exclude(dr)
m1 <- randomForest(
  formula = Sale_Price ~ .,
  data    = ames_train
)
train$temp1=p3
nrow(dr)
dr[1800,]
i=data.frame(importance(ba))
plot(i)
i
i$at=as.factor(rownames(i))
barplot(i$IncNodePurity,names.arg = i$at,col="green")
str(i)
library(gbm)
varImpPlot(ba,type="scatterplot")
head(dr)
gb= gbm(rating???.,data=train[,1:6], distribution="gaussian",n.trees=5000)
summary(gb)  
ggplot()+geom_bar(mapping=aes(y=summary(gb)$rel.inf),data=summary(gb))

s=summary(gb)
s$var
ggplot(data=c, aes(x=nm, y=W)) +
  geom_bar(stat="identity", fill=c("orange","red","blue","green","purple"))+
  labs(y="ATTRIBUTE WEIGHT",x="ATTRIBUTE")+coord_flip()
  
gp=predict(gb,train[,1:6])
gp1=predict(gb,test)
a5=mean((gp-train$ratings)^2)
a6=mean((gp1-test$ratings)^2)
a5^0.5
a6^0.5

dr$gpredi=gp
train$temp2=gp




library(plotly)

f=plot_ly(data=c,labels=~W,)
fig <- plot_ly(c, labels = ~W, values = ~nm, type = 'pie',
               textposition = 'inside'
               
               )
fig
head(dr)


USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

fig <- plot_ly(c, labels = ~nm, values = ~W, type = 'pie', textposition="inside")
fig
fig %>%layout(title="Weights of Each Parameter considered",hovermode='all')
tail(dr,7)
??hovermode
acc=data.frame(a1,a2,a3^0.5,a4^0.5,a5^0.5,a6^0.5)
acc
tail(test)
head(zs)
nrow(zs)
colnames(dr[,1:6])=colnames(test)
write.csv(zt,"C:\\Users\\ADMIN\\Desktop\\acads\\zomres5.csv")



fig=plot_ly(data=c,x =~c$W, y = ~c$nm, type = 'bar',text=c$W,color=~c$nm,orientation = 'h',showlegend=FALSE)

fig=c%>%plot_ly
fig%>%add_trace(x = ~W, y = ~nm, type = 'bar',color=c$nm)%>%add_text(x=c$w,y=c$nm,text=c$W)
          
p=


p %>% layout(xaxis =list(title="Weights"))
fig%>%add_trace(text=c$W)
k=read.csv("zomres5.csv")
head(k)
k[1,]
nrow(k)
write.csv(acc,"C:\\Users\\ADMIN\\Desktop\\acads\\accuracy.csv")
tail(train)

z=read.csv("zomres1.csv")
nrow(z)

library(ggplot2)
c=data.frame(nm=c("PROCESSOR","BATTERY","MOTOR","SPEED","COST"),W=c(0.416212445,0.261787988,0.161050407,0.098572773,0.062376387))

ggplot+geom_bar(data=c,mapping = aes(x=nm,y=w))


pv=c(0.515328,0.11685 ,0.463516 ,0.441266 ,0.671526, 0.588932)
pv=sort(pv,decreasing = TRUE)

nme=c("HRP-5P","NAO","ROBOTHESPIAN","DARWIN OP2","ATLAS","VALKYRIE")
df=data.frame(nme,pv)
ggplot()+geom_bar(data=df,mapping = aes(x=pv,y=nme))


gt=ggplot(data=df, aes(x=nme, y=pv)) +
  geom_bar(stat="identity", fill=c("orange","red","blue","green","purple","brown"))+
  labs(y="PERFORMANCE SCORE WITH RANKING (LABELLED)",x="ROBOT")+geom_label(df,mapping=aes(x=nme,y=pv),label=c(1:6))+coord_flip()
gt+geom_label(df,mapping=aes(x=nme,y=pv),label=pv)
gt+scale_fill_discrete(
gt





m1=matrix(c(0.2,0.3,0.15,0.35),nrow = 1)
m1
m2=matrix(c(0.25,0.15,0.35,0.25,0.45,0.15,0.05,0.35,0.35,0.05,0.35,0.25,0.15,0.15,0.15,0.55),nrow=4,byrow=TRUE)
m=m1%*%m2
0.2*0.25+0.3*0.45+0.15*0.35+0.15*0.35
library(markovchain)
library(diagram)
cit=c("PARIS","TOKYO","NEWYORK CITY","BORA BORA")
cit
rownames(m2)=cit
colnames(m2)=cit
plotmat(m,pos = c(1,2),
        lwd = 1, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.1,
        box.type = "circle",
        box.prop = 0.5,
        box.col = "light yellow",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = .13)
      
plotmat(m,pos=c(1,4))

m=new("markovchain",states=cit,transitionMatrix=m2)
m


plotmat(m2,pos =c(1,2,1),
        lwd = 1, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.1,
        box.type ="circle",
        box.prop = 0.5,
        box.col="red",
        
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01,
        self.shiftx = 0.13)
plotmat(m,pos = c(1,2,1),
        lwd = 1, box.lwd = 2,
        cex.txt = 0.8,
        box.size = 0.1,
        box.type = "circle",
        box.prop = 0.5,
        box.col ="red",
        arr.length=.1,
        arr.width=.1,
        self.cex = .4,
        self.shifty = -.01
)
      


m1  
m2
m=m1%*%m2

m
j=1

while(1)
{
  
  n=m%*%m2
  if(n==m)
    break
  
  m=n
  j=j+1
  
  
  
  
  
  
  
}
j

n

plot(n)
n%*%m2

a=m1%*%m2
a%*%m2
n

m2
m2^%2%
library(matrixcalc)
matrix.power(m2,3)
m2

m2%*%m2%*%m2
df=data.frame(x=cit)
df
df$y=n
n=t(n)
n
df
n
library(plotly)
library(ggplot2)

remove.packages(ggplot2)
library(ggplot2)
library(tibble)
install.packages("ggplot2", dependencies = TRUE)
ggplot
plot_
p=plot_ly(df,x=df$x, y =df$y, type = 'bar',
            textposition ='outside',showlegend=FALSE)
p
l=list(n)
l
sum(n)
n
str(df)

df[1,]=as.numeric(df[1,])
df
sum(df[1,])
sum(l)
plot(x=df$x,y=df$y,type='b')
fig <- plot_ly(df, labels = ~x, values = ~y, type = 'pie')
fig
ggplot(df,aes(y=y))+geom_bar()+coord_polar("y")

m1=as.numeric(m1)

library(ggplot2)
df$y=as.numeric(df$y)
fig <- plot_ly(df, labels =~cit, values =~m1, type = 'pie',textposition='inside')
fig=plot_ly()
df1=data.frame(x=cit)
df1$y=m1
df1
df
fig


fig=fig%>%add_pie(df,labels=cit,values=df$y,textposition='inside')
fig
fig=fig%>%add_pie(df1,labels=~cit,values=~m1)
fig
library(plotly)

plot_ly(df ,x=df$x, y = df$y, type = 'bar')
df$y            




library(matrixcalc)
library(matlib)


b=c(700000,0,500)
gaussianElimination(a, b)

f b=matrix(c(32,8,0,8,32,0,0,0,12),nrow=3,byrow=TRUE)
b
c=a%*%b
c%*%g
c
a
g=t(a)



a=matrix(c(-2,1,0,0,0,1,-2,1,0,0,0,1,-2,1,0,0,0,1,-2,1,0,0,0,1,-1.002),byrow =TRUE,nrow=5)
a
b=c(-1000,0,0,0,-0.6)








