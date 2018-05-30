#Script for R demonstration.
trial <- read.csv('trial.csv')
  
trial[1:10,]
trial[2,]

x <- c(57,61,38,16,84)

write.csv(trial,file='hope.csv')

6+8
32-10
9*6
48/4
5^3
16^.5 #square root
exp(-3)
log(10) #natural log
log(10,10) # log base 10

#Constants e and pi:
exp(1)
pi

#Trigonometric values
sin(pi/2) 

nyyankees <- read.csv('nyyankees.csv',header=T)
nyyankees
table(nyyankees$bats)
which(nyyankees$bats==4)
nyyankees$bats[5]<-1
nyyankees$bats
nyyankees
mean(nyyankees$avg)
nyyankees$avg[8]<-287
nyyankees

#Still too high due to missing data (999)
nyyankees$avg[nyyankees$avg==999]<-NA
nyyankees
mean(nyyankees$avg,na.rm=TRUE)

#Subset right-handed, left-handed and switch hitters
mean(nyyankees$avg[nyyankees$bats==1],na.rm=TRUE)
mean(nyyankees$avg[nyyankees$bats==2],na.rm=TRUE)
mean(nyyankees$avg[nyyankees$bats==3],na.rm=TRUE)

#nyyankees$bats==1

#Reorder by names
nyyankees[order(nyyankees$name),]
nyyankees[order(nyyankees$avg,decreasing=T),]


std.origin <- read.csv('origin.csv',header=F)
std.counts<-table(std.origin)
std.counts

barplot(std.counts,col=c("purple","red","blue","yellow","green"),main="Student Origins arranged in a Bar Graph",xlab='country',ylab='count')

pie(std.counts,col=c("purple","red","blue","yellow","green"),main="Student Origins arranged in a Pie Chart")

tv.hours <- read.csv('tvhours.csv',header=F)
hist(tv.hours[,1],breaks=c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),xlab= "Hours TV viewed During New Year's Eve and New Year's Day",main="Histogram Showing TV Viewing Practices During Holiday Period",col="blue")
hist(tv.hours[,1],breaks=10,main="Another Histogram", xlab="Too Much TV Viewing?",col="red")
hist(tv.hours[,1],breaks=5,prob=TRUE,main="Another Histogram Showing Densities Rather Than Actual Counts", xlab="No, Not Nearly Enough TV Viewing!!",col="green")
#stem(tv.hours)
#stem(tv.hours,scale=2)
#stem(tv.hours,scale=4)

#Descriptive statistics:
x<-c(57,61,38,16,84)
x
mean(x)
median(x)
mode(x)

#Percentiles:
quantile(x,probs=c(0.0,0.25,0.5,0.75,1))
range(x)
IQR(x)
var(x)
sd(x)
sd(x)/mean(x)

gpa.sat<-read.csv("gpa.csv")
cov(gpa.sat$GPA,gpa.sat$SAT)
sd(gpa.sat$GPA)
sd(gpa.sat$SAT)
cov(gpa.sat$GPA,gpa.sat$SAT)/(sd(gpa.sat$GPA)*sd(gpa.sat$SAT))
cor(gpa.sat$GPA,gpa.sat$SAT)

library(fBasics)
kurtosis(gpa.sat$SAT)
skewness(gpa.sat$SAT)

#Combinations: how many ways may we take 2 from a total of 3 items:
choose(3,2)
choose(10,1)

#Factorial:
prod(5:1)

#Permutations: similar as combinations, however order counts.
choose(3,2)*prod(2:1)
choose(6,3)*prod(3:1)

#Distributions:
#d: prob. mass values (discrete) or prob. density values (continuous distr.) 
#p: cumm probab.
#q: quantiles
#r: generates numbers form any of the distrib.

#Binomial distribution:
#Probability distribution of number of successes in a sequence of n independent yes/no experiments.
#If we flip an unbiased coin 5 times, what is the probability of getting exactly 3 heads in the 5 flips? In this instance, n = 5 trials (or 'flips'), x = 3 successes (or 'heads'), and p = .5 (that is, the probability of getting a 'success,' or heads, on any given trial is .5, or fifty-fifty)
dbinom(3,5,.5)

dbinom(2,5,.5)
dbinom(1,5,.5)
dbinom(0,5,.5)
dbinom(0,5,.5)+dbinom(1,5,.5)+dbinom(2,5,.5)+dbinom(3,5,.5)

#p: cumm probab.
pbinom(3,5,.5)
dbinom(1,3,1/6)

#Poisson distribution:
##Probability of a number of events occurring in a fixed period of time if these events occur with a known average rate and independently of time since last event.
#Since the number of arrivals at the drive-in teller window of a bank during a 15 minute period is Poisson distributed with a mean rate of 10, what is the probability of exactly 5 arrivals in 15 minutes?
dpois(5,10)
ppois(5,10)

#Hypergeometric Probability Distribution: Probability that a sample will have certain number of successes of n draws without replacement. In this case, we know we do not have a binomial experiment since the probability of 'success' changes from trial-to-trial. 
#For example, suppose we wish to draw a sample of size n = 3 from an urn which contains five marbles, N = 5, 
#three black (n, 'success') and two white (m, 'failure'), and compute the probability that we will draw 
#2 black and one white marbles:
#x, q	vector of quantiles representing the number of white balls drawn without replacement from an urn which contains both black and white balls.
#m	 the number of white balls in the urn.
#n	 the number of black balls in the urn.
#k	 the number of balls drawn from the urn
#dhyper(x, m, n, k, log = FALSE)
dhyper(1,2,3,3,log=FALSE)

#Uniform distribution: defined to exist between a finite lower value and a finite upper value.
#suppose that the flight times of commercial aircraft traveling from Chicago to New York are uniformly distributed between 120 minutes and 140 minutes. What is the probability that a flight will take between 120 minutes and 130 minutes?
punif(130,min=120,max=140,lower.tail=T,log.p=F)

#Normal distribution:
#probability that the normally distributed random variable will take on a value of -1.96 or less
pnorm(-1.96)

pnorm(1.96)-pnorm(-1)
1-pnorm(1.09)

#qnorm: value of Z which cuts off an area or probability in the tail of the distribution.  
qnorm(.975)
qnorm(.95)
qnorm(.99)

#Exponential distribution:
#this distribution describes phenomena such as the time between occurrences (arrivals at a drive-up teller window, for example) and the distance between occurrences (such as the number of bubbles found in plate glass windows produced by a certain process).
#What is the probability that loading a truck will take 6 minutes or less?
#pexp(q, rate = 1, lower.tail = TRUE, log.p = FALSE)
pexp(6,1/15)

#prob. that loading a truck will take between 6 and 18 min.:
pexp(18,1/15)-pexp(6,1/15)

#Student's t
#for a t distr. with df=12,prob. that t will take on a value of 1.782 or less
pt(1.782,12)

#for a t distr. with df=12,prob. that t will take on a value of 2.681 or greater:
1-pt(2.681,12)

#value of t which cutts off an area of 0.025 in the upper tail with df=69.
qt(.95,69)

salary<-c(18,20,21,21,25)
salary

#conf. level 95%. Null hypothesis that salary is zero:
t.test(salary)

#with another conf. level
t.test(salary,conf.level=.99)

#Sample size to control both confidence level and level of precision.
#size of the sample (n)  obtained from t=(X-mu)/(sd/sqroot(n))
(qnorm(.975)*20/1)^2





# Chapter 2 Lab: Introduction to R

# Basic Commands

x <- c(1,3,2,5)
x
x = c(1,6,2)
x
y = c(1,4,3)
length(x)
length(y)
x+y
ls()
rm(x,y)
ls()
rm(list=ls())
?matrix
x=matrix(data=c(1,2,3,4), nrow=2, ncol=2)
x
x=matrix(c(1,2,3,4),2,2)
matrix(c(1,2,3,4),2,2,byrow=TRUE)
sqrt(x)
x^2
x=rnorm(50)
y=x+rnorm(50,mean=50,sd=.1)
cor(x,y)
set.seed(1303)
rnorm(50)
set.seed(3)
y=rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

# Graphics

x=rnorm(100)
y=rnorm(100)
plot(x,y)
plot(x,y,xlab="this is the x-axis",ylab="this is the y-axis",main="Plot of X vs Y")
pdf("Figure.pdf")
plot(x,y,col="green")
dev.off()
x=seq(1,10)
x
x=1:10
x
x=seq(-pi,pi,length=50)
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
contour(x,y,f,nlevels=45,add=T)
fa=(f-t(f))/2
contour(x,y,fa,nlevels=15)
image(x,y,fa)
persp(x,y,fa)
persp(x,y,fa,theta=30)
persp(x,y,fa,theta=30,phi=20)
persp(x,y,fa,theta=30,phi=70)
persp(x,y,fa,theta=30,phi=40)

# Indexing Data

A=matrix(1:16,4,4)
A
A[2,3]
A[c(1,3),c(2,4)]
A[1:3,2:4]
A[1:2,]
A[,1:2]
A[1,]
A[-c(1,3),]
A[-c(1,3),-c(1,3,4)]
dim(A)

# Loading Data

Auto=read.table("Auto.data")
fix(Auto)
Auto=read.table("Auto.data",header=T,na.strings="?")
fix(Auto)
Auto=read.csv("Auto.csv",header=T,na.strings="?")
fix(Auto)
dim(Auto)
Auto[1:4,]
Auto=na.omit(Auto)
dim(Auto)
names(Auto)

# Additional Graphical and Numerical Summaries

plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)
cylinders=as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T,horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")
hist(mpg)
hist(mpg,col=2)
hist(mpg,col=2,breaks=15)
pairs(Auto)
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)
plot(horsepower,mpg)
identify(horsepower,mpg,name)
summary(Auto)
summary(mpg)


# Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN

# The Stock Market Data

library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

# Logistic Regression

glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]
glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(Direction)
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)
train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
glm.fit=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fit,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76)
predict(glm.fit,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")

# Linear Discriminant Analysis

library(MASS)
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class=lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)
sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]
sum(lda.pred$posterior[,1]>.9)

# K-Nearest Neighbors

library(class)
train.X=cbind(Lag1,Lag2)[train,]
test.X=cbind(Lag1,Lag2)[!train,]
train.Direction=Direction[train]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Direction,k=1)
table(knn.pred,Direction.2005)
(83+43)/252
knn.pred=knn(train.X,test.X,train.Direction,k=3)
table(knn.pred,Direction.2005)
mean(knn.pred==Direction.2005)

