# Shortest possible excerpt from supplementary R script from
# Naimi and Whitcomb (2020) https://doi.org/10.1093/aje/kwaa004

set.seed(123)
N<-100
y<-as.numeric(rnorm(N,5,1)>6.25)
x<-rbinom(N,1,.5)
c1<-rbinom(N,1,.5)
c2<-rbinom(N,1,.5)
c3<-rbinom(N,1,.5)
c4<-rbinom(N,1,.5)
c5<-rbinom(N,1,.5)
d<-data.frame(y,x,c1,c2,c3,c4,c5)

mod <- glm(y~.,data=d)

# How can you find out that this glm(y~.,data=d) is NOT the logistic regression model?