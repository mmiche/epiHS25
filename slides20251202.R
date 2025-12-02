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

# 1. Check the manual for the glm function (what is the default for the argument family?).
# Online manual of glm (see top of website, Usage):
# https://stat.ethz.ch/R-manual/R-devel/library/stats/html/glm.html

# 2. Make the model return predicted probabilities (that means: use the predict function, set type = "response"), then check whether they are between 0 and 1.
glmOutput <- predict(mod, newdata=d, type="response")
# Check range (all returned values between 0 and 1?)
range(glmOutput)
# Negative probabilities do not exist. Something is wrong here.

# Do the same thing, but with the logistic regression model:
logregMod <- glm(y~., family=binomial(link="logit"), data=d)
logregOutput <- predict(logregMod, newdata = d, type="response")
range(logregOutput) # Much better, thanks. What a relief.
# -----------------------------------------------