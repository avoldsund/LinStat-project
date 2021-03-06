# set up design
library(FrF2)
library(MASS)
?FrF2

# set.seed(123) - if you want to be able to reproduce this
# setting up the 2^4 experiment in standard run order (no randomization)
plan <- FrF2(nruns=16,nfactors=4,randomize=FALSE)
plan

stargazer(plan)
# this is only done because I have entered the data in standard order,
# when the DOE experiment is done then "randomize=TRUE" should be chosen!!

# now you should perform the experiments and then read data into y below (in the same order as the plan desides on

y <- c(195,168,85,86,169,210,101,97,172,177,94,100,224,213,93,73)
y

# now we add the responses to the design - to be a unit!
plan <- add.response(plan,y)
plan

install.packages("stargazer")
library(stargazer)

stargazer(plan, summary=FALSE)
# here
# A=backpack
# B=time
# C=slope
# D=speed

# now we have an ordinary data set up to be used with lm
lm4 <- lm(y~(.)^4,data=plan)
summary(lm4)
effects <- 2*lm4$coeff
effects
names(lm4)
anova(lm4)
stargazer(effects)

# to make sure, I have implemented Lenths method as a function
lenth <- function(lmobj,alpha=0.05)
{
  abseffects <- abs(2*lmobj$coeff)[-1]
  medabseffects <- median(abseffects)
  medabseffects
  s0 <- 1.5*medabseffects
  keepeffects <- abseffects[abseffects< 2.5*s0]
  PSE <- 1.5*median(keepeffects)
  signlimit <-qt(1-alpha/2,length(abseffects)/3)*PSE
  return(list("PSE"=PSE,"Signlimit"=signlimit,"Number of sign"=sum(abseffects>signlimit)))
}

lenthres <- lenth(lm4)
lenthres
# see that factors with effects larger than lenthres$Signlimit should be assumed
# to have effect different from zero
effects
abs(effects)>lenthres$Signlimit
# here A, B, C, D and  BD
names(effects) <- c("Intercept","A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD")


# How to make a relatively ok Paretoplot for the effects
barplot(sort(abs(effects[-1]),decreasing=FALSE),las=1,horiz=TRUE,cex.names=1.0, main = 'Paretoplot for the effects')
abline(v=lenthres$Signlimit,col=2,lwd=2)
#dev.copy2pdf(file="barplotTread.pdf")

# and main and interaction effects plots
MEPlot(lm4)
IAPlot(lm4)

# we then go for only modelling main effects and first order interactions
#newy <- 1/y


lm2 <- lm(y~(.)^2,data=plan)
summary(lm2)
effects <- 2*lm2$coeff
effects
names(lm2)
anova(lm2)

# model check by residual plots
# 1 fitted vs studentized residuals
rres <- rstudent(lm2)
plot(lm2$fitted,rres, main = 'Residual plot')

# 2 if 1 strange, each x vs. studentized residuals
# not seem to be needed here

# 3 normality of residuals
qqnorm(rres)
qqline(rres)
library(nortest)
ad.test(rstudent(lm2))
# looks pretty ok

# 4 observation order vs. time?
# not here?

#What if? What if the residuals looked strange? Need transformation of y?
boxcox(lm2,plotit=TRUE)
# lambda=1 inside 95% CI, so dont need transformation.
# if not lambda=1 inside the CI maybe you need to transform the data and
# redo the analyses
# lambda=0 means to need newy=log(y),
# lambda=-1 means you need newy=1/y
# lambda=0.5 newy=sqrt(y)

# presentation and interpretation of results
MEPlot(lm2)
IAPlot(lm2)
