# set up design
library(FrF2)
?FrF2
require(MASS)

# set.seed(123) - if you want to be able to reproduce this
# setting up the 2^4 experiment in standard run order (no randomization)
plan <- FrF2(nruns=16,nfactors=4,randomize=FALSE)
plan

# this is only done because I have entered the data in standard order,
# when the DOE experiment is done then "randomize=TRUE" should be chosen!!

# now you should perform the experiments and then read data into y below (in the same order as the plan desides on

y <- c(195,168,85,86,169,210,101,97,172,177,94,100,224,213,93,73)
y

# now we add the responses to the design - to be a unit!
plan <- add.response(plan,y)
plan

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

# Significant Effect for lenth's Method
lenthsign <- lenth(lm4)
lenthsign
names(effects) <- c("Intercept","A","B","C","D","AB","AC","AD","BC","BD","CD","ABC","ABD","ACD","BCD","ABCD")

# Pareto Plot
barplot(sort(abs(effects[-1]),decreasing=FALSE),las=1,horiz=TRUE,cex.names=1.0)
abline(v=lenthsign$Signlimit,col=2,lwd=2)

# Model with third-, and fourth-order interactions removed
lm2 <- lm(y~(.)^2,data=plan)
summary(lm2)
effects <- 2*lm2$coeff
effects
names(lm2)
ano <- anova(lm2)

plot(lm2$fitted,rstudent(lm2),pch=20)
qqnorm(rstudent(lm2),pch=20)
qqline(rstudent(lm2))

# Finding s by using Mean Square Residuals from anova
s = sqrt(ano[11,3])

# Constructing s_effect
s_effect = sqrt(4*s^2/)