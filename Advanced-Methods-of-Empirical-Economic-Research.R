
# Simulation of a data for nonparametric regression

MyMatrNr=6811992
set.seed(MyMatrNr)  

# The generated data are unified for you and
# stay unchanged if you run the program again 

n0=500                        ###number of observations
X=((1:n0)-0.5)/n0             ###X-values in (0, 1)
g= 50*X+30*exp(-30*(X-0.5)^2)-30 ###the trend function
e=rnorm(n0)*20                  ###the error terms, depending on your Matrikelnummer
Y=g+e

# Display the results

par(mfrow=c(2,1), mai=c(0.8, 0.8, 0.7, 0.5))
matplot(X, cbind(Y, g), type="ll", lty=c(2, 1))
title("The simulated data together")


# Fit local linear regressions using the (relative) bandwidths h1 = 0:01 to h30 = 0:3 
# with a grid of 0.01. Display RMSE(hj) against hj .

mu.K=2
nu=0
p=1

source("W4452-LPSmooth-Proj.txt")
h.all=(0:29)/100+0.01
RMSE=(1:30)*0

for(i in 1:30)
{h=h.all[i]
Results=LPSmooth(X,Y,p,nu,h,mu.K)
X=Results$X
Y=Results$Y
Ye=Results$Ye

RMSE[i]=(sqrt(mean((Ye[26:475]-g[26:475])**2)))}

RMSE
min(RMSE)

h.opt=h.all[RMSE==min(RMSE)]
plot(h.all, RMSE, type="l")
title("RMSE for all bandwidths")
abline(v=h.opt, col=2)

h.opt=h
mu.K=2
nu=0
p=1

h.all

source("W4452-LPSmooth-Proj.txt")
Results=LPSmooth(X,Y,p,nu,h,mu.K)
X=Results$X
Y=Results$Y
Ye=Results$Ye
matplot(X, cbind(Y,g,Ye), type="pll", pch="*", col=c(1,2,3), lwd=3, lty = c(1,1))
title("Estimated Regression together with the data")


# Fit a local linear regression with a bandwidth selected by CV. Display the CV
# criterion against hj and state the optimal bandwidth following the CV criterion.


source("W4452-LPSmooth.CV.txt")
Results=LPSmooth.CV(X, Y, p, mu.K)
h.all.LL=Results$h.all
h.rel.LL=Results$h.rel
CV.h.LL=Results$CV.h
h.opt.LL=Results$h.opt
CV.h.LL
min(CV.h.LL)
h.opt.LL

X=Results$X
Y=Results$Y
YeLL=Results$Ye
cat("The selected optimal bandwidth by CV's is", h.opt.LL, fill=TRUE)
cat("The selected optimal bandwidth bz CV's is", h.rel.LL, fill=TRUE)

par(mfrow=c(2,1), mai=c(0.9, 0.9, 0.5, 0.5))
plot(h.all.LL, CV.h.LL, type="l", col=4, lwd=3, 
     main="Plot or the CV criterion")
abline(v=h.rel.LL)

par(mfrow=c(1,1), mai=c(0.9, 0.9, 0.2, 0.2))
matplot(X, cbind(Y, g, YeLL), type="pll", pch = "*", 
        col=c(1,2,3), lwd=3, lty = c(1,1))
title("Estimated regression together with the observations")



# Fit three local linear regressions, which dier in the bandwidth used. Choose a small
# bandwidth, that overts the data, a large bandwidth that underts the data and a band-
# width that you consider to work well in describing the data. Show the data and the three
# tted regression curves in one suitable gure and comment on your results. Calculate and
# state the variance for each of the tted local linear regressions. What do you nd and how
# can your ndings be seen in the gure? Theoretically discuss the eect of the bandwidth
# on the bias.

Z=read.table("data_local_linear.txt")

X=Z[,4]
Y=Z[,2]

n=length(Y)
X=1:n

############# Display the results 
#### Code for LL-regression

mu.K=2          #### Smoothness of the kernel regression
nu=0
p=1            ### order of polynomial between 0 to 4 


source("W4452-LPSmooth-Proj.txt")

h11=0.03

Results1=LPSmooth(X, Y, p, nu, h11, mu.K)

X=Results1$X        
Y=Results1$Y       
Ye1=Results1$Ye      

h12=0.12
Results2=LPSmooth(X, Y, p, nu, h12, mu.K)
Ye2=Results2$Ye     

h13=0.35
Results3=LPSmooth(X, Y, p, nu, h13, mu.K)
Ye3=Results3$Ye     

# Displaying Results


par(mfrow=c(1,1),mai=c(0.8, 0.8, 0.7, 0.5))
matplot(X, cbind(Ye1, Ye2, Ye3, Y), type="llll", col=c(2,5,9,3), lwd=c(2, 2, 2, 2))
title ("Canadian occupational prestige on income by p=1, h11, h12 & h13") 


var(Ye1)
var(Ye2)
var(Ye3)  


# Display the histograms of your variables together with the density curves in suitable
# figures. Make sure that the vertical axis has a relative frequency scale and does not plot
# the counts in the bins. Is your data (nearly) normal distributed or clearly not with e.g.
# more than one mode? Depending on the properties of your data choose the best and the
# worst kernel density estimator from the slides of the lecture on the \sm.density" function
# in the R package \sm" for each of your variables.

X=Z[,4]
Y=Z[,2]

hist(X, freq= FALSE, breaks=20)
lines(density(X),lwd= 2, col = 5)

hist(Y, freq= FALSE, breaks=20)
lines(density(Y),lwd=2, col = 5)

qqnorm(X, pch= 19)
qqline(X)
qqnorm(Y, pch= 19)
qqline(Y)


#### sm.density

install.packages("sm")
library("sm")

par(mfrow=c(1,1))
h.select(X)
Xd1.1=sm.density(X, h.select(X))
X1=Xd1.1$eval.points
fx1.1=Xd1.1$estimate
title("Kernel density estimator with h.select")

hcv(X)
Xd1.2=sm.density(X, hcv(X))
fx1.2=Xd1.2$estimate
title("Kernel density estimator with hcv")

hsj(X)
Xd1.3=sm.density(X, hsj(X))
fx1.3=Xd1.3$estimate
title("Kernel density estimator with hsj")


par(mfrow=c(1,1))
h.select(Y)
Yd2.1=sm.density(Y, h.select(Y))
Y2=Yd2.1$eval.points
fy2.1=Yd2.1$estimate
title("Kernel density estimator with h.select")

h.select(Y, method="cv")
Yd2.2=sm.density(Y,h.select(Y, method="cv"))
fy2.2=Yd2.2$estimate
title("Kernel density estimator with hcv")

hsj(Y)
Yd2.3=sm.density(Y, hsj(Y))
fy2.3=Yd2.3$estimate
title("Kernel density estimator with hsj")


# Fit kernel densities to each of the two variables

par(mfrow=c(1,1))

###Display in a graph variable X

matplot(X1, cbind(fx1.1, fx1.2, fx1.3), xlab="X", lty=c(1,1,1), type="lll", col=c(2,4,3), lwd=c(1,1,1))
title("fx with h.select (red), hcv (blue) and hsj (green)")


###Display in a graph variable Y

matplot(Y2, cbind(fy2.1, fy2.2, fy2.3), xlab="Y", lty=c(1,1,1), type="lll", col=c(2,4,3), lwd=c(1,1,1))
title("fy with h.select (red), hcv (blue) and hsj (green)")

# 2-dimensional KDE

install.packages("np")
library("np")
data("cps71")

A=cps71
logwage=A$logwage   ### WAGE: Canadian cross-section wage data
age=A$age   ### AGE: age data (integer)

plot(logwage~age, pch=20, col= "darkgreen")

hist(logwage)
hist(age)

install.packages("sm")
library(sm)

h.select(age)
Xd1=sm.density(age, h.select(age), display="none")
fx1=Xd1$estimate
X0=Xd1$eval.points

hcv(age)
Xd2=sm.density(age, hcv(age), display="none")
fx2=Xd2$estimate

matplot(X0, cbind(fx1, fx2), type="l")

CPS2D=A[,c(1, 2)]

h.select(CPS2D)
Xd12D=sm.density(CPS2D, h.select(CPS2D))  ### 
title("2-D density using h.select")

hcv(CPS2D)
Xd22D=sm.density(CPS2D, hcv(CPS2D), col=2)  ### 
title("2-D density using hcv")

cor(logwage,age)





 
