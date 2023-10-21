#Libraries
install.packages("reliaR")
install.packages("remotes")
install.packages("devtools")
install.packages("plotly")
install_github('spedygiorgio/lifecontingencies')
install_github("tidyverse/ggplot2")
install_github("dutangc/CASdatasets", subdir="pkg",force=T)
install_github("bcgov/elucidate")
library(zoo)
library(xts)
library(sp)
library(lifecontingencies)
library(reliaR)
library(CASdatasets)
library(ggplot2)
library(plotly)
library(gapminder)
library(bcgov/elucidate)
library(DataEditR)
library(tidyverse)
library(tidyquant)
#Modeles de Survie avec Les Lois Pour L'exemple Age=22
x <- 22
#parameters
alpha <- log(1.078316)
theta <- 1.921184e-4
A <- 0.01
k <- 1.921184e-1
scale <- 1
#Gompertz
pgompertz(x+8, alpha=alpha, theta=theta, lower=FALSE) /
pgompertz(x, alpha=alpha, theta=theta, lower=FALSE)
#Makeham
pgompertz(x+8, alpha=alpha, theta=theta, lower=FALSE) /
pgompertz(x, alpha=alpha, theta=theta, lower=FALSE)*exp(-A)
#Weibull
pweibull(x+8, k, scale, lower=FALSE) / pweibull(x, k, scale, lower=FALSE)
#Modeles de Survie avec Les Lois Pour L'exemple Age=80
x=80
#Gompertz
pgompertz(x+8,alpha = alpha,theta = theta,lower=FALSE)/pgompertz(x,alpha = alpha,theta = theta,lower=FALSE)
#Makeham
pgompertz(x+8, alpha=alpha, theta=theta, lower=FALSE) /
pgompertz(x, alpha=alpha, theta=theta, lower=FALSE)*exp(-A)
#weibull
pweibull(x+8, k, scale, lower=FALSE) / pweibull(x, k, scale, lower=FALSE)
#La difference est donc Bien Claire au niveau des deux ages et cela jouera un role dans le calcul des rentes .
#Modeles de Survie avec Les Tables de Mortalité sur l'age 80 ans:
#DataSet1988-1990 
df8890<-data(freTD8890) 
TD8890 <- new("lifetable", x=freTD8890$x, lx= freTD8890$lx)
summary(TD8890)
data_edit(x=df8890)
qxt(TD8890, 80, 1)
pxt(TD8890, 80, 1)
#esperance de vie résiduelle
exn(TD8890, x=80)
#DataSet 2000
dfTGF05<-data(freTGF05)
TGF05 <- new("lifetable", x=freTD8890$x, lx= freTD8890$lx)
summary(TGF05)
data_edit(x=dfTGF05)
qxt(TGF05, 80, 1)
pxt(TGF05, 80, 1)
#esperance de vie résiduelle
exn(TD8890, x=80)
#Dataset 2005-2006
df0002<-data(freTH0002)
freTH0002 <- new("lifetable", x=freTH0002$x, lx= freTH0002$lx)
summary(freTH0002)
data_edit(x=df0002)
qxt(freTH0002, 80, 1)
pxt(freTH0002, 80, 1)
#esperance de vie résiduelle
exn(freTH0002, x=80)

#Modeles de Survie avec Les Tables de Mortalité sur l'age 22 ans:
#DataSet1988-1990 
df8890<-data(freTD8890) 
TD8890 <- new("lifetable", x=freTD8890$x, lx= freTD8890$lx)
summary(TD8890)
data_edit(x=df8890)
qxt(TD8890, 22, 1)
pxt(TD8890, 22, 1)
#esperance de vie résiduelle
exn(TD8890, x=80)
#DataSet 2000
dfTGF05<-data(freTGF05)
TGF05 <- new("lifetable", x=freTD8890$x, lx= freTD8890$lx)
summary(TGF05)
data_edit(x=dfTGF05)
qxt(TGF05, 22, 1)
pxt(TGF05, 22, 1)
#esperance de vie résiduelle
exn(TD8890, x=22)
#Dataset 2005-2006
df0002<-data(freTH0002) 
freTH0002 <- new("lifetable", x=freTH0002$x, lx= freTH0002$lx)
summary(freTH0002)
data_edit(x=df0002)
qxt(freTH0002, 22, 1)
pxt(freTH0002, 22, 1)
#esperance de vie résiduelle
exn(TD8890, x=22)

#Exemples de Rente Sur Age 22 ans et Dataset 1988-1990
TD8890tb <- new("actuarialtable", x=TD8890@x, lx=TD8890@lx, interest=2.5/100)
axn(TD8890tb, x=22, m=0)
axn(TD8890tb, x=22, m=1)
i <- 1/0.975-1
c(axn(TD8890tb, x=22, m=0, n=3, i=i), axn(TD8890tb, x=22, m=0, n=3, i=i, pay="arrears"))
c(axn(TD8890tb, x=22, m=2, n=3, i=i), axn(TD8890tb, x=22, m=2, n=3, i=i, pay="arrears"))
c(axn(TD8890tb, x=22, m=5, n=3, i=i), axn(TD8890tb, x=22, m=5, n=3, i=i, pay="arrears"))
#Exemples de Rente sur Age 22 ans et Dataset 2005-2006
freTH0002tb <- new("actuarialtable", x=freTH0002@x, lx=freTH0002@lx, interest=2.5/100)
axn(freTH0002tb, x=22, m=0)
axn(freTH0002tb, x=22, m=1)
i <- 1/0.975-1
c(axn(freTH0002tb, x=22, m=0, n=3, i=i), axn(freTH0002tb, x=22, m=0, n=3, i=i, pay="arrears"))
c(axn(freTH0002tb, x=22, m=2, n=3, i=i), axn(freTH0002tb, x=22, m=2, n=3, i=i, pay="arrears"))
c(axn(freTH0002tb, x=22, m=5, n=3, i=i), axn(freTH0002tb, x=22, m=5, n=3, i=i, pay="arrears"))
#Exemples de Rente sur Age 22 ans et Dataset 2000
TGF05tb <- new("actuarialtable", x=TGF05@x, lx=TGF05@lx, interest=2.5/100)
axn(TGF05tb, x=22, m=0)
axn(TGF05tb, x=22, m=1)
i <- 1/0.975-1
c(axn(TGF05tb, x=22, m=0, n=3, i=i), axn(TGF05tb, x=22, m=0, n=3, i=i, pay="arrears"))
c(axn(TGF05tb, x=22, m=2, n=3, i=i), axn(TGF05tb, x=22, m=2, n=3, i=i, pay="arrears"))
c(axn(TGF05tb, x=22, m=5, n=3, i=i), axn(TGF05tb, x=22, m=5, n=3, i=i, pay="arrears"))

#Exemples de Rente Sur Age 80 ans et Dataset 1988-1990
TD8890tb <- new("actuarialtable", x=TD8890@x, lx=TD8890@lx, interest=2.5/100)
axn(TD8890tb, x=80, m=0)
axn(TD8890tb, x=80, m=1)
i <- 1/0.975-1
c(axn(TD8890tb, x=80, m=0, n=3, i=i), axn(TD8890tb, x=80, m=0, n=3, i=i, pay="arrears"))
c(axn(TD8890tb, x=80, m=2, n=3, i=i), axn(TD8890tb, x=80, m=2, n=3, i=i, pay="arrears"))
c(axn(TD8890tb, x=80, m=5, n=3, i=i), axn(TD8890tb, x=80, m=5, n=3, i=i, pay="arrears"))
#Exemples de Rente sur Age 22 ans et Dataset 2005-2006
freTH0002tb <- new("actuarialtable", x=freTH0002@x, lx=freTH0002@lx, interest=2.5/100)
axn(freTH0002tb, x=80, m=0)
axn(freTH0002tb, x=80, m=1)
i <- 1/0.975-1
c(axn(freTH0002tb, x=80, m=0, n=3, i=i), axn(freTH0002tb, x=80, m=0, n=3, i=i, pay="arrears"))
c(axn(freTH0002tb, x=80, m=2, n=3, i=i), axn(freTH0002tb, x=80, m=2, n=3, i=i, pay="arrears"))
c(axn(freTH0002tb, x=80, m=5, n=3, i=i), axn(freTH0002tb, x=80, m=5, n=3, i=i, pay="arrears"))
#Exemples de Rente sur Age 22 ans et Dataset 2000
TGF05tb <- new("actuarialtable", x=TGF05@x, lx=TGF05@lx, interest=2.5/100)
axn(TGF05tb, x=80, m=0)
axn(TGF05tb, x=80, m=1)
i <- 1/0.975-1
c(axn(TGF05tb, x=80, m=0, n=3, i=i), axn(TGF05tb, x=80, m=0, n=3, i=i, pay="arrears"))
c(axn(TGF05tb, x=80, m=2, n=3, i=i), axn(TGF05tb, x=80, m=2, n=3, i=i, pay="arrears"))
c(axn(TGF05tb, x=80, m=5, n=3, i=i), axn(TGF05tb, x=80, m=5, n=3, i=i, pay="arrears"))


#Effet du différé sur une rente viagere 
x<-seq(0, 110,1)
y1 <- axn(TD8890tb,x=0:110,m=0, n=3, i=i)
y2 <- axn(TD8890tb, x=0:110, m=5, n=3, i=i)
y3 <- axn(TD8890tb, x=0:110, m=10, n=3, i=i)
differe <- data.frame(x,y1,y2,y3)
p<-ggplot(differe, aes(x)) +                    
geom_line(aes(y=y1, colour="red")) + 
geom_line(aes(y=y2, colour="green"))+
geom_line(aes(y=y3, colour="blue")) +
  labs(title="Effet Du Différé" ,x ="Age", y = "Rente Viagère")+
  scale_color_identity(
                       breaks = c("green", "red", "blue"),
                       labels = c("m=5", "m=0", "m=10"),
                       guide = "legend")

ggplotly(p)
#Effet du temporaire sur une rente viagere 
y1 <- axn(TD8890tb,x=0:110,m=0, n=10, i=i)
y2 <- axn(TD8890tb, x=0:110, m=0, n=20, i=i)
y3 <- axn(TD8890tb, x=0:110, m=0, n=30, i=i)
p<-temporaire <- data.frame(x,y1,y2)
ggplot(temporaire, aes(x)) +                    
geom_line(aes(y=y1, colour="red")) + 
geom_line(aes(y=y2, colour="green"))+
geom_line(aes(y=y3, colour="blue"))+
  labs(title="Effet Du Temporaire" ,x ="Age", y = "Rente Viagère")+
  scale_color_identity(
    breaks = c("green", "red", "blue"),
    labels = c("N=10", "N=20", "N=30"),
    guide = "legend")
ggplotly(p)

#Exemple de Capital aux Déces à l'age 22 ans et sur Dataset 1988-1990
Axn(TD8890tb, x=22)
i <- 1/0.975-1
c(Axn(TD8890tb, x=22, n=2, m=0, i=i), Axn(TD8890tb, x=22, n=2, m=2, i=i), Axn(TD8890tb, x=22,n=3, m=5, i=i))
c(Axn(TD8890tb, x=22, n=3, m=0, i=i), Axn(TD8890tb, x=22, n=3, m=2, i=i), Axn(TD8890tb, x=22,n=3, m=5, i=i))
#Exemple de Capital aux Déces à l'age 22 ans et sur Dataset 2005-2006
Axn(freTH0002tb, x=22)
c(Axn(freTH0002tb, x=22, n=2, m=0, i=i), Axn(freTH0002tb, x=22, n=2, m=2, i=i), Axn(freTH0002tb, x=22,n=3, m=5, i=i))
c(Axn(freTH0002tb, x=22, n=3, m=0, i=i), Axn(freTH0002tb, x=22, n=3, m=2, i=i), Axn(freTH0002tb, x=22,n=3, m=5, i=i))
#Exemple de Capital aux Déces à l'age 22 ans et sur Dataset 2000
Axn(TGF05tb, x=22)
c(Axn(TGF05tb, x=22, n=2, m=0, i=i), Axn(TGF05tb, x=22, n=2, m=2, i=i), Axn(TGF05tb, x=22,n=3, m=5, i=i))
c(Axn(TGF05tb, x=22, n=3, m=0, i=i), Axn(TGF05tb, x=22, n=3, m=2, i=i), Axn(TGF05tb, x=22,n=3, m=5, i=i))

#Exemple de Capital aux Déces à l'age 80 ans et sur Dataset 1988-1990
Axn(TD8890tb, x=80)
i <- 1/0.975-1
c(Axn(TD8890tb, x=80, n=2, m=0, i=i), Axn(TD8890tb, x=80, n=2, m=2, i=i), Axn(TD8890tb, x=80,n=3, m=5, i=i))
c(Axn(TD8890tb, x=80, n=3, m=0, i=i), Axn(TD8890tb, x=80, n=3, m=2, i=i), Axn(TD8890tb, x=80,n=3, m=5, i=i))
#Exemple de Capital aux Déces à l'age 80 ans et sur Dataset 2005-2006
Axn(freTH0002tb, x=80)
c(Axn(freTH0002tb, x=80, n=2, m=0, i=i), Axn(freTH0002tb, x=80, n=2, m=2, i=i), Axn(freTH0002tb, x=80,n=3, m=5, i=i))
c(Axn(freTH0002tb, x=80, n=3, m=0, i=i), Axn(freTH0002tb, x=80, n=3, m=2, i=i), Axn(freTH0002tb, x=80,n=3, m=5, i=i))
#Exemple de Capital aux Déces à l'age 80 ans et sur Dataset 2000
Axn(TGF05tb, x=80)
c(Axn(TGF05tb, x=80, n=2, m=0, i=i), Axn(TGF05tb, x=80, n=2, m=2, i=i), Axn(TGF05tb, x=80,n=3, m=5, i=i))
c(Axn(TGF05tb, x=80, n=3, m=0, i=i), Axn(TGF05tb, x=80, n=3, m=2, i=i), Axn(TGF05tb, x=80,n=3, m=5, i=i))


#Effet du différé sur le capital décés
x<-seq(0, 110,1)
y1 <- Axn(TD8890tb,x=0:100,m=5, n=3, i=i)
y2 <- Axn(TD8890tb, x=0:100, m=10, n=3, i=i)
y3 <- Axn(TD8890tb, x=0:100, m=15, n=3, i=i)
differe <- data.frame(x,y1,y2,y3)
ggplot(differe, aes(x)) +                    
  geom_line(aes(y=y1, colour="red")) + 
  geom_line(aes(y=y2, colour="green"))+
  geom_line(aes(y=y3, colour="blue"))+
  labs(title="Effet Du Différé" ,x ="Age", y = "Capital Au Déces")+
  scale_color_identity(
    breaks = c("green", "red", "blue"),
    labels = c("m=5", "m=0", "m=10"),
    guide = "legend")
ggplotly(p)
#Effet du temporaire sur le capital décés
x<-seq(0, 100,1)
y1 <- Axn(TD8890tb,x=0:100,m=0, n=10, i=i)
y2 <- Axn(TD8890tb, x=0:100, m=0, n=20, i=i)
y3 <- Axn(TD8890tb, x=0:100, m=0, n=30, i=i)
temporaire <- data.frame(x,y1,y2,y3)
ggplot(temporaire, aes(x)) +                    
  geom_line(aes(y=y1, colour="red")) + 
  geom_line(aes(y=y2, colour="green"))+
  geom_line(aes(y=y3, colour="blue"))  +
  labs(title="Effet Du Temporaire" ,x ="Age", y = "Capital Au Déces")+
  scale_color_identity(
    breaks = c("green", "red", "blue"),
    labels = c("N=10", "N=20", "N=30"),
    guide = "legend")
ggplotly(p)

#Provisions Mathématiques
Vkx.capuniq <- function(x, k, K)
  K * AXn(TD8890tb, x=x+k)


Vkx.capannu <- function(x, k, K)
{
  prem <- Axn(TD8890tb, x=x)*K / axn(TD8890tb, x=x, m=0)
  K * AXn(TD8890tb, x=x+k) - prem * aXn(TD8890tb, x=x+k, m=0)
}

aXNM <- Vectorize(axn, c("x", "n", "m"))
Vkx.retraite <- function(x, n, k, R)
{
  prem <- axn(TD8890tb, x=x, m=n-x)*R / axn(TD8890tb, x=x, m=0, n=n-x)
  k1 <- k[k < n-x]
  k2 <- k[k >= n-x]
  res <- aXNM(TD8890tb, x=x+k1, m=n-x-k1)*R - prem * aXNM(TD8890tb, x=x+k1, m=0, n=n-x-k1)
  c(res, aXNM(TD8890tb, x=x+k2)*R)
}
#Effet de l'age sur Capital Unique

X <- seq(0,60,by=1)
K<-5000
plot(X,Vkx.capuniq(X,30,K),xlab="Âge",ylab="Provision", type="l", lwd=2)
lines(X,Vkx.capuniq(X,40,K), type="l",lty=2 ,lwd=2, col="dark slate grey")
X <- seq(0,50,by=1)
lines(X,Vkx.capuniq(X,50,K),type="l",lty=3 ,lwd=2,col="dark turquoise")
X <- seq(0,40,by=1)
lines(X,Vkx.capuniq(X,60,K), type="l",lty=4 ,lwd=2,col="dark violet")
leg.txt <- c("x=30","x=40","x=50","x=60")
legend("bottomright",leg=leg.txt,lty=1:4)
title(main = "Effet de l'âge",
      cex.main =2,   font.main= 4, col.main= "deep pink",
)

#Effet de l'age sur Capital Annuités

K<-7000
plot(X,Vkx.capannu(X,30,K),xlab="Âge",ylab="provision", type="l", lwd=2,col="red")
lines(X,Vkx.capannu(X,40,K),type="l", lty=2,lwd=2, col="grey")
lines(X,Vkx.capannu(X,50,K),type="l", lty=3,lwd=2, col="green")
lines(X,Vkx.capannu(X,60,K), type="l", lty=4,lwd=2, col="black")
leg.txt <- c("x=30","x=40","x=50","x=60")
legend("bottomright",leg=leg.txt,lty=1:4)
title(main = "Effet de l'âge",
      cex.main =2,   font.main= 4, col.main= "dark blue",
)

#Effet de l'age sur Capital Retraite

plot(X,Vkx.retraite(60,70,X,1000),xlab="Âge",ylab="Provision", type="l" ,lwd=2,col="red")
lines(X,Vkx.retraite(50,70,X,1000), type="l", lty=2,lwd=2,col="black")
lines(X,Vkx.retraite(40,70,X,1000), type="l", lty=3,lwd=2,col="grey")
lines(X,Vkx.retraite(30,70,X,1000), type="l", lty=4,lwd=2,col="green")
leg.txt <- c("x=60","x=50","x=40","x=30")
legend("bottomright",leg=leg.txt,lty=1:4)
title(main = "Effet de l'âge",
      cex.main =2,   font.main= 4, col.main= "dark blue",
)


