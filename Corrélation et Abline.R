setwd("C:/Users/lenovo/Desktop/Master Stat-Eco/analyse de donnees/Projet¨¤Rendre2021-20201227T102906Z-001/Projet¨¤Rendre2021")
getwd()
data=read.csv("cereales.csv", header=TRUE, sep=",")
summary(data)
y=data$rating
x=data$sugars
sapply(data,mode)
cor(x,y)
cor.test(x,y)
View(data)
help(lm)S
Reg=lm(y~x)
summary(Reg)
abline(Reg, col="blue")
View(Reg)
anova(Reg)
plot(y,x)
coeff=coefficients(Reg)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
eq
abline(Reg, col="blue")
plot(x,y)
abline(a = 55.4, b=-2.5)

