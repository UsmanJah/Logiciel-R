#Importaion des donnees
mesdonnees = read.table("C:/Users/lenovo/Desktop/Master Stat-Eco/Regression Lin�aire/Devoir_UT/dataR.txt" ,header=TRUE)
#Matrice de correlation des variables explicatives 
Y  = mesdonnees$Y
X1 = mesdonnees$X1
X2 = mesdonnees$X2
X3 = mesdonnees$X3
X4 = mesdonnees$X4
X5 = mesdonnees$X5
X  = cbind(X1,X2,X3,X4,X5)
#mesdonnees.cor = cor(mesdonnees)
mesdonnees.cor = cor(X)
#cor.test(X1,X2,X3,X4,X5)
#corr�lation de pearson
cor(mesdonnees.cor, method = c("pearson"))
#P-value
library(Hmisc)
rcorr(mesdonnees.cor, type=c("pearson"))
#P-value
#Comme r�sultat, la fonction rcorr() renvoie une liste avec les �l�ments suivants : - r : la matrice de corr�lation. - n :
#La matrice du nombre d'observations utilis� dans l'analyse de chaque paire de variables. - P : les p-values
#correspondant aux niveaux de significativit� des corr�lations.
#Effectuons une regression y en fonctions des variables explicatives
Reg=lm(Y~X)
summary(Reg)
#Le R2 ajust� est de 0.92
#V�rifions la multicolin�arite par la r�gle de Klein 
#On calcule la matrice carr� p � p compos�e des
#estimations ponctuelles des corr�lations
c = cor(X,X)
c^2
#Si une ou plusieurs valeurs au carr� sont proches de R2 alors on soup�onne que les variables associ�es sont colin�aires.
#On peut d'apr�s nos r�sultats on peut dire que les valeurs sotn colin�aires
#S�lection de variables par la crit�re BIC
BIC(Reg)
library(leaps)
v = regsubsets(Y ~X1 + X2 + X3 + X4+ X5 ,mesdonnees, method = "backward")
plot(v, scale = "bic")
#Repr�senter Y en fonction des valeurs pr�dites par le mod�le
summary(Reg)
plot(xy.coords(xlabel="X",ylabel="",Y))
abline(a = 0.83294, b =  2.14056 , h = 1.87098, v = -0.13228, untf = -0.23118)
plot(rstudent(Reg))








