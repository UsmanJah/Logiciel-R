#Importaion des donnees
mesdonnees = read.table("C:/Users/lenovo/Desktop/Master Stat-Eco/Regression Linéaire/Devoir_UT/dataR.txt" ,header=TRUE)
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
#corrélation de pearson
cor(mesdonnees.cor, method = c("pearson"))
#P-value
library(Hmisc)
rcorr(mesdonnees.cor, type=c("pearson"))
#P-value
#Comme résultat, la fonction rcorr() renvoie une liste avec les éléments suivants : - r : la matrice de corrélation. - n :
#La matrice du nombre d'observations utilisé dans l'analyse de chaque paire de variables. - P : les p-values
#correspondant aux niveaux de significativité des corrélations.
#Effectuons une regression y en fonctions des variables explicatives
Reg=lm(Y~X)
summary(Reg)
#Le R2 ajusté est de 0.92
#Vérifions la multicolinéarite par la régle de Klein 
#On calcule la matrice carré p × p composée des
#estimations ponctuelles des corrélations
c = cor(X,X)
c^2
#Si une ou plusieurs valeurs au carré sont proches de R2 alors on soupçonne que les variables associées sont colinéaires.
#On peut d'aprés nos résultats on peut dire que les valeurs sotn colinéaires
#Sélection de variables par la critére BIC
BIC(Reg)
library(leaps)
v = regsubsets(Y ~X1 + X2 + X3 + X4+ X5 ,mesdonnees, method = "backward")
plot(v, scale = "bic")
#Représenter Y en fonction des valeurs prédites par le modèle
summary(Reg)
plot(xy.coords(xlabel="X",ylabel="",Y))
abline(a = 0.83294, b =  2.14056 , h = 1.87098, v = -0.13228, untf = -0.23118)
plot(rstudent(Reg))








