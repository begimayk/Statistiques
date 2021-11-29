#--------PROJET-----------------
#on charge le fichier csv dans le tableau "data"
mydata <- read.table(file = "./Data-Konushbaeva-Begimai.csv", header=TRUE ,sep=";")
# on utilise la fonction summary pour afficher les caracteristiques des 2 échantillon 
summary(mydata)
#pour faciliter le code on definit l'echantillon des notes en maths en tant que x1 et celui en français en tant que x2
mydata$Maths->x1
mydata$Français->x2
#on trouve l'ecart type de chaque echantillon a l'aide de la formule de la variance empirique
sqrt(sum(x1*x1)/length(x1)-(sum(x1)/length(x1))^2)
sqrt(sum(x2*x2)/length(x2)-(sum(x2)/length(x2))^2)
#on trace les graphique des deux echantillon a l'aide de la fonctions boxplot 
boxplot(x1, x2,main = "Les notes en maths et français",at = c(1,2),names = c("Maths", "Français"), xlab = "Notes par personne",col = "blue",border = "black",horizontal = TRUE)
#on trace les histogrammes avec la commande hist
par(mfrow=c(1,2))
hist(x1,xlab="Notes en maths",col='blue',freq=FALSE)
curve(dnorm(x1,mean(x1),sd(x1)),col = "red" , add=T)
hist(x2,xlab="Notes en français",col='green',freq=FALSE)
curve(dnorm(x2,mean(x2),sd(x2)),col = "red" , add=T)
#on trace les qqplots
par(mfrow=c(1,2))
qqnorm(x1,main="Les notes en maths")
qqline(x1,col="blue")
qqnorm(x2,main="Les notes en français")
qqline(x2,col="green")
#pour estimer l'esperance:
mean(x1)
mean(x2)
#pour construire l'intervalle de confiance
t.test(x1)
t.test(x2)
#pour avoir une precision de +-12 ( niveau de confiance 80%)
t.test(x1, conf.level=0.8)
t.test(x2, conf.level=0.8)
#estimation de l'écart-type:
sd(x1)
sd(x2)
#pour construire l'intervalle de confiance
alpha=0.05
n<-length(x1)
z1=qchisq(1-alpha/2,n-1)
z2=qchisq(alpha/2,n-1)
var1=var(x1)*(n-1)/n
var2=var(x2)*(n-1)/n
Binfx1=sqrt(n*var1/z1)
Bsupx1=sqrt(n*var1/z2)
Binfx2=sqrt(n*var2/z1)
Bsupx2=sqrt(n*var/z2)
#test statistiques:
t.test(x1,mu=70, alternative ="less")
t.test(x2, mu=65, alternative="two.sided")
#comparaison des 2 echantillon
t.test(x2,x1,alternative="greater")
D=x2-x1
t.test(D, mu=10, alternative="two.sided")



