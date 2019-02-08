library(readxl)
Copy_of_2018_PFG_Skill_Assessment_Database_Aggregation_12262018 <- read_excel("~/Copy of 2018 PFG Skill Assessment Database Aggregation_12262018.xlsx")
View(Copy_of_2018_PFG_Skill_Assessment_Database_Aggregation_12262018)

skills<-Copy_of_2018_PFG_Skill_Assessment_Database_Aggregation_12262018[,5:26]
colnames(skills)<-LETTERS[1:22]
matrix.sk<-as.matrix(skills)

eigenvalue<-eigen(cov(matrix.sk))$values
eigenvector<-eigen(cov(matrix.sk))$vectors
varexplained <-c()
for (i in 1:22){
  (varexplained[i]<-sum(eigenvalue[1:i])/sum(eigenvalue))
}

fit <- princomp(matrix.sk, cor=TRUE)
summary(fit) 
loadings(fit) 
plot(fit,type="lines") 
biplot(fit)

install.packages("psych")
library(psych)
pca.fit <- principal(matrix.sk, nfactors=7, rotate="varimax")
print(pca.fit, digits=2, cutoff=.6, sort=TRUE)

fa.fit <- factor.pa(matrix.sk, nfactors=7, rotate="varimax")
fa.fit$loadings

