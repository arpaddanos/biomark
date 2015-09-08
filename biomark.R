##################################
#require(C50)
library(genefilter)
library(Hmisc)
#library(rpart)
#library(rpart.plot)
library("randomForest")
dunn=read.csv("239N IRON all TC level data sets for modeling Aug 18 2015.csv")

fdunn=dunn
fdunn$L.category.numeric=as.factor(fdunn$L.category.numeric)

#Adjust A as desired to get a larger or smaller amount of genes (variable number)

X=t(fdunn[,8:18653])
ffun=filterfun(pOverA(p = 0.2, A = 10), cv(a = 0.7, b = 10))
filt=genefilter(2^X,ffun)
dunnf=as.data.frame(t(X[filt,]))
dunnf$AGE=dunn$AGE
dunnf1=dunnf
dunnf2=dunnf
target=dunn$L.category.numeric[vamp]
tetarget=dunn$L.category.numeric[tet]
dunnf2$L=dunn$L
dunnf1_train=dunnf1[vamp,]
dunnf1_test= dunnf1[tet,]
target=as.factor(target)
tree_1=randomForest(x=dunnf1_train, y=target, mtry=3,ntree = 10001, proximity=TRUE)
pred1=predict(tree_1, dunnf1_test, type="response")
tab=table(tetarget,predict=pred1)
overall_accuracy=((tab[1,1]+tab[2,2]+tab[3,3])/sum(tab))*100
variable_number=length(colnames(dunnf1))
tab
overall_accuracy
variable_number
###########################