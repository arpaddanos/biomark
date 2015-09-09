##################################
#require(C50)
library(genefilter)
library(Hmisc)
#library(rpart)
#library(rpart.plot)
library("randomForest")
#dunn=read.csv("239N IRON all TC level data sets for modeling Aug 18 2015.csv")

#vamp=sample(1:nrow(dunn),139)
#rr=c(1:nrow(dunn))
#tet=setdiff(rr,vamp)

setwd("C:/R/biomark")

#fdunn=dunn
#fdunn$L.category.numeric=as.factor(fdunn$L.category.numeric)

#Adjust A as desired to get a larger or smaller amount of genes (variable number)

#setwd("/Users/ogriffit/git/biostar-tutorials/MachineLearning")
datafile="trainset_gcrma.txt" 
clindatafile="trainset_clindetails.txt"
#outfile="trainset_RFoutput.txt"
#varimp_pdffile="trainset_varImps.pdf"
#MDS_pdffile="trainset_MDS.pdf"
#ROC_pdffile="trainset_ROC.pdf"
#case_pred_outfile="trainset_CasePredictions.txt"
#vote_dist_pdffile="trainset_vote_dist.pdf"

data_import=read.table(datafile, header = TRUE, na.strings = "NA", sep="\t")
clin_data_import=read.table(clindatafile, header = TRUE, na.strings = "NA", sep="\t")
----------------------------------------

clin_data_order=order(clin_data_import[,"GEO.asscession.number"])
clindata=clin_data_import[clin_data_order,]
data_order=order(colnames(data_import)[4:length(colnames(data_import))])+3 #Order data without first three columns, then add 3 to get correct index in original file
rawdata=data_import[,c(1:3,data_order)] #grab first three columns, and then remaining columns in order determined above
#header=colnames(rawdata)

fdunn=as.data.frame(t(rawdata[,4:length(colnames(rawdata))]))

X=t(fdunn)
ffun=filterfun(pOverA(p = 0.2, A = 100), cv(a = 0.7, b = 10))
filt=genefilter(2^X,ffun)
dunnf=as.data.frame(t(X[filt,]))

vamp=sample(1:nrow(dunnf),146)
rr=c(1:nrow(dunnf))
tet=setdiff(rr,vamp)

target= clindata[,"relapse..1.True."]
target[target==0]="NoRelapse"
target[target==1]="Relapse"
target=as.factor(target)

#dunnf$AGE=dunn$AGE
#dunnf1=dunnf
#dunnf2=dunnf
#target=dunn$L.category.numeric[vamp] # could this be a prob area in main branch
target1=target[vamp]
tetarget=target[tet]
#dunnf2$L=dunn$L
dunnf_train=dunnf[vamp,]
dunnf_test= dunnf[tet,]
target1=as.factor(target1)
tree_1=randomForest(x=dunnf_train, y=target1, mtry=3,ntree = 10001, proximity=TRUE)
pred1=predict(tree_1, dunnf_test, type="response")
tab=table(tetarget,predict=pred1)
overall_accuracy=((tab[1,1]+tab[2,2])/sum(tab))*100
#variable_number=length(colnames(dunnf1))
tab
overall_accuracy
#variable_number
###########################