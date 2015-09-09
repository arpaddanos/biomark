a=sample(0:3,100,replace=TRUE)
b=sample(0:3,100,replace=TRUE)
tab=table(a,predict=b)
overall_accuracy=((tab[1,1]+tab[2,2]+tab[3,3])/sum(tab))*100
tab
overall_accuracy