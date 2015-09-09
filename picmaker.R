setwd("foto")
cc=colnames(dunn)
fdunn=dunn
fdunn$L.category.numeric=as.factor(fdunn$L.category.numeric)


for (i in 8:length(cc) ) {
  # foo.squared[i] = foo[i]^2
 # i=8
  ID <- cc[i]
  print(ID)
  #}
  #ggplot(pdd,aes(x= pdd$as.name(cc[1]), fill = Survive)) +
  #ggplot(pdd,aes(x= "ID", fill = Survive)) +
  #ggplot(pdd,aes(x= pdd$CDH3, fill = Survive)) + # This works as needed
pp=ggplot(fdunn,aes(x=fdunn[,i], fill = L.category.numeric)) +  
    geom_histogram(binwidth = 0.08) +
    #facet_wrap(~Title + Pclass) +
    ggtitle(ID)   +
    xlab("expression")       +
    ylab("Total Count") +
    labs(fill = "L cat")
  #}
  
  ggsave(pp,filename=paste(ID,".png",sep="")) 
}
