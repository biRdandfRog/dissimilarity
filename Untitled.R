
bird.tax$plotsin<-plotsin
  
plotsin<-colSums(length(bird.sum[,sps] >0))
plotsin
length(bird.sum[,sps][bird.sum[,sps] > 0])

nef<-(bird.sum[,sps])

plotty<-nef%>%
  summarize_each(funs(sum))
ploti<-melt(plotty,value.name="plots_in",variable.name="species")
ploti$per_plots<-ploti$plots_in/32
View(ploti)
plotin<-as.data.frame(t(plotty))

bird.tax2<-left_join(bird.tax,ploti[,"species","per_plots"],by="species")

View(bird.tax2)

View(plotin)

str(plotin)

plotin$percent_plots<-plotin[,1]/32


sumf<-function(x){
  sum(x>0)
}
View(plotty)


###functional diversity 
View(sp.list)
View(bird.sum)

##need to melt bird sum data so there is an ID column
##join by ID to sp.list & remove species not present
##use that as base df for other analyses

sp.melt<-melt(bird.sum[,-3],value.name="abundance",variable.name="ID",id.vars=c("DIVERSITY","PLOT"))
View(sp.melt)     
sp.melt.pres<-filter(sp.melt,abundance >= 1)
View(sp.melt.pres)
sp.traits<-left_join(sp.melt.pres,sp.list[,c("ID","Order","Family","fg_food","fg_sub","fg_tech","major","n_habitat","avg.mass","migrant")],by="ID")
View(sp.traits)
##should there be a line for each bird? ie melt abundance down? 

##species by trait matrix
library(tidyr)
View(sp.melt)

birdies<-sp.melt%>%
  group_by(ID)%>%
  summarize(abun = sum(abundance))
View(birdies)

trait.mat<-left_join(birdies,sp.list[,c("ID","fg_food","fg_sub","fg_tech","major","n_habitat","avg.mass","migrant")],by="ID")%>%
  select(-abun)
View(trait.mat)  ##sp x trait matrix

library(FD)
trait.mat$n_habitat<-as.integer(trait.mat$n_habitat)
trait.mat$avg.mass<-as.numeric(trait.mat$avg.mass)

library(cluster)
trait.matrix<-as.data.frame(trait.mat[1:55,c("fg_food","fg_sub","fg_tech","major","n_habitat","avg.mass","migrant")],row.names=trait.mat[1:55,1])
b.mat<-as.matrix(bird.sum.mat)
trait.dist.v<-daisy(trait.matrix,metric="gower") 
trait.fdisp<-fdisp(trait.dist.v,b.mat)

trait.dbFD<-dbFD(trait.dist.v,b.mat,corr='cailliez',calc.FGR=TRUE)
h2<-hclust(trait.dist.v,method="ward.D")
library('ggdendro')
ggdendrogram(h2,rotate=TRUE)

##sqrt trans not enough- caillez works, lingoes

View(trait.dbFD)
trait.dbFD
str(trait.dbFD)
fd.df<-bird.sum[,c("DIVERSITY","PLOT","total_abun","richness")]
fd.df$Frich<-trait.dbFD$FRic
fd.df$Feve<-trait.dbFD$FEve
fd.df$Fdiv<-trait.dbFD$FDiv
fd.df$Fdis<-trait.dbFD$FDis
fd.df$FGR<-trait.dbFD$FGR

View(fd.df)
write.csv(fd.df,file="~/FDDR.csv")
##does FD ary with plot Diversity?
frich.aov<-aov(Frich~DIVERSITY, data=fd.df)
summary(fd.aov)

feve.aov<-aov(Feve~DIVERSITY, data=fd.df)
summary(feve.aov)##also this?

fd.div<-aov(Fdiv~DIVERSITY, data=fd.df)
summary(fd.aov)

fdis.aov<-aov(Fdis~DIVERSITY, data=fd.df)
summary(fdis.aov)###YESSSSSS

fgr.aov<-aov(FGR~DIVERSITY, data=fd.df)
summary(fgr.aov) ##YESS ALSO VERY IMPORTANT p=0.0005

##functional group richness and functional dispersion differ between mono and poly plots
##fdis is the average distance to the abudance weighted cnetroid
##unaffected by sr
#similar to Raos q
##used as a beta diversity measure- multivariate dispersion (Laliberte and LEgendre 2010)
#abundance weighted but not drivn
##mean distance of individual species to the centroid of all species in the community
#unaffected by richness
##correlted with FDiv and RaoQ


##summarize, plot
fd.summ<-fd.df%>%
  group_by(DIVERSITY)%>%
  summarize_each(funs(mean,length,se))
source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme
library(dplyr)
fdis.plot<-ggplot(fd.summ,aes(x=DIVERSITY,y=Fdis_mean))+geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=Fdis_mean-Fdis_se,ymax=Fdis_mean+Fdis_se))+theme_mooney()
fdis.plot
##higher dispersion in polyculture plots
fgr.plot<-ggplot(fd.summ,aes(x=DIVERSITY,y=FGR_mean))+geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=FGR_mean-Fdis_se,ymax=FGR_mean+Fdis_se))+theme_mooney()
fgr.plot
##higher richness in poly
View(pred)

####################
##filter sp.list to species that were observed in July
j.sp<-colnames(comm)
str(j.sp)
length(j.sp)##length 50, not 55?
##make normal as names
j.list<-unlist(strsplit(j.sp,split='_'))
str(j.list)
View(j.list)
j.list
write.csv(j.list,"~/j.list.csv")
jj<-data.frame(sp=j.sp,
               genus=j.list)

