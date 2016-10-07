##Measuring species diversity in R
#By Colleen Nell (www.collnell.com)

##this script covers
#1. Measuring site species diversity using Shannon and Simpson Indices
#2. Calculating species eveness at sites
#3. Calculating overall species diversity and evenness for multiple sites
#4. calculating mean diversity across sites 
##This code can be run using any community data matrix that is organized sites x samples
##for more on this type of data structure see: https://collnell.shinyapps.io/dissimilarity

#load libraries
library(dplyr)#data wrangling
library(vegan)#diversity indices
library(ggplot2)##species composition chart
source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme

##for demonstration thi code uses the 'dune' dataset provided in R
#to use this data to follow along run
data(dune)
df.in<-data.frame(dune)
str(dune)
##save species names in list
sps<-colnames(df.in)
#as well as the dune.ev data that contains the grouping variables for dune
data(dune.env)
df.env<-dune.env
str(df.env) ##we will consider environmental variables 'Management' 
df<-cbind(df.env,df.in)
str(df) ##site x species matrix is merged with env variables


##or enter your own data
df<-read.csv("filename.csv")


##1. Measuring site-level species diversity
df$shannon<-diversity(df.in,index="shannon") #shannon diversity
df$simpson<-diversity(df.in,index="simpson") #simpson diversity

#2. Calculate species evennes
df$richness<-specnumber(df.in) #need species richness
df$evenness<-df$shannon/log(df$richness)

div.df<-(df[,c("shannon","simpson","richness","evenness")]) ##look at new variables

#3. Calcualte overall diversity and evenness by sites/groups
##If your rows represent sites, observations etc of a larger group, you may want to calculate
#the overall diversity of the groups, incorporating data from multiple sites

#measuring diversity and evenness in 'dune' for 'Management' type
mgmt<-as.factor(df.env$Management)#make factor
df.mgmt<-cbind(df.in,mgmt) #attach management to df

mgmt.div<-df.mgmt%>%
  group_by(mgmt)%>%##grouping variable of interest
  summarize_each(funs(sum))##total each column 
head(mgmt.div) #one row for each mgmt gorup
#create matrix that excludes mgmt
mgmt.mat<-mgmt.div[,-1]
#diversity
mgmt.div$shannon<-diversity(mgmt.mat,index="shannon")#exclude mgmt in calculation
mgmt.div$simpson<-diversity(mgmt.mat,index="simpson")
mgmt.div$richness<-specnumber(mgmt.mat)
mgmt.div$evenness<-mgmt.div$shannon/log(mgmt.div$richness)

head(mgmt.div[,c("shannon","simpson","richness","evenness")]) # look at results for overall diversity measures

#4. Calculate mean diversity across sites
#using the same management groups:
str(div.df)
div.df.mgmt<-cbind(div.df,mgmt)##add in grouping variable
str(div.df.mgmt)

se <- function(x) sqrt(var(x)/length(x)) #function that calcualtes standard error (SEM)

#start with df containing shannon simpson values
df.summary<-div.df.mgmt%>%
  group_by(mgmt)%>%
  summarize_each(funs(mean,se,length))
View(df.summary)##this df contains the mean values for the diversity measures for each mgmt group
#as well as standard error and N (length)

##plot shannon diversity for mgmt types
mgmt.plot<-ggplot(df.summary,aes(x=mgmt,y=shannon_mean))+geom_bar(stat="identity")+theme_mooney()+
  geom_errorbar(aes(ymin=shannon_mean-shannon_se,ymax=shannon_mean+shannon_se))
mgmt.plot
