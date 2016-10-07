##Community Data Structures in R
#By Colleen Nell (www.collnell.com)

##This code covers:
#1. Converting a data frame between long and short forms
#2. Transposing data
#3. Data transformations, and converting abundance data to proportions
#4. Converting a community matrix to a distance matrix
#5. Running ANOSIM (analysis of similarity)
#6. Plotting values in a heatmap


#load needed libraries
#any packages that have not been used before need to be installed using install.packages("packagename")

library(dplyr) ##calculating proportion data
library(reshape2) #long to short form
library(vegan) #calculate distance matrix, ANOSIM
library(d3heatmap) #plotting heatmaps

##read in community data
df<-read.csv("datafile.csv") #change with file location

##for demonstration purposes I will use the 'dune' dataset in R which is a site x species community matrix of species abundances
df<-data.frame(dune) #do not run if using own data. replace variable names in code with appropriate variables in own df
head(df)#dune data is already in short form- sites x species
##for demonstration purposes, I am adding 'Plot' and "Site' goruping variables to this dataframe
df$Site<-rep(1:5, each=4)
df$Plot<-rep(1:4,5)

##1. convert short data to long data
df.long<-melt(df, id.vars=c("Site","Plot"),variable.name="Species",na.rm=FALSE,value.name="Abundance")
#id.vars are any grouping variables for your dataset
#variable.name names a new variable that stores the column names that are 'melted', default is 'variable'
#value.name sets a name for the variable storing values from 'melted' columns, default is 'value'

View(df.long) # see that each species abservation now has its' own row with the respective grouping variables
##and a column of species names + column of species abundances


##convert long data to short data
#reversing what we jsut did
df.short<-dcast(df.long,Site+Plot~Species) #uses abundance by default or can be set using value.var="Abundance"

#2. transpose data so rows are species and columns are sites
t.short<-t(as.matrix(df.short))

##if needed replace NA's with 0
df.short[is.na(df.short)]<-0

#3. transforming the data frame

#square root
df.sqrt<-sqrt(df.short)

#convert to proportions using 'dplyr'

df.prop<-df.short[,c(-1,-2)]%>% ##create from df, excluding grouping variables
  mutate(total_abun = rowSums(df.short))%>% ##set as grouping variables
  mutate_each(funs(prop.table))%>% ##calculate proportions of species
  cbind(df.short[,c(1,2)])##add back in grouping vars

head(df.prop)
  
#4. Creating a distance matrix in 'vegan'
##bray curtis
dist.bc<-vegdist(df.short[,c(-1,-2)],method="bray")
##jaccard
dist.jacc<-vegdist(df.short[,c(-1,-2)],method="jaccard")
##there are many other useful dissimilarity measures that can be used in this package
#run ?vegdist to see options
##this calculates distance between rows, or sites. To calculate distances between species, transpose the data first


#5. Analysis of similarity
grouping<-as.factor(df.short$Site)##make grouping variable a factor

df.anosim<-anosim(dist.bc, grouping =grouping,permutations=500)
df.anosim #see results
##this informs us whether there is a significant difference between two or more groups using a dissimilarity matrix
##a significant p value indicates that 2+ groups differ in species composition

#6. Visualize distance matrix in heatmap
heat.map.dist<-d3heatmap(dist.bc,scale="column",colors="RdYlBu",breaks=7,Rowv=FALSE,Colv=FALSE,dendrogram='none')
heat.map.dist


