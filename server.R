library(DT)
library(devtools)
library(shiny)
library(ggplot2)
library(dplyr)
library(d3heatmap)
library(vegan)
library(reshape2)


source_url("https://raw.githubusercontent.com/collnell/R_vis/master/theme_mooney.R")#ggplot theme
doon<-as.data.frame(dune)
doon.df<-round(doon,2)
dune.dist<-vegdist(doon)
dune.df<-round(as.matrix(dune.dist),2)

shinyServer(function(input,output){

  values<-reactiveValues(dist.df=dune.df,
                         sxsp=data.frame(dune))

  observeEvent(input$runit,{
    if(is.null(input$getcsv)) {
      sxsp<-as.data.frame(dune)
      values$sxsp<-sxsp
    } else{
      inFile<-input$getcsv
      temp.df<-read.csv(inFile$datapath)
      temp2<-temp.df[sapply(temp.df,is.numeric)]
      temp2[is.na(temp2)]<-0
      values$sxsp<-temp2
    }
  })
  
  observeEvent(input$runit2,{
    if(is.null(input$getcsv)) {
      sxsp<-as.data.frame(dune)
      values$sxsp<-sxsp
    } else{
      inFile<-input$getcsv
      temp.df<-read.csv(inFile$datapath)
      temp2<-temp.df[sapply(temp.df,is.numeric)]
      temp2[is.na(temp2)]<-0
      values$sxsp<-temp2
    }
  })
  observeEvent(input$runit2,{
    if(input$df.tr=="None"){
      values$t.df<-values$sxsp
    }else if (input$df.tr == "square root"){
      df.sqrt<-sqrt(values$sxsp)
      values$t.df<-df.sqrt
    
    }else if (input$df.tr == "proportions"){
      values$t.df<-values$sxsp
    }else{
      values$t.df<-values$sxsp
  }
    
  })

  
  observeEvent(input$runit2,{
    if (input$distin == "bray"){##why is this outputting only
      distmat<-vegdist(values$t.df,method="bray")
      dist.m<-as.matrix(distmat)
      dist.round<-round(dist.m,2)
      values$dist.df<-dist.round
    } else if (input$distin == "jacc"){
      distmat<-vegdist(values$t.df,method="jaccard")
      dist.m<-as.matrix(distmat)
      dist.round<-round(dist.m,2)
      values$dist.df<-dist.round
    }else {
      
    }
    
  })
  
  
  output$heat<-renderD3heatmap({

    he.ma<-d3heatmap(values$dist.df,scale="column",colors="RdYlBu",breaks=7,Rowv=FALSE,Colv=FALSE,dendrogram='none')
    he.ma
  })
  
  output$comm.viz<-renderD3heatmap({
    comm.heat<-d3heatmap(values$sxsp,scale="column",colors="OrRd",breaks=5,Rowv=FALSE,Colv=FALSE,dendrogram='none')
  })
  
  output$dist.matrix<-renderDataTable(
    values$dist.df,extensions='FixedColumns',
    options=list(pageLength=nrow(dune.df),dom='t',scrollX=TRUE,scrollY='300px',fixedColumns=list(leftColumns=1)),
    class="compact"
    
  )
  
  output$comm.matrix<-renderDataTable(
    values$sxsp,extensions='FixedColumns',
    options=list(pageLength=nrow(values$sxsp),dom='t',scrollX=TRUE,scrollY='300px',fixedColumns=list(leftColumns=1)),
    class="compact"
    
  )
  
  short.df<-doon
  Site<-rep(1:5, each=4)
  Plot<-rep(1:4,5)
  shorty.df<-cbind(Site,Plot,short.df)
  
  long<-shorty.df
  long.df<-melt(long, id.vars=c("Site","Plot"),variable.name="Species",na.rm=FALSE,value.name="Abundance")

  output$short<-renderDataTable(
    shorty.df,extensions='FixedColumns',
    options=list(pageLength=nrow(shorty.df),dom='t',scrollX=TRUE,scrollY='300px',fixedColumns=list(leftColumns=1)),
    class="compact"
  )
  output$long<-renderDataTable(
    long.df,extensions='FixedColumns',
    options=list(pageLength=nrow(long.df),dom='t',scrollX=TRUE,scrollY='300px',fixedColumns=list(leftColumns=1)),
    class="compact"
  )
  output$jaccard<-renderUI({
    withMathJax(
      helpText("$$J(A,B) =\\frac{ | A\\bigcup B |-|A\\bigcap B}{ | A\\bigcup B  |}$$")
  )
  })
  
  output$bray<-renderUI({
    withMathJax(
      helpText("$$BC_{ij} = 1-\\frac{2C_{ij}}{S_{i}+S_{j}}$$")
    )
  })


})