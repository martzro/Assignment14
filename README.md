# Assignment14
R Code and data for assignment 14
---
title: "Group Project - TICO"
author: "Roman Martinez"
date: "10/3/2021"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



```{r}
setwd("~/Desktop/FALL 2021/HooverAssignments/GroupAssignment")
library(readr)
FLinfo <- read_csv("FLinfo.csv")
myData <- FLinfo
```

Business Understanding and Data Dictionary

Text about business understanding
library(readr)
FLinfo <- read_csv("FLinfo.csv")                                                                                                                                         
```{r}

dataDictionary <- data.frame(sapply(myData, class))
colnames(dataDictionary)[1] <- "Type"
  
  
```

Data Preparation

```{r}
#Explore dataset and understand each variable

mydata.summary <- data.frame( TYPE = sapply(myData, typeof), LENGTH = sapply(myData, length))

#Reduce variables by finding ones with very high correlation

myData.death <- myData[, c(1,2,9:12,14:19)]

#Add Variable for Population Density

myData.death$PopDensity <- (myData.death$Population / myData.death$SqMiles)

#See summary of new variable

summary(myData.death$PopDensity)

#Add variable to measure how high the pop density is given our summary statistics

df <- ifelse(myData.death$PopDensity > 1.5*mean(myData.death$PopDensity), 1,0)

myData.death$PopDensityHigh <- df

#Drop variables for Population and SqMiles since we have PopDensity now.

myData.death <- myData.death[,-c(9,12)]

#See which variables are most similar using Euclidian distance
#Need to remove non numeric Variables and variables that are less relevant for clustering

myData.death.norm <- myData.death[, -c(1,2,5,12)]

#Normalize
myData.death.norm <- sapply(myData.death.norm, scale)

#Remove NA values
records.missing <- rowSums(is.na(myData.death.norm))>0 
myData.death.norm <- myData.death.norm[!records.missing, ]

#Get Euclidian Distance
d.norm <- dist(myData.death.norm, method = "euclidian")

#Complete linkage clustering
d.hc <- hclust(d.norm, method = "complete")
plot(d.hc, hang = -1, ann = FALSE)

#Cut 5 clusters
d.cut <- cutree(d.hc, k = 5) # cut 5 clusters
centers <- aggregate( . ~ d.cut, data = as.data.frame(myData.death.norm), FUN = mean)
centers

km <- kmeans(myData.death.norm, 5)
km$centers

km.deaths.clusters <- data.frame(myData.death.norm, kmcluster=factor(km$cluster))
km.deaths.clusters[1:3,]

myData.death$Cluster <- km.deaths.clusters$kmcluster
```

Modeling and Model Assesment


```{r}




```

Evaluation

```{r}


```

Deployment

```
