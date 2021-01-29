rm(list=ls()) # clear the list of objects
graphics.off() # clear the list of graphs
options(digits = 3) # number of digits to display
options(scipen=999)


library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(purrr)
library(cluster)
library("flexclust")

# Clustering:

# Veremos:

# * K MEANS
# * HIERARQUICAL CLUSTERING


### KMEANS

setwd("C:/Users/vsald/Desktop/Auxiliar Machine Learning")

dat <- read.csv("financial.csv",sep=",")


# DEFINIR SEMILLA
set.seed(1)

ind <- sample(nrow(dat), nrow(dat)*0.3)

dat[["train"]] <- TRUE
dat[["train"]][ind] <- FALSE

## ELBOW
tot_withinss <- map_dbl(1:12,  function(k){
  model <- kmeans(x = dat[dat[["train"]]==TRUE, 2:4], centers =k)
  model$tot.withinss
})

elbow_df <- data.frame(
  k = 1:12 ,
  tot_withinss = tot_withinss
)

# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12)+labs(title="Elbow")



### NOS QUEDAMOS CON k=3

# VISUALIZAR VARIABLES
pairs(dat[,2:4])


# PAQUETE KCCA 

cl1 = kcca(dat[dat[["train"]]==TRUE, 2:4], k=3, kccaFamily("kmeans"))
cl1

#  USAR PREDICT DIRECTAMENTE

pred_train <- predict(cl1)
pred_test <- predict(cl1, newdata=dat[dat[["train"]]==FALSE, 2:4])
pred_test

### PLOTEAR

image(cl1)
points(dat[dat[["train"]]==TRUE, 2:4], col=pred_train, pch=19, cex=0.3)
points(dat[dat[["train"]]==FALSE, 2:4], col=pred_test, pch=22, bg="orange")

####### CON PAQUETE BASE (KMEANS)
 

modelkm <- kmeans(dat[dat[["train"]]==TRUE, 2:4],center=3, nstart = 20)
modelkm
ClusterKM<- modelkm$cluster
ClusteringKM <- mutate(dat[dat[["train"]]==TRUE, 2:5],cluster=ClusterKM)

# GRAFICO DE TODAS LAS VARIABLES
pairs(ClusteringKM[,-4],col=factor(ClusteringKM$cluster))

# GRAFICO DE BALANCE VS CREDIT LIMIT
ggplot(ClusteringKM,aes(x=BALANCE,y=CREDIT_LIMIT, color=factor(cluster)))+geom_point()+
  labs(title = "Cluster con K-means",x="BALANCE",y="CREDIT LIMIT",color="Clase")

# GRAFICO DE BALANCE VS PAYMENTS
ggplot(ClusteringKM,aes(x=BALANCE,y=PAYMENTS, color=factor(cluster)))+geom_point()+
  labs(title = "Cluster con K-means",x="BALANCE",y="PAYMENTS",color="Clase")


# TRANSFORMAR A KCCA OBJECT PARA PREDECIR
km1 <- as.kcca(modelkm,data=dat[dat[["train"]]==TRUE, 2:4])
prediccion2 <- predict(km1,newdata=dat[dat[["train"]]==FALSE, 2:4])
prediccion2

Predicha <-  mutate(dat[dat[["train"]]==FALSE, 2:5],cluster=prediccion2)

# GRAFICAR LAS PREDICCIONES

# BALANCE VS CREDIT LIMIT
ggplot(Predicha,aes(x=BALANCE,y=CREDIT_LIMIT, color=factor(cluster)))+geom_point(shape=17)+
  labs(title = "Cluster con K-means",x="BALANCE",y="CREDIT LIMIT",color="Clase")

# BALANCE VS PAYMENTS
ggplot(Predicha,aes(x=BALANCE,y=PAYMENTS, color=factor(cluster)))+geom_point(shape=17)+
  labs(title = "Cluster con K-means",x="BALANCE",y="PAYMENTS",color="Clase")


# BASE PARA VISUALIZAR DONDE ESTAN LOS OTROS DATOS
Graf2 <-  rbind(Predicha,ClusteringKM)
ggplot(Graf2,aes(BALANCE,CREDIT_LIMIT,color=factor(cluster)))+geom_point(aes(shape=factor(train)),size=2)+ scale_shape_manual(
  values = c(0,17))
ggplot(Graf2,aes(BALANCE,PAYMENTS,color=factor(cluster)))+geom_point(aes(shape=factor(train)),size=2)+ scale_shape_manual(
  values = c(0,17))


## VEMOS COMO SE COMPORTAN LOS CLUSTER FORMADOS EN LOS PREDICHOS

Caracterizacion <- Predicha %>% group_by(cluster) %>% 
  summarise(Balance=mean(BALANCE),Pagos= mean(PAYMENTS), credito= mean(CREDIT_LIMIT),
            Cantidad=n())

head(Caracterizacion)
########################
########################
########################

# Cluster jerarquico

########################
########################


### SOLO CON TRAIN

Distancia <- dist(dat[dat[["train"]]==TRUE, 2:4])
Complete <- hclust(Distancia,method = "complete")
Single <- hclust(Distancia,method = "single")
Average <- hclust(Distancia,method = "average")

## DENDOGRAMAS

par(mfrow=c(1,3))
plot(Complete,main="Complete", xlab="", ylab="", cex=.9,labels=FALSE)
plot(Single,main="Single", xlab="", ylab="", cex=.9,labels=FALSE)
plot(Average,main="Average", xlab="", ylab="", cex=.9,labels=FALSE)


######## GRAFICAR EN 2 D

Complete_k3 <- cutree(Complete,k=3)
Complete_k3 <-mutate(dat[dat[["train"]]==TRUE, 1:4], cluster=Complete_k3)


ggplot(Complete_k3,aes(x=BALANCE,y=CREDIT_LIMIT, color=factor(cluster)))+geom_point()+
  labs(title = "Cluster con Hierarquical",x="V1",y="V2",color="Clase")


## TRANSFORMAR A KCCA

modelJerarquico <- as.kcca(Complete,data=dat[dat[["train"]]==TRUE, 2:4],k=3)

modelJerarquico

Complete_k3_Pred <-  mutate(dat[dat[["train"]]==FALSE, 2:5],cluster=predict(modelJerarquico,newdata=dat[dat[["train"]]==FALSE, 2:4]))

Complete_k3 <- mutate(dat[dat[["train"]]==TRUE, 2:5],cluster=predict(modelJerarquico,newdata=dat[dat[["train"]]==TRUE, 2:4]))


ggplot(Complete_k3,aes(x=BALANCE,y=CREDIT_LIMIT, color=factor(cluster)))+geom_point()+
  labs(title = "Cluster con con Hierarquical",x="BALANCE",y="CREDIT LIMIT",color="Clase")

ggplot(Complete_k3_Pred,aes(x=BALANCE,y=CREDIT_LIMIT, color=factor(cluster)))+geom_point(shape=17)+
  labs(title = "Cluster con con Hierarquical",x="BALANCE",y="CREDIT LIMIT",color="Clase")

Graf <-  rbind(Complete_k3,Complete_k3_Pred)
ggplot(Graf,aes(BALANCE,CREDIT_LIMIT,color=factor(cluster)))+geom_point(aes(shape=factor(train)),size=2)+ scale_shape_manual(
  values = c(0,17))



#########



