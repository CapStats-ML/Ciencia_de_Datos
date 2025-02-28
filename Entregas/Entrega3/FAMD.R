library(readr)
library(dplyr)
library("FactoMineR")
library(ggplot2)
library(ggrepel)

#install.packages("fastDummies")
library(fastDummies)


# Increase max.overlaps to a higher value (e.g., 20 or more)
options(ggrepel.max.overlaps = 40)

setwd("~/REPOS GIT/Ciencia_de_Datos/Datos")
data <- read_csv("data2.csv")
data <- data[data$A1Cresult != "No",]

# Muestrear cada 10 filas de los datos
data <- data[seq(1, nrow(data), 4),]

#Ahora un prueba con otro muestreo probabilisisco


Num <- c("time_in_hospital", "num_lab_procedures", "num_medications",
         "num_procedures", "number_diagnoses")
Cat <- c("Edad", "Insulin", "A1Cresult")

corrplot::corrplot(cor(data[,Num]))

data <- data[,c(Num, Cat)] 
data[, Cat] <- lapply(data[, Cat], factor)
Na <- sapply(data, function(x) sum(is.na(x))); Na

summary(data)

FA <- FAMD(data, graph = FALSE, ncp = 10)
summary(FA)

plot(FA, choix = "var", axes = c(1,3), lab.ind = FALSE, graph.type = "ggplot")
plot(FA, choix = "ind", axes = c(1,2), lab.ind = FALSE, graph.type = "ggplot")
plot(FA, choix = "quanti", axes = c(1,2), lab.ind = FALSE, graph.type = "ggplot")
plot(FA, choix = "quali", axes = c(1,2), lab.ind = FALSE, graph.type = "ggplot")


FA$eig


# Probemos volviendo las varibales categoricas como dummies 

data <- read_csv("data2.csv")
data <- data[data$A1Cresult != "No",]
data <- data[seq(1, nrow(data), 4),]

Num <- c("time_in_hospital", "num_lab_procedures", "num_medications",
         "num_procedures", "number_diagnoses")
Cat <- c("Edad", "Insulin", "A1Cresult")

data <- data[,c(Num, Cat)]

data$Edad <- as.factor(data$Edad)
data$Insulin <- as.factor(data$Insulin)
data$A1Cresult <- as.factor(data$A1Cresult)

data <- dummy_cols(data, select_columns = c("Edad", "Insulin", "A1Cresult"))
data <- data[, -c(6, 7, 8)]
summary(data)

FA <- FAMD(data, graph = FALSE, ncp = 10)
summary(FA)









