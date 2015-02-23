#FILENAME: poPyhelper.R
#R version: 3.1.2
#DESCRIPTION: This function creates a population pyramid using pyramid function in plotrix 
# package
#AUTHOR: R. Montanez
#DATE: February 13, 2015

library(plotrix)


#Extract data from file and assign it to variable
#raw.data <- read.csv("popPiramides.csv", header=TRUE)
#if file not found error

population.pyramid <- function (raw.data){
  

# Set data frames - must unlist() for transformation
age.labels <- as.character(unlist(raw.data[1]))
raw.males <- as.numeric(unlist(raw.data[2]))
raw.females <- as.numeric(unlist(raw.data[3]))

#calculate total for each group and the population total
total.pop <- sum(raw.males) + sum(raw.females)

#calculate ratios for each population
ratio.males <- (raw.males/total.pop) * 100
ratio.females <- (raw.females/total.pop) * 100

#if data not extracted error

#set colors for each piramid, based on number of rows of original data
length <- length(age.labels)
male.color <- color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1), length)
female.color <- color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),length)

#plot piramid

windows()
par(mar=pyramid.plot(ratio.males, ratio.females, age.labels, 
                     main = "Piramide de Poblacion", lxcol = male.color, 
                     rxcol = female.color, top.labels=c("Hombres", "Edad", "Mujeres"),
                     gap = 1.25, laxlab=c(0,1,2,3,4,5), raxlab=c(0,1,2,3,4,5),
                     show.values = TRUE))
}

