#FILENAME: poPyhelper.R
#R version: 3.1.2
#DESCRIPTION: This function creates a population pyramid using pyramid function in plotrix 
#package. The function takes two arguments: raw.data is the data stream from the file with the 
#numeric values, and headers a vector which hold the titles for the graphic (header vector is defines as
#header[1] = main title of the graph, header[2] = left side title, header[3] = center title, header[4] = 
# right hamd title. The defaults value of headers set the title to a predefined values set at the begining of
# the function)
#AUTHOR: R. Montanez
#DATE: March 5, 2015
#USAGE: To call this function use 'population.pyramid(raw.data)' or population.pyramid(raw.data, header)
#USO:  Para invocar esta funcion utilizar 'population.pyramid(raw.data)' o 'population.pyramid(raw.data, header)'S


library(plotrix)


#Extract data from file and assign it to variable
#raw.data <- read.csv("popPiramides.csv", header=TRUE)
#if file not found error

population.pyramid <- function (raw.data, headers=vector(length = 4)){

#if graph headers values are not set, set to 'default'
  if (headers[1] == FALSE) {headers[1] <- "Piramide de Poblacion"}
  if (headers[2] == FALSE) {headers[2] <- "Hombres"}
  if (headers[3] == FALSE) {headers[3] <- "Edad"}
  if (headers[4] == FALSE) {headers[4] <- "Mujeres"}
  
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
                     main = headers[1], lxcol = male.color,
                     rxcol = female.color, top.labels= headers[2:4],
                     gap = 1.5, laxlab=c(0,1,2,3,4,5), raxlab=c(0,1,2,3,4,5),
                     show.values = TRUE))
}


