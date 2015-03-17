# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm
# Review the slides on simulations for this assignment.
# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.
# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.
# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).
library("ggplot2")
sim.doctors <- function(initial.doctors, n.doctors, n.days, p){
	if (p>1 | p<0){
		stop("Invalid p value.")
	}
	if (length(initial.doctors)!=n.doctors){
		stop("Length of initial doctors not equal to the number of doctors.")
	}
	has.adopted=matrix(data=initial.doctors,nrow=n.doctors,ncol=n.days)
	for (i in seq(1,n.days)){
		doctors=sample(seq(1,length(initial.doctors)),2)
		if (initial.doctors[doctors[1]]!=initial.doctors[doctors[2]]){
			if (initial.doctors[doctors[1]]==0){
				initial.doctors[doctors[1]]=sample(c(0,1),prob=c(1-p,p),1)
				has.adopted[c(doctors[1]),seq(i,n.days)]=1
			} else {
				initial.doctors[doctors[2]]=sample(c(0,1),prob=c(1-p,p),1)
				has.adopted[c(doctors[2]),seq(i,n.days)]=1
			}
		}
	}
	return(has.adopted)
}
set.seed(42)
initial.doctors=sample(c(0,1),prob=c(0.9,0.1),size=100,rep=TRUE)
simulate=function(days=seq(10,1000,10),initial_doctors=initial.doctors,p){
	n.doctors=c()
	for (i in days){
		converted=0 ; has.adopted=sim.doctors(initial_doctors,length(initial_doctors),i,p)
		for (j in seq(1,nrow(has.adopted))){
			if (1 %in% (has.adopted[j,])){
				converted=converted+1
			}
		}
		n.doctors[i/(days[2]-days[1])]=converted
	}
	return(list(days,n.doctors))
}
data1=simulate(p=0.1)
data2=simulate(p=0.2)
data3=simulate(p=0.3)
data4=simulate(p=0.4)
data5=simulate(p=0.5)
data6=simulate(p=0.6)
data7=simulate(p=0.7)
data8=simulate(p=0.8)
data9=simulate(p=0.9)
data10=simulate(p=1)

gplot=ggplot()
gplot=gplot+geom_line(size=1,aes(x=data1[[1]], y=data1[[2]]),color="Red")
gplot=gplot+geom_line(size=1,aes(x=data2[[1]], y=data2[[2]]),color="Orange")
gplot=gplot+geom_line(size=1,aes(x=data3[[1]], y=data3[[2]]),color="Yellow")
gplot=gplot+geom_line(size=1,aes(x=data4[[1]], y=data4[[2]]),color="Purple")
gplot=gplot+geom_line(size=1,aes(x=data5[[1]], y=data5[[2]]),color="Blue")
gplot=gplot+geom_line(size=2,aes(x=data6[[1]], y=data6[[2]]),color="Red")
gplot=gplot+geom_line(size=2,aes(x=data7[[1]], y=data7[[2]]),color="Orange")
gplot=gplot+geom_line(size=2,aes(x=data8[[1]], y=data8[[2]]),color="Yellow")
gplot=gplot+geom_line(size=2,aes(x=data9[[1]], y=data9[[2]]),color="Purple")
gplot=gplot+geom_line(size=2,aes(x=data10[[1]], y=data10[[2]]),color="Blue")
gplot=gplot+xlab("Days")+ylab("Converted Doctors")+ggtitle("Converted doctors vs Days")
gplot
