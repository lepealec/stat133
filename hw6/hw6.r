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
      	change=sample(c(0,1),prob=c(1-p,p),1)
        initial.doctors[doctors[1]]=change
        has.adopted[c(doctors[1]),seq(i,n.days)]=change
      } 
      if (initial.doctors[doctors[2]]==0) {
      	change=sample(c(0,1),prob=c(1-p,p),1)
        initial.doctors[doctors[2]]=change
        has.adopted[c(doctors[2]),seq(i,n.days)]=change
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
    has.adopted=sim.doctors(initial_doctors,length(initial_doctors),i,p)
    for (j in seq(1,nrow(has.adopted))){
        converted=sum(has.adopted[,ncol(has.adopted)])
    }
    n.doctors[i/(days[2]-days[1])]=converted
  }
  return(list(days,n.doctors))
}
data1=simulate(p=0.1)
data2=simulate(p=0.25)
data3=simulate(p=0.5)
data4=simulate(p=0.75)
data5=simulate(p=0.9)

plot(x=data1[[1]],y=data1[[2]],xlab="Days",ylab="Converted Doctors",col="Red",type="l",ylim=c(10,100))
lines(x=data2[[1]],y=data2[[2]],col="Orange")
lines(x=data3[[1]],y=data3[[2]],col="Yellow")
lines(x=data4[[1]],y=data4[[2]],col="Purple")
lines(x=data5[[1]],y=data5[[2]],col="Blue")
legend("topleft",cex=0.65,fill=c("Red","Orange","Yellow","Purple","Blue"),c("0.1","0.25","0.5","0.75","0.9"))


