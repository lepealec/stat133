
present='yes'

## Stat 133 Midterm 2
## Thursday April 2nd

## General R commands

# [1 pt]
# Create [x], a numeric vector of length 2000 with 
# entries: 6, 12, 18, etc.
x=seq(6,,6,length=2000)
# [1 pt]
# Create [y], a logical vector of length 2000 
# with y[i]=T if x[i] is divisible by 10, otherwise F
y=c()
for (i in seq(1,2000)){
  if (i%%10==0){
      y[i]=T
  }
  else {
    y[i]=F
  }
}

# [1 pt]
# Create [w], a random permutation of the numeric values of a deck of cards
# (i.e. just the numbers 1 through 13 each repeated 4 times)
set.seed(2718)
w <- sample(c(seq(1,13),seq(1,13),seq(1,13),seq(1,13)))
# [1 pt]
# Create [m], a matrix of size 10x10 with entries that are 
# Exponential random variables (hint: rexp) with rate 3
# (arrange the values by column, as per default)
set.seed(344)
m <- matrix(rexp(rate=3,n=100),nrow=10,ncol=10)
# [1 pt]
# Create [l], a list with 12 elements, each a vector of length 100.
# Each vector of length 100 of Poisson (hint:rpois) random variables with mean 5
set.seed(71)
l=list()
for (i in seq(1,12)){
  l[[i]]=rpois(lambda=5,n=100)
}


# for the next two tasks you will use the data frame infants (size 1236x15)
# LEAVE AS IS:
load("KaiserBabies.rda") 
# [2 pt]
# Create a table [t] of the education level ($ed) of all married ($marital) first time ($parity=1) mothers:
t <- table(infants$ed[infants$parity==1)


# [2 pt]
# Calculate [mw], the average birthweight ($bwt) of all babies whose were full term, i.e. gestation equal or more than 259 days.
mw <- mean(infants$bwt[infants$gestation>=259])


# For the next few tasks you will use the data frame family (size 14x5)
# LEAVE AS IS:
load("family.rda")

# [1 pt]
# Create [f1] a subset of family with only women over age 50
f <- family[family$gender=='f' & family$age>50,]

# [1 pt]
# Create [f2] a subset of family with only men 6 foot tall or more
f2 =family[family$gender=='m' & family$height>=6*12,]

# [1 pt]
# Create [f3] a subset of family of people whose name starts with T
f3 <- family[family$name=='Tim' | family$name=='Tom',]



# [1 pt]
# Create [f4] a subset of family with just the youngest individual (so just one row)
f4 <- family[family$age==min(family$age),]
## Plotting

# We will now use the dataset "iris" which is icluded in the R package.
# To look at the dataframe you can just type "iris" at the prompt
# It is a data frame of size 150x5 with measurements of 4 attributes
# for 150 flowers, 50 each of 3 different species of irises.
# [2 pts]
# Make a box plot of Sepal Length by Species (so 3 boxplots in one plot)
boxplot(iris$Sepal.Length[iris$Species==levels(iris$Species)[1]],iris$Sepal.Length[iris$Species==levels(iris$Species)[2]],iris$Sepal.Length[iris$Species==levels(iris$Species)[3]])

# [3 pts]
# Make a scatterplot of petal width (y-axis) versus petal length (x-axis)
# The axes labels should be "Petal Length" and "Petal Width",
# Color the plotting symbol by Species (any 3 colors)
plot(
  x=c(iris$Petal.Length[iris$Species==levels(iris$Species)[1]],iris$Petal.Length[iris$Species==levels(iris$Species)[2]],iris$Petal.Length[iris$Species==levels(iris$Species)[3]]),
  y=c(iris$Petal.Width[iris$Species==levels(iris$Species)[1]],iris$Petal.Width[iris$Species==levels(iris$Species)[2]],iris$Petal.Width[iris$Species==levels(iris$Species)[3]]),
  col=c('blue','red','green'),
  xlab='Petal Length',
  ylab='Petal Width',
  main='Petal Length vs Petal Width'
)


# [3 pt]
# Make a scatterplot of ( sepal length / petal length) as a function of index (order)
# Color the plotting symbol by Species (any 3 colors)
plot(
  x=c(seq(1,50),seq(1,50),seq(1,50)),
  y=c(iris$Sepal.Length[iris$Species==levels(iris$Species)[1]]/iris$Petal.Length[iris$Species==levels(iris$Species)[1]],
      iris$Sepal.Length[iris$Species==levels(iris$Species)[2]]/iris$Petal.Length[iris$Species==levels(iris$Species)[2]],
      iris$Sepal.Length[iris$Species==levels(iris$Species)[3]]/iris$Petal.Length[iris$Species==levels(iris$Species)[3]]),
  col=c('blue','red','green'),
  ylab='Sepal Length/Petal Length',
  xlab='Index',
  main='Sepal Length/Petal Length vs Index'
  )

##  apply statements

# For the next few tasks you will use the list Cache500 
# (list of length 500, each element is a numeric vector of various lengths)
# LEAVE AS IS:
load("Cache500.rda")

# [3 pts]
# Create [first.cache], a vector where each entry is the _first_ element of the
# corresponding vector in the list Cache500
first.cache = c()
for (i in seq(1,length(Cache500))){
  first.cache[i]=Cache500[[i]][1]
}

# [3 pts]
# Create [mean.cache], a vector of length 500 where each entry is the mean 
# of the corresponding element of the list Cache500
mean.cache <- c()
for (i in seq(1,length(Cache500))){
  mean.cache[i]=mean(Cache500[[i]])
}



# [2 pts]
# Create [sd.cache], a vector of length 500 where each entry is the sd
# of the corresponding element of the list Cache500
sd.cache <- c()
for (i in seq(1,length(Cache500))){
  sd.cache[i]=sd(Cache500[[i]])
}

# [4 pts]
# Create [mean.long.cache], a vector where 
# mean.long.cache[i] is:
# the mean of Cache500[[i]] IF it has 50 or more entries.
# NA IF Cache500[[i]] has less than 50 entries.
mean.long.cache <- c()
for (i in seq(1,length(Cache500))){
  if (length(Cache500[[i]])>=50){
  mean.long.cache[i]=mean(Cache500[[i]])
  }
  else{
    mean.long.cache[i]=NA
  }
}