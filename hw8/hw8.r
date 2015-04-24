xUnique = 1:5
trueCoeff = c(0, 1, 1)
getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}
###
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  return(tapply(y,x,sample,replace=T))
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  return(fit+sample(err,size=length(fit)))
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  if (degree==1){
    return(as.vector(coefficients(lm(y~x))))
  }
  if (degree==2){
    return(as.vector(coefficients(lm(y~x+I(x^2)))))
  }
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  ### Use fitModel to fit a model to this bootstrap Yf
  if (is.null(fit)){
    yval=as.vector(unlist(genBootY(data[,1],data[,2])))
  } else {
    yval=genBootR(fit[,1],fit[,2])
  }
  return(fitModel(data[,1], yval, degree))
}

repBoot = function(data, B = 1000){
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic replicate a call to oneBoot B times
  ### format the return value so that you have a list of length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the fit is for a line or a quadratic Return this list
  ### Replicate a call to oneBoot B times for each of the four conditions
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the fit is for a line or a quadratic
  ### Return this list
  #oneboot(data,fit,degree)
  x=data[,1] ; y=data[,2] ; output=list()
  input1=matrix(0,ncol=2,nrow=B) ; input2=matrix(0,ncol=2,nrow=B) ; input3=matrix(0,ncol=3,nrow=B) ; input4=matrix(0,ncol=3,nrow=B)
  for (i in seq(1,B)){
    lin_e=fitModel(x,y)
    lin=as.matrix(lin_e[1]+lin_e[2]*x)
    quad_e=fitModel(x,y,2)
    quad=as.matrix(quad_e[1]+quad_e[2]*x+quad_e[3]*x*x)
    input1[i,]=oneBoot(data,cbind(y,y-lin))
    input2[i,]=oneBoot(data)
    input3[i,]=oneBoot(data,cbind(quad,y-quad),degree=2)
    input4[i,]=oneBoot(data,degree=2)
  }
  output[[4]]=input4 ; output[[3]]=input3 ; output[[2]]=input2 ; output[[1]]=input1
  return(output)
}

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data ; coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients that generated the data 
  ### Make a scatter plot of data
  ### Add lines or curves for each row in coeff use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  plot(x,y,col="black")
  if (ncol(coeff)==2){
    for (i in seq(1,nrow(coeff))){
      abline(coef=coeff[i,])
    }
    q=seq(min(x),max(x),0.001)
    lines(q,trueCoeff[1]+trueCoeff[2]*q+trueCoeff[3]*q*q,col="Red",cex=2)
  }
  if (ncol(coeff)==3){
    for (i in seq(1,nrow(coeff))){
      q=seq(min(x),max(x),0.001)
      lines(q,coeff[i,][1]+coeff[i,][2]*q+coeff[i,][3]*q*q,col="Black")
      }
    lines(q,trueCoeff[1]+trueCoeff[2]*q+trueCoeff[3]*q*q,col="Red",cex=2)
  }
}

### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}