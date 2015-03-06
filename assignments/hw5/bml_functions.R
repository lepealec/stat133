#################################################################################
#### Functions for BML Simulation Study
#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)
#################################################################################
#### Functions for BML Simulation Study
#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)
bml.init <- function(r, c, p){
  if (p>1 | p<0){
    stop("p is not valid") ; }
  if (round(p*r*c)==0){
    stop("Invalid number of possible cars.") ; }
  if (round(p*r*c)==r*c){
    stop ("No blank spaces.")
  }
  m=matrix(sample(c(0,1,2), size = r*c, prob = c(1-p,p/2,p/2), replace = TRUE), nrow = r)
  blue_cars=length(m[m==2]) ; total=round(p*r*c)/2 ; red_cars=length(m[m==1])
  while (blue_cars>total){
    row=sample(seq(1,r),1) ; col=sample(seq(1,c),1)
    if (m[row,col]==2){
      m[row,col]=0 ; blue_cars=blue_cars-1
    } ; }
  while (blue_cars<total){
    row=sample(seq(1,r),1) ; col=sample(seq(1,c),1)
    if (m[row,col]==0){
      m[row,col]=2 ; blue_cars=blue_cars+1        
    } ; }
  while (red_cars>total){
    row=sample(seq(1,r),1) ; col=sample(seq(1,c),1)
    if (m[row,col]==1){
      m[row,col]=0 ; red_cars=red_cars-1
    } ; }
  while (red_cars<total){
    row=sample(seq(1,r),1) ; col=sample(seq(1,c),1)
    if (m[row,col]==0){
      m[row,col]=1 ; red_cars=red_cars+1
    } ; }
  return (m) ; }
#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.
## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.
bml.step <- function(m){
  old=m ; nrow=nrow(m) ; ncol=ncol(m) ; c=1 ; r=1
  while (r<=nrow){
    loop=FALSE
    if (m[r,1]==0 & m[r,ncol]==1) { # right red 1 first
      loop=TRUE ; } ; c=1
    while (c<=ncol-1){
    if (m[r,c+1]==0 & m[r,c]==1){
        m[r,c+1]=1 ; m[r,c]=0 ; c=c+1 ; } ; c=c+1 }
    if (loop==TRUE){
      m[r,1]=1 ; m[r,ncol]=0
    } ; r=r+1 }
      c=1 ; r=1
  while (c<=ncol){
    loop=FALSE
    if (m[1,c]==2 & m[nrow,c]==0){ #up blue 2 second
      loop=TRUE ; } ; r=nrow
    while (r>=2) {
      if (m[r-1,c]==0 & m[r,c]==2) { 
        m[r-1,c]=2 ; m[r,c]=0 ; r=r-1 ; } ; r=r-1}
      if (loop==TRUE) {
        m[nrow,c]=2 ; m[1,c]=0
        } ; c=c+1 ; }
        grid.new=identical(old,m)
      return (list(m,grid.new)) ; }
#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)
bml.sim <- function(r, c, p){
  m=bml.step(bml.init(r,c,p))[[1]] ; counter = 0
  identical=FALSE ; rep=list(); rep2=list() ; counter2=1 ; counter3=1
  while (identical==FALSE){
    counter=counter+1 ; new_grid=bml.step(m)
    m=new_grid[[1]] ; identical=new_grid[[2]]
    image(t(m)[,ncol(m):1],col=c("Black","Red","Blue"))
    if (counter2<=r){
      rep[[counter2]]=c(m)
    } else {
      rep2[[counter2-r]]=c(m)
    }
    if (counter2==2*r){
       if (identical(rep2,rep)) {
        return(list("Looping",counter))
       } else {
      counter2=0
    } ; }
    counter2=counter2+1 ; }
  return(list("Gridlock",counter)) ; }