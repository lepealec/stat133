# Please load in the dataset included in the midterm1 directory (SFHousing-2.rda). 
# It will be required to perform the following tasks. The dataset includes data for houses
# in the city of Berkeley.
# calculate the mean lot size (lsqft) of houses in Berkeley. Store it as the
# variables <mean.lsqft>.
housing=housing[is.na(housing$lsqft)==FALSE,]
housing=housing[is.na(housing$bsqft)==FALSE,]
mean.lsqft <- mean(housing$lsqft)
# How many unique area codes are there in the dataset?  Store them in the variable
# <n.zipcode>
n.zipcode <- length(levels(housing$zip))
# For each house in the dataset, calculate how large the house is relative to
# the lot size, i.e. square foot of house over square foot of lot.
# Store it in the variable <rel.sqft>.
#square foot of house / square foot of lot
#bsqft house size / lqsft
rel.sqft <- housing$bsqft/housing$lsqft
# Please create two new data frames with the following two subsets
# and store them with the indicated names:
# 1) houses whose bsqft is strictly greater than <mean.bsqft>:  <bsft.greater>
# 2) houses who bsqft is less than or equal to  <mean.bsqft>: <bsqft.less>
mean.bsqft=mean(housing$bsqft)
bsqft.greater <- housing$bsqft[housing$bsqft>mean.bsqft]
bsqft.less <- housing$bsqft[housing$bsqft<=mean.bsqft]
# For each of your subsets, create a vector giving the price of each house. Name
# these variables <rooms.greater.price> and <rooms.less.price>.
rooms.greater.price <- housing$price[housing$bsqft[housing$bsqft>mean.bsqft]]
rooms.less.price <- housing$price[housing$bsqft[housing$bsqft<=mean.bsqft]]
# Please implement the function priceByRooms. Your function should take the
# following arguments:
#
# <room.range>: a numeric vector of length 2 whose first and second observations
#   give the minimum and maximum number of rooms to consider
# <br>: a numeric vector giving the number of bedrooms for each observation
# <prices>: a numeric vector giving the price of each observation associated
#   with <br>
#
# Your function should return the average of <prices> for all observations with
# <br> in the range (inclusive) specified by <room.range>
priceByRooms <- function(room.range, br, prices) {
  max.rooms=room.range[2] ; min.rooms=room.range[1]
  return (mean(prices[br<=min.rooms & br<=max.rooms]))
}
# Please create a plot of house price (y-axis) against br (x-axis). Your plot
# should include the following features:
# 1) a title "Housing price vs Number of Rooms"
# 2) axis labels: "Price" and "#rooms"
# 3) plotting character set to 19
plot(housing$price,housing$br,main="Housing price vs Number of Rooms",xlab="Price",ylab="#rooms",pch=19)