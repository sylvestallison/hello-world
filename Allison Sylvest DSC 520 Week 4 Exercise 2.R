# Assignment: ASSIGNMENT 4 Exercise 2
# Name: Sylvest, Allison
# Date: 2021-04-11

library(readxl)
week_6_housing <- read_excel("C:/Users/sylve/Downloads/week-6-housing.xlsx")
View(week_6_housing)
str(week_6_housing)
nrow(week_6_housing)

# Use the apply function on a variable in your data set
# looking at sale price
# apply function must be used on a matrix

dm <- data.matrix(week_6_housing)
apply(dm[,c(2), drop = F], 2, sum, na.rm = TRUE)

# Use the aggregate function on a variable in your data set

aggregate(Sale.Price ~ square_feet_total_living, week_6_housing, mean)

# Use the plyr function on a variable in your dataset
d <- data.frame(year = rep(2000:2002, each = 3),count = round(runif(9, 0, 20)))
print(d)


library(plyr)
ddply(d, "year", function(x) {
  mean.count <- mean(x$count)
  sd.count <- sd(x$count)
  cv <- sd.count/mean.count
  data.frame(cv.count = cv)
})
ddply(d, "year", summarise, mean.count = mean(count))
ddply(d, "year", transform, total.count = sum(count))
ddply(d, "year", mutate, mu = mean(count), sigma = sd(count),cv = sigma/mu)


week_6_housing.dat <- subset(week_6_housing, 'sale year' > 2000)
x <- ddply(week_6_housing, c("'sale year'", "ctyname"), summarize, homeruns = 
             sum(("Sale.Price")))
           head(x)
           

# Check distributions of the data

library(fitdistrplus)
normal_dist <- fitdist(week_6_housing$`Sale.Price`, "norm")
plot(normal_dist)

# Identify if there are any outliers
summary(week_6_housing$`Sale.Price`)
hist(week_6_housing$`Sale.Price`,xlab = "Price",main = "Histogram of Price",breaks = sqrt(nrow(dat))) 

# Create at least 2 new variables
country<-rep("US",12865)
serial_no<-c(1:12865)

new_df<-data.frame(serial_no,week_6_housing,country)
head(new_df)
tinytex::install_tinytex()
