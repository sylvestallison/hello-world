# Assignment: ASSIGNMENT Survey
# Name: Sylvest, Allison
# Date: 2021-04-04

install.packages ("pastecs")
library (pastecs)

library(ggplot2)
ACS14 <- read_csv("acs-14-1yr-s0201.csv", col_names = TRUE)

##i.
class(ACS14$Id)
class(ACS14$Id2)
class(ACS14$Geography)
class(ACS14$PopGroupID)
class(ACS14$`POPGROUP.display-label`)
class(ACS14$RacesReported)
class(ACS14$HSDegree)
class(ACS14$BachDegree)


##ii.
str(ACS14)
nrow(ACS14)
ncol(ACS14)


##iii.
ACSHistogram <- ggplot(ACS14, aes(HSDegree)) + theme(legend.position = "none") + geom_histogram(aes(y = ..density..), binwidth = .5, colour = "black", fill = "white") + labs(title = "County Populations with HS Degrees", x = "% of Population with HS Degrees", y = "Density")
ACSHistogram

##iv.
##1) Based on the histogram, the data distribution is unimodal, and the mode is 89.1%.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(ACS14$HSDegree)

##2) The histogram is not symmetrical as there some outliers and it skewed toward higher percentages of the population possessing a high school degree.
##3) If you do not consider the outliers, the histogram is relatively bell-shaped. If you consider the outliers, it is not.
##4) The histogram is not normal.
shapiro.test(ACS14$HSDegree)

### The p-value is 3.194e-09 (much smaller than .05), which indicates the distribution is not normal.
##5) The histogram is negatively skewed because the data skews toward the right on the graph.
##6)
ACSHistogram + stat_function(fun = dnorm, args = list(mean = mean(ACS14$HSDegree, na.rm = TRUE), sd = sd(ACS14$HSDegree, na.rm = TRUE)), colour="blue", size=1)

##7) The data distribution does not reflect the model of a normal distribution and so it is not an accurate model for this data set or this histogram. 

##v.
qqplotHSDegree <- qplot(sample = ACS14$HSDegree)
qqplotHSDegree

##vi.
##1) The distribution is not approximately normal and is still negatively skewed.
##2) This is indicated by the curve in the line.

##vii.
round(stat.desc(ACS14$HSDegree, basic = FALSE, norm = TRUE), digits = 3)
![Screenshot.]("Screenshot1 DSC 520.png")

##viii. This is what is an odd set of data. The text mentions that in a normal distribution the value of the skew and the kurtosis should be zero. In this data set, the skew and kurtosis measurements are not zero. The skew value is negative, and this reflects the heavy distribution of the values on the right. The kurtosis value is positive and relatively far from a zero value which is reflected in the graph. A change in the sample size could cause the values to become more normal as there may be fewer individuals with high school degrees, but that is not a guarantee. I think it more so depends on what is being measured.

