#Visualize the correlation between all variable in a meaningful
#way, clear representation of correlations. Find out top 3
#reasons for having more crime in a city.
datacrime2<-read.csv("C:/Users/aruna/Desktop/Assignments/home work acadgild/COBRA-2019.csv")
require(Amelia)
library(Rcpp)
data2<-datacrime2
data2[4:10,3] <- rep(NA,7)
data2[1:5,4] <- NA
data2 <- data2[-c(5,6)]
summary(data2)
names(data2)
#"Report.Number"           "Report.Date"             "Occur.Date"              "Occur.Time"             
#[5] "Beat"                    "Apartment.Office.Prefix" "Apartment.Number"        "Location"               
#[9] "Shift.Occurrence"        "Location.Type"           "UCR.Literal"             "UCR.."                  
#[13] "IBR.Code"                "Neighborhood"            "NPU"                     "Latitude"               
#[17] "Longitude" 
library(readr)
view(data2,title=NULL)
help(view)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data2,2,pMiss)
apply(data2,1,pMiss)
library(mice)
md.pattern(data2)
library(VIM)
aggr_plot <- aggr(data2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data2), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
# All below charts provide the visualization of missing data in the data set
m <- matrix(data2=cbind(rnorm(30, 0), rnorm(30, 2), rnorm(30, 5)), nrow=30, ncol=3)
apply(m, 1, mean)
