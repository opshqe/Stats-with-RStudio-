################################################################################
#Sheet III
################################################################################
library(tidyverse)
result2013 <- c(0.268, 0.205, 0.126, 0.107, 0.092, 0.089, 0.062, 0.050)
results2017<- C(0.341, 0.257, 0.047, 0.048, 0.086, 0.084, 0.074, 0.062)

party c <- c("CDU", "SPD", "AFD", "FDP", "DIE LINKE","GRUENE", "CSU", "Others")

pie(results2017, labels = paste (party, "(", results2017,")"))

barplot(results2017,names.arg=party,
                ylim=c(0,0.7), xlab=”Parties”, ylab =”2017 Votes (%)”)



################################################################################
#descriptive statstics
################################################################################
#1a)
x <- c(1,3,5,7,9)
y <- c(3,4,5,6,7)
mean(x); mean(y)
sd(x); sd(y)

#b)
xb <-c(1,3,5,7,9)
yb <-c(1,3,5,7,8)
mean(xb); mean(yb); median(xb); median(yb)

#c)
cx <-c(1,3,5,7,9)
cy <-c(1,3,5,7,14)
mean(xc); mean(yc); median(xc); median(yc)

#2a)
x <-1000
ret <-c(0.13,0.22,0.12,-0.05,-0.13)
value <-1000*cumprod(1+ret)

#5)
library(tidyverse)
freq_tab <-tibble(no = 1:8, 
                  nobs = c(5,4,1,7,2,3,1,2))
freq_tab

#ordered raw data
x <-rep(freq_tab$no, freq_tab$nobs)

#mean
mean(x)

#geometric mean
prod(x)^(1/length(x))

#harmonic mean
length(x)/sum(1/x)

#trimmed 20% mean
mean(x,trim = 0.2)

#6)
#Which of the following measures of dispersion can be used for a qualitative variable resp.a quantitative continuous variable?
#qualtitative variable, i.e. a nominal scaled variable, none
#ordinal variable: range and interquartile range
#ordinal variable: range and interquartile range

#7)
x <-c(3,7,2,5,6,10,6,3,6,5)
mean(x)
prod(x)^(1/length(x))
length(x)/sum(1/x)
mean(x,trim = 0.1)

#quantile
quantile(x, c(0.25,0.5,0.75),type = 1)
quantile(x,c(0.25,0.5,0.75), type = 7)

#8)
library(tidyverse)
data <- matrix(c(
  22.1,32.5,40.1,
  22.3,37.1,45.6,
  26.2,39.1,51.2,
  29.6,40.5,56.4,
  31.7,45.5,58.1,
  33.5,51.3,71.1,
  38.9,52.6,74.9,
  39.7,55.7,75.9,
  43.2,55.9,80.3,
  43.2,57.7,85.3), nrow=10, ncol=3, byrow=TRUE)
colnames(data) <- c("Non-players","Beginners","Tournament")

char_numbers <- rbind(
  apply(data,2,min),
  apply(data,2,max),
  c(quantile(data[,1],probs=c(0.25),type=1),
    quantile(data[,2],probs=c(0.25),type=1),
    quantile(data[,3],probs=c(0.25),type=1)),
  c(quantile(data[,1],probs=c(0.5),type=1),
    quantile(data[,2],probs=c(0.5),type=1),
    quantile(data[,3],probs=c(0.5),type=1)),
  c(quantile(data[,1],probs=c(0.75),type=1),
    quantile(data[,2],probs=c(0.75),type=1),
    quantile(data[,3],probs=c(0.75),type=1)),
  apply(data,2,mean),
  apply(data,2,var))
char_numbers <- rbind(char_numbers, char_numbers[5,] - char_numbers[3,])
rownames(char_numbers) <-c("min","max","q1","q2","q3","mean","variance",
                           "interquartil range")
char_numbers

#boxplots
boxplot(data[,1],data[,2],data[,3], names=colnames(data),
        main = "side by side boxplots",
        xlab = "player type", ylab = "rem. chess positions")


#9)
library(tidyverse)
library(xtable)

#generating the data
distance <- c(12.5,29.9,14.8,18.7,7.6,16.2,16.5,27.4,12.1,17.5)
altitude <- c(342,1245,502,555,398,670,796,912,238,466)

#sorted data
sort(distance)
sort(altitude)

#mean & median
mean(distance)
mean(altitude)

#quantiles
quantile(distance,probs = c(0.25,0.5,0.75),type=1)
quantile(altitude,probs = c(0.25,0.5,0.75),type=1)

#interquartial range
quantile(distance,probs=0.75,type=1)- quantile(distance,probs=0.25,type=1)
quantile(altitude,probs=0.75,type=1)- quantile(altitude,probs=0.25,type=1)

#variance
var(altitude)
var(distance)

#coefficients of variation
sd(distance)/mean(distance)
sd(altitude)/mean(altitude)

#boxplots
par(mfrow=c(1,2))
boxplot(altitude,xlab="",ylab="Altitude (in m)", 
        main = "altitude",
        cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 
boxplot(distance,xlab="",ylab="Distance (in km)", 
        main = "distance",
        cex.axis=1.5,lwd=3,cex.lab=1.75,cex.main=1.75) 


#10a)
distance <-c(12.5, 29.9, 14.8, 18.7, 7.6, 16.2, 16.5, 27.4, 12.1, 17.5)
altitude <-c(342, 1245, 502, 555, 398, 796, 912, 238, 466)

sort(distance)
sort(altitude)

mean(distance)
mean(altitude)

median(distance)
median(altitude)

#b)
quantile(distance)
quantile(altitude)















#11)
library(tidyverse)
?mpg()


tab <-
  mpg %>%
  select(displ,hwy) %>%
  mutate(displ_class = 
           cut(displ,breaks = c(1,3,5,8),
               labels = c("small","medium","big"))
  )
tab
tex_tab <- xtable(tab)
print(tex_tab, include.rownames = FALSE, floating = FALSE)














