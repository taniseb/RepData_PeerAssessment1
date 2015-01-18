---
title: "PeerAssignment"
output: html_document
---


```r
a<-read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
a$date=as.Date(a$date)
```

```
## Error in as.Date(a$date): object 'a' not found
```

```r
b<-a$date
```

```
## Error in eval(expr, envir, enclos): object 'a' not found
```

```r
c<-a$steps
```

```
## Error in eval(expr, envir, enclos): object 'a' not found
```

```r
d<-a$interval
```

```
## Error in eval(expr, envir, enclos): object 'a' not found
```

```r
avgstepsday=aggregate(c~b, FUN=sum)
```

```
## Error in eval(expr, envir, enclos): object 'b' not found
```


```r
#histogram & stats
hist(avgstepsday$c)
```

```
## Error in hist(avgstepsday$c): object 'avgstepsday' not found
```

```r
mean(avgstepsday$c)
```

```
## Error in mean(avgstepsday$c): object 'avgstepsday' not found
```

```r
median(avgstepsday$c)
```

```
## Error in median(avgstepsday$c): object 'avgstepsday' not found
```



```r
#plot
avgstepsdayn=aggregate(c~d, FUN=mean)
```

```
## Error in eval(expr, envir, enclos): object 'd' not found
```

```r
plot(avgstepsdayn$d,avgstepsdayn$c, type="l")
```

```
## Error in plot(avgstepsdayn$d, avgstepsdayn$c, type = "l"): object 'avgstepsdayn' not found
```



```r
#Number of NA's, only one that has NA's is steps.
sum(is.na(a$steps))
```

```
## Error in eval(expr, envir, enclos): object 'a' not found
```


```r
#Replacing: 
f<-a
```

```
## Error in eval(expr, envir, enclos): object 'a' not found
```

```r
library(Hmisc)
```

```
## Loading required package: grid
## Loading required package: lattice
## Loading required package: survival
## Loading required package: splines
## Loading required package: Formula
## 
## Attaching package: 'Hmisc'
## 
## The following objects are masked from 'package:base':
## 
##     format.pval, round.POSIXt, trunc.POSIXt, units
```

```r
f$steps<-impute(f$steps,fun=median(f$date))
```

```
## Error in impute(f$steps, fun = median(f$date)): object 'f' not found
```

```r
c1<-f$steps
```

```
## Error in eval(expr, envir, enclos): object 'f' not found
```

```r
b1<-f$date
```

```
## Error in eval(expr, envir, enclos): object 'f' not found
```

```r
avgstepsday2=aggregate(c1~b1, FUN=sum)
```

```
## Error in eval(expr, envir, enclos): object 'c1' not found
```

```r
c1<-as.numeric(c1)
```

```
## Error in eval(expr, envir, enclos): object 'c1' not found
```


```r
#descriptive stats again!
hist(avgstepsday2$c1)
```

```
## Error in hist(avgstepsday2$c1): object 'avgstepsday2' not found
```

```r
mean(avgstepsday2$c1)
```

```
## Error in mean(avgstepsday2$c1): object 'avgstepsday2' not found
```

```r
median(avgstepsday2$c1)
```

```
## Error in median(avgstepsday2$c1): object 'avgstepsday2' not found
```


```r
#creating weekdays and weekends and plotting it
library(lubridate)
f$e<-wday(f$date)
```

```
## Error in wday(f$date): object 'f' not found
```

```r
n=nrow(f)
```

```
## Error in nrow(f): object 'f' not found
```

```r
f$g=rep(0,n)
```

```
## Error in eval(expr, envir, enclos): object 'n' not found
```

```r
#dividing weekdays and weekends

for (i in 1:n){
if(f$e[i]>2)f$g[i]<-1

}
```

```
## Error in eval(expr, envir, enclos): object 'n' not found
```

```r
f$stepswd=f$steps*f$g
```

```
## Error in eval(expr, envir, enclos): object 'f' not found
```

```r
f$h=-(f$g-1)
```

```
## Error in eval(expr, envir, enclos): object 'f' not found
```

```r
f$stepswe=f$steps*f$h
```

```
## Error in eval(expr, envir, enclos): object 'f' not found
```

```r
wd=aggregate(f$stepswd~f$interval, FUN=mean)
```

```
## Error in eval(expr, envir, enclos): object 'f' not found
```

```r
we=aggregate(f$stepswe~f$interval, FUN=mean)
```

```
## Error in eval(expr, envir, enclos): object 'f' not found
```

```r
tw<-merge(wd,we,by="f$interval")
```

```
## Error in merge(wd, we, by = "f$interval"): object 'wd' not found
```

```r
names(tw)<-c("interval","weekdays", "weekends" )
```

```
## Error in names(tw) <- c("interval", "weekdays", "weekends"): object 'tw' not found
```

```r
xyplot(weekdays+weekends~interval, data=tw, layout=c(1,2), type="l", outer=TRUE)
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'tw' not found
```
