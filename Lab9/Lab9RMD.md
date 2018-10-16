---
title: "Lab 9"
author: "Prithviraj Kadiyala"
date: "October 15, 2018"
output:
  html_document:
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    toc: yes
    toc_float: yes
    theme: spacelab
---



#Task 1

```r
getwd()
```

```
## [1] "C:/Users/prith/OneDrive/Documents/MATH4753_FA18/Lab9"
```

#Task 2
## Code Analysis
```
A. Create a sample with replacement and prob is not given because it is uniform.
B. Calculating the confidence intervals for the given data.
```

##Equal probability
```
Question : As used in the myboot() function, each datum in x will be selected with equal probability. Why is this necessary?
```

```
Answer : Since the data is uniform having given no probability. The samples can be created with uniform distribution.
```

##Replace=TRUE

```r
set.seed(35)
sam=round(rnorm(20,mean=10,sd=4),2)
unique(sample(sam,20,replace=TRUE))
```

```
##  [1]  2.89  8.00 13.46 11.40 12.05  8.62 11.64  7.75 23.35  2.37  6.93
## [12] 16.69
```

```r
unique(sample(sam,20,replace=TRUE))
```

```
##  [1]  8.62 23.35  9.86 11.64 14.76  2.37  8.43 10.53 16.69 11.40 12.05
## [12] 13.46 17.35  6.93
```

```r
unique(sample(sam,20,replace=TRUE))
```

```
##  [1] 11.64  8.00 12.05 11.78  9.86  7.75 23.35  6.93  8.62  2.89 11.40
## [12] 13.46 10.53 14.76
```

```r
unique(sample(sam,20,replace=TRUE))
```

```
##  [1] 11.78  8.43  8.62 14.76  9.82 17.35 10.53 23.35  6.93 11.64 14.26
## [12]  9.86  7.75 13.46  2.89
```

```r
unique(sample(sam,20,replace=TRUE))
```

```
##  [1] 16.69 11.40  2.37 14.76  7.75 23.35 13.46 11.78  9.82 10.53  8.43
```

```
We get a set of unique sample when we run the line each time
```

##Replace=FALSE

```r
unique(sample(sam,20,replace=FALSE))
```

```
##  [1] 14.26 11.64 11.78 10.53  6.93 11.40  8.00  8.62  7.75  9.86 13.46
## [12]  2.89  9.82 16.69 23.35 17.35  2.37  8.43 14.76 12.05
```

```r
unique(sample(sam,20,replace=FALSE))
```

```
##  [1]  6.93 11.40 10.53  2.37  9.82 11.78 14.26 14.76 11.64  2.89 16.69
## [12] 13.46 12.05  7.75 17.35  8.62  9.86  8.43 23.35  8.00
```

```r
unique(sample(sam,20,replace=FALSE))
```

```
##  [1] 11.40  8.62 16.69 11.64 11.78  9.86 17.35 13.46  9.82  6.93  8.00
## [12] 23.35  7.75  2.89 10.53  8.43 14.76  2.37 14.26 12.05
```

```r
unique(sample(sam,20,replace=FALSE))
```

```
##  [1]  7.75 13.46  9.86 14.76  2.37 11.78  6.93  8.62 17.35 23.35  8.43
## [12]  9.82 16.69  2.89  8.00 11.40 11.64 14.26 10.53 12.05
```

```r
unique(sample(sam,20,replace=FALSE))
```

```
##  [1] 23.35  8.62  2.37  6.93  9.82 11.78 13.46 17.35  8.43 14.26 11.64
## [12]  9.86 10.53 14.76  2.89 12.05 11.40  8.00  7.75 16.69
```

```
The sample set we get each time is same. Just re-arranged.
```

##21 sample
```
unique(sample(sam,21,replace=FALSE))
```

```
It says cannot take a larger sample than the population
```

#Task 3

##myboot2() function

```r
myboot2<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
n=length(x)   #sample size

y=sample(x,n*iter,replace=TRUE)
rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it 
ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
# A histogram follows
# The object para will contain the parameters used to make the histogram
para=hist(xstat,freq=FALSE,las=1,
main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
...)

#mat will be a matrix that contains the data, this is done so that I can use apply()
mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

#pte is the point estimate
#This uses whatever fun is
pte=apply(mat,2,fun)
abline(v=pte,lwd=3,col="Black")# Vertical line
segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

# plot the point estimate 1/2 way up the density
text(pte,max(para$density)/2,round(pte,2),cex=cx)

return(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}
```

##Alpha=95%

```r
set.seed(39); sam=rnorm(25,mean=25,sd=10)
myboot2(10000,x=sam,fun=function(x) mean(x),alpha=0.05,xlab="mean(x)",col="Blue",cx=1.5)
```

![](Lab9RMD_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```
## $ci
##     2.5%    97.5% 
## 21.31693 26.53540 
## 
## $fun
## function (x) 
## mean(x)
## <bytecode: 0x0000000018df2828>
## 
## $x
##  [1] 23.14434 12.70757 20.72797 19.04018 29.67324 29.21639 14.78348
##  [8] 18.77523 33.37002 34.61627 26.04230 24.40206 32.03731 17.45030
## [15] 19.54464 20.45388 23.83394 30.47815 25.79640 22.67536 10.27711
## [22] 26.77487 37.10044 21.59809 22.50625
```

```r
set.seed(30); sam=rchisq(20,df=3)
myboot2(10000,x=sam,fun=function(x) mean(x),alpha=0.05,xlab="mean(x)",col="Blue",cx=1.5)
```

![](Lab9RMD_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```
## $ci
##     2.5%    97.5% 
## 2.052178 3.862641 
## 
## $fun
## function (x) 
## mean(x)
## <bytecode: 0x0000000018d3d258>
## 
## $x
##  [1] 0.2531032 1.6194423 5.3578133 7.3134789 0.1194100 5.0344239 5.8189991
##  [8] 4.3689673 1.6690814 3.2533362 0.9050305 2.5730754 1.9610329 2.9675027
## [15] 1.5722396 1.6959031 1.9261106 3.9198072 6.0919801 0.4082447
```

```r
set.seed(40); sam=rgamma(30,shape=2,scale=3)
myboot2(10000,x=sam,fun=function(x) mean(x),alpha=0.05,xlab="mean(x)",col="Blue",cx=1.5)
```

![](Lab9RMD_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```
## $ci
##     2.5%    97.5% 
## 5.122351 7.959613 
## 
## $fun
## function (x) 
## mean(x)
## <bytecode: 0x0000000018b85c88>
## 
## $x
##  [1]  6.4265012  6.5077401  1.8958495  5.3836641  1.4179366  0.7925928
##  [7]  8.0380097 14.7293204  5.2917468  2.7989621  7.9632983  2.8733832
## [13]  7.4419824  2.2828464  5.9877646 12.9354365  1.4996069  1.7478044
## [19] 12.3674161  8.4093535 10.2053201  8.0243948  6.5178039  7.9118846
## [25]  1.6924333 10.7938057  5.7341134 15.1533278  3.8545321  8.1861897
```

```r
set.seed(10); sam=rbeta(20,shape1=3,shape2=4)
myboot2(10000,x=sam,fun=function(x) mean(x),alpha=0.05,xlab="mean(x)",col="Blue",cx=1.5)
```

![](Lab9RMD_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

```
## $ci
##      2.5%     97.5% 
## 0.3415605 0.4742732 
## 
## $fun
## function (x) 
## mean(x)
## <bytecode: 0x0000000018c094e8>
## 
## $x
##  [1] 0.4325491 0.3899825 0.1714409 0.3068918 0.4920588 0.5129946 0.1974398
##  [8] 0.3533577 0.1343329 0.3751208 0.6722481 0.5946901 0.3788602 0.6467397
## [15] 0.5914547 0.4476104 0.2407876 0.3877302 0.5336340 0.3074436
```


##Point Estimate

```r
abs(25-23.88)*100/25
```

```
## [1] 4.48
```

```r
abs(3-2.94)*100/3
```

```
## [1] 2
```

```r
abs(6-6.5)*100/6
```

```
## [1] 8.333333
```

```r
abs(0.4286-0.41)*100/0.4286
```

```
## [1] 4.339711
```
 
##Alpha=80%

```r
set.seed(39); sam=rnorm(25,mean=25,sd=10)
myboot2(10000,x=sam,fun=function(x) var(x),alpha=0.20,xlab="var(x)",col="Blue",cx=1.5)
```

![](Lab9RMD_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```
## $ci
##      10%      90% 
## 30.45325 58.85675 
## 
## $fun
## function (x) 
## var(x)
## <bytecode: 0x0000000018cb4350>
## 
## $x
##  [1] 23.14434 12.70757 20.72797 19.04018 29.67324 29.21639 14.78348
##  [8] 18.77523 33.37002 34.61627 26.04230 24.40206 32.03731 17.45030
## [15] 19.54464 20.45388 23.83394 30.47815 25.79640 22.67536 10.27711
## [22] 26.77487 37.10044 21.59809 22.50625
```

```r
set.seed(30); sam=rchisq(20,df=3)
myboot2(10000,x=sam,fun=function(x) var(x),alpha=0.20,xlab="var(x)",col="Blue",cx=1.5)
```

![](Lab9RMD_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```
## $ci
##      10%      90% 
## 2.922344 5.689590 
## 
## $fun
## function (x) 
## var(x)
## <bytecode: 0x0000000018d85310>
## 
## $x
##  [1] 0.2531032 1.6194423 5.3578133 7.3134789 0.1194100 5.0344239 5.8189991
##  [8] 4.3689673 1.6690814 3.2533362 0.9050305 2.5730754 1.9610329 2.9675027
## [15] 1.5722396 1.6959031 1.9261106 3.9198072 6.0919801 0.4082447
```

```r
set.seed(40); sam=rgamma(30,shape=2,scale=3)
myboot2(10000,x=sam,fun=function(x) var(x),alpha=0.20,xlab="var(x)",col="Blue",cx=1.5)
```

![](Lab9RMD_files/figure-html/unnamed-chunk-7-3.png)<!-- -->

```
## $ci
##      10%      90% 
## 11.22596 20.21214 
## 
## $fun
## function (x) 
## var(x)
## <bytecode: 0x0000000018d9aae8>
## 
## $x
##  [1]  6.4265012  6.5077401  1.8958495  5.3836641  1.4179366  0.7925928
##  [7]  8.0380097 14.7293204  5.2917468  2.7989621  7.9632983  2.8733832
## [13]  7.4419824  2.2828464  5.9877646 12.9354365  1.4996069  1.7478044
## [19] 12.3674161  8.4093535 10.2053201  8.0243948  6.5178039  7.9118846
## [25]  1.6924333 10.7938057  5.7341134 15.1533278  3.8545321  8.1861897
```

```r
set.seed(10); sam=rbeta(20,shape1=3,shape2=4)
myboot2(10000,x=sam,fun=function(x) var(x),alpha=0.20,xlab="var(x)",col="Blue",cx=1.5)
```

![](Lab9RMD_files/figure-html/unnamed-chunk-7-4.png)<!-- -->

```
## $ci
##        10%        90% 
## 0.01582077 0.03050548 
## 
## $fun
## function (x) 
## var(x)
## <bytecode: 0x0000000016342180>
## 
## $x
##  [1] 0.4325491 0.3899825 0.1714409 0.3068918 0.4920588 0.5129946 0.1974398
##  [8] 0.3533577 0.1343329 0.3751208 0.6722481 0.5946901 0.3788602 0.6467397
## [15] 0.5914547 0.4476104 0.2407876 0.3877302 0.5336340 0.3074436
```

#Task 4
##myboot2() edited

```r
myboot21<-function(iter=10000,x,fun="mean",alpha=0.05,cx=1.5,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size
  
  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it 
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)
  
  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)
  
  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)
  
  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)
  
  return(list(ci=ci,fun=fun,x=x, xstat=xstat))# Some output to use if necessary
}
```

##Barplot

```r
#barplot of xstat
sam=c(1,1,1,2,2,2,2,3,3,3,4,4)
data = myboot21(10000,x=sam,fun=function(x) median(x),xlab="median(x)",col="Blue",cx=1.5)
```

![](Lab9RMD_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
barplot(data$xstat, xlab="iter", ylab="xstat")
```

![](Lab9RMD_files/figure-html/unnamed-chunk-9-2.png)<!-- -->

##(L,U)
```
From the graph (L,U) = (0.5,3)
```




















