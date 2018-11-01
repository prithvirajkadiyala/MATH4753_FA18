---
title: "Lab 11"
author: "Prithviraj Kadiyala"
date: "November 1, 2018"
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
## [1] "C:/Users/prith/OneDrive/Documents/MATH4753_FA18/Lab11"
```

#Task 2


```r
d=c(5.0581, 4.9707, 5.0893, 4.9334, 4.9777, 5.0285, 4.8555, 4.9565, 
    4.9769, 4.9722, 4.999, 4.9925, 4.9686, 5.0662, 4.9239, 4.9781, 
    5.0485, 5.0014, 4.9957, 5.0195, 5.0118, 4.9928, 5.0361, 5.0185, 
    4.9879)
mp = c(-1,1)
n=length(d)
```

##Ci of 95%

```r
bounds = mean(d)+(mp*qt(1-0.05/2,24)*(sd(d)/sqrt(25)))
bounds
```

```
## [1] 4.974137 5.014607
```

##ci of 90%

```r
bounds = mean(d)+(mp*qt(1-0.1/2,24)*(sd(d)/sqrt(25)))
bounds
```

```
## [1] 4.977598 5.011146
```

##ci of 80%

```r
bounds = mean(d)+(mp*qt(1-0.2/2,24)*(sd(d)/sqrt(25)))
bounds
```

```
## [1] 4.981452 5.007292
```

##ci of 50%

```r
bounds = mean(d)+(mp*qt(1-0.5/2,24)*(sd(d)/sqrt(25)))
bounds
```

```
## [1] 4.987658 5.001086
```

##ci using t.test()

```r
obj=t.test(d,conf.level=0.8)
obj$conf.int
```

```
## [1] 4.981452 5.007292
## attr(,"conf.level")
## [1] 0.8
```

## 95% ci for the $\sigma^2$

```r
conf_Int = 95
alpha = 1 - conf_Int/100
xsq1=qchisq(1-alpha/2,n-1)
xsq2=qchisq(alpha/2,n-1)
ci=c()
ci[1]=(n-1)*var(d)/xsq1
ci[2]=(n-1)*var(d)/xsq2
ci
```

```
## [1] 0.001465121 0.004650629
```

## 90% ci for the $\sigma^2$

```r
conf_Int = 90
alpha = 1 - conf_Int/100
xsq1=qchisq(1-alpha/2,n-1)
xsq2=qchisq(alpha/2,n-1)
ci=c()
ci[1]=(n-1)*var(d)/xsq1
ci[2]=(n-1)*var(d)/xsq2
ci
```

```
## [1] 0.001583773 0.004164600
```

## 80% ci for the $\sigma^2$

```r
conf_Int = 80
alpha = 1 - conf_Int/100
xsq1=qchisq(1-alpha/2,n-1)
xsq2=qchisq(alpha/2,n-1)
ci=c()
ci[1]=(n-1)*var(d)/xsq1
ci[2]=(n-1)*var(d)/xsq2
ci
```

```
## [1] 0.001737340 0.003683142
```


## 50% ci for the $\sigma^2$

```r
conf_Int = 50
alpha = 1 - conf_Int/100
xsq1=qchisq(1-alpha/2,n-1)
xsq2=qchisq(alpha/2,n-1)
ci=c()
ci[1]=(n-1)*var(d)/xsq1
ci[2]=(n-1)*var(d)/xsq2
ci
```

```
## [1] 0.002042167 0.003029489
```

#Task 3

```r
blue=c(21.65, 17.48, 20.1, 21.57, 14.82, 19.17, 21.08, 18.23, 22.93, 15.66, 20.89, 21.66, 18.5, 20.59, 18.63, 18.91, 19.53, 17.7, 16.5, 19.03)
snapper=c(31.65, 27.48, 30.1, 31.57, 24.82, 29.17, 31.08, 28.23, 32.93, 25.66, 30.89, 31.66, 28.5, 30.59, 28.63)
n_snapper = length(snapper)
n_blue = length(blue)
mp=c(-1,1)
```

##  95% ci for $\mu_{blue}-\mu_{snapper}$  

```r
conf_Int = 95
alpha = 1 - conf_Int/100
Sp_2 = ((n_blue-1)*var(blue)+(n_snapper-1)*var(snapper))/(n_blue+n_snapper-2)
ci=mean(blue)-mean(snapper)+mp*qt(1-alpha/2,n_blue+n_snapper-2)*sqrt(Sp_2*(1/n_blue+1/n_snapper))
ci
```

```
## [1] -11.840749  -8.757585
```

##  95% ci for $\mu_{snapper}-\mu_{blue}$ 

```r
ci=mean(snapper)-mean(blue)+mp*qt(1-alpha/2,n_blue+n_snapper-2)*sqrt(Sp_2*(1/n_blue+1/n_snapper))
ci
```

```
## [1]  8.757585 11.840749
```

## Use t.test(,var.equal=TRUE)$conf.int to calculate a 
### 	95% ci for $\mu_{snapper}-\mu_{blue}$

```r
t.test(snapper,blue,conf.level=0.95,var.equal=TRUE)$conf.int
```

```
## [1]  8.757585 11.840749
## attr(,"conf.level")
## [1] 0.95
```


### 	85% ci for $\mu_{snapper}-\mu_{blue}$ 

```r
t.test(snapper,blue,conf.level=0.85,var.equal=TRUE)$conf.int
```

```
## [1]  9.182414 11.415919
## attr(,"conf.level")
## [1] 0.85
```


### 	75% ci for $\mu_{snapper}-\mu_{blue}$ 

```r
t.test(snapper,blue,conf.level=0.75,var.equal=TRUE)$conf.int
```

```
## [1]  9.411911 11.186422
## attr(,"conf.level")
## [1] 0.75
```


### 	25% ci for $\mu_{snapper}-\mu_{blue}$ 

```r
t.test(snapper,blue,conf.level=0.25,var.equal=TRUE)$conf.int
```

```
## [1] 10.05570 10.54263
## attr(,"conf.level")
## [1] 0.25
```

##What happens when the CI decreases?
```
When the CI decreases, The intervals shrink too
```
#Task 4

```r
Exam1=c(40.98, 59.36, 46.69, 41.8, 61.63, 65.31, 62.96, 60.21, 56.89, 
        78.41, 53.44, 75.2, 60.54, 52.43, 41.41, 70.79, 73.55, 55.65, 
        61.43, 63.84, 58.07, 53.79, 54.45, 67.18, 44.46)

Exam2=c(50.22, 66.19, 58.75, 51.88, 66.61, 70.86, 74.25, 70.23, 69.55, 
        87.18, 63.62, 81.7, 70.5, 66.02, 51.35, 80.92, 85.65, 65.44, 
        74.37, 75.28, 67.86, 59.92, 64.42, 73.57, 57.15)

n = length(Exam1)

d=(Exam1-Exam2)
d
```

```
##  [1]  -9.24  -6.83 -12.06 -10.08  -4.98  -5.55 -11.29 -10.02 -12.66  -8.77
## [11] -10.18  -6.50  -9.96 -13.59  -9.94 -10.13 -12.10  -9.79 -12.94 -11.44
## [21]  -9.79  -6.13  -9.97  -6.39 -12.69
```

##CI of 95%

```r
conf_Int = 95
alpha = 1 - conf_Int/100
ci = mean(d)+mp*qt(1-alpha/2,n-1)*sd(d)/sqrt(n)
ci
```

```
## [1] -10.731329  -8.710271
```

##Interpret the interval practically
```
The underlying difference between the means of the two dependent population is contained in the interval -10.7313293, -8.7102707 with 95% confidence.
```
##T.test()
###90%

```r
t.test(d,conf.level=0.90,var.equal=TRUE)$conf.int
```

```
## [1] -10.558485  -8.883115
## attr(,"conf.level")
## [1] 0.9
```

###80%

```r
t.test(d,conf.level=0.80,var.equal=TRUE)$conf.int
```

```
## [1] -10.366041  -9.075559
## attr(,"conf.level")
## [1] 0.8
```

###70%

```r
t.test(d,conf.level=0.70,var.equal=TRUE)$conf.int
```

```
## [1] -10.239465  -9.202135
## attr(,"conf.level")
## [1] 0.7
```

###60%

```r
t.test(d,conf.level=0.60,var.equal=TRUE)$conf.int
```

```
## [1] -10.140335  -9.301265
## attr(,"conf.level")
## [1] 0.6
```

###50%

```r
t.test(d,conf.level=0.10,var.equal=TRUE)$conf.int
```

```
## [1] -9.782981 -9.658619
## attr(,"conf.level")
## [1] 0.1
```

#Task 5

```r
NZBIRDS.df=read.csv("NZBIRDS.csv")
head(NZBIRDS.df)
```

```
##   Species          Name Extinct Habitat Nest.Site Nest.Density Diet Flight
## 1  Grebes  P. cristatus      No       A         G            L    F    Yes
## 2  Grebes P. rufopectus      No       A         G            L    F    Yes
## 3 Petrels      P. gavia     Yes       A        GC            H    F    Yes
## 4 Petrels  P. assimilis     Yes       A        GC            H    F    Yes
## 5 Petrels  P. urinatrix     Yes       A        GC            H    F    Yes
## 6 Petrels  P. georgicus      No       A        GC            H    F    Yes
##   Body.Mass Egg.Length
## 1      1100         57
## 2       250         43
## 3       300         57
## 4       200         54
## 5       130         38
## 6       120         39
```

```r
# o	Verify the table
Extinct_Flight=with(NZBIRDS.df, table(Extinct,Flight))
n1_2=rowSums(Extinct_Flight)
n1_2
```

```
##  No Yes 
##  78  38
```

##Verify the table

```r
conf_Int = 95
alpha = 1 - conf_Int/100
n1 = n1_2[2]
n2 = n1_2[1]
p1=Extinct_Flight[2,1]/n1
p2=Extinct_Flight[1,1]/n2
q1=1-p1
q2=1-p2
ci = (p1 -p2)+mp*qnorm(1-alpha/2,mean=0,sd=1)*sqrt(p1*q1/n1+p2*q2/n2)
ci
```

```
## [1] 0.2925475 0.6332285
```

#Task 6

```r
set.seed(35);sam1=rnorm(25,mean=10,sd=5) 
set.seed(45);sam2=rnorm(34,mean=40,sd=8)

n1=length(sam1)
n2=length(sam2)
```

##95% Confidence Interval

```r
conf_Int = 95
alpha = 1 - conf_Int/100
qf1=qf(1-alpha/2,df1=n1-1,df2=n2-1)
qf2=qf(alpha/2,df1=n1-1,df2=n2-1)
ci=c()
ci[1] = (var(sam1)/var(sam2))/qf1 
ci[2] = (var(sam1)/var(sam2))/qf2
ci
```

```
## [1] 0.2257732 1.0309437
```

```r
var.test(sam1,sam2,conf.level=0.95)$conf.int 
```

```
## [1] 0.2257732 1.0309437
## attr(,"conf.level")
## [1] 0.95
```

##80%

```r
var.test(sam1,sam2,conf.level=0.80)$conf.int 
```

```
## [1] 0.2920457 0.7831464
## attr(,"conf.level")
## [1] 0.8
```

##70%

```r
var.test(sam1,sam2,conf.level=0.70)$conf.int 
```

```
## [1] 0.3204384 0.7105597
## attr(,"conf.level")
## [1] 0.7
```

##60%

```r
var.test(sam1,sam2,conf.level=0.60)$conf.int 
```

```
## [1] 0.3449688 0.6581343
## attr(,"conf.level")
## [1] 0.6
```

##50%

```r
var.test(sam1,sam2,conf.level=0.50)$conf.int 
```

```
## [1] 0.3675269 0.6165110
## attr(,"conf.level")
## [1] 0.5
```






































































