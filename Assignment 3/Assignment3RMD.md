---
title: "Assignment 3"
author: "Prithviraj Kadiyala"
date: "October 29, 2018"
output:
  html_document:
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    toc: yes
    toc_float: yes
    theme: spacelab
---



#Questions{7/16}

##Question 1
###a 

```r
pexp(120, 1/95, lower.tail = FALSE)
```

```
## [1] 0.2827597
```

###b

```r
PH = read.csv("PHISHING.csv")
hist(PH$INTTIME,main = "Phishing ")
```

![](Assignment3RMD_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
hist(rexp(267, rate = 1/95))
```

![](Assignment3RMD_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

The two graphs are very similar to each other.

Yes, the data appears to follow an exponential distribution.

##Question 2
###a

```r
alpha=3
beta=0.07
mean1=alpha*beta
mean1
```

```
## [1] 0.21
```

```r
var1=alpha*beta^2
var1
```

```
## [1] 0.0147
```

###b

```r
alpha=3
beta=0.07
mean1=alpha*beta
var1=alpha*beta^2
lower=mean1-3*var1^0.5
upper=mean1+3*var1^0.5
paste("Interval is","(",lower,",",upper,")")
```

```
## [1] "Interval is ( -0.153730669589464 , 0.573730669589464 )"
```

##Question 3
###a

```r
alpha1=2
beta1=2
mean1=alpha1*beta1
var1=alpha1*beta1^2
mean1
```

```
## [1] 4
```

```r
var1
```

```
## [1] 8
```

```r
alpha2=1
beta2=4
mean2=alpha2*beta2
var2=alpha2*beta2^2
mean2
```

```
## [1] 4
```

```r
var2
```

```
## [1] 16
```

###b

```r
#Formula A
p1=pgamma(1,shape=2,scale = 2)
p1
```

```
## [1] 0.09020401
```

```r
#Formula B
p2=pgamma(1,shape=1,scale = 4)
p2
```

```
## [1] 0.2211992
```

Therefore, Formula B has higher probability of generating a human reaction in less than 1 minute. 

##Question 4
###a

```r
pweibull(2,shape = 4, scale = 2)
```

```
## [1] 0.6321206
```

### b

```r
alpha=2
beta=4
mean1=beta^(1/alpha)*gamma((alpha+1)/alpha)
var1=beta^(2/alpha)*(gamma((alpha+2)/alpha)- (gamma((alpha+1)/alpha))^2)
sd1=sqrt(var1)
mean1
```

```
## [1] 1.772454
```

```r
sd1
```

```
## [1] 0.9265028
```

### c

```r
alpha=2
beta=4
mean1=beta^(1/alpha)*gamma((alpha+1)/alpha)
var1=beta^(2/alpha)*(gamma((alpha+2)/alpha)- (gamma((alpha+1)/alpha))^2)
sd1=sqrt(var1)
lower=mean1-2*sd1
upper=mean1+2*sd1
paste("Interval is","(",lower,",",upper,")")
```

```
## [1] "Interval is ( -0.0805516497989005 , 3.62545935160993 )"
```

### d

```r
1-pweibull(6,shape = 4, scale = 2)
```

```
## [1] 0
```

No.

##Question 5
### a

```r
alpha=2
beta=9
mean1=alpha/(alpha+beta)
var1=(beta*alpha)/((alpha+beta)^2*(alpha+beta+1))
mean1
```

```
## [1] 0.1818182
```

```r
var1
```

```
## [1] 0.01239669
```
Mean = 0.18182
Variance = 0.01240

### b

```r
1-pbeta(0.4,shape1 = 2, shape2 = 9)
```

```
## [1] 0.0463574
```

### c

```r
pbeta(0.1,shape1 = 2, shape2 = 9)
```

```
## [1] 0.2639011
```

## Question 6

### a
```
alpha = 2
beta = 16
```


### b

```r
alpha=2
beta=16
mean=beta^(1/alpha)*gamma((alpha+1)/alpha)
var=beta^(2/alpha)*(gamma((alpha+2)/alpha)-(gamma((alpha+1)/alpha))^2)
mean
```

```
## [1] 3.544908
```

```r
var
```

```
## [1] 3.433629
```

### c

```r
1-pweibull(6,shape=2,scale=16^(1/2))
```

```
## [1] 0.1053992
```


##Question 7
###a
P(x,y)=$\frac{1}{36}$

###b
$$
P_1(x) = \frac{1}{6}\\
P_2(y) = \frac{1}{6}
$$

###c
$$
\begin{align}
P(x|y) &= \frac{p(x,y)}{p(y)}=\frac{\frac{1}{36}}{\frac{1}{6}}=\frac{1}{6}
\end{align}
$$

$$
\begin{align}
P(y|x) &= \frac{p(x,y)}{p(x)}=\frac{\frac{1}{36}}{\frac{1}{6}}=\frac{1}{6}
\end{align}
$$

###d
```
Since the conditional probabilities and the unconditional probabilities are equivalent, x and y don't affect each other.
```

##Question 8
###a








