---
title: "Lab 13"
author: "Prithviraj Kadiyala"
date: "November 13, 2018"
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
## [1] "C:/Users/prith/OneDrive/Documents/MATH4753_FA18/Lab13"
```

#Task 2

```r
set.seed(13); x=rnorm(40,mean=15,sd=5)# 1
set.seed(20); y=rnorm(35,mean=10,sd=4)# 2
var.test(x,y)# 3
```

```
## 
## 	F test to compare two variances
## 
## data:  x and y
## F = 1.1344, num df = 39, denom df = 34, p-value = 0.7119
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.5815198 2.1799598
## sample estimates:
## ratio of variances 
##            1.13444
```

##var.test()
### Are the variances equal?

```
No, They are not equal.  Since the ratio of the variances is not equal to 1
```
### What do you conclude from the var.test()?

```
In the var.test, we have confidence interval ranging between (0.5815198, 2.1799598) which tells us that variances have a chance to be equal.
The high p-value also suggests that there is a good chance that the variances are equal.
```

###$\mu_x > \mu_y$
####Null and Alternate Hypothesis
$$
H_o =\mu_x - \mu_y < 0 \\
H_1 =\mu_x - \mu_y > 0
$$

####Use Appropriate test to determine if null is rejected?

```r
t.test(x,y,alt = "greater")
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  x and y
## t = 5.1792, df = 72.62, p-value = 9.587e-07
## alternative hypothesis: true difference in means is greater than 0
## 95 percent confidence interval:
##  3.639445      Inf
## sample estimates:
## mean of x mean of y 
## 14.659765  9.294292
```

####95% Conf int

(3.6394447, \infty{})

####Interpret the Interval
```
The interval ranges from (3.6394447, \infty{}), Meaning the least difference is 3.63 and the most is infinity.
So the $\mu_x$ is always greater than $\mu_y$
```

###Change $\mu_y$

```r
set.seed(20); y=rnorm(35,mean=14,sd=4)# 2  
t.test(x,y,alt="greater")
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  x and y
## t = 1.3181, df = 72.62, p-value = 0.09581
## alternative hypothesis: true difference in means is greater than 0
## 95 percent confidence interval:
##  -0.3605553        Inf
## sample estimates:
## mean of x mean of y 
##  14.65977  13.29429
```

####Interpret the p-value
```
The p-value is 0.09581 or 9.5%.
Which is still higher than 5%. We get the idea that the mean of y is smaller than mean of x. But we only have 95% confidence interval.

The rest 5% of the values might contain the mean of Y could be greater than or equal to mean of X
```
####Interpret the confidence interval
```
The confidence Interval ranges between (-0.3605553, \infty{}).
It shows us that the difference is atleast -0.36 meaning that mean of Y is greater than mean of X by a small margin. 
Goes until infinity to show that they have a large positive difference i.e., mean of X is greater than mean of Y
```
#Task 3
##Tests 1 and 2

```r
set.seed(50); x=rnorm(30,mean=50, sd=10)
set.seed(40); y=rnorm(40,mean=55, sd=20)

var.test(x,y)
```

```
## 
## 	F test to compare two variances
## 
## data:  x and y
## F = 0.19794, num df = 29, denom df = 39, p-value = 2.09e-05
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  0.1008949 0.4023510
## sample estimates:
## ratio of variances 
##          0.1979425
```

```r
var.test(y,x)
```

```
## 
## 	F test to compare two variances
## 
## data:  y and x
## F = 5.052, num df = 39, denom df = 29, p-value = 2.09e-05
## alternative hypothesis: true ratio of variances is not equal to 1
## 95 percent confidence interval:
##  2.485392 9.911306
## sample estimates:
## ratio of variances 
##           5.051971
```

###Equivalent terms
```
F-values
test#1 => F-value= 0.19794
test#2 => F-value= 5.052
Not Equivalent

p-values
test#1 => p-value=2.09e-05
test#2 => p-value=2.09e-05
Equivalent

confint
test#1 => (0.1008949, 0.402351)
test#2 => (2.4853919, 9.9113059)
Not Equivalent
```

##Tests 3 and 4

```r
var.test(x,y,alt="less")#3
```

```
## 
## 	F test to compare two variances
## 
## data:  x and y
## F = 0.19794, num df = 29, denom df = 39, p-value = 1.045e-05
## alternative hypothesis: true ratio of variances is less than 1
## 95 percent confidence interval:
##  0.0000000 0.3581523
## sample estimates:
## ratio of variances 
##          0.1979425
```

```r
var.test(y,x,alt="greater")#4
```

```
## 
## 	F test to compare two variances
## 
## data:  y and x
## F = 5.052, num df = 39, denom df = 29, p-value = 1.045e-05
## alternative hypothesis: true ratio of variances is greater than 1
## 95 percent confidence interval:
##  2.792108      Inf
## sample estimates:
## ratio of variances 
##           5.051971
```

###Interpret the results
```
The p-value of both tests are the same.
The tests are basically the same.
```

###Compare the results
```
F-values
test#1 => F-value= 0.19794
test#2 => F-value= 5.052
Not Equivalent

p-values
test#1 => p-value=1.04e-05
test#2 => p-value=1.04e-05
Equivalent

confint
test#1 => (0, 0.3581523)
test#2 => (2.7921085, \infty{})
Not Equivalent
```

#Task 4
##Conjugate Distributions
```
In Bayesian probability theory, if the posterior distributions p(\theta | x) are in the same probability distribution family as the prior probability distribution p(\theta), the prior and posterior are then called conjugate distributions
```
##What distribution will the posterior take?
```
Beta Distribution
```

##What distribution will the posterior take? If we use Beta Prior
$$
\begin{align}
f(y|\theta) &= \binom{n}{y}\theta^y(1-\theta)^{n-y}\\
p(y|\theta)\;&\alpha \;p(\theta) \; f(x|\theta)\\
&\alpha\; \theta^{\alpha-1}(1-\theta)^{\beta-1} \theta^y (1-\theta)^{n-y}\\
& \alpha\; \theta^{\alpha + \beta -1} (1-\theta)^{n-y+\beta-1}\\
& \alpha\; \theta^{\alpha'-1}(1-\theta)^{\beta'-1}\\
n=10\\y=4\\\alpha=1\\\beta=1\\
\theta|y&=Beta(\alpha+y,n-y+\beta)\\
\theta|y&=Beta(5,7)\\
\end{align}
$$

##Moment Estimate
$$
\hat\theta = \frac{\hat\alpha}{\hat\alpha+\hat\beta}
$$

##Mean Value
$$
\begin{align}
E(X)&=\frac{\hat\alpha}{\hat\alpha+\hat\beta}\\
&=\frac{5}{5+7}\\
&=0.4166
\end{align}
$$

##Compare the two point estimates
$$
\begin{align}
E(X)&=\frac{\hat\alpha}{\hat\alpha+\hat\beta}\\
&=\frac{5}{5+7}\\
&=0.4166
\\
\\
\\
\\
\\
E(X)&=\frac{\alpha}{\alpha+\beta}\\
&=\frac{1}{1+1}\\
&=0.5
\end{align}
$$

```
From these two values we can say that the estimate is close to the real value.
```

##95% Confidence Interval

```r
qbeta(c(0.025,0.975),5,7)
```

```
## [1] 0.1674881 0.6920953
```

##Calculate using the formula

```r
theta=0.42
n=10
m=c(-1,1)
ci=theta-qnorm(0.025,mean=0,sd=1)*m*sqrt((theta*(1-theta)/n))
ci
```

```
## [1] 0.1140949 0.7259051
```

##Compare these two intervals
```
The two confidence intervals are close to each other. Varying in the scale of 10e-2.
```









