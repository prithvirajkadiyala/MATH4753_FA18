---
title: "Exam 1 Makeup"
author: "Prithviraj Kadiyala"
date: "October 7, 2018"
output:
  html_document:
    fig_caption: yes
    keep_md: yes
    number_sections: yes
    toc: yes
    toc_float: yes
    theme: spacelab
---



#Questions Correct 4/7
#Questions in this = 3/7
#Points scored in Exam1 = 67/70


#Question1
##a $E(Y)=aE(X)+b$ Correct
$$
Y=aX+b\\
E(Y)= E(aX+b)\\
E(Y)= E(aX)+E(b)\\
E(Y)=aE(X)+b\\
\\
Rules\; used\; are\\
E(Y_1+Y_2)=E(Y_1)+E(Y_2)\\
E(aY) = aE(Y)\\
E(c) = c
$$

##b $V(Y)=a^2V(X)$ Incorrect
$$
\begin{align}
Y&=aX+b\\
E(Y)&=aE(X)+b\\
V(Y)&=E((Y-\mu)^2)\\
&=E((Y-E(Y))^2)\\
&=E((aX+b-E(aX+b))^2)\\
&=E((aX+b-E(aX)-b)^2)\\
&=E((aX-E(aX))^2)\\
&=E(a^2(X-E(X))^2)\\
&=a^2E((X-E(X))^2)\\
&=a^2V(X)
\end{align}
$$


##c Correct
$$
\begin{align}
Y&=-2X+3\\
H&=3Y-2\\
H&=3(-2X+3)-2\\
H&=-6X+9-2\\
H&=-6X+7
\end{align}
$$

###i E(Y) Correct
$$
\begin{align}
E(Y)&=-2E(X)+3\\
&=-2*0+3\\
&=3
\end{align}
$$

###ii V(Y) Correct
$$
\begin{align}
V(Y)&=(-2)^2*V(X)\\
&=4*4\\
&=16
\end{align}
$$

###iii V(H) Correct
$$
\begin{align}
V(H)&=(-6)^2*V(X)\\
&=36*4\\
&=144
\end{align}
$$

#Question 6
##a Correct
$$
P(User|+)=\frac{P(User)*P(+|User)}{P(User)*P(+|User)+P(\hat User)*P(+| \hat User)}
$$

##b Correct
$$
P(B)=\sum_{i=1}^n P(A_i)*P(B|A_i)
$$

##c Incorrect
Prior = P(U)

##d Correct
$$
\begin{align}
P(User|+)&=\frac{P(U)*P(+|U)}{P(U)*P(+|U)+P(\hat U)*P(+|\hat U)}\\
&=\frac{0.004*0.98}{(0.004*0.98)+(0.996*0.03)}\\
&=\frac{0.00392}{0.00392+0.02988}\\
&=0.115976\\
&=0.116\\
&=11.6\%
\end{align}
$$

##e Correct
$$
\begin{align}
P(\hat User|+)&=\frac{P(\hat U)*P(+|\hat U)}{P(\hat U)*P(+|\hat U)+P(U)*P(+|U)}\\
&=\frac{0.996*0.03}{(0.996*0.03)+(0.0004*0.98)}\\
&=\frac{0.02988}{0.02988+0.00392}\\
&=0.88402\\
&=0.884\\
&=88.4\%
\end{align}
$$

#Question 7
##a $P(X=10)$ Correct

```r
round(dbinom(10,size=10,prob=0.7),4)
```

```
## [1] 0.0282
```

##b P(X \leq 2) Correct

```r
round(pbinom(2,size=10,prob=0.7),4)
```

```
## [1] 0.0016
```


##c P(X < 2) Correct

```r
round(pbinom(1,size=10,prob=0.7),4)
```

```
## [1] 1e-04
```


##d P(X > 5) Correct

$$
\begin{align}
P(X>5)&=1-P(X \leq 5)\\
&=1- pbinom(5,size=10,prob=0.7)
\end{align}
$$

```r
1-round(pbinom(5,size=10,prob=0.7),4)
```

```
## [1] 0.8497
```



##e $P(8 \leq Y \leq 10)$ Incorrect

```r
pbinom(10,size=10,prob=0.3)-pbinom(7,size=10,prob=0.3)
```

```
## [1] 0.001590386
```














