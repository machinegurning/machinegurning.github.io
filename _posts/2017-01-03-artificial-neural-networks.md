---
title: "Newyearal Networks"
author: matt_upson
comments: yes
date: '2017-01-03'
modified: 2017-01-14
layout: post
excerpt: "Back to basics with Artificial Neural Networks"
published: true
status: processed
tags:
- machine learning
- artificial neural networks
- neural network
- data science
- R
categories: Rstats
output: html_document
---
 

 
One of my New Year resolutions is to get to grips with deep learning.
I thought a good place to start would be a refresher into 'shallow' neural networks, and that is what this post and the one that follows it will be about.
I'll go through the maths behind a quick dirty neural network, and implement it in R.
 
I'm using as my sources the tome [Elements of Statistical Learning](http://statweb.stanford.edu/~tibs/ElemStatLearn/), Andrew Ng's excellent [machine learning](https://www.coursera.org/learn/machine-learning) course on coursera, and a short course I have been doing on Udemy: [Deep Learning in Python](https://www.udemy.com/data-science-deep-learning-in-python/learn/v4/).
I can recommend them all.
 
## An example
 
I'm going to deal with a very simple neural network here: a single hidden layer network trained with back-propagation, and starting out with a sigmoid activation function.
 
I'll use just two input nodes $x_1$ and $x_2$, set the hidden layer to have just four nodes ($z_1$, $z_2$, $z_3$, and $z_4$), with a single output node $y$.
So, my obligatory network diagram should look like: 
 
<img src='http://www.machinegurning.com/figures/2017-01-03-neural_network.svg' alt='Neural Network' style='display: block; margin: auto; padding: 10px 10px;'>
 
Note that I have included additional 'bias' nodes to $X$ and $Z$ (the top ones: $x_0$ and $z_0$), which are always equal to one.
I'll explain what this is for as we go along.
 
## The maths
 
So how does it work? 
Once we have set the architecture of the network, there are two processes that we can do with our network: training, and prediction.
 
For predicting, Hastie et al. give the following equations:
 
$$
\begin{array}{ll}  
Z_m = \sigma(\alpha_{0m} + \alpha_m^TX),\:\text{for}\:m=\{1, \ldots, M\}
    & \\
T_k = \beta_{0k} + \beta_k^TZ,\:\text{for}\:k=\{1, \ldots, K\}
    & \\
f_k(X) = g_k(T),\:\text{for}\:k=\{1, \ldots, K\}
\end{array}
$$
 
So here, $Z$ is our hidden layer, which takes the input matrix $X$ and multiplies this with the matrix transpose of $\alpha$, which is our first matrix of weights. 
Here, Hastie, et al. refer to the bias as $\alpha_{0m}$ (and $\beta_{0k}$). 
Straight away this tells us that $\alpha$ must have the same number of rows as $X$, so that when transposed ($\alpha^T$) it will have the same number of columns ($M$) as there are rows (training examples) in our design matrix $X$.
Here $\sigma$ refers to the logistic function: 
 
$$
\sigma(v) = \dfrac{1}{1+e^{-v}}
$$
 
The second equation is essentially the same except it does not apply the logistic function, instead passing onto the third equation where the function $g_k$ is applied instead.
Here $g_k$ is the softmax function: 
 
$$
g_k(T) = \dfrac{e^{T_k}}{\sum^K_{l=1}e^{T_l}}
$$
 
To keep this example really simple I'm going to use the sigmoid logistic function throughout, but I'll come back to talking about softmax in a later post.
Note that $K$ is the number of nodes in the vector $Z$, so again, we know that $\beta$ must have the same number of rows as $Z$ ($K$) in order that it has the correct number of columns when transposed and multiplied with $Z$.
 
Along with including the bias nodes in the vectors $X$ and $Z$, this simplification allows us to simplify the equations into:
 
$$
\begin{array}{ll}  
Z_m = \sigma(\alpha_m^TX),\:\text{for}\:m=\{1, \ldots, M\} \\
y = \sigma(\beta_k^TZ),\:\text{for}\:k=\{1, \ldots, K\}
\end{array}
$$
 
If you've done any of Andrew Ng's machine learning course, or read any of my earlier blogs about this, this should be starting to look awfully familiar - we are essentially using the vectorised form of logistic regression.
 
Written out in full, for the network architecture I outlined above, $Z$ becomes:
 
 
$$
\begin{array}{ll}  
z_0 = 1 \\
z_1 = \sigma(\alpha_{0,0}x_0 + \alpha_{1,0}x_1 + \alpha_{2,0} x_2) \\
z_2 = \sigma(\alpha_{0,1}x_0 + \alpha_{1,1}x_1 + \alpha_{2,1} x_2) \\
z_3 = \sigma(\alpha_{0,2}x_0 + \alpha_{1,2}x_1 + \alpha_{2,2} x_2) \\
z_4 = \sigma(\alpha_{0,3}x_0 + \alpha_{1,3}x_1 + \alpha_{2,3} x_2) \\
\end{array}
$$
 
So essentially we need to do an element-wise multiplication of the input matrix $X$, and a weight corresponding to each element of the vector $Z$.
From this we know that their are $M\times K$ weights in $\alpha$, and since we already know that $\alpha$ must have the same number of rows as $X$, we know that it must have the same number of columns as there are nodes in $Z$ (not counting the bias node $Z_0$ which is added later).
 
For my example then, $\alpha$ must be of dimensions $M \times K$.
The same can be said of $\beta$, except that we know it must be of dimensions $K \times 1$, as there is just a single output node $y$.
 
In this first example, I will set the matrices of weights $\alpha$ and $\beta$ to be entirely composed of ones. 
I set $X$ to be comprise just $[0,1]$, along with the bias node $x_0=1$, so that:
 
$$
 
\begin{array}{ll}
 
X=\begin{bmatrix}
1 \\
0 \\
1 \\
\end{bmatrix}
 
& 
  
\alpha=\begin{bmatrix}
1 & 1 & 1 & 1 \\
1 & 1 & 1 & 1 \\
1 & 1 & 1 & 1 \\
\end{bmatrix}
 
&
 
\beta=\begin{bmatrix}
1 \\ 1 \\ 1 \\ 1 \\ 1 \\
\end{bmatrix}
 
 
\\
\end{array}
$$
 
And, to recap on the dimensions of each of these matrices:
 
$$
\begin{array}{ll}
X\in\mathbb{R}^{(2 + 1)\times1} & 
\alpha\in\mathbb{R}^{3\times4} &
Z\in\mathbb{R}^{(4+1)\times1} &
\beta\in\mathbb{R}^{5\times1} &
y\in\mathbb{R}^1
\end{array}
$$
 
### Implementing this in R
 
Implementing this in R is very simple.
Recall that I have chosen to use the logistic function throughout, so I do not need to define the softmax function at this point (unlike Hastie et al.).
 
 

{% highlight r %}
# Define the sigmoid function
 
s <- function(x) 1/(1+exp(-x))
 
# Define the matrix X (including the bias node x_0). Note that R is smart enough
# to to coerce X into a matrix, so we can pass it a row vector with c(), instead
# of a column vector.
 
X <- c(1,0,1)
M <- length(X)
 
# Setting K here sets the number of nodes in the input layer
 
K <- 5
 
# Set up weights matrices. Note that I use K - 1 in a because we only add the 
# bias node z_0 after first calculating Z
 
a <- matrix(rep(1, M * (K - 1)), nrow = M)
b <- matrix(rep(1, K), nrow = K)
 
# Transpose and multiply a with X, then add the bias node.
 
Z <- s(t(a) %*% X)
Z <- rbind(1, Z)
 
# Finally calculate y
 
y <- s(t(b) %*% Z)
 
y
{% endhighlight %}



{% highlight text %}
##           [,1]
## [1,] 0.9892622
{% endhighlight %}
 
It would be good to tidy this up into a more generic function which we can test.
Such a function will need to take a number of inputs: an input vector (or matrix) $X$, and two sets of weights $\alpha$ and $\beta$.
 

{% highlight r %}
nn_predict <- function(x, alpha, beta) {
  
  # Define the matrix extents
  
  M = length(x)
  K = ncol(alpha) + 1
  
  # Define the sigmoid function
  
  s <- function(x) 1 / (1 + exp(-x))
  
  # Check whether there is more than one row in X, and if so use apply to run 
  # over the whole matrix. Note here I use crossprod instead of %*% which is
  # slightly more performant.
  
  Z <- if (is.null(nrow(x))) s(crossprod(alpha, x)) else
    apply(x, 1, function(x) s(crossprod(alpha,x)))
 
  # Add the bias node to Z, and calculate beta'Z
  
  Z <- rbind(1, Z)
  y <- s(crossprod(beta,Z))
  
  # Transpose and convert to a dataframe to be matched against origin data
  
  y <- data.frame(pred = t(y))
  
  return(y)
  
}
{% endhighlight %}
 
So using the same inputs as before...
 

{% highlight r %}
nn_predict(X, a, b)
{% endhighlight %}



{% highlight text %}
##        pred
## 1 0.9892622
{% endhighlight %}
 
Seems legit.
Our neural network prediction should also work under instances when our input data is a matrix of values, i.e. a dataset with more than one training example $X$.
 
I'll try first with just two identical rows:
 

{% highlight r %}
X <- unname(rbind(X, X))
 
nn_predict(X, a, b)
{% endhighlight %}



{% highlight text %}
##        pred
## 1 0.9892622
## 2 0.9892622
{% endhighlight %}
 
So far so good. 
Now what about with the `mtcars` dataset.
Note that on real data it is noral practice to normalise each variable to a mean of $0$ and a standard deviation of $1$; failing to do so can have a very negative effect on the training of the network.
 

{% highlight r %}
# Define the normalise function
 
normalise <- function(x) {
  
  y <- (x - mean(x))/sd(x) 
  return(y)
  
}
 
# Normalise the mtcars data, add bias node, and convert to matrix
 
X <- apply(mtcars, 2, normalise)
X <- as.matrix(cbind(1, X))
 
M <- ncol(X)
K <- 5
 
# When sacling the input variables it is usual to use random weights with a
# uniform distribution between -0.7 and 0.7, following Hastie, et al.
 
a <- matrix(runif(M *(K - 1), min = -0.7, max = 0.7), nrow = M)
b <- matrix(runif(K, min = -0.7, max = 0.7), nrow = K)
 
nn_predict(X, a, b)
{% endhighlight %}



{% highlight text %}
##                          pred
## Mazda RX4           0.3548774
## Mazda RX4 Wag       0.3511640
## Datsun 710          0.2973615
## Hornet 4 Drive      0.2728596
## Hornet Sportabout   0.3278089
## Valiant             0.2655280
## Duster 360          0.3385054
## Merc 240D           0.2843532
## Merc 230            0.2629109
## Merc 280            0.2808949
## Merc 280C           0.2694809
## Merc 450SE          0.3238025
## Merc 450SL          0.3196923
## Merc 450SLC         0.3065954
## Cadillac Fleetwood  0.3094050
## Lincoln Continental 0.3151334
## Chrysler Imperial   0.3437211
## Fiat 128            0.3283872
## Honda Civic         0.3270231
## Toyota Corolla      0.3291828
## Toyota Corona       0.2633779
## Dodge Challenger    0.3175960
## AMC Javelin         0.3048592
## Camaro Z28          0.3400135
## Pontiac Firebird    0.3373499
## Fiat X1-9           0.3028328
## Porsche 914-2       0.3790375
## Lotus Europa        0.3698069
## Ford Pantera L      0.4161256
## Ferrari Dino        0.3995544
## Maserati Bora       0.4261293
## Volvo 142E          0.2963969
{% endhighlight %}
 
Great, so the 'feed-forward' part works - we are able to predict using the neural network. 
Of course, without training our network first, these predictions are meaningless.
My next post will deal with training the neural network using 'back-propagation', so that we can use the `nn_predict` function for classification.
To round out this post, I'll recap on why we need those pesky bias nodes.
 
## So why a bias node again??
 
The easiest way to understand this is to look back at a simple univariate linear regression.
I've [blogged](http://www.machinegurning.com/rstats/linear_regression/) about this in some detail in the past, but I will recap it quickly here.
 
Recall the linear regression model for a single case:
 
$$
y = a + bx + \epsilon
$$
 
For weights $a = 10$ and $b=20$, and $x=1$ (and ignoring error $\epsilon$), we would expect $y=30$:  
 
$$
\begin{array}{rl} 
y = &a + bx \\
y = &10 + 20x \\
y = &10 + (20 \times1) \\
y = &30 \\
\end{array}
$$
 
This can be restated in matrix terms as $y=\alpha^{T}x$ where $\alpha$ is a vector of weights, but if we leave $x$ unaltered, we get the following:  
 
$$
\begin{array}{rl}
h(\alpha) = & \alpha^Tx \\
= & \begin{bmatrix}10\\20\end{bmatrix}^{T}\times1 \\
= & \begin{bmatrix}10\times1&20\times1\end{bmatrix} \\
= & \begin{bmatrix}10&20\end{bmatrix} \\
\end{array}
$$
 
So that's no good as a vector multiplied by a scalar is still a vector. 
If we actually want to get $30$, we need to add an additional column to $x_1$, converting it to a matrix $X$, containing columns $x_0$, and $x_1$. 
Note that when $\alpha^{T}X$ works for a single column vector, for the matrix $X$ we simply use $X\alpha$.  
 
$$
\begin{array}{rl}
h(\alpha) = & \alpha^TX \\
= & \begin{bmatrix}1&1\end{bmatrix}\begin{bmatrix}10\\20\end{bmatrix} \\
= & (10\times1) + (20\times1) \\
= & 30 \\
\end{array}
$$
 
In a more realistic situation where we have multiple training examples ($M=5$), and we have a single column in $X$ (and our bias node), it is still common to write $\alpha^{T}X$, but actually this is not quite what we mean.
We can't multiply $\alpha^{T}\in\mathbb{R}^{1\times2}$ by $X\in\mathbb{R}^{5,2}$, as the the number of rows of $\alpha^{T}$ does not equal the number of columns of $X$. We really mean $\alpha^{T}X$ applied row-wise, and $\alpha^{T}\in\mathbb{R}^{2\times1}$ by $X\in\mathbb{R}^{1,2}$ does work. 
We can achieve the same effect with $X\alpha$.
 

{% highlight r %}
# When m = 1
 
a <- matrix(c(10,20), nrow = 2, ncol = 1)
X <- matrix(rep(1,2), nrow = 1, ncol = 2)
t(a) %*% X[1,]
{% endhighlight %}



{% highlight text %}
##      [,1]
## [1,]   30
{% endhighlight %}



{% highlight r %}
X %*% a
{% endhighlight %}



{% highlight text %}
##      [,1]
## [1,]   30
{% endhighlight %}



{% highlight r %}
# What about when m = 5
 
X <- matrix(rep(1,10), nrow = 5, ncol = 2)
t(a) %*% X
{% endhighlight %}



{% highlight text %}
## Error in t(a) %*% X: non-conformable arguments
{% endhighlight %}



{% highlight r %}
# What about X %*% alpha
 
X %*% a
{% endhighlight %}



{% highlight text %}
##      [,1]
## [1,]   30
## [2,]   30
## [3,]   30
## [4,]   30
## [5,]   30
{% endhighlight %}
 
#### Why is this relevant to neural networks?
 
In the neural network example that I have dealt with so far, we use the logistic function as the activation function for our network, which is essentially linear regression pushed through a sigmoid function. 
We use the same $\alpha^{T}X$, which must include the bias terms $x_{m,0} = 1$. 
 
$$
h(\alpha) = \dfrac{1}{1+e^{-\alpha^TX}}
$$
 
This rings true for the other non-linearities, like softmax, which I'll come back to in my next post.
 
## References
 
Hastie, T., Tibshirani, R., and Friedman, J. (2009). *The Elements of Statistical Learning (Second Edition)*. Springer-Verlag. 763 pages. 
 

{% highlight r %}
devtools::session_info()
{% endhighlight %}



{% highlight text %}
##  setting  value                       
##  version  R version 3.3.2 (2016-10-31)
##  system   x86_64, linux-gnu           
##  ui       RStudio (1.0.44)            
##  language en_GB:en                    
##  collate  en_GB.UTF-8                 
##  tz       GB                          
##  date     2017-01-14                  
## 
##  package      * version  date      
##  AlgDesign      1.1-7.3  2014-10-15
##  assertthat     0.1      2013-12-06
##  car            2.1-4    2016-12-02
##  caret        * 6.0-73   2016-11-10
##  checkpoint     0.3.18   2016-10-31
##  coda           0.18-1   2015-10-16
##  codetools      0.2-15   2016-10-05
##  colorspace     1.3-1    2016-11-18
##  data.table     1.9.8    2016-11-25
##  DBI            0.5-1    2016-09-10
##  devtools       1.12.0   2016-06-24
##  digest         0.6.10   2016-08-02
##  dplyr        * 0.5.0    2016-06-24
##  emoa           0.5-0    2012-09-25
##  estimability   1.2      2016-11-19
##  evaluate       0.10     2016-10-11
##  foreach        1.4.3    2015-10-13
##  GGally       * 1.3.0    2016-11-13
##  ggplot2      * 2.2.0    2016-11-11
##  ggthemes     * 3.3.0    2016-11-24
##  govstyle     * 0.1.1    2016-12-17
##  gtable         0.2.0    2016-02-26
##  highr          0.6      2016-05-09
##  iterators      1.0.8    2015-10-13
##  knitr          1.15.1   2016-11-22
##  labeling       0.3      2014-08-23
##  lattice      * 0.20-34  2016-09-06
##  lazyeval       0.2.0    2016-06-12
##  lme4           1.1-12   2016-04-16
##  lsmeans        2.25     2016-11-19
##  magrittr     * 1.5      2014-11-22
##  MASS           7.3-45   2015-11-10
##  Matrix         1.2-7.1  2016-09-01
##  MatrixModels   0.4-1    2015-08-22
##  mco            1.0-15.1 2014-11-29
##  memoise        1.0.0    2016-01-29
##  mgcv           1.8-16   2016-11-07
##  minqa          1.2.4    2014-10-09
##  ModelMetrics   1.1.0    2016-08-26
##  multcomp       1.4-6    2016-07-14
##  munsell        0.4.3    2016-02-13
##  mvtnorm        1.0-5    2016-02-02
##  nlme           3.1-128  2016-05-10
##  nloptr         1.0.4    2014-08-04
##  nnet           7.3-12   2016-02-02
##  pbkrtest       0.4-6    2016-01-27
##  plyr           1.8.4    2016-06-08
##  prettyunits    1.0.2    2015-07-13
##  progress       1.1.2    2016-12-14
##  purrr        * 0.2.2    2016-06-18
##  quantreg       5.29     2016-09-04
##  R6             2.2.0    2016-10-05
##  randomForest   4.6-12   2015-10-07
##  RColorBrewer   1.1-2    2014-12-07
##  Rcpp           0.12.8   2016-11-17
##  readr        * 1.0.0    2016-08-03
##  reshape        0.8.6    2016-10-21
##  reshape2       1.4.2    2016-10-22
##  rgp          * 0.4-1    2014-08-08
##  rmd2md       * 0.1.2    2016-10-23
##  rpart          4.1-10   2015-06-29
##  rsm            2.8      2016-10-16
##  rstudioapi     0.6      2016-06-27
##  sandwich       2.3-4    2015-09-24
##  scales         0.4.1    2016-11-09
##  SparseM        1.74     2016-11-10
##  SPOT         * 1.1.0    2016-06-09
##  stringi        1.1.2    2016-10-01
##  stringr        1.1.0    2016-08-19
##  survival       2.39-4   2016-05-11
##  TH.data        1.0-7    2016-01-28
##  tibble       * 1.2      2016-08-26
##  tidyr        * 0.6.0    2016-08-12
##  tidyverse    * 1.0.0    2016-09-09
##  withr          1.0.2    2016-06-20
##  xgboost      * 0.6-4    2017-01-05
##  xtable         1.8-2    2016-02-05
##  zoo            1.7-13   2016-05-03
##  source                                    
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.1)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  Github (ukgovdatascience/govstyle@2ef672b)
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.1)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.2.5)                            
##  CRAN (R 3.3.1)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.1)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.5)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  cran (@1.4.2)                             
##  CRAN (R 3.3.2)                            
##  Github (ivyleavedtoadflax/rmd2md@3fa6541) 
##  CRAN (R 3.2.1)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.1)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)                            
##  CRAN (R 3.2.3)                            
##  CRAN (R 3.3.2)
{% endhighlight %}
