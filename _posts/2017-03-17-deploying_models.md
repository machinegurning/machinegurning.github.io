---
title: "Deploying a model"
author: matt_gregory
comments: yes
date: '2017-03-17'
modified: 2017-03-20
layout: post
excerpt: "How to show off your random forest"
published: TRUE
status: processed
tags:
- model
- export
- deploy
- http
- jug
- classification
- random forest
categories: Rstats
output: html_document
---
 

 
This blog post draws heavily on Chapter 10 in the excellent [Practical Data Science with R](https://www.manning.com/books/practical-data-science-with-r).  
 
To understand the different layers of a [full-stack](https://www.quora.com/What-does-the-term-full-stack-programmer-mean) development it can be useful to produce a reference deployment of your model. This can be a good way to jump-start deployment as it can allow experienced engineers (who are better suited to true production deployment) to tinker and experiment with your work, test [corner cases](https://en.wikipedia.org/wiki/Corner_case) and build [acceptance tests](https://en.wikipedia.org/wiki/Acceptance_testing).  
 
We'll work through using the [Student Performance dataset](https://archive.ics.uci.edu/ml/datasets/Student+Performance) that we have seen a few times on this [blog](http://www.machinegurning.com/rstats/student-performance/). We are interested in predicting whether students are likely to pass or fail their end of year exam (`G3` variable above a made-up threshold of 10). Again we use the Maths results only reading off the web from our [Github data repo](https://github.com/machinegurning/machinegurning.github.io/tree/master/data).  
 

{% highlight r %}
library(tidyverse)
 
d <- readr::read_delim("https://raw.githubusercontent.com/machinegurning/machinegurning.github.io/master/data/2016-03-01_student_performance.csv", delim = ";")
 
d$outcome <- NULL
 
d$outcome <- factor(
  ifelse(d$G3 >= 10, 1, 0), 
  labels = c("fail", "pass")
  )
 
d <- select(d, -G3)
{% endhighlight %}
 
To help with the wrangling and tidying of data, I have developed a series of [data stories on Github](https://github.com/mammykins/data_stories) which provide some standard useful code for preparing and exploring data. We employ some of that here. Given our history with this data we don't go into detail. See if you can follow the code. 
 

{% highlight r %}
#  names(d)
#  inspect data, any need normalising? or logicising or 
to_normalise <- names(select(d, age, Medu:Fedu, traveltime:failures,
                             famrel:G2))
factorise  <- names(select(d,
                           school, sex, address:Pstatus,
                           Mjob:guardian, schoolsup:romantic,
                           outcome))
logicise <- c()
 
library(scales)  #  rescale handles NAs, there are no NAs in this data
#  nrow(d) - sum(complete.cases(d))
 
d_norm <- d %>%
  na.omit() %>%
  mutate_each_(funs(rescale), to_normalise) %>%
  mutate_each_(funs(as.factor), factorise)
 
glimpse(d_norm)
{% endhighlight %}



{% highlight text %}
## Observations: 395
## Variables: 33
## $ school     <fctr> GP, GP, GP, GP, GP, GP, GP, GP, GP, GP, GP, GP, GP...
## $ sex        <fctr> F, F, F, F, F, M, M, F, M, M, F, F, M, M, M, F, F,...
## $ age        <dbl> 0.4285714, 0.2857143, 0.0000000, 0.0000000, 0.14285...
## $ address    <fctr> U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U, U,...
## $ famsize    <fctr> GT3, GT3, LE3, GT3, GT3, LE3, LE3, GT3, LE3, GT3, ...
## $ Pstatus    <fctr> A, T, T, T, T, T, T, A, A, T, T, T, T, T, A, T, T,...
## $ Medu       <dbl> 1.00, 0.25, 0.25, 1.00, 0.75, 1.00, 0.50, 1.00, 0.7...
## $ Fedu       <dbl> 1.00, 0.25, 0.25, 0.50, 0.75, 0.75, 0.50, 1.00, 0.5...
## $ Mjob       <fctr> at_home, at_home, at_home, health, other, services...
## $ Fjob       <fctr> teacher, other, other, services, other, other, oth...
## $ reason     <fctr> course, course, other, home, home, reputation, hom...
## $ guardian   <fctr> mother, father, mother, mother, father, mother, mo...
## $ traveltime <dbl> 0.3333333, 0.0000000, 0.0000000, 0.0000000, 0.00000...
## $ studytime  <dbl> 0.3333333, 0.3333333, 0.3333333, 0.6666667, 0.33333...
## $ failures   <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ...
## $ schoolsup  <fctr> yes, no, yes, no, no, no, no, yes, no, no, no, no,...
## $ famsup     <fctr> no, yes, no, yes, yes, yes, no, yes, yes, yes, yes...
## $ paid       <fctr> no, no, yes, yes, yes, yes, no, no, yes, yes, yes,...
## $ activities <fctr> no, no, no, yes, no, yes, no, no, no, yes, no, yes...
## $ nursery    <fctr> yes, no, yes, yes, yes, yes, yes, yes, yes, yes, y...
## $ higher     <fctr> yes, yes, yes, yes, yes, yes, yes, yes, yes, yes, ...
## $ internet   <fctr> no, yes, yes, yes, no, yes, yes, no, yes, yes, yes...
## $ romantic   <fctr> no, no, no, yes, no, no, no, no, no, no, no, no, n...
## $ famrel     <dbl> 0.75, 1.00, 0.75, 0.50, 0.75, 1.00, 0.75, 0.75, 0.7...
## $ freetime   <dbl> 0.50, 0.50, 0.50, 0.25, 0.50, 0.75, 0.75, 0.00, 0.2...
## $ goout      <dbl> 0.75, 0.50, 0.25, 0.25, 0.25, 0.25, 0.75, 0.75, 0.2...
## $ Dalc       <dbl> 0.00, 0.00, 0.25, 0.00, 0.00, 0.00, 0.00, 0.00, 0.0...
## $ Walc       <dbl> 0.00, 0.00, 0.50, 0.00, 0.25, 0.25, 0.00, 0.00, 0.0...
## $ health     <dbl> 0.50, 0.50, 0.50, 1.00, 1.00, 1.00, 0.50, 0.00, 0.0...
## $ absences   <dbl> 0.08000000, 0.05333333, 0.13333333, 0.02666667, 0.0...
## $ G1         <dbl> 0.1250, 0.1250, 0.2500, 0.7500, 0.1875, 0.7500, 0.5...
## $ G2         <dbl> 0.3157895, 0.2631579, 0.4210526, 0.7368421, 0.52631...
## $ outcome    <fctr> fail, fail, pass, pass, pass, pass, pass, fail, pa...
{% endhighlight %}
 
## Training and test datasets
 
We need to split the data so we can build the model and then test it, to see if it generalises well. This gives us confidence in the external validity of the model. The data arrived in a random order thus we don't need to worry about sampling at random.  
 

{% highlight r %}
data_train <- d_norm[1:350, ]
data_test <- d_norm[351:395, ]  #  we normalised with our data sets merged, unrealistic
{% endhighlight %}
 
## Building the model
 
Prior to building the model we prepare some model evaluation tools to report the model quality. As a reminder the random forest approach is useful as it tries to de-correlate the trees of which it is ensembled by randomising the set of variables that each tree is allowed to use. It also initiates by drawing a bootstrapped sample from the training data.  
 

{% highlight r %}
# these were defined in Chapter 9 of Practical Data Science with R
loglikelihood <- function(y, py) {
  pysmooth <- ifelse(py == 0, 1e-12,
                     ifelse(py == 1, 1 - 1e-12, py))
  sum(y * log(pysmooth) + (1 - y)*log(1 - pysmooth))
}
accuracyMeasures <- function(pred, truth, threshold=0.5, name="model") {
  dev.norm <- -2*loglikelihood(as.numeric(truth), pred)/length(pred)
  ctable = table(truth = truth,
                 pred = pred)
  accuracy <- sum(diag(ctable))/sum(ctable)
  precision <- ctable[2,2]/sum(ctable[,2])
  recall <- ctable[2,2]/sum(ctable[2,])
  f1 <- precision*recall
  print(paste("precision=", precision, "; recall=" , recall))
  print(ctable)
  data.frame(model = name, accuracy = accuracy, f1 = f1, dev.norm)
}
{% endhighlight %}
 
 
We train a simple random forest classifier.
 

{% highlight r %}
library(randomForest)
#  make a list of avaialble variables if necessary
varslist <- names(select(d_norm, -outcome))
customFormula <- paste('outcome ~ ', paste(varslist, collapse = ' + '))
 
 
set.seed(1337)
fmodel <- randomForest(as.formula(customFormula),
                      data = data_train,
                      importance = T)
{% endhighlight %}
 
#### Training
 

{% highlight r %}
# print('training')
rtrain <- data.frame(truth = data_train$outcome, pred = predict(fmodel, newdata = data_train))
# print(accuracyMeasures(rtrain$pred, rtrain$truth))
# ggplot(rtrain, aes(x=pred, color=(truth==1),linetype=(truth==1))) + 
#    geom_density(adjust=0.1)
{% endhighlight %}
 
#### Testing
 

{% highlight r %}
# print('testing')
rtest <- data.frame(truth = data_test$outcome, pred = predict(fmodel, newdata = data_test))
# print(accuracyMeasures(rtest$pred, rtest$truth))
# ggplot(rtest, aes(x=pred, color=(truth==1),linetype=(truth==1))) + 
#    geom_density(adjust=0.1)
{% endhighlight %}
 
Notice the negligible fall-off from training to test performance, the default random forest provided an OK fit. However, we are more interested in the export of the model, so we move on to that now. If interested run this code to examine variable importance (try to guess what variables are probably the most useful for predicting end of year exam performance?).   
 

{% highlight r %}
varImpPlot(fmodel, type = 1)
{% endhighlight %}
 
## Deploying models by export
 
Training the model is the hard part, lets export our finished model for use by other systems. When exporting the model we let our development partners deal with the difficult parts of development for production. We chose the `randomForest` function as the help suggests that the underlying trees are accessible using the `getTree` function. Our Forest is big but simple.
 
### Save the workspace
 
Training the model and exporting it are likely to happen at different times. We can save the workspace that includes the random forest model and load it along with the `randomForest` library prior to export at a later date if required. We show how to save the workspace below, or you could save the `randomForest` object using the `saveRDS` function.
 

 
A [random forest](https://en.wikipedia.org/wiki/Random_forest) model is a collection of decision trees. A decision tree is a series of tests traditionally visualised as a diagram of decision nodes. With the random forest saved as an object we can define a function that joins the tree tables from the random forest `getTree` method into one large table of trees. This can then be exported as a table representation of the random forest model that can be used by developers.
 
We look at the first decision tree from our random forest model, `fmodel`. We can also count the number of rows in the decision table.
 

{% highlight r %}
nrow(getTree(fmodel, k = 1, labelVar = FALSE))
{% endhighlight %}



{% highlight text %}
## [1] 67
{% endhighlight %}
 
And see the output as a matrix. We could export like this if we want to avoid characters.
 

{% highlight r %}
getTree(fmodel, k = 1, labelVar = FALSE) %>%
  head(10)
{% endhighlight %}



{% highlight text %}
##    left daughter right daughter split var split point status prediction
## 1              2              3        15   0.5000000      1          0
## 2              4              5        32   0.5000000      1          0
## 3              6              7        31   0.2812500      1          0
## 4              8              9        13   0.8333333      1          0
## 5             10             11        29   0.8750000      1          0
## 6              0              0         0   0.0000000     -1          1
## 7             12             13        25   0.7500000      1          0
## 8             14             15        10   4.0000000      1          0
## 9              0              0         0   0.0000000     -1          2
## 10             0              0         0   0.0000000     -1          2
{% endhighlight %}
 
## Interpreting the decision tree as a table
 
Read the help using `?getTree`. We set the argument for `labelVar=TRUE` below to provide better human readable labels for our splitting variables and predicted class providing the output as a dataframe.  
 

{% highlight r %}
getTree(fmodel, k = 1, labelVar = TRUE) %>%
  head(12)
{% endhighlight %}



{% highlight text %}
##    left daughter right daughter  split var split point status prediction
## 1              2              3   failures   0.5000000      1       <NA>
## 2              4              5         G2   0.5000000      1       <NA>
## 3              6              7         G1   0.2812500      1       <NA>
## 4              8              9 traveltime   0.8333333      1       <NA>
## 5             10             11     health   0.8750000      1       <NA>
## 6              0              0       <NA>   0.0000000     -1       fail
## 7             12             13   freetime   0.7500000      1       <NA>
## 8             14             15       Fjob   4.0000000      1       <NA>
## 9              0              0       <NA>   0.0000000     -1       pass
## 10             0              0       <NA>   0.0000000     -1       pass
## 11            16             17         G1   0.4687500      1       <NA>
## 12            18             19         G2   0.5000000      1       <NA>
{% endhighlight %}
 
### Worked example
 
We demonstrate the interpretation using an example. Imagine you had a test case for the student Joe Bloggs; a non-romantic student, who has failed three times before and with first term (G1) scaled attainment score of 0.22. Joe has promised he has turned over a new leaf since hearing about the use of machine learning in his school!    
 
We start at the first row and will proceed until we have a prediction for our student at a terminal node (a row with the `status` variable as `-1` and `left daughter` and `right daughter` variables as zero; e.g. rows 6, 9 and 10 are terminal nodes). 
 
* Start at row one and ask has your student failed the exam fewer times than the split point?
 
For numerical predictors, data with values of the variable less than or equal to the splitting point go to the left daughter node. Our student failed three times and this is greater than the `split point`. However, we must be careful and remember to transform our inputs in the same way we did for training our model, we could do this by getting the percentile our student's number of failures is in and reminding ourselves of the distribution of the `failures` variable (during production this would be automated, we show it here for understanding).
 

{% highlight r %}
table(d$failures)  #  all the data
{% endhighlight %}



{% highlight text %}
## 
##   0   1   2   3 
## 312  50  17  16
{% endhighlight %}



{% highlight r %}
table(data_train$failures)  # just training, normalised / scaled
{% endhighlight %}



{% highlight text %}
## 
##                 0 0.333333333333333 0.666666666666667                 1 
##               280                42                14                14
{% endhighlight %}
 
Three failures is the maximum seen and was therefore scaled to one. One is greater than the `split point` therefore we proceed to the `right daughter` row of the decision table (row 3). 
 
* At row 3 we ask Joe Bloggs whether his `G1` scaled score was less than 0.28?
 
Joe scored 0.22 which is less than 0.28, thus we proceed to the `left daugther`.
 
* At row 6 we notice zeroes and `NA`, we also notice a `status` of `-1`. We are at a terminal node! A decision has been made, Joe Bloggs is at risk of `fail`!
 
#### Always make sure your inputs in production are bounded
 
What would happen if a student failed four times? Would the production model predictions be able to cope? Developers can help you to defend against such problems. This is one issue of exporting a model, you have to produce a specification of the data treatment.   
 
#### Always make sure your predictions in production are bounded
 
For a classification problem, your predictions are automatically bounded between 0 and 1. If this were a regression we would want to limit the predictions to be between the `min` and `max`
 observed in the training set.
According to the help, for categorical predictors, the splitting point is represented by an integer, whose binary expansion gives the identities of the categories that goes to left or right. 
 
### How do I convert this into a percentage?
 
You can think of each decision tree in your forest as being one expert which has a slightly different life experience. It's seen different students and might have prioritised some variables over others (sort-of). If each expert votes `pass` or `fail` then you can produce a percentage or probability of each `outcome` for each new student.
 
### Seeing the forest of the trees
 
So your developer partners would need access to all the decision trees in a table to then build tools to read the model trees and evaluate the trees on new data. We simply need to define a function that joins the tree tables from the random forest `getTree()` method into one large table of trees. We write the table as a [tab-separated values table](https://github.com/machinegurning/machinegurning.github.io/tree/master/data) (or whatever is easy for the developers software to read).  
 

{% highlight r %}
extract_trees <- function(rfModel) {
  ei <- function(i) {
    ti <- getTree(rfModel, k = i, labelVar = TRUE)
    ti$nodeid <- 1:dim(ti)[[1]]
    ti$treeid <- i
    ti
  }
  nTrees <- rfModel$ntree
  do.call("rbind", sapply(1:nTrees, ei, simplify = FALSE))
}
 
#  write_tsv is tidyverse, ergo no row numbers, however the nodeid variable covers this
readr::write_tsv(extract_trees(fmodel),
            path = "../data/2017-03-17-rf_export.txt")  
{% endhighlight %}
 
Open the raw text file we produced and inspect it. You should see 500 trees of varying thickness (number of nodes). Delve into the tenebrious forest to discover insight and excellent prediction accuracy.  
 
### JSON format
 
We can adjust our output for our colleagues as necessary, mapping between R objects and JSON using [jsonlite](https://arxiv.org/abs/1403.2805).  
 

{% highlight r %}
#  http://stackoverflow.com/questions/25550711/convert-data-frame-to-json
library(jsonlite)
x <- extract_trees(fmodel)
y <- toJSON(unname(split(x, 1:nrow(x))))  #  takes a while
{% endhighlight %}
 
## Export summary
 
You should be comfortable exporting a random forest model to others allowing model evaluation to be reimplemented in a production environment. If it were just coefficients of a linear regression it would be even easier!  
 
# Deploying models as R HTTP services
 
An alternative, that is also quite easy to set-up, is to expose the R model as an HTTP service. One could copy the code and modify to our specific example from this [Github](https://github.com/mammykins/zmPDSwR/tree/master/Buzz) example. See the comments in the repo for guidance and for a more detailed tutorial see this older [blog post](https://www.r-bloggers.com/a-simple-web-application-using-rook/) or this one using [googleVis](http://www.magesblog.com/2012/08/rook-rocks-example-with-googlevis.html).
 

 
# Deploy model through a simple API
 
We can also build a sort of "black-box" model which is accessible through a web-based API. The advantage of this is that a web call can be very easily made from (almost) any programming language, making integration of the ML model quite easy. Below we show you the structure of how you might do this based on Bart's [example](http://fishyoperations.com/2015/11/24/making-an-r-based-machine-learning-model-accessible-a-simple-api.html). For brevity I did not complete this but you get the idea... 
 

{% highlight r %}
# http://fishyoperations.com/2015/11/24/making-an-r-based-machine-learning-model-accessible-a-simple-api.html
 
#  SAVE and Load if required
# saveRDS(fmodel, "../data/2017-03-17-rf_fit.Rdata")
# fmodel <- readRDS("../data/2017-03-17-rf_fit.Rdata")
 
varslist <- names(data_train)
the_predictors <- varslist[-33]  #  we drop the outcome variable
 
predict_outcome <- function(the_predictors){
 
  new_data <- data.frame(school = as.factor(school, levels = c("GP", "MS")),
                         sex = as.factor(sex, levels = c("F", "M")),
                         age = as.numeric(age) # etc., need to code input transformation!
                         )
 
  predict(fmodel
          , newdata = new_data)
 
}
 
 
 
library(jug)
 
jug() %>%
  post("/rf_api", decorate(predict_outcome)) %>%
  simple_error_handler() %>%
  serve_it()
{% endhighlight %}
 
The result is that we now have a live web-based API. We can post data to it and get back a predicted value. We could post a query using the command line tool by seeing the URL with [curl](https://en.wikipedia.org/wiki/CURL) and passing the necessary student characteristics.
 

{% highlight r %}
#  write some code here to convert input into this format
#  or manually enter for demonstration
 
curl -s --data "school=MS&sex=M&age=0.57&... etc." http://127.0.0.1:8080/rf_api
 
#  Compare to
#  predict(fmodel, data_test[1, ])
{% endhighlight %}
 
Voila!
 
# Take home
 
Show your model off; export it or set up an HTTP service or build an API.
 

{% highlight r %}
sessionInfo()
{% endhighlight %}



{% highlight text %}
## R version 3.3.2 (2016-10-31)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows >= 8 x64 (build 9200)
## 
## locale:
## [1] LC_COLLATE=English_United Kingdom.1252 
## [2] LC_CTYPE=English_United Kingdom.1252   
## [3] LC_MONETARY=English_United Kingdom.1252
## [4] LC_NUMERIC=C                           
## [5] LC_TIME=English_United Kingdom.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
##  [1] GGally_1.2.0        caret_6.0-72        lattice_0.20-34    
##  [4] xgboost_0.6-3       ggthemes_3.2.0      jug_0.1.4          
##  [7] magrittr_1.5        jsonlite_1.1        Rook_1.1-1         
## [10] randomForest_4.6-12 scales_0.4.1        dplyr_0.5.0        
## [13] purrr_0.2.2         readr_1.0.0         tidyr_0.6.0        
## [16] tibble_1.2          ggplot2_2.1.0       tidyverse_1.0.0    
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.7        assertthat_0.1     rprojroot_1.2     
##  [4] digest_0.6.11      foreach_1.4.3      rmd2md_0.1.4      
##  [7] R6_2.2.0           plyr_1.8.4         chron_2.3-47      
## [10] backports_1.0.4    MatrixModels_0.4-1 stats4_3.3.2      
## [13] evaluate_0.10      lazyeval_0.2.0     curl_2.2          
## [16] minqa_1.2.4        data.table_1.9.6   SparseM_1.72      
## [19] infuser_0.2.5      car_2.1-3          nloptr_1.0.4      
## [22] Matrix_1.2-7.1     rmarkdown_1.3      splines_3.3.2     
## [25] webutils_0.4       lme4_1.1-12        stringr_1.1.0     
## [28] munsell_0.4.3      httpuv_1.3.3       base64enc_0.1-3   
## [31] mgcv_1.8-15        htmltools_0.3.5    nnet_7.3-12       
## [34] codetools_0.2-15   reshape_0.8.6      MASS_7.3-45       
## [37] ModelMetrics_1.1.0 grid_3.3.2         checkpoint_0.3.18 
## [40] nlme_3.1-128       gtable_0.2.0       DBI_0.5-1         
## [43] stringi_1.1.2      reshape2_1.4.2     brew_1.0-6        
## [46] iterators_1.0.8    tools_3.3.2        rsconnect_0.7     
## [49] pbkrtest_0.4-6     parallel_3.3.2     yaml_2.1.14       
## [52] colorspace_1.2-7   knitr_1.15.1       quantreg_5.29
{% endhighlight %}
