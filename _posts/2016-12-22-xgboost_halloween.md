---
title: "Monster Hunting with XGBoost"
author: matt_gregory
comments: yes
date: '2017-01-04'
modified: 2017-01-12
layout: post
excerpt: "The prize winning, scalable, portable and distributed Gradient Boosting algorithm"
published: true
status: processed
tags:
- Machine Learning
- XGBoost
- Decision Tree
- Data Science
categories: Rstats
output: html_document
---
 

 
Kaggle's bread-and-butter is its Machine Learning competitions, or Predictive Analytics competitions. The processes used in these scenarios encompass a very small fraction of the Data Science process. However, this [gameified version of Data Science](https://www.kaggle.com/c/ghouls-goblins-and-ghosts-boo) can be engaging and may be an interesting hook for some newcomers, as it side-steps some of the more time consuming data wrangling processes. I introduce it here to help those who are looking for a Data Science related resolution or goal.     
 
In order to boost one's position up the Kaggle leader board one should employ algorithms that have conistently scored highly or won competitions (through fine tuning of parameters). Broadly, Kaggle competitions are won through use of [Deep Learning](https://en.wikipedia.org/wiki/Deep_learning) or [Extreme Gradient Boosting](http://homes.cs.washington.edu/~tqchen/2016/03/10/story-and-lessons-behind-the-evolution-of-xgboost.html) (XGBoost) methods ([Chen & Guestrin, 2016](https://arxiv.org/abs/1603.02754)).  
 
We introduce a gradient boosted tree using the (award winning)[http://stat-computing.org/awards/jmc/winners.html] `xgboost` which comes with lots of tuneable parameters to allow optimisation of a model. Following the [Unix philosophy](https://en.wikipedia.org/wiki/Unix_philosophy), `xgboost` performs one thing well and is easily integrated with other data science packages or processes. To demonstrate, we also introduce  using `caret` to create simple dummy (binary) variables prior to model building.  
 
There are lots of excellent tutorials for using `xgboost` in R, however I found there was a dearth of blogs explaining how to prepare one's data for use with `xgboost`, particularly when already split into training and test data sets, as is the norm for Kaggle competitions.  
 

 
# Data
 
The data can be downloaded from the [Kaggle Halloween competition page](https://www.kaggle.com/c/ghouls-goblins-and-ghosts-boo/data). We use this classifciation problem as the competition has now closed, thus avoiding any spoilers. We read in the training data to train our learner or model in order to predict the class of the test data.  
 

{% highlight r %}
train <- readr::read_csv(file  = "data/2017-01-04-train.csv",
                       col_names = TRUE)  #  this is a bad name as train is also a function in caret
 
test <- readr::read_csv("data/2017-01-04-test.csv")
{% endhighlight %}
 
Inspecting the data reveals some of the characteristics of the Monsters and how they relate. Perhaps there is enough of a difference to predict the class of a Monster by its characteristics alone? Some `id` are missing, presumably they make up the set of Monsters we are tasked with classifying correctly in the test set. We also notice how there are three types of Monster, this is a multi-level classification problem.  
 

{% highlight r %}
glimpse(train)
{% endhighlight %}



{% highlight text %}
## Observations: 371
## Variables: 7
## $ id            <int> 0, 1, 2, 4, 5, 7, 8, 11, 12, 19, 22, 23, 25, 27,...
## $ bone_length   <dbl> 0.3545122, 0.5755599, 0.4678755, 0.7766525, 0.56...
## $ rotting_flesh <dbl> 0.3508390, 0.4258684, 0.3543304, 0.5087225, 0.87...
## $ hair_length   <dbl> 0.4657609, 0.5314014, 0.8116161, 0.6367656, 0.41...
## $ has_soul      <dbl> 0.7811417, 0.4398989, 0.7912250, 0.8844637, 0.63...
## $ color         <chr> "clear", "green", "black", "black", "green", "gr...
## $ type          <chr> "Ghoul", "Goblin", "Ghoul", "Ghoul", "Ghost", "G...
{% endhighlight %}
 
## Graphical Data Analysis
 
Let's visualise these relationships and try to spot any patterns. Here I draw just one graphic which conveys a lot of information. Plotting it took minimal effort, don't waste time making it pretty! Spend time making various plots and looking at the data from many perspectives to work out which variables are likely to be useful in classifying the Monsters correctly.   
 

{% highlight r %}
ggpairs(train, 
    columns = , c("bone_length", "rotting_flesh", "hair_length", "has_soul"),
    mapping = ggplot2::aes(color = type),
    lower = list(continuous = wrap("density", alpha = 0.5), combo = "box"),
    # upper = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot", alpha = 0.4)),
    title = "Monsters!",
    axisLabels = "show")
{% endhighlight %}

![plot of chunk 2017-01-04-splom](/./2017-01-04-splom-1.svg)
 
First look at the density plots on the diagonals which are "sorta" histograms for the associated variable (read off the column or row, they are the same). Helpfully they are lie between zero and one, no need to [normalise](http://www.machinegurning.com/rstats/student-performance/), thanks Kaggle! The lower panel visualises all interactions and may indicate which ones are useful for distinguishing between Monster classes (read off from the y axis and the x axis to identify the interaction being plotted). We can even quantify these correlations as shown in the top panel for those of us who prefer numbers to graphics ("Boo!" said the Ghost). The corporeal monsters appear more similar than their ethereal counterpart. Eyeballing this it looks like linear discriminant analysis (LDA) might perform well (Spoiler alert; using default `mlr` LDA outperforms default XGBoost).    
 
## Dummy variables
 
The algorithm can't deal with multi-level factors so we express our colour variable as a series of dummy variables.
 

{% highlight r %}
train_tedious <- train %>%
  mutate(color = as.factor(color), type = as.factor(type)) %>%
  mutate(black = if_else(color == "black", 1, 0),
         blood = if_else(color == "blood", 1, 0),
         blue = if_else(color == "blue", 1, 0),
         clear = if_else(color == "clear", 1, 0),
         green = if_else(color == "green", 1, 0),
         white = if_else(color == "white", 1, 0)
         )
{% endhighlight %}
 
Hadely would not approve; I repeated myself! Instead write your own function or better yet, rely on others' tested code (see `caret::dummyVars` which creates a full set of dummy variables). I use the above method as it's more explicit for the reader. 
 
## Create additional variables using expert knowledge
 
We don't know much about the biology of these Monster species, if we did we could create new variables that might help us to train a better model. An excellent example of this is the use of the `xgboost` to help classify success or failure of [Higgs Boson production in the Large Hadron Collider](https://github.com/TimSalimans/HiggsML), where this second place entry describes standard normalisation (transformation of skewed variables) as well as feature engineering using physics knowledge.  
 
## XGBoost compatible
 
`xgboost` likes numbers; we convert the Monster type or label of our Monsters to a number using the following conversion. We then put the data into a matrix for handling by this [fast and efficient algorithm](https://www.r-bloggers.com/an-introduction-to-xgboost-r-package/), after dropping the variables we no longer need. Then we construct a xgb.DMatrix object from our matrix.
 

{% highlight r %}
label <- as.integer(train_tedious$type) - 1 #  range from 0 to number of classes
# Ghost, 1 Ghoul, 2 Goblin, 3
 
dat <- as.matrix(select(train_tedious, -id, -color, -type),
              nrow = dim(train_tedious)[1], ncol = dim(train_tedious)[2] - 3,
              byrow = FALSE)
 
dtrain <- xgb.DMatrix(data = dat, label = label)
{% endhighlight %}
 
## Training the model
 
We train the model using a specified objective. The nice thing about this function is we can specify our own objective if we need to. The key point here is we specify the the objective argument as being `multi:softmax`, to handle the multi-class nature, there be three Monster types. The most critical step to using this algorithm is understanding or at least being aware of the [customisable parameters available](https://www.analyticsvidhya.com/blog/2016/01/xgboost-algorithm-easy-steps/), this allows fine tuning of one's model and helps you avoid issues such as overfitting for example (by adjusting `eta` argument).
 

{% highlight r %}
# train model
 
bst_DMatrix <- xgboost(data = dtrain,
                       nthread = 2, nround = 5,
                       objective = "multi:softmax",
                       num_class = 3, seed = 1337)
{% endhighlight %}



{% highlight text %}
## [1]	train-merror:0.105121 
## [2]	train-merror:0.083558 
## [3]	train-merror:0.061995 
## [4]	train-merror:0.067385 
## [5]	train-merror:0.053908
{% endhighlight %}
 
Try increasing the maximum number of iterations (`nrounds`), this reduces the training error at the risk of overfitting. You can add and adjust the `eta` argument to mitigate this (default = 0.3). Turn all the nobs and see what happens (taking heed of the help in R of course; `?xgboost()`).
 
## Variable or feature importance
 
Let's inspect which features are worth including in our model. 
 

{% highlight r %}
# feature importance
 
importance_matrix <- xgb.importance(names(select(train,
                                                 -id, -color, -type)),
                                    model = bst_DMatrix)
 
xgb.plot.importance(importance_matrix)
{% endhighlight %}

![plot of chunk 2017-01-04-barplot](/./2017-01-04-barplot-1.svg)
 
All of our features seem to be fairly relevant. You could try removing the least important and rerunning the model and see whether it reduces the error.  
 
## Testing the model
 
We prepare the test data from Kaggle in a similar way to our training set, except we obviously don't need to remove the Monster type.
 

{% highlight r %}
test <- test %>%
  mutate(color = as.factor(color)) %>%
  mutate(black = if_else(color == "black", 1, 0),
         blood = if_else(color == "blood", 1, 0),
         blue = if_else(color == "blue", 1, 0),
         clear = if_else(color == "clear", 1, 0),
         green = if_else(color == "green", 1, 0),
         white = if_else(color == "white", 1, 0)
  )
 
# make test
dat_test <- as.matrix(select(test, -id, -color),
                 nrow = dim(train)[1], ncol = dim(train)[2] - 3,
                 byrow = FALSE)
 
dtest <- xgb.DMatrix(data = dat_test)
 
# predict
 
pred <- predict(bst_DMatrix, dtest)
{% endhighlight %}
 
## Convert for Kaggle Submission
 
Remember how we changed the Monster labels to numeric for the training, we now need to convert them back again to make our predictions interpretable.
 

{% highlight r %}
## Write for submission to Kaggle
 
testid <- as_tibble(test$id) %>%
  rename(id = value)
 
submission <- pred %>%  #  $class
  as_tibble() %>%
  rename(type = value) %>%
  bind_cols(testid) %>%
  select(id, type) %>%
  mutate(type = if_else(type == 0, "Ghost", 
                        false = if_else(type == 1, "Ghoul", "Goblin"))) %>%
  mutate(type = as.factor(type))
 
# write_csv(x = submission, path = "submission_xgboost.csv")
{% endhighlight %}
 
## Conclusion
 
There's plenty of good XGBoost posts around but there was a dearth of posts dealing with the Kaggle situation; when the data is pre-split into training and test with the test classes hidden. This post demonstrated how to implement the famous XGBoost algorithm in R using data from an [old learning Kaggle competition](https://www.kaggle.com/c/ghouls-goblins-and-ghosts-boo). Hopefully this will XGBoost your position on the Kaggle leaderboards! To extend this code, [try creating new features from the interactions of the variables](https://www.kaggle.com/amberthomas/ghouls-goblins-and-ghosts-boo/ghosts-goblins-and-ghouls-oh-my#) and training your model using these.  
 

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
##  [1] GGally_1.2.0    caret_6.0-72    lattice_0.20-34 xgboost_0.6-3  
##  [5] ggthemes_3.2.0  dplyr_0.5.0     purrr_0.2.2     readr_1.0.0    
##  [9] tidyr_0.6.0     tibble_1.2      ggplot2_2.1.0   tidyverse_1.0.0
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.7        highr_0.6          nloptr_1.0.4      
##  [4] git2r_0.15.0       plyr_1.8.4         iterators_1.0.8   
##  [7] tools_3.3.2        lme4_1.1-12        digest_0.6.10     
## [10] evaluate_0.10      memoise_1.0.0      gtable_0.2.0      
## [13] nlme_3.1-128       mgcv_1.8-15        Matrix_1.2-7.1    
## [16] foreach_1.4.3      DBI_0.5-1          parallel_3.3.2    
## [19] curl_2.2           SparseM_1.72       withr_1.0.2       
## [22] httr_1.2.1         stringr_1.1.0      knitr_1.15.1      
## [25] MatrixModels_0.4-1 devtools_1.12.0    stats4_3.3.2      
## [28] nnet_7.3-12        grid_3.3.2         reshape_0.8.6     
## [31] data.table_1.9.6   R6_2.2.0           minqa_1.2.4       
## [34] car_2.1-3          reshape2_1.4.2     magrittr_1.5      
## [37] splines_3.3.2      MASS_7.3-45        scales_0.4.0      
## [40] codetools_0.2-15   ModelMetrics_1.1.0 pbkrtest_0.4-6    
## [43] assertthat_0.1     checkpoint_0.3.18  rmd2md_0.1.2      
## [46] colorspace_1.2-7   labeling_0.3       quantreg_5.29     
## [49] stringi_1.1.2      lazyeval_0.2.0     munsell_0.4.3     
## [52] chron_2.3-47
{% endhighlight %}
