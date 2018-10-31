---
title: "Evaluating impact of interventions"
author: matt_gregory
comments: yes
date: '2018-05-07'
modified: 2018-05-14
layout: post
excerpt: "Protect yourself from costly bandits"
published: true
status: processed
tags:
 - Bayes
 - Bayesian
 - hypothesis
 - inference
 - a b
 - a b testing
 - Bayesian
 - Frequentist
categories: Rstats
output: html_document
---
 

 
Imagine you are responsible for the training budget of an organisation, let's say a [multi academy trust](https://www.oneeducation.co.uk/services/academies/academy-conversion/multi-academy-trust-faqs/) (MAT). Schools or [academies](https://en.wikipedia.org/wiki/Academy_(English_school)) run a range of interventions to benefit pupils; from peer tutoring programmes, to curriculum boosters, and farm-visits. These often-external-charity-led interventions can have positive, negative or negligible effects at a non-trivial cost to the school. But schools often find it extremely challenging to meaningfully assess their impact. This evaluation deficit is common across organisations, the public-and-private sector, and charities, resulting in ineffective spending. This blog post explores the problem of quantifying impact, updating knowledge with new evidence and deciding when to try something new. We take inspiration from various sources, including the [medical literature](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3179209/).    
 
In some ways this could be considered analogous to the classic multi-armed bandit problem which we introduced in a [previous Bayesian themed post](https://www.machinegurning.com/rstats/bayes_r/). It's like organising a trip with friends to a casino and divvying out the money to your group. They disperse and spend the money how they see fit (without the benefit of a global view which together you could construct). They find a one-armed-bandit slot machine and put in a coin (or many coins), they either win or lose. In our scenario, the school's students either get a good return on the intervention (a win or success - the students show some improvement in a measurable thing) or they do not (a loss or failure - the money is wasted as the students show some decline or no change in a measurable thing). (Often this is easier said than done!) Based on their experience they might change machine (explore) or if they feel like it's a "good bandit", they might stick around and exploit it further. The critical requirement here is that the school knows when the intervention has been successful. Typically this would require some before and after testing. A control group will also be required as often students will improve at a thing through time regardless of any intervention  ( [also not easy!](https://educationendowmentfoundation.org.uk/projects-and-evaluation/projects/response-to-intervention/)); it's a [non-stationary problem](https://en.wikipedia.org/wiki/Stationary_process).   
 
There are plenty of great analytics being conducted by schools and charities trying to tackle these problems (see the [Education Endowment Fund](https://educationendowmentfoundation.org.uk/)). The academic research community are also actively involved providing excellent resources on how to measure difficult to measure characteristics of human behaviour (e.g. [SPECTRUM measures](https://educationendowmentfoundation.org.uk/projects-and-evaluation/evaluating-projects/measuring-essential-skills)). Education is an interesting and important domain, we are using this blog post to explore and enthuse data scientists ([sign up as a governor for a local school](https://www.gov.uk/become-school-college-governor)). We opted for this problem domain to make it more relevant and everyday than the classic "Click Through Rate" problem for web pages, but the same principles apply ( [David Robinson](http://varianceexplained.org/) used baseball in his comprehensive and more detailed series of blog posts and book on this topic).     
 
## Randomised Control Trials (RCT) as a Gold Standard 
 
Often organisations that find themselves in a situation as described above, will be-cry the fact that they cannot possibly implement an RCT to ascertain causality. That might be the case, due to resource and ethical constraints, but that should not restrict our creativity and thoroughness of thought when considering non-randomised experimental design of the intervention, as there are other sources of bias than non-random allocation of subjects (however, whenever you can use double-blind RCT, do it!).  Looking for inspiration in the medical education field is a natural place to start, given their familiarity with using RCTs, what can we learn from them when this methodology is difficult to implement?  
 
## Barriers to RCT
 
Let us start by considering education implementation within any large organisation (continuing with our example for school interventions; our comments are generaliseable). Imagine we want to implement various training programmes or interventions achieve a measurable outcome in our students, what problems might we experience in a large multi-site organisation like a MAT.  
 
Paraphrased from Sullivan (2011) and adapted to the multi-school context:  
 
With students moving through educational processes in real time, the difficulties of randomization become immediately clear. Students usually experience charity interventions at different times: those experiencing the intervention later may learn from intervening experiences and no longer be comparable to those experiencing the intervention at an earlier time. Also, students (and / or parents) have the option to refuse randomization and may have to contribute financially a small fee for their child to participate on an intervention (biasing those students "treated" by the intervention). Use of a “placebo” is often contraindicated due to parents' sensitivities to schools acting as a place for "experimentation". Charitable interventions that operate on multiple-sites, while providing more subjects (with greater power to detect differences) and more generalisability, present challenges due to differences in students. Also, the cost, coordination and data sharing of interventions across multiple sites can be high and public support for education trials may be weak to non-existent (Walser, 2014).  
 
In addition to feasibility considerations, experts question the applicability of research models, derived from clinical research, for education studies. The highly complex system of education may be a poor fit for the RCT model, which requires clear inclusion / exclusion criteria and interventions administered identically via multiple teachers / facilitators. In education studies, variables can rarely be controlled tightly and blinding of subjects and study personnel may be unethical or impossible or unsupported by the public. Finally, defining the therapeutic intervention in education research is much more difficult than in clinical trials. One cannot “apply curriculum daily” in the same way that one can prescribe a medication.
 
: Table 1 Typical sources of error and potential work-arounds  

| Source of Error | Solutions |  
|------|-----|  
| Different sites of intervention | Explicitly describe sources of variability and potential effects on outcomes |  
| Students at different stages in education and physical development | Concurrent comparison / control group |  
| Different implementers of intervention | Intervention standard operating procedure; enumerate and describe variations explicitly |  
| High dropout rate | Describe characteristics of dropouts in as much detail as possible |  
| No concurrent comparison group | Nonconcurrent control group; describe variables that may be different between the groups and potential effects upon outcomes |  
| Use of "historical" controls |  Compare characteristics of groups as well as detailed description of previous education experiences. This design may preclude firm conclusions about the intervention |  
| No control group | Results can suggest hypotheses for further testing with a stronger design. Multiple sources of error preclude firm conclusions |  
| Control group in same school; [contamination](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4365356/) | Nonconcurrent with intervention period |  
| Difficult to ["blind"](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2947122/) | Students are aware they are being studied or know their treatment group; use objective assessment  |  
 
Note how some solutions create new problems that puts you in a Catch-22. In this situation you want to design the experiment to be the least bad as possible; compromise is necessary throughout any scientific endeavour. The important point is to record what you did and why, so that those using your findings will be aware of these limitations.  
 
## Alternatives to Randomisation
 
Randomisation is awesome but not always practical. How can we move from opinion-based to qualitative to a more quantitative design of our education research in ascertaining whether the intervention is any good and worth the expense?   
 
After whetting your appetite on this topic we point you towards Cronbach (2011) for further development of these ideas; they suggest moving towards "pragmatic trials".  
 
Our key message is, if you can't do a RCT don't turn off your brain! Thinking about your design carefully will reduce your [scientific debt](http://varianceexplained.org/r/scientific-debt/) and build your confidence in what is really happening.  
 
This also highlights a short-coming in training and education more generally across many industries, particularly assessing impact of educational / training interventions in organisations and schools. These areas are probably ripe for [start-ups to tackle some complex esoteric problems well](https://www.youtube.com/watch?v=9bbS-trc8ys). We explore this theme further with some code, simulated data and statistics.  
 
## Start simple
 
Made-up charity "Kale-and-Yoga" (these are both hip, so appropriate to lampoon; this post does not use any real data) offers interventions to schools at a fee. Students are taken away from doing other things to attend this intervention. Their intervention sounds plausible as it targets teachers' putative predisposition to Kale and Yoga, how can we protect our schools from this kind of bias? If tested before and after using an appropriate test that captures what it is Kale-and-Yoga's intervention is trying to improve, we can build up an evidence base to estimate the intervention efficiency (the proportion of students who attend the intervention that improve). Let's pretend that the "Kale-and-Yoga" charity says that their intervention makes students more resilient, thus we might be inclined to assess students before and after using Duckworth et al.'s [grit-test](https://en.wikipedia.org/wiki/Grit_(personality_trait)#Scientific_findings_and_controversy).  
 
Assuming history can be used to predict the future (ignoring the problem of induction) the expected value of the intervention efficiency of a random charity can be described. For the moment we ignore any effects of differences between the experimental conditions from which the intervention efficiencies were derived and assume that intervention efficiency of Kale-and-Yoga is described by a mean efficiency and a random error term.  
 
The first intervention at a school might look like this:
 

{% highlight r %}
# load some useful functions
library(tidyverse)
 
# make our results reproducible
set.seed(1997)
 
# create data where before and after are from the same distribution
# they vary due to chance
df <- tibble(student_id = 1:30,
             before = rnorm(30, mean = 0.5, sd = 0.1),
             after = rnorm(30, mean = 0.5, sd = 0.1))
 
# convert to binary of success (1) or failure (0)
# has our student improved?!
df <- df %>%
  mutate(success = if_else(after > before, 1, 0))
 
dplyr::glimpse(df)
{% endhighlight %}



{% highlight text %}
## Observations: 30
## Variables: 4
## $ student_id <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ...
## $ before     <dbl> 0.4232492, 0.5905089, 0.3893392, 0.4887934, 0.32624...
## $ after      <dbl> 0.5344662, 0.4460156, 0.5290277, 0.5574233, 0.46181...
## $ success    <dbl> 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, ...
{% endhighlight %}
 
We can see the impact of chance on our random samples. This is why using statistics is important, otherwise we might consider the intervention to have been effective as it had shown an improvement in students' performance. The scores after the intervention are higher. This does not necessarily mean the intervention was effective.  
 

{% highlight r %}
mean(df$before)
{% endhighlight %}



{% highlight text %}
## [1] 0.4526768
{% endhighlight %}



{% highlight r %}
mean(df$after)
{% endhighlight %}



{% highlight text %}
## [1] 0.5023315
{% endhighlight %}
 
Intervention efficiency is a derived variable (`success`). It is assumed to be emergent from a series of independent and identically distributed random variables (X1, X2, X3... Xn; range: success (Student improved) =1, failure (No improvement) = 0), with constant probability of success p. Thus for the experiment we can consider who improved and who didn't or use our `before` and `after` scores. For this post we explore the success or failure, binary nature of data as this tends to be more common for these types of interventions (are our students really i.i.d.?). Note how this contrasts to Poisson count data, where we know how many times an event occurred, but not how many times it did not occur.   
 

{% highlight r %}
table(df$success)
{% endhighlight %}



{% highlight text %}
## 
##  0  1 
##  9 21
{% endhighlight %}
 
We can test the null hypothesis that the underlying probability of success is p (here we use the default of p set to 0.5). However, as with estimating any standard error, the sample must be random to ensure independence of observations. If one is dealing with a convenience sample, then estimation of the standard error is meaningless. (Unfortunately this would rule out a great deal of research! We will instead continue on the basis that we must restrict our inference to the group of students 'at hand'). This is likely the case here, but for convenience we pretend that we sampled our school students at random (or limit our inference), the reality is it is often a convenience sample where a class of students is chosen and then assigned the intervention, or students are selected to participate. (Note: we also lost information by transforming our response variable, which could have been the difference in before and after score, rather than a less informative binary variable; we did that for the purposes of this tutorial, assuming binary data is the more typical response variable for intervention outcomes).  
 

{% highlight r %}
# we need to pass a count of successes x and a count of trials n
prop.test(x = sum(df$success), n = length(df$success), p = 0.5)
{% endhighlight %}



{% highlight text %}
## 
## 	1-sample proportions test with continuity correction
## 
## data:  sum(df$success) out of length(df$success), null probability 0.5
## X-squared = 4.0333, df = 1, p-value = 0.04461
## alternative hypothesis: true p is not equal to 0.5
## 95 percent confidence interval:
##  0.5044209 0.8458720
## sample estimates:
##   p 
## 0.7
{% endhighlight %}
 
Here we reject the null hypothesis (as the p-value < 0.05; note, distinct from p mentioned earlier, which was the probability of success in a series of Bernoulli trials), giving us the belief that the intervention has improved students' performance in the grit test. However, we know this to be wrong and a false positive, as we have generated the data ourselves from a known distribution.  This demonstrates the importance of continued testing of an intervention because our conclusions from the first experiment could be a false positive or negative. Furthermore, an intervention might benefit from the [novelty effect](https://en.wikipedia.org/wiki/Novelty_effect) and students awareness that they are being tested. Gradually through time we should build up an evidence base. This would be greatly sped up if schools across a MAT or a region could share their data and experiences with specific interventions. However, the frequentist framework is not as conducive to this passing on of knowledge (although meta-analyses of the [Cochrane reviews](http://www.cochranelibrary.com/cochrane-database-of-systematic-reviews/index.html) and Education Endowment Fund show the gold standard approaches to this problem).  
 
For a comprehensive list of methods, and the functions to carry them out in R, see the [proportion](https://www.sciencedirect.com/science/article/pii/S2352711017300018#f000005) package.  
 
### p-values versus confidence interval
 
An approach, closely related to p-values, is to focus on the precision of an observed result or treatment effect - rather than worrying about whether or not to reject a (frequently arbitrary) null hypothesis. This is done by estimating its confidence interval. Although it may not be immediately obvious, confidence intervals and null hypothesis significance tests are actually facets of exactly the same underlying model - which is why confidence intervals are often used as surrogate tests.   
 
The confidence interval is most commonly defined as that range which, when attached to a sample statistic, would enclose the true parametric value on a given proportion (1 − α) of occasions. Since α is usually taken as 0.05 (as we do here), we therefore end up with 95% confidence intervals. To think about it another way, a claim to 95% confidence simply means that the researcher has seen one possible interval from a large number of possible ones, from which nineteen out of twenty intervals (on average) contain the true value of the parameter.  
 
Some statisticians have recommended doing away with p-values altogether, and only quoting the confidence interval for a treatment effect. Others use the confidence interval as a surrogate statistical test, by assessing whether the interval overlaps 0 (if the treatment effect is expressed as a difference) or 1 (if the treatment effect is expressed as a ratio). Typically you will need to adhere to traditional standards of citing the p-value if you seek publication or approval from your peers.  
 
### Bayes approach
 
We care about what information can be extracted from the data. We want to know how wide an interval must be, given your specific data, in order to contain a certain parameter with some pre-selected probability. 
 

{% highlight r %}
# useful package for learning Bayes with plenty of examples
library(LearnBayes)
{% endhighlight %}
 
Our workflow involves building a prior distribution that matches our knowledge, we then update this knowledge with data to create a posterior distribution of our new understanding of the world.  
 
#### Constructing a beta prior
 
Suppose we are interested in the proportion, p, of students that improve their grit score after an intervention. The function `bayes.select` is a convenient tool for specifying a beta prior based on knowledge of two prior quantiles. Suppose my prior median for the proportion of improving students is `0.5` and my 75th percentile is `.6` based on my historical data of students' grit scores the previous year.    
 
We need to convert this knowledge into a [beta distribution](https://en.wikipedia.org/wiki/Beta_distribution). The beta disitribution has two shape parameters (called alpha and beta). We use the convenient `beta.select` to work this out for us. There are alternative methods to estimating the hyper-parameters of your distribution of interest described in David Robinson's excellent book on [Empirical Bayes](http://varianceexplained.org/r/empirical-bayes-book/).    
 

{% highlight r %}
beta.par <- beta.select(list(p = 0.5, x = 0.5), list( p = 0.75, x = .6))
beta.par
{% endhighlight %}



{% highlight text %}
## [1] 5.82 5.82
{% endhighlight %}
 
A beta(5.82, 5.82) prior matches this prior information.  
 
#### Updating with data
 
Given our earlier 30 observations (there were 21 successes and 9 failures). The posterior distribution is therefore beta with shape parameters  5.82 + 21 and 5.82 + 9. See how this is simply us updating the prior with the data (likelihood) to create the posterior.  
 
The `triplot` function shows the prior, likelihood, and posterior on the same display; the inputs are the vector of prior parameters and the data vector.  
 

{% highlight r %}
triplot(prior = beta.par, 
        data =  c(sum(
          df$success),
          nrow(df) - sum(df$success)))
{% endhighlight %}

![plot of chunk 2018-05-07-triplot-informative](/figures/2018-05-07-triplot-informative-1.svg)
 
This visual representation is very useful and intuitive. The posterior refelects our updated belief for the interventions efficacy given our informative prior (typically you would use an uninformative or flat prior; shown in the code chunk below).  
 

{% highlight r %}
# Jeffrey's uniformative prior, alpha = 0.5, beta = 0.5
triplot(c(0.5, 0.5), c(sum(
  df$success),
  nrow(df) - sum(df$success)))
{% endhighlight %}
 
#### Simulating from Posterior to Perform Inference
 
From `LearnBayes::BinomialInference` vignette.  
 
One can perform inference about the proportion p by simulating a large number of draws from the posterior and summarizing the simulated sample. Here the `rbeta` function is used to simulate from the beta posterior and the quantile function is used to construct a 90 percent probability interval for p.
 

{% highlight r %}
beta.post.par <- beta.par + c(sum(
  df$success),
  nrow(df) - sum(df$success))
 
post.sample <- rbeta(1000, beta.post.par[1], beta.post.par[2])
quantile(post.sample, c(0.05, 0.95))
{% endhighlight %}



{% highlight text %}
##        5%       95% 
## 0.5191727 0.7582548
{% endhighlight %}
 
Similar to the frequentist approach our credible interval does not contain p = 0.5. What you'll notice is that confidence intervals and credible intervals will tend to converge as n increases and the prior's importance is relatively reduced (by making up a lower proportion of the total data).    
 
#### Collecting more data
 
Impressed by this data (partly due to experimental design constraints), the school might commission another intervention with this charity. However, this shouldn't be the end of the story, the school and charity should continue to collect data for impact assessment. The charity could adjust its programme (guided by A/B testing) if it consistently does not impact the thing they want it to (or change their claim i.e. "Kale-and-Yoga" reduces the body mass index of students) and the school can ensure it is spending its money on effective evidence-based-interventions only.  
 
With the Bayesian approach it's easy to update our knowledge. Our new prior will be our previous posterior. The likelihood will be the data collected during our next experiment. From these two we will generate our new posterior. Imagine we repeated the experiment with more students by randomly sampling students across our school network, exposing them to the "Kale-and-Yoga" then assessing whether they got grittier or not.      
 

{% highlight r %}
triplot(prior = beta.post.par, 
        data = c(50, 50)
        )
{% endhighlight %}

![plot of chunk 2018-05-07-triplot-next](/figures/2018-05-07-triplot-next-1.svg)
 
Thus we can accumulate knowledge and build on it in a more straightforward way compared to the Frequentist approach. In the Bayesian world, the prior "pulls back" on any point estimates, and the updated posterior distribution applies to inference at any time and requires no complex sample space considerations. This allows us to spot that the intervention did not seem to do so well this time. We also benefit from being able to apply some expert knowledge in the form of a prior.  
 
#### Building a business case by predicting a future sample
 
To assess impact we need to be able to compare the cost of doing an intervention with the predicted gain in benefits. Often these might be somewhat intangible and it can be hard to assign economic value to a cost or benefit. Economists have lots of strategies to tackle this but even a back-of-the-beer-mat can be informative and should be part of the business case for how an organisation like a school decides on what interventions to pay for.  
 
Suppose we want to predict the number of students who improve their grit-score following the intervention. To do the programme costs £200 per student. We estimate that improving the grit-score of a student (if this can be shown to be a permanent change) will improve exam success and lifetime performance to the value of £300 (working not shown; made-up thought experiment). Thus if we run the intervention on 50 students from our school what's our expected cost-benefit ratio?  
 
We know the cost, we know the number of students and we know the benefit. We can work out the minimum number of students that need to improve to work out our break even point. 
 

{% highlight r %}
students <- 50 # future sample size
s <- 0:students # vector of number of successes in future experiment
cost <- 200 * students
benefit <- 300
break_even <- cost / benefit # need more than 34 students to break even
{% endhighlight %}
 
We need more than 34 students to "succeed" to break even.   
 

{% highlight r %}
# simulation
pred.probs <- pbetap(beta.post.par, students, s)
plot(s, pred.probs, type = "h", 
     ylab = "Probability", xlab = "Total number of successes")
{% endhighlight %}

![plot of chunk 2018-05-07-triplot-costing](/figures/2018-05-07-triplot-costing-1.svg)
 
We have the probabilty of getting each number of successes between 0 and 50. We can add up some of these probabilities to answer our question, what is the probability of getting more than 34 successes?  
 

{% highlight r %}
# range is from 0 to 50
length(s)
{% endhighlight %}



{% highlight text %}
## [1] 51
{% endhighlight %}



{% highlight r %}
# thus for 34 we need to reference from 34 - 1
sum(pred.probs[(34 + 1):(50 + 1)])
{% endhighlight %}



{% highlight text %}
## [1] 0.4107668
{% endhighlight %}



{% highlight r %}
#sum(pred.probs[33:50])
{% endhighlight %}
 
Thus there is a NA% chance the school will break even in paying for this Charity's intervention. Seems a bit risky!  
 
## Importance of a control group
 
The above example was somewhat contrived and missed a non-trivial feature of science; comparison to a control group. Any intervention costs money, if the intervention costs more than the benefit then it is more cost effective to do nothing. In even worse scenarios organisations might pay for interventions that are detrimental to a students performance. There is the opportunity cost of doing the thing.  
 
Imagine your students would of gotten better anyway, as the before and after is seperated by some time where anything could happen (that might have increased their score anyway, without the  Charity's intervention). This time our students are randomly sampled from school and assigned their treatment using an intra-school lottery, making our statistical assumptions more in-line with reality. To make the control commensurate and useful, students are given a green liquid to taste and do some basic stretches by a non-yogi (there should be an additional control of do nothing (business as usual) but we leave that out here for simplicity).   
 

{% highlight r %}
# make our results reproducible
set.seed(1337)
 
# create data where before and after are from different distributions
# they vary due to non-stationary distribution
# the control helps us account for this
df <- bind_rows(
  tibble(student_id = 1:30,
             before = rnorm(30, mean = 0.5, sd = 0.1),
             after = rnorm(30, mean = 0.6, sd = 0.1),
             treatment = "kale-and-yoga"), 
  tibble(student_id = 31:60,
             before = rnorm(30, mean = 0.5, sd = 0.1),
             after = rnorm(30, mean = 0.6, sd = 0.1),
             treatment = "control")
             ) %>%
  mutate(school = "Grange Hill") %>%
  # convert to factors
  mutate(treatment = as.factor(treatment),
         school = as.factor(school)) %>%
  # convert to binary of success (1) or failure (0)
  # has our student improved?!
  mutate(success = if_else(after > before, 1, 0))
  
glimpse(df)
{% endhighlight %}



{% highlight text %}
## Observations: 60
## Variables: 6
## $ student_id <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, ...
## $ before     <dbl> 0.5192492, 0.3553298, 0.4676819, 0.6622296, 0.43109...
## $ after      <dbl> 0.7262649, 0.7099949, 0.4876445, 0.9446079, 0.52802...
## $ treatment  <fctr> kale-and-yoga, kale-and-yoga, kale-and-yoga, kale-...
## $ school     <fctr> Grange Hill, Grange Hill, Grange Hill, Grange Hill...
## $ success    <dbl> 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, ...
{% endhighlight %}
 
Without the control the `treatment = kale-and-yoga` looks promising. However, when considering that students on the control programme showed similar improvement at much reduced cost, we question its value.  
 

{% highlight r %}
table(df$treatment, df$success)
{% endhighlight %}



{% highlight text %}
##                
##                  0  1
##   control        9 21
##   kale-and-yoga  6 24
{% endhighlight %}
 
We all have bias and pre-held beliefs, we might be willing the novel new approach to beat a "boring" and traditional approach. The age of a practice does not predict its efficacy, it's fallacious to reason so (although their may be some correlation in efficacy if the scientific method is used for selection; good ideas might stick around, although bad ideas can persist also).  
 
We need statistics to help us here. Which is better A or B? Or the intervention, versus the control or the current best practice.  
 

{% highlight r %}
# we need to pass a count of successes x and a count of trials n
# given our data has been ordered by studentid and treatment we can use the row number to filter
prop.test(
  x = c(
    sum(df[1:30, "success"]), # 1:30 gets our kale
    sum(df[31:60, "success"])
    ),
  n = c(
    nrow(df[1:30, "success"]), # 31:60 gets our control
    nrow(df[31:60, "success"])
            )
  )
{% endhighlight %}



{% highlight text %}
## 
## 	2-sample test for equality of proportions with continuity
## 	correction
## 
## data:  c(sum(df[1:30, "success"]), sum(df[31:60, "success"])) out of c(nrow(df[1:30, "success"]), nrow(df[31:60, "success"]))
## X-squared = 0.35556, df = 1, p-value = 0.551
## alternative hypothesis: two.sided
## 95 percent confidence interval:
##  -0.1509982  0.3509982
## sample estimates:
## prop 1 prop 2 
##    0.8    0.7
{% endhighlight %}
 
Given the extra information in the control treatment, we can control for the students having improved anyway. We accept the null hypothesis for now, the evidence suggests the intervention does not improve the performance of our students in the grit-test relative to the control.  
 
In the above example, we lumped together our successes and failures and then analysed the total counts of successful students (those that improved grit scores). The question you may need to ask yourself when considering to treat the data as proportion data or binary response is: *do I have unique values of one or more explanatory variable for each and every individual case?* In our example above, we didn't as students only varied in their intervention type, all else was assumed to be constant. As the answer is no, then there is nothing to be gained and we should reduce our data by aggregating the counts to the resolution at which each *count* does have a unique set of explanatory variables.  
 
## Comparing a few interventions across schools
 
Now imagine you need to make this type of comparison of intervention efficacy across a range of charitable interventions (we pick an intervention that has some support on [EEF website](https://educationendowmentfoundation.org.uk/evidence-summaries/teaching-learning-toolkit/outdoor-adventure-learning/)) as well as our control group (do nothing; go about your lives) and across schools. You risk further false positives and false negatives due to [multiple testing](https://en.wikipedia.org/wiki/Multiple_comparisons_problem). This time we will hide the code for creating the data by setting the Rmarkdown chunk `echo` option to `FALSE`. Now you face the uncertainty of the real world!  
 
 

{% highlight text %}
##    student_id        before           after                 treatment  
##  Min.   : 1.00   Min.   :0.2526   Min.   :0.06212   control      :120  
##  1st Qu.:15.75   1st Qu.:0.4366   1st Qu.:0.51815   desert-island: 60  
##  Median :30.50   Median :0.5072   Median :0.61824   kale-and-yoga: 60  
##  Mean   :30.50   Mean   :0.5064   Mean   :0.63241                      
##  3rd Qu.:45.25   3rd Qu.:0.5680   3rd Qu.:0.73358                      
##  Max.   :60.00   Max.   :0.8104   Max.   :1.19010                      
##  school     success      
##  A:120   Min.   :0.0000  
##  B:120   1st Qu.:0.0000  
##          Median :1.0000  
##          Mean   :0.7167  
##          3rd Qu.:1.0000  
##          Max.   :1.0000
{% endhighlight %}
 
The old fashioned way of modelling these sorts of success and failure data was to use percentage as the response variable. There are four problems with this approach:
 
* the errors are not homoschedastic (homogeneity of variance)  
* the variance is not constant (it's npq)  
* the response is bounded between 0 and 1 (as probability)  
* or intuitively by calculating the percentage we lose information of n, the sample size from which the proportion was estimated  
 
The trick is to do a weighted regression using the individual sample sizes as weights and the logit link funciton to ensure linearity.
 
For comparison of two samples of proportions, like in [A/B testing](https://www.machinegurning.com/rstats/bayes_r/), use `prop.test`. Alternatively use a [Bayesian approach](http://www.sumsar.net/blog/2014/06/bayesian-first-aid-prop-test/), like`bayes.prop.test` which you might prefer for explaining the results to regular humans.  
 
For frequentist approach we use a `glm` (pronounced glim) with logit link function, with the family argument set to binomial. This is described in detail in the [R book by Mick Crawley](https://onlinelibrary.wiley.com/doi/book/10.1002/9780470515075) so we summarise it in brief here. A generalised linear model has three important properties:  
 
* the error structure  
* the linear predictor  
* the link function  
 
These are lkely to be unfamiliar concepts, that's OK, but the ideas behind them are straighforward, explained clearly in Crawley's R book (this book formed part of my first R course at post-graduate level).
 
First we must condense the data down, at the moment we represent it at the level of each student, we can collapse this down into number of successes and failures for each treatment and school combination. These could be neighbouring schools in a MAT that are keen to share knowledge with one-another as to the efficacy of these interventions. Then we create a two-column object for the response, using `cbind` to join together the two vectors of successes and failures.  
 

{% highlight r %}
# note, as our controls are assumed to be the same for both interventions, they are grouped together
# this might not always be true
df2 <- df %>% 
  dplyr::select(-student_id, -before, -after) %>%
  group_by(school, treatment)  %>% 
  summarise(n = n())
 
df2
{% endhighlight %}



{% highlight text %}
## # A tibble: 6 x 3
## # Groups:   school [?]
##   school     treatment     n
##   <fctr>        <fctr> <int>
## 1      A       control    60
## 2      A desert-island    30
## 3      A kale-and-yoga    30
## 4      B       control    60
## 5      B desert-island    30
## 6      B kale-and-yoga    30
{% endhighlight %}
 
We want variables of success and failure, so that we can bind them together to create our response variable.  
 

{% highlight r %}
# count successes and failures by summing logical
df3 <- df %>%
  dplyr::select(-student_id, -before, -after) %>%
  group_by(school, treatment) %>%
  summarise(s = sum(success == 1), failures = sum(success == 0))
 
df3
{% endhighlight %}



{% highlight text %}
## # A tibble: 6 x 4
## # Groups:   school [?]
##   school     treatment     s failures
##   <fctr>        <fctr> <int>    <int>
## 1      A       control    39       21
## 2      A desert-island    30        0
## 3      A kale-and-yoga    24        6
## 4      B       control    38       22
## 5      B desert-island    23        7
## 6      B kale-and-yoga    18       12
{% endhighlight %}
 
We do that within the model specification. We start with an additive model ignoring the interaction term between the treatment used at each school and the proportion of students "succeeding" (improving their grit-score) (this is done to speed up proceedings; typically you should start with the maximal model and work backwards through model simplification).  
 

{% highlight r %}
m1 <- glm(cbind(s, failures) ~ school + treatment, data = df3, family = "binomial")
summary(m1)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## glm(formula = cbind(s, failures) ~ school + treatment, family = "binomial", 
##     data = df3)
## 
## Deviance Residuals: 
##       1        2        3        4        5        6  
## -1.0269   2.3207   0.4807   0.9656  -1.2410  -0.4139  
## 
## Coefficients:
##                        Estimate Std. Error z value Pr(>|z|)    
## (Intercept)              0.9013     0.2519   3.579 0.000346 ***
## schoolB                 -0.6109     0.2989  -2.044 0.040981 *  
## treatmentdesert-island   1.4640     0.4480   3.268 0.001084 ** 
## treatmentkale-and-yoga   0.2701     0.3435   0.787 0.431574    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 26.5618  on 5  degrees of freedom
## Residual deviance:  9.3154  on 2  degrees of freedom
## AIC: 37.067
## 
## Number of Fisher Scoring iterations: 5
{% endhighlight %}
 
Hold your horses prior to interpretation! First we check for overdispersion, residual deviance > residual df. If overdispersed set family to quasibinomial.   
 

{% highlight r %}
m2 <- glm(cbind(s, failures) ~ school + treatment, data = df3, family = "quasibinomial")
summary(m2)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## glm(formula = cbind(s, failures) ~ school + treatment, family = "quasibinomial", 
##     data = df3)
## 
## Deviance Residuals: 
##       1        2        3        4        5        6  
## -1.0269   2.3207   0.4807   0.9656  -1.2410  -0.4139  
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)
## (Intercept)              0.9013     0.4707   1.915    0.196
## schoolB                 -0.6109     0.5586  -1.094    0.388
## treatmentdesert-island   1.4640     0.8373   1.749    0.222
## treatmentkale-and-yoga   0.2701     0.6419   0.421    0.715
## 
## (Dispersion parameter for quasibinomial family taken to be 3.492913)
## 
##     Null deviance: 26.5618  on 5  degrees of freedom
## Residual deviance:  9.3154  on 2  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 5
{% endhighlight %}
 
The linear predictor is in logits and you can back transform. Proceed with model simplificaiton from maximal to minimal. As our intervention type is categorical variable we use F-test to compare the original and simplified models.  
 

{% highlight r %}
m3 <- update(m2, ~.-school)
anova(m2, m3, test = "F")
{% endhighlight %}



{% highlight text %}
## Analysis of Deviance Table
## 
## Model 1: cbind(s, failures) ~ school + treatment
## Model 2: cbind(s, failures) ~ treatment
##   Resid. Df Resid. Dev Df Deviance     F Pr(>F)
## 1         2     9.3154                         
## 2         3    13.5664 -1   -4.251 1.217 0.3849
{% endhighlight %}
 
The `school` is not informative in our model and is not signficant. There is no compelling evidence that the two different schools respond differently to the `treatment`.  
 

{% highlight r %}
summary(m3)
{% endhighlight %}



{% highlight text %}
## 
## Call:
## glm(formula = cbind(s, failures) ~ treatment, family = "quasibinomial", 
##     data = df3)
## 
## Deviance Residuals: 
##       1        2        3        4        5        6  
##  0.1348   2.7282   1.2426  -0.1344  -1.7855  -1.1640  
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)
## (Intercept)              0.5826     0.3615   1.612    0.205
## treatmentdesert-island   1.4418     0.8449   1.706    0.186
## treatmentkale-and-yoga   0.2647     0.6457   0.410    0.709
## 
## (Dispersion parameter for quasibinomial family taken to be 3.605976)
## 
##     Null deviance: 26.562  on 5  degrees of freedom
## Residual deviance: 13.566  on 3  degrees of freedom
## AIC: NA
## 
## Number of Fisher Scoring iterations: 5
{% endhighlight %}
 
Again we update by removing `treatment` as there are no significant terms, this will leave the null model of just the grand mean.  
 

{% highlight r %}
m4 <- update(m3, ~.-treatment)
anova(m3, m4, test = "F")
{% endhighlight %}



{% highlight text %}
## Analysis of Deviance Table
## 
## Model 1: cbind(s, failures) ~ treatment
## Model 2: cbind(s, failures) ~ 1
##   Resid. Df Resid. Dev Df Deviance      F Pr(>F)
## 1         3     13.566                          
## 2         5     26.562 -2  -12.995 1.8019 0.3062
{% endhighlight %}
 
Again, `treatment` has non significant effect. However, this doesn't mean there is not a difference between treatments. Thus, it's important for schools to revisit different interventions to be sure that they got a fair run, perhaps they were just unlucky the first few times. While its important to exploit good interventions, we must also consider how we continue to explore new and old interventions while carefully tracking performance against some agreed metric to ascertain effiacy.  
 
Visit the machine gurning repo to find out what the data generation process looked like for the above (in `./rmd_/2018-05-07-bayes_thinking.Rmd` of our Github repo).  
 
In some situations we might not always have a control and might simply be comparing two competing charity interventions. In this instance we can treat the situation as an A/B test if the assumptions have been met. The Frequentist and Bayesian approaches to A/B testing is described in a Hacker-News-worthy earlier [blog post](https://www.machinegurning.com/rstats/bayes_r/). This is also the "pragmatic trial" that was described in our mini-literature review at the start of the blog post.  
 
### Bayesian approach
 
In this way we could think of each intervention as being a one-armed-bandit in the Casino, with our paying for a student to experience the intervention, our putting a coin in the slot machine and awaiting the payout (or not). The payout could be a positive real number (success or win) or zero (failure or lose). Every time we play the machine we build up information of p, the probability of success, and can compare it to other machines (or Charity interventions; all learning is carried out via the means of trial-and-error and value estimation). Thus we are solving a [multi-armed bandit problem](https://towardsdatascience.com/solving-the-multi-armed-bandit-problem-b72de40db97c) with our schools deciding which slot-machines to gamble on.  
 
*Thus, how can we design a systematic strategy that adapts to these stochastic rewards?*
 
What strategy can we use to select the "best" bandit (student intervention) out of a group of bandits (student interventions). What is a unique bandit in this example? Is it every Charity Intervention? Or is it every charity-intervention-school combination? For simplicity we assume that each bandit has the same p regardless of the school it's in. We need to exploit good bandits but at the same time explore bandits that are poorly sampled, especially if bandits payout probability are non-stationary.  
 
We start off by using the `bayesAB` package which we used in a previous post focused on [A/B testing in R](https://www.machinegurning.com/rstats/bayes_r/). This helps us solve a two-armed bandit problem. Imagine we have two interventions to compare. `bayesTest` is the main driver function of the `bayesAB` package. The input takes two vectors of data, corresponding to recipe A and recipe B of an A/B test (the data we have accumulated through this blog post for the two treatments; A - "kale-and-yoga", B - "desert-island").  
 

{% highlight r %}
kale_and_yoga_binary <- df %>%
  dplyr::filter(treatment == "kale-and-yoga") %>%
  dplyr::select(success)
  
desert_island_binary <- df %>%
  dplyr::filter(treatment == "desert-island") %>%
  dplyr::select(success)
{% endhighlight %}
 
We use all available data combined with our beta distribution priors (assuming data is gernerated by Bernoulli i.i.d) to fit a Bayesian model. For our priors we assume an uninformative, Jeffrey's (alpha and beta set to 1/2).    
 
When asking the probability kale-and-yoga is better, we’re asking “if I picked a random draw from kale-and-yoga’s distribution and a random draw from desert-island’s, what’s the probability kale-and-yoga is higher”?
 

{% highlight r %}
library(bayesAB)
 
m1 <- bayesTest(A_data = kale_and_yoga_binary$success, B_data = desert_island_binary$success,
          priors = list("alpha" = 0.5,
                        "beta" = 0.5),
          n_samples = 1e+05,
  distribution = c("bernoulli"))
 
summary(m1)
{% endhighlight %}



{% highlight text %}
## Quantiles of posteriors for A and B:
## 
## $Probability
## $Probability$A
##        0%       25%       50%       75%      100% 
## 0.4164875 0.6581587 0.6991656 0.7375637 0.8981633 
## 
## $Probability$B
##        0%       25%       50%       75%      100% 
## 0.6402358 0.8513054 0.8813108 0.9072804 0.9850652 
## 
## 
## --------------------------------------------
## 
## P(A > B) by (0)%: 
## 
## $Probability
## [1] 0.00668
## 
## --------------------------------------------
## 
## Credible Interval on (A - B) / B for interval length(s) (0.9) : 
## 
## $Probability
##          5%         95% 
## -0.32928402 -0.07579463 
## 
## --------------------------------------------
## 
## Posterior Expected Loss for choosing B over A:
## 
## $Probability
## [1] 0.2682128
{% endhighlight %}
 
We could use the `plot` method on this object `m1` which is quite intuitive and provides pretty defaults. Instead we read off the answer to our above question from the `summary`, where the probability that A is better than B (A > B) is 0.6%. We are also reasonably confident that by choosing B over A next time for our students, they will benefit and we only have a 26% chance of them being worse off.  
 
It's important to continue to collect more data: we fit a multi-armed bandit object based on a `bayesTest` object which can serve recommendations and adapt to new data. Imagine that given our previous results  the `kale-and-yoga` had fallen out of fashion, it's still important for us to continue to collect the data if the intervention is still being used, we also update with the latest students' results to go through the intervention. We update the object with the `setResults()` method.   
 

{% highlight r %}
set.seed(36912)
 
# A was kale-and-yoga; B was desert-island
binomialBandit <- banditize(m1, higher_is_better = TRUE)
binomialBandit$setResults(list('A' = c(1, 0, 1, 0, 0, 1), 'B' = rbinom(n = 30, size = 1, prob = 0.75)))
{% endhighlight %}



{% highlight text %}
## [1] 0
{% endhighlight %}
 
The documentation tells us `banditize` is an 'object-oriented' implementation of multi-armed bandits in `bayesAB`. It is useful in conjunction with a Shiny app or Plumber deployment. The object itself is mutable and can adapt / learn from new data without having to re-assign the variable.  
 
We can see how many times it's been updated by using one of the five methods it comes with. This would allow us to keep track.  
 

{% highlight r %}
binomialBandit$getUpdates()
{% endhighlight %}



{% highlight text %}
## 1 updates to this bandit.
## A has had 6 additions with a mean of 0.5 .
## B has had 30 additions with a mean of 0.6666667 .
{% endhighlight %}
 
We can `serveRecipe()` to show the best intervention based on samples from both updated posteriors.  
 

{% highlight r %}
# desert-island is better than kale-and-yoga, B is better than A
binomialBandit$serveRecipe()
{% endhighlight %}



{% highlight text %}
## [1] "B"
{% endhighlight %}
 
Returns most up-to-date object that we can again plot or use summary on.  
 

{% highlight r %}
m2 <- binomialBandit$getBayesTest()
summary(m2)
{% endhighlight %}



{% highlight text %}
## Quantiles of posteriors for A and B:
## 
## $Probability
## $Probability$A
##        0%       25%       50%       75%      100% 
## 0.4300950 0.6412312 0.6806889 0.7184018 0.8914915 
## 
## $Probability$B
##        0%       25%       50%       75%      100% 
## 0.5989390 0.7814019 0.8102866 0.8370761 0.9419873 
## 
## 
## --------------------------------------------
## 
## P(A > B) by (0)%: 
## 
## $Probability
## [1] 0.03156
## 
## --------------------------------------------
## 
## Credible Interval on (A - B) / B for interval length(s) (0.9) : 
## 
## $Probability
##          5%         95% 
## -0.29190583 -0.01900606 
## 
## --------------------------------------------
## 
## Posterior Expected Loss for choosing B over A:
## 
## $Probability
## [1] 0.1998586
{% endhighlight %}
 
This demonstrates the ease at which you can continuously update your understanding of the world without falling foul of problems associated with the Frequentist approach, such as repeated testing. It also is much easier to explain the findings to non-statisticians. Would you rather say “the probability that A is greater than B is 10%”, or “Assuming the null hypothesis that A and B are equal is true, the probability that we would see a result this extreme in A vs B is equal to 3%”? Furthermore, since we get a probability distribution over the parameters of the distributions of A and B, we can say something such as “There is a 74.2% chance that A’s λ is between 0.4 and 0.55.” directly from the methods themselves (paraphrased from the `bayesAB` vignette).  
 
### Building a model for the priors
 
Can we use the Bayesian approach which has some advantages above to model all interventions an organisation might encounter. For example, a school MAT might have up to fifty charities offering interventions who they have paid for before. If data were collected on the effect of those interventions on the students, could we build a model to capture this and help the organisation make evidence-based decisions (combined with cost-benefit analysis) for the school budget and its students' future. A MAT may also have the scale and purchasing power to insist upon those offering intervention-provision; data-collection, assessment before and after, use of a comparison group and use of standard intervention procedure guidance for implementers.  This is non-trivial and I've probably only scratched the surface.  
 
We probably want a unique model for each organisation or MAT to deploy, that has a unique prior for each intervention type (so that we control for the relationship between the number of students exposed to an intervention and the intervention's true impact average). A beta-binomial regression could be used to control for this and could also incorporate information (specifically the number of students a charity intervention had "treated") into a per-intervention prior distribution. This method could be extended: in particular, the model could include other factors that might change our prior expectations of an intervention. These are each particular applications of Bayesian hierarchical modeling, where the priors for each intervention are not fixed, but rather depend on other latent variables. In an empirical Bayesian approach to hierarchical modeling, one could estimate the prior using beta binomial regression, and then apply it to each intervention (Chapter 8, Robinson).   
 
For example based on meta-analysis and literature review provided by other organisations, like the [Education Endowment Fund (EEF)](https://educationendowmentfoundation.org.uk/school-themes/character/), we could adjust an intervention's priors based on such evidence bases. Part of the philosophy of the Bayesian approach is to bring our prior expectations in mathematically. Let’s try doing that with some factors that influence intervention success on student character development through intervention.  
 
For example, an intervention that is based on [Metacognition and self-regulation](https://educationendowmentfoundation.org.uk/evidence-summaries/teaching-learning-toolkit/meta-cognition-and-self-regulation/) might have a more optimistic prior than an [Aspiration based intervention](https://educationendowmentfoundation.org.uk/evidence-summaries/teaching-learning-toolkit/aspiration-interventions/). We could capture this in a beta-binomial regression model (in the Generalised Additive Model family for location, site and scale (GAMLSS)), the structure of which looks very similar to the `glm` earlier in this blog post (GAMLSS are univariate distributional regression models, where all the parameters of the assumed distribution for the response can be modelled as additive functions of the explanatory variables). Thus if we had these characteristics of the interventions being offered by charities we might be able to build up a model that better captures our prior distributions.      
 
Broadly, this approach can be summarised by this quandary:
 
Suppose you were a head-teacher inspecting potential training-interventions, and were choosing between two that have "treated" 100 students each:  
* A metacognition-based intervention that has had 30 successes / 100 trials   
* An aspiration-based intervention that has had 30 successes / 100 trials   
 
Which would you guess was the better intervention? This seems like a silly question: they both have the same exact success record. But what if I told you that historically, metacognition-based interventions are slightly better aspiration-based interventions? How could you incorporate that evidence?  
 
We simulate some data of interventions that look promising. They are new on the scene, how do we decide what their prior should be?  
 

{% highlight r %}
set.seed(67)
 
# generate some data
# 100L, the L makes it an integer, no real reason
 
interventions <- bind_rows(tidyr::crossing(intervention_id = letters[1:13],
                                 flavour = c("metacognition"),
                                 successes = rbinom(n = 13, 50, prob = 0.8),
                                 trials = 100L),
                           tidyr::crossing(intervention_id = letters[13:26],
                                 flavour = c("aspiration"),
                                 successes = rbinom(n = 13, 50, prob = 0.5),
                                 trials = 100L)
                           ) %>%
  mutate(flavour = as.factor(flavour))
 
glimpse(interventions)
{% endhighlight %}



{% highlight text %}
## Observations: 217
## Variables: 4
## $ intervention_id <chr> "a", "a", "a", "a", "a", "a", "a", "b", "b", "...
## $ flavour         <fctr> metacognition, metacognition, metacognition, ...
## $ successes       <int> 36, 37, 38, 39, 40, 41, 43, 36, 37, 38, 39, 40...
## $ trials          <int> 100, 100, 100, 100, 100, 100, 100, 100, 100, 1...
{% endhighlight %}
 
We don't run the code here as it was causing our rmarkdown to markdown function to fail for a reason we couldn't debug quickly. Thus instead we show the working for you to implement yourself and print the model output.  
 

{% highlight r %}
# this package can help, has some decent tutorials and demos, is also used in David Robinson's book
library(gamlss)
 
# like a glm earlier we cbind successes and failures
# we also control for those Charities' interventions that have been used less, 
# Maybe their success average is lower just because of higher variance associated with lower trials
# in this data all our trials are the same but we include it here just 'cos
fit1 <- gamlss(cbind(successes, trials - successes) ~ log(trials) + flavour, data = interventions,
               family = BB(mu.link = "identity"))
{% endhighlight %}
 
This fits a model that allows the mean successfully improving grit-score average µ to depend on the number of students that had been "treated" by an intervention as well as controlling for the "flavour" (is it metacognition and self-regulation-based or aspiration-based) of the intervention. This would help an organisation or group of data-sharing organisations to determine if there is a statistically significant advantage to prioritising the use of different flavours of interventions.  
 
We can then look at the coefficients using the `tidy()` function from broom.  
 

{% highlight r %}
# you should try, pretty output
broom::tidy(fit1)
{% endhighlight %}
 
We print the output here for your convenience.  
 

{% highlight r %}
# this is what the model output looks like
tibble::tibble(parameter = c("mu", "mu", "sigma"),
               term = c("intercept", "flavourmetacognition", "intercept"),
               estimate = c(0.24, 0.14, -35.96),
               std_error = c(0.0040, 0.0068, 0.0033),
               statistic = c(61.88, 21.47, -10879.74),
               p_value = c(5e-139, 2e-55, 0))
{% endhighlight %}



{% highlight text %}
## # A tibble: 3 x 6
##   parameter                 term estimate std_error statistic p_value
##       <chr>                <chr>    <dbl>     <dbl>     <dbl>   <dbl>
## 1        mu            intercept     0.24    0.0040     61.88  5e-139
## 2        mu flavourmetacognition     0.14    0.0068     21.47   2e-55
## 3     sigma            intercept   -35.96    0.0033 -10879.74   0e+00
{% endhighlight %}
 
 
According to our beta-binomial regression, there is indeed a statistically significant advantage to being a "metacognition" flavoured intervention, with these intervention types getting a succesful improvement in grit-score about 16% more often. It has standard interpretation rules for R summary tables of a model. To get the mean probability of success for "metacognition" flavoured interventions we add the mu intercept ecoefficient of 0.23 to 0.16, to get 0.39, the mean.  
 

{% highlight r %}
sum(filter(interventions, flavour == "metacognition")$successes) / sum(filter(interventions, flavour == "metacognition")$trials)
{% endhighlight %}



{% highlight text %}
## [1] 0.3914286
{% endhighlight %}
 
If we were to use [empirical Bayes estimation](https://www.r-bloggers.com/introducing-the-ebbr-package-for-empirical-bayes-estimation-using-baseball-statistics/), it would mean that every combination of flavour and number of trials would have its own prior. The prior can still be overcome with enough evidence. Other things to consider might be time; when the experiments were conducted. Controlling for time by including it in the model could identify novelty effects, for example, after controlling for time since an intervention was deployed, it might turn out its not very effective. This could be due to the novelty wearing off and a more general observation that students change through time and may be habituated to some appraoches.  
 
## When to try something new  
 
Thus far we've thought about quantifying impact and updating knowledge with new evidence, and how one might model it. Now we consider how one might determine what interventions to try out, as after all, typically you might just recommend picking the one with the highest mean success rate. However, this leaves you closed off to any new innovation. This is the classic [explore / exploit dilemma](http://www.princeton.edu/~rcw2/papers/WilsonEtAl_RLDM2013_horizon.pdf): the tradeoff between pursuing a known reward and sampling unknown, potentially better opportunities (Wilson et al., 2013). This dilemma is famously embodied by the N-armed bandit problem, where a gambler must decide whether to keep playing at a slot machine, or to try playing at a new one.  
 
In our scenario, not all bandits have the same entry fee and same payout chance. There is also some autonomy demanded by schools, where teachers and students may have a preference for certain interventions for various extraneous reasons. With this caveat, there are [several standard approaches](https://www.datasciencecentral.com/profiles/blogs/some-reinforcement-learning-the-greedy-and-explore-exploit) to this dilemma: 
 
* The Naive Round-Robin algorithm basically chooses every action once to complete a round and repeats the rounds. Obviously it’s far from optimal since it explores too much and exploits little.  
* Pick the bandit with the highest upper 95% credible interval. This probably explores too much, and may suffer when you have many bandits and few coins to play.  
* The greedy algorithm tries to choose the arm that has maximum average reward, with the drawback that it may lock-on to a sub-optimal action forever.  
* The epsilon greedy and optimistic greedy algorithms are variants of the greedy algorithm that try to recover from the drawback of the greedy algorithm. Epsilon-greedy chooses an action uniformly at random with probability epsilon, whereas the optimistic greedy algorithm initialized the estimated reward for each action to a high value, in order to prevent locking to a sub-optimal action.  
 
## Deploying the bandit as a JSON API
 
Remember our bandit from earlier? We can turn the `bayesBandit` object called `m2` into an API and serve/update requests through HTTP. 
 

{% highlight r %}
deployBandit(m2, port = 8000)
{% endhighlight %}
 
 
`deployBandit` turns a Bayesian bandit into a JSON API that accepts curl requests. Two of the five methods of `bayesBandit` classes are exposed: `serveRecipe` and `setResults`. Assuming the API is deployed on localhost this is an example of how it would be hit:
 

{% highlight r %}
curl http://localhost:8000/serveRecipe
 
curl --data '{"A":[1, 0, 1, 1], "B":[0, 0, 0, 1]}' http://localhost:8000/setResults
{% endhighlight %}
 
Thus, we could extend this to create a Shiny app.  
 
## Conclusion
 
Charities and training-providers are increasingly required to demonstrate that they are achieving impact, based on robust evaluation of their work. While it is important to ensure resources are being used effectively, undertaking evaluation often poses challenges for charities, which may well have limited resources or experience with which to do this.  
 
Charities need to know about the impact of their activities so that they can ensure their efforts are having their intended effect. That's not to say that impact evaluation will necessarily be able to capture all of the valuable outcomes produced as a result of a charity's work - some outcomes may be extremely difficult or impossible to measure, and it doesn't mean those outcomes are not important. However, just because it may be difficult to measure the impact of something, it doesn't mean we shouldn't try!  
 
Given the complexities described above and the esoteric language used, it strikes me that it would be helpful for larger organisations or programmes to help design and demonstrate best practice or for some tech start-ups to build some apps to facilitate this process. For example, in the school domain, MATs presumably have some purchasing power and could insist upon some standards for Charity intervention impact evaluation frameworks, an app could help ensure the consistency of any intervention delivery and assessment, the outcome of which could be served to the school leaders across the network. This would ensure quality experimental design which would output better data which could then be analysed and interpreted in a better way.  
 
In this blog post we introduced some Frequentist and Bayesian approaches to answering some of the typical questions that an organisation's leader might face; how good is an intervention, which is better between these two and how can we learn from previous experience in the literature using informative priors and considerations for the explore/exploit dilemma.  
 
## References
 
* [Hardert et al. (1999). Best Evidence Medical Education](https://www.ncbi.nlm.nih.gov/pubmed/21281174) 
* Crawley, M. (2007). The R Book. John Wiley & Sons.  
* LearnBayes package pdf in R, http://fportman.com/blog/bayesab-a-new-r-package-for-bayesian-ab-testing/  
* proportion package pdf in R  
* Robinson, David (200?) Introduction to Empirical Bayes and [blog](http://varianceexplained.org/)  
* Sullivan, G. M. (2011). Getting Off the “Gold Standard”: Randomized Controlled Trials and Education Research. Journal of Graduate Medical Education, 3(3), 285–289. http://doi.org/10.4300/JGME-D-11-00147.1  
* Walser, Tamara M. (2014). Quasi-Experiments in Schools: The Case for Historical Cohort Control Groups.
Practical Assessment, Research & Evaluation, 19(6). Available online:
http://pareonline.net/getvn.asp?v=19&n=6  
* Wilson, R. C., Geana, A., White, J. M., Ludvig, E. A. & Cohen, J. D. (2013). Exploration strategies in human decision making. Reinforcement Learning & Decision Making  
 

{% highlight r %}
devtools::session_info()
{% endhighlight %}



{% highlight text %}
##  setting  value                       
##  version  R version 3.4.3 (2017-11-30)
##  system   x86_64, darwin15.6.0        
##  ui       RStudio (1.1.419)           
##  language (EN)                        
##  collate  en_GB.UTF-8                 
##  tz       Europe/London               
##  date     2018-05-14                  
## 
##  package      * version    date      
##  assertthat     0.2.0      2017-04-11
##  backports      1.1.1      2017-09-25
##  base         * 3.4.3      2017-12-07
##  base64enc      0.1-3      2015-07-28
##  bayesAB      * 1.1.0      2017-09-26
##  bindr          0.1        2016-11-13
##  bindrcpp     * 0.2        2017-06-17
##  broom          0.4.3      2017-11-20
##  caret        * 6.0-78     2017-12-10
##  cellranger     1.1.0      2016-07-27
##  class          7.3-14     2015-08-30
##  cli            1.0.0      2017-11-05
##  codetools      0.2-15     2016-10-05
##  colorspace     1.3-2      2016-12-14
##  compiler       3.4.3      2017-12-07
##  crayon         1.3.4      2017-09-16
##  CVST           0.2-1      2013-12-10
##  datasets     * 3.4.3      2017-12-07
##  ddalpha        1.3.1      2017-09-27
##  DEoptimR       1.0-8      2016-11-19
##  deSolve      * 1.20       2017-07-14
##  devtools       1.13.4     2017-11-09
##  digest         0.6.12     2017-01-27
##  dimRed         0.1.0      2017-05-04
##  dplyr        * 0.7.4      2017-09-28
##  DRR            0.0.2      2016-09-15
##  e1071        * 1.6-8      2017-02-02
##  evaluate       0.10.1     2017-06-24
##  forcats      * 0.2.0      2017-01-23
##  foreach        1.4.3      2015-10-13
##  foreign        0.8-69     2017-06-22
##  gamlss       * 5.0-8      2018-04-30
##  gamlss.data  * 5.0-1      2018-04-30
##  gamlss.dist  * 5.0-5      2018-04-30
##  ggplot2      * 2.2.1      2016-12-30
##  glue           1.2.0      2017-10-29
##  gower          0.1.2      2017-02-23
##  graphics     * 3.4.3      2017-12-07
##  grDevices    * 3.4.3      2017-12-07
##  grid           3.4.3      2017-12-07
##  gtable         0.2.0      2016-02-26
##  haven          1.1.0      2017-07-09
##  here         * 0.1        2017-11-24
##  highr          0.6        2016-05-09
##  hms            0.4.0      2017-11-23
##  httr           1.3.1      2017-08-20
##  ipred          0.9-6      2017-03-01
##  iterators      1.0.8      2015-10-13
##  jsonlite       1.5        2017-06-01
##  keras        * 2.1.4.9000 2018-03-04
##  kernlab        0.9-25     2016-10-03
##  knitr          1.18       2017-12-27
##  labeling       0.3        2014-08-23
##  lattice      * 0.20-35    2017-03-25
##  lava           1.5.1      2017-09-27
##  lazyeval       0.2.1      2017-10-29
##  LearnBayes   * 2.15.1     2018-03-18
##  lubridate      1.7.1      2017-11-03
##  magrittr       1.5        2014-11-22
##  MASS         * 7.3-47     2017-02-26
##  Matrix       * 1.2-12     2017-11-20
##  memoise        1.1.0      2017-04-21
##  methods      * 3.4.3      2017-12-07
##  minqa          1.2.4      2014-10-09
##  mnormt         1.5-5      2016-10-15
##  ModelMetrics   1.1.0      2016-08-26
##  modelr         0.1.1      2017-07-24
##  munsell        0.4.3      2016-02-13
##  nlme         * 3.1-131    2017-02-06
##  nnet           7.3-12     2016-02-02
##  openxlsx     * 4.0.17     2017-03-23
##  parallel     * 3.4.3      2017-12-07
##  pkgconfig      2.0.1      2017-03-21
##  plyr           1.8.4      2016-06-08
##  prodlim        1.6.1      2017-03-06
##  psych          1.7.8      2017-09-09
##  purrr        * 0.2.4      2017-10-18
##  R6             2.2.2      2017-06-17
##  Rcpp           0.12.15    2018-01-20
##  RcppRoll       0.2.2      2015-04-05
##  readr        * 1.1.1      2017-05-16
##  readxl       * 1.0.0      2017-04-18
##  recipes        0.1.1      2017-11-20
##  reshape2       1.4.3      2017-12-11
##  reticulate     1.5        2018-02-14
##  rlang          0.2.0      2018-02-20
##  rmd2md         0.1.1      2017-06-02
##  robustbase     0.92-8     2017-11-01
##  rpart          4.1-11     2017-03-13
##  rprojroot      1.2        2017-01-16
##  rstudioapi     0.7        2017-09-07
##  Rtsne        * 0.13       2017-04-14
##  rvest          0.3.2      2016-06-17
##  scales         0.5.0      2017-08-24
##  sfsmisc        1.1-1      2017-06-08
##  simecol      * 0.8-10     2017-10-22
##  splines      * 3.4.3      2017-12-07
##  stats        * 3.4.3      2017-12-07
##  stats4         3.4.3      2017-12-07
##  stringi        1.1.6      2017-11-17
##  stringr      * 1.2.0      2017-02-18
##  survival       2.41-3     2017-04-04
##  tensorflow     1.5        2018-01-17
##  tfruns         1.3        2018-02-18
##  tibble       * 1.3.4      2017-08-22
##  tidyr        * 0.7.2      2017-10-16
##  tidyselect     0.2.4      2018-02-26
##  tidyverse    * 1.2.1      2017-11-14
##  tidyxl       * 1.0.0      2017-11-26
##  timeDate       3042.101   2017-11-16
##  tools          3.4.3      2017-12-07
##  utils        * 3.4.3      2017-12-07
##  whisker        0.3-2      2013-04-28
##  withr          2.1.0      2017-11-01
##  xml2           1.2.0      2018-01-24
##  yaml           2.1.17     2018-02-27
##  zeallot        0.1.0      2018-01-28
##  source                                   
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.2)                           
##  local                                    
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.2)                           
##  cran (@0.1)                              
##  cran (@0.2)                              
##  CRAN (R 3.4.1)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.2)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.0)                           
##  local                                    
##  CRAN (R 3.4.1)                           
##  CRAN (R 3.4.0)                           
##  local                                    
##  CRAN (R 3.4.2)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.1)                           
##  cran (@1.13.4)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.0)                           
##  cran (@0.7.4)                            
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.0)                           
##  cran (@0.10.1)                           
##  CRAN (R 3.4.0)                           
##  cran (@1.4.3)                            
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.0)                           
##  cran (@1.2.0)                            
##  CRAN (R 3.4.0)                           
##  local                                    
##  local                                    
##  local                                    
##  CRAN (R 3.4.0)                           
##  cran (@1.1.0)                            
##  Github (krlmlr/here@93593ee)             
##  CRAN (R 3.4.0)                           
##  cran (@0.4.0)                            
##  cran (@1.3.1)                            
##  CRAN (R 3.4.0)                           
##  cran (@1.0.8)                            
##  cran (@1.5)                              
##  Github (rstudio/keras@3715d5e)           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.2)                           
##  cran (@0.2.1)                            
##  CRAN (R 3.4.4)                           
##  CRAN (R 3.4.2)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.0)                           
##  local                                    
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.1)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.0)                           
##  local                                    
##  cran (@2.0.1)                            
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.1)                           
##  cran (@0.2.4)                            
##  cran (@2.2.2)                            
##  cran (@0.12.15)                          
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.1)                           
##  cran (@1.5)                              
##  cran (@0.2.0)                            
##  Github (ivyleavedtoadflax/rmd2md@3434815)
##  CRAN (R 3.4.2)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.1)                           
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.0)                           
##  cran (@0.5.0)                            
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.2)                           
##  local                                    
##  local                                    
##  local                                    
##  cran (@1.1.6)                            
##  CRAN (R 3.4.0)                           
##  CRAN (R 3.4.3)                           
##  CRAN (R 3.4.3)                           
##  cran (@1.3)                              
##  cran (@1.3.4)                            
##  cran (@0.7.2)                            
##  cran (@0.2.4)                            
##  CRAN (R 3.4.2)                           
##  Github (nacnudus/tidyxl@2481967)         
##  CRAN (R 3.4.2)                           
##  local                                    
##  local                                    
##  CRAN (R 3.4.0)                           
##  cran (@2.1.0)                            
##  cran (@1.2.0)                            
##  cran (@2.1.17)                           
##  cran (@0.1.0)
{% endhighlight %}
