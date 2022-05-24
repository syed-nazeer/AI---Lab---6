---
title: "Lab Assignment -5"
author: "chakradhar"
date: "30/03/2022"
output: 
  html_document: 
    toc: yes
    fig_caption: yes
    keep_md: yes
  pdf_document: 
    toc: yes
    fig_caption: yes
    keep_tex: yes
  word_document: 
    toc: yes
    fig_caption: yes
  html_notebook: 
    toc: yes
    fig_caption: yes
---



# Lab Assignment 5
Question -
Understand the graphical models for inference under uncertainty, build Bayesian Network in R, Learn the structure and CPTs from Data, naive Bayes classification with dependency between features.


```r
library(bnlearn)
library(epiDisplay)
```

```
## Loading required package: foreign
```

```
## Loading required package: survival
```

```
## Loading required package: MASS
```

```
## Loading required package: nnet
```

```r
library(bnclassify)
```

```
## 
## Attaching package: 'bnclassify'
```

```
## The following objects are masked from 'package:bnlearn':
## 
##     modelstring, narcs, nparams
```

```r
library(caret)
```

```
## Loading required package: ggplot2
```

```
## 
## Attaching package: 'ggplot2'
```

```
## The following object is masked from 'package:bnclassify':
## 
##     vars
```

```
## The following object is masked from 'package:epiDisplay':
## 
##     alpha
```

```
## Loading required package: lattice
```

```
## 
## Attaching package: 'lattice'
```

```
## The following object is masked from 'package:epiDisplay':
## 
##     dotplot
```

```
## 
## Attaching package: 'caret'
```

```
## The following object is masked from 'package:survival':
## 
##     cluster
```


```r
grades.grades <- read.csv('C:\\Users\\chakr\\OneDrive\\Desktop\\2020_bn_nb_data.txt', sep = "\t", head = TRUE, stringsAsFactors=TRUE)
grades.courses <- grades.grades[, -9]
```


```r
head(grades.grades)
```

```
##   EC100 EC160 IT101 IT161 MA101 PH100 PH160 HS101 QP
## 1    BC    CC    BB    BC    CC    BC    AA    BB  y
## 2    CC    BC    BB    BB    CC    BC    AB    BB  y
## 3    AB    BB    AB    AB    BB    CC    BC    AB  y
## 4    BC    CC    BB    BB    BB    BB    BC    BB  y
## 5    BC    AB    CD    BC    BC    BC    BC    CD  y
## 6    DD    CC    DD    CD    CD    CC    BC    BC  n
```


```r
summary(grades.grades)
```

```
##      EC100        EC160        IT101        IT161        MA101        PH100   
##  BC     :48   BC     :59   BC     :49   BC     :49   BC     :54   CC     :40  
##  CC     :36   CC     :47   CC     :42   CC     :42   BB     :52   BB     :36  
##  BB     :35   CD     :37   CD     :35   BB     :35   CC     :49   BC     :34  
##  F      :35   BB     :31   BB     :34   CD     :35   CD     :24   CD     :30  
##  CD     :29   DD     :22   AB     :25   AB     :25   DD     :21   AA     :26  
##  AB     :22   AB     :16   DD     :23   DD     :23   F      :15   AB     :26  
##  (Other):27   (Other):20   (Other):24   (Other):23   (Other):17   (Other):40  
##      PH160        HS101    QP     
##  BC     :68   AA     :42   n: 72  
##  CC     :43   BB     :40   y:160  
##  AB     :32   BC     :36          
##  BB     :30   AB     :34          
##  AA     :27   DD     :29          
##  CD     :21   CC     :26          
##  (Other):11   (Other):25
```

## Part 1) Learning dependencies between the courses
We need to learn the dependencies between the courses using two models, they are K2 and bic.

Using the hill climbing greedy search function ’hc’ which is present in the ’bnlearn’ package in R, one can learn
the structure of the Bayesian network and the dependencies between grades obtained in different courses. Hill climbing is a score-based structure learning algorithm. Since our dataset is categorical in nature, we learn the discrete Bayesian network - k2 and bic and compare both of them
### Using k2 score


```r
grades.hc.k2 <- hc(grades.courses, score = "k2")
grades.hc.k2
```

```
## 
##   Bayesian network learned via Score-based methods
## 
##   model:
##    [IT161][IT101|IT161][MA101|IT101][HS101|IT101][EC100|MA101][PH160|HS101]
##    [EC160|EC100][PH100|EC100]
##   nodes:                                 8 
##   arcs:                                  7 
##     undirected arcs:                     0 
##     directed arcs:                       7 
##   average markov blanket size:           1.75 
##   average neighbourhood size:            1.75 
##   average branching factor:              0.88 
## 
##   learning algorithm:                    Hill-Climbing 
##   score:                                 Cooper & Herskovits' K2 
##   tests used in the learning procedure:  105 
##   optimized:                             TRUE
```


```r
plot(grades.hc.k2, main = "Hill Climbing with k2 score")
```

![](Lab-5_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
we can see 7 edges in the above Fig. 1 and the model string showing this dependency of the graph is discussed in lab report.
### Using bic score
The Bayesian Information Criterion, or BIC for short, is a method for scoring and selecting a model. It is named for the field of study from which it was derived: Bayesian probability and inference. Like AIC, it is appropriate for models fit under the maximum likelihood estimation framework.

```r
grades.hc.bic <- hc(grades.courses, score = "bic")
grades.hc.bic
```

```
## 
##   Bayesian network learned via Score-based methods
## 
##   model:
##    [EC100][EC160][IT101][IT161][PH160][HS101][MA101|EC100][PH100|EC100] 
##   nodes:                                 8 
##   arcs:                                  2 
##     undirected arcs:                     0 
##     directed arcs:                       2 
##   average markov blanket size:           0.50 
##   average neighbourhood size:            0.50 
##   average branching factor:              0.25 
## 
##   learning algorithm:                    Hill-Climbing 
##   score:                                 BIC (disc.) 
##   penalization coefficient:              2.723369 
##   tests used in the learning procedure:  42 
##   optimized:                             TRUE
```


```r
plot(grades.hc.bic, main = "Hill Climbing with bic score")
```

![](Lab-5_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
There are only 2 edges in the above and the model string is discussed in the report.
## Part 2) Learning the CPTs for each node
As from the above figures we can see that figure 1 portrays the dependencies more accurately and clearly than the other, and the k2 approach is made specially to work on the large data-sets effectively.As a result, we'll only use the network learned using the k2 score in the following sections.

```r
grades.courses.fitted <- bn.fit(grades.hc.k2, grades.courses)
```


```r
grades.courses.plots <- lapply(grades.courses.fitted, bn.fit.barchart)
```

![](Lab-5_files/figure-html/unnamed-chunk-10-1.png)<!-- -->![](Lab-5_files/figure-html/unnamed-chunk-10-2.png)<!-- -->![](Lab-5_files/figure-html/unnamed-chunk-10-3.png)<!-- -->![](Lab-5_files/figure-html/unnamed-chunk-10-4.png)<!-- -->![](Lab-5_files/figure-html/unnamed-chunk-10-5.png)<!-- -->![](Lab-5_files/figure-html/unnamed-chunk-10-6.png)<!-- -->![](Lab-5_files/figure-html/unnamed-chunk-10-7.png)<!-- -->![](Lab-5_files/figure-html/unnamed-chunk-10-8.png)<!-- -->

## Part 3) What grade will a student get in PH100 if he earns DD in EC100, CC in IT101 and CD in MA101


```r
grades.courses.PH100Grade <- data.frame((cpdist(grades.courses.fitted, nodes=c("PH100"), evidence= (EC100 == "DD") & (IT101 == "CC") & (MA101 == "CD"))))
tab1(grades.courses.PH100Grade, sort.group = "increasing", main = "Distribution of grades in PH100 with given evidence")
```

![](Lab-5_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```
## grades.courses.PH100Grade : 
##         Frequency Percent Cum. percent
## AA              0     0.0          0.0
## AB              0     0.0          0.0
## BC              0     0.0          0.0
## BB             11     4.9          4.9
## F              20     8.9         13.8
## DD             42    18.8         32.6
## CC             45    20.1         52.7
## CD            106    47.3        100.0
##   Total       224   100.0        100.0
```

Therefore, the student is most likely to earn a CD grade.

## Splitting Data into test and train sets


```r
split <- sample(c(rep(0, 0.7*nrow(grades.grades)), rep(1, 0.3*nrow(grades.grades))))

table(split)
```

```
## split
##   0   1 
## 162  69
```


```r
data_train <- grades.grades[split == 0,]
data_test <- grades.grades[split == 1,]
head(data_test)
```

```
##    EC100 EC160 IT101 IT161 MA101 PH100 PH160 HS101 QP
## 11    BC    BC    BC    BC    BC    BC    BB    BC  y
## 16    BC    CC    CD    CC    BB    AA    BB    AA  y
## 21    AA    AA    AA    AA    AB    AA    BB    AA  y
## 31    BB    BC    CC    BC    BC    BB    CC    CD  y
## 35    BC    CC    AB    AB    BB    AA    AB    AA  y
## 36    BC    BC    CD    CD    BC    BB    BC    BC  y
```

```r
split_data <- function() {
  split <- sample(c(rep(0, 0.7*nrow(grades.grades)), rep(1, 0.3*nrow(grades.grades))))
  data_train <- grades.grades[split == 0,]
  data_test <- grades.grades[split == 1,]
  list("data_train" = data_train, "data_test" = data_test)
}
```

## Part 4) Naive Bayes classifier for independent data


```r
nb.grades_indep <- nb(class = "QP", dataset = data_train)
nb.grades_indep <- lp(nb.grades_indep, data_train, smooth = 0)
plot(nb.grades_indep)
```

![](Lab-5_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


```r
p_indep <- predict(nb.grades_indep, data_test)
confusionMatrix(p_indep, data_test$QP)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  n  y
##          n 23  0
##          y  2 44
##                                           
##                Accuracy : 0.971           
##                  95% CI : (0.8992, 0.9965)
##     No Information Rate : 0.6377          
##     P-Value [Acc > NIR] : 2.627e-11       
##                                           
##                   Kappa : 0.9362          
##                                           
##  Mcnemar's Test P-Value : 0.4795          
##                                           
##             Sensitivity : 0.9200          
##             Specificity : 1.0000          
##          Pos Pred Value : 1.0000          
##          Neg Pred Value : 0.9565          
##              Prevalence : 0.3623          
##          Detection Rate : 0.3333          
##    Detection Prevalence : 0.3333          
##       Balanced Accuracy : 0.9600          
##                                           
##        'Positive' Class : n               
## 
```

```r
for (i in 1:20){
  data <- split_data()
  data_test <- data$data_test
  data_train <- data$data_train
  nb.grades_indep <- nb(class = "QP", dataset = data_train)
  nb.grades_indep <- lp(nb.grades_indep, data_train, smooth = 0)
  p_indep <- predict(nb.grades_indep, data_test)
  print(accuracy(p_indep, data_test$QP))
}
```

```
## [1] 0.9857143
## [1] 0.9710145
## [1] 0.9571429
## [1] 0.9710145
## [1] 0.942029
## [1] 0.9714286
## [1] 0.9142857
## [1] 0.9714286
## [1] 0.9571429
## [1] 0.9571429
## [1] 0.9130435
## [1] 0.9855072
## [1] 0.9855072
## [1] 1
## [1] 0.9565217
## [1] 0.9285714
## [1] 0.9571429
## [1] 0.9710145
## [1] 0.9275362
## [1] 1
```

## Part 5) Naive Bayes classifier for dependent data


```r
nb.grades_dep <- tan_cl("QP", data_train)
nb.grades_dep <- lp(nb.grades_dep, data_train, smooth = 1)
plot(nb.grades_dep)
```

![](Lab-5_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
p_dep <- predict(nb.grades_dep, data_test)
confusionMatrix(p_dep, data_test$QP)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction  n  y
##          n 15  1
##          y  1 52
##                                           
##                Accuracy : 0.971           
##                  95% CI : (0.8992, 0.9965)
##     No Information Rate : 0.7681          
##     P-Value [Acc > NIR] : 2.929e-06       
##                                           
##                   Kappa : 0.9186          
##                                           
##  Mcnemar's Test P-Value : 1               
##                                           
##             Sensitivity : 0.9375          
##             Specificity : 0.9811          
##          Pos Pred Value : 0.9375          
##          Neg Pred Value : 0.9811          
##              Prevalence : 0.2319          
##          Detection Rate : 0.2174          
##    Detection Prevalence : 0.2319          
##       Balanced Accuracy : 0.9593          
##                                           
##        'Positive' Class : n               
## 
```



```r
for (i in 1:20){
  data <- split_data()
  data_test <- data$data_test
  data_train <- data$data_train
  nb.grades_dep <- tan_cl("QP", data_train)
  nb.grades_dep <- lp(nb.grades_dep, data_train, smooth = 1)
  p_dep <- predict(nb.grades_dep, data_test)
  print(accuracy(p_dep, data_test$QP))
}
```

```
## [1] 0.9855072
## [1] 0.9571429
## [1] 0.9428571
## [1] 0.9710145
## [1] 0.9855072
## [1] 0.9275362
## [1] 0.9275362
## [1] 0.942029
## [1] 0.9855072
## [1] 0.9130435
## [1] 0.9571429
## [1] 0.9565217
## [1] 0.9571429
## [1] 0.9710145
## [1] 0.942029
## [1] 0.9428571
## [1] 0.9275362
## [1] 0.9565217
## [1] 0.9710145
## [1] 0.9565217
```
