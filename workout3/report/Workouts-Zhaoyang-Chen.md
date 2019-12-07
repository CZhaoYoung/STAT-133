Workout3-Zhaoyang Chen
================
Zhaoyang Chen
12/6/2019

#### required packages

``` r
library(stringr)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
```

#### data preparation

``` r
A.B_citations <-as.data.frame(read.csv(file ='/home/chaoyoung/Desktop/workouts/workout3/data/cleandata/A.B_citations.csv'))

E.D_citations <-as.data.frame(read.csv(file ='/home/chaoyoung/Desktop/workouts/workout3/data/cleandata/E.D_citations.csv'))
```

#### Section 1

1)  For the two scholars, how many of their paper titles begin with a
    word that starts with a vowel, respectively?

<!-- end list -->

``` r
temp1 <- str_match(A.B_citations$article_title,"^[aeiou/AEIOU]")
temp1 <- na.omit(temp1)
# number of A.B's papers begin with a Vowel
ans1 <- length(temp1)
ans1
```

    ## [1] 118

``` r
temp2 <- str_match(E.D_citations$article_title,"^[aeiou/AEIOU]")
temp2 <- na.omit(temp2)
# number of A.B's papers begin with a Vowel
ans2 <- length(temp2)
ans2
```

    ## [1] 118

``` r
# The numbers of this two scholars' paper titles starting with a vowel are exactly the same! amazing!
```

2)  For the two scholars, how many of their paper titles end with “s”
    respectively?

<!-- end list -->

``` r
temp1 <- str_match(A.B_citations$article_title,"[s]$")
temp1 <- na.omit(temp1)
# number of A.B's papers end with "s"
ans1 <- length(temp1)
ans1
```

    ## [1] 78

``` r
temp2 <- str_match(E.D_citations$article_title,"[s]$")
temp2 <- na.omit(temp2)
# number of E.D's papers end with "s"
ans2 <- length(temp2)
ans2
```

    ## [1] 74

3)  For the two scholars, find the longest title, respectively
    (“longest” in terms of number of characters).

<!-- end list -->

``` r
# note which.max() will return the index of max number
temp1 <- str_length(A.B_citations$article_title)
ans1 <- A.B_citations$article_title[which.max(temp1)]
# The longest title
ans1
```

    ## [1] Voters be Primed to Choose Better Legislators? Experimental Evidence from Rural India,” October 2010. mimeo, Harvard Universiy. 4, 27, 29, Selvan Kumar, Rohini Pande, and Felix …
    ## 475 Levels: ¿ Cuál es tu evidencia? ...

``` r
temp2 <- str_length(E.D_citations$article_title)
ans2 <- E.D_citations$article_title[which.max(temp2)]
# The longest title
ans2
```

    ## [1] Controlling the costs of HIV/AIDS management--unique management software to empower organisations to formulate and implement strategies best suited for their specific requirements.
    ## 461 Levels: ‘Beating the Odds’ versus ‘Changing the Odds’: Poverty, Resilience, and Family Policy. ...

4)  For the two scholars, calculate the variable “number of punctuation
    symbols in the their titles”. Display summary() statistics of these
    variables, and the corresponding histograms.

<!-- end list -->

``` r
temp1 <- str_count(A.B_citations$article_title, "[[:punct:]]")
summary(temp1)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00    0.00    1.00    1.64    2.00   21.00

``` r
number_of_punctuations <- temp1
hist(number_of_punctuations, main="Frequency of Abhijit Banerjee's paper including punctuations")
```

![](Workouts-Zhaoyang-Chen_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
temp2 <- str_count(E.D_citations$article_title, "[[:punct:]]")
summary(temp2)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    0.00    0.00    1.00    1.54    2.00   21.00

``` r
number_of_punctuations <- temp2
hist(number_of_punctuations, main="Frequency of Esther Duflo's paper including punctuations")
```

![](Workouts-Zhaoyang-Chen_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

5)  Remove stop words(“the”, “a”, “an”, “and”, “in”, “if”, “but”),
    numbers and punctua- tions from the
titles.

<!-- end list -->

``` r
# add space " " after each stop words in case that str_remove will remove "if" in some words including "if", such as making "differ" as "dfer".

ans1 <- str_remove_all(A.B_citations$article_title, 
        "[[:punct:]]|[[:digit:]]|The |the |If |if |A |a |An|an |And |and |In |in |But |but ")

ans2 <- str_remove_all(E.D_citations$article_title, 
        "[[:punct:]]|[[:digit:]]|The |the |If |if |A |a |An|an |And |and |In |in |But |but ")
```

6)  Excluding stop words, numbers and punctuations, what are the 10 most
    frequent words in scholar A’s titles and scholar B’s titles?

**In this part, I encountered a problem.** I cannot remove extra
“spaces” from the title. I think there is something wrong with the
original title in the html file. So I returned the first 11 words,
including the meaningless word “space” and I believe my answer is
correct. :)

``` r
# 
# In this part, I will ues the answer from the previous questions!
# use str_split
mytext <- str_split(ans1, "[[:space:]]", simplify = FALSE)
# convert list as vector
mytext <- unlist(mytext, use.names=FALSE)
mytable_1 <- sort(table(mytext), decreasing=T)
# top 10 most frequent words of Abhijit Banerjee's paper 
mytable_1[1:11]
```

    ## mytext
    ##          of                      to        from    Evidence       India 
    ##         172         125          67          66          47          43 
    ##         for          DP Development          on   Economics 
    ##          37          30          25          22          21

``` r
mytext <- str_split(ans2, "[[:space:]]", simplify = FALSE)
# convert list as vector
mytext <- unlist(mytext, use.names=FALSE)
mytable_2 <- sort(table(mytext), decreasing=T)
# top 10 most frequent words of Esther Duflo's paper
mytable_2[1:11]
```

    ## mytext
    ##          of                    from          to    Evidence         for 
    ##         178         102          99          79          74          52 
    ##       India          DP          on  randomized Development 
    ##          49          36          33          33          25

#### Section 2 Wordcloud

Actually I think other meanless words also need to be delete, such as
“of”, “for”, but I still include them in the following wordcloud.

``` r
A.B_wordcloud_df <- as.data.frame(mytable_1)
A.B_wordcloud_df <- A.B_wordcloud_df[-c(2), ]
set.seed(1234)
png(filename = "/home/chaoyoung/Desktop/workouts/workout3/images/A.B_wordcloud.png")
wordcloud(words = A.B_wordcloud_df$mytext, freq = A.B_wordcloud_df$Freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()
```

    ## png 
    ##   2

``` r
E.D_wordcloud_df <- as.data.frame(mytable_2)
E.D_wordcloud_df <- E.D_wordcloud_df[-c(2), ]
png(filename = "/home/chaoyoung/Desktop/workouts/workout3/images/E.D_wordcloud.png")
wordcloud(words = E.D_wordcloud_df$mytext, freq = E.D_wordcloud_df$Freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
dev.off()
```

    ## png 
    ##   2

The image of wordcloud of Abhijit Banerjee

![Caption for the
picture.](/home/chaoyoung/Desktop/workouts/workout3/images/A.B_wordcloud.png)

The image of wordcloud of Esther Duflo

![Caption for the
picture.](/home/chaoyoung/Desktop/workouts/workout3/images/E.D_wordcloud.png)
