trendsentiments
================
Angela Cordoba Perez
2021-05-02

A package that analyses the sentiment Twitter users have about trending
topics in a country by using
[rtweet](https://github.com/ropensci/rtweet) package to pull Twitter
data and by using the function that finds the average sentiment of a
variable from [sentimentr](https://github.com/trinker/sentimentr). This
README is based on Kurt Wirth’s excellent
[botscan](https://github.com/kurtawirth/botscan) documentation.

## Install

Install from GitHub with the following code:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}
devtools::install_github("angelacordoba/trendsentiments")
```

This package connects <code>rtweet</code> to <code>sentimentr</code>. As
a result, each user must have previously acquired authentication from
Twitter and instructions to do that [can be found
here](http://rtweet.info/articles/auth.html).

## Usage

Next time users start an R session on the same computer, rtweet should
automatically find their token. If they want to make sure it works, they
can run the following code and check that the app name and the api\_key
match.

library(rtweet)

get\_token()

Next, the entertainment begins with <code>trendsentiments</code>.

Its first and only argument takes the name of a country, in lowercase
letters and surrounded by quotation marks, that users want to
analyze the trends from.

The package will analyze 100 tweets from each of the top 5 trends of the
selected country. If the user does not specify a country when using the
function, the default is to analyze the trends from the United States.

``` r
## load trendsentiments
library(trendsentiments)

## Enter country surrounded by quotation marks
trendsentiments("canada")
#> [1]  trend               average_sentiment
#   1   #occupationhood     0.004160021
#   2   #TLMEP             -0.027170831
#   3   #90DayFiance       -0.005011163
#   4   #NationStarAc       0.068488918
#   5   #VWFC              -0.005308967
       

## Result is a table with the top 5 trends of Canada and their corresponding 
  #average sentiment based on tweets, and a bar graph visualizing the table.
```

![alt text](https://github.com/angelacordoba/trendsentiments/blob/main/images/Canada.png)


This process might take a couple of minutes while
<code>trendsentiments</code> streams tweets from the trends.

Twitter rate limits cap the number of Search results returned to 18,000
every 15 minutes. Thus, excessive use of <code>trendsentiments</code> in
a short amount of time may result in a warning and inability to pull
results. In this event, simply wait 15 minutes and try again. For more
details visit [rtweet](https://github.com/ropensci/rtweet).
