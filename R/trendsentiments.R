#' trendsentiments
#'
#' The function visualizes the average sentiment of each of the top 5 Twitter
#' trends from a country the user selects.
#'
#' The function pulls Twitter trends from the selected country and isolates the
#' top 5. Then it pulls 100 tweets from each trend, calculates the sentiment of
#' those and based on them it calculates the average sentiment of each trend.
#' Finally it visualizes the top 5 trends with its corresponding average sentiment.
#' The default country is "united states".
#'
#' @author Angela Cordoba Perez
#'
#' @param country A country the user wants to analyze the Twitter trends from
#' written in lowercase. united states is the default.
#'
#' @return A table that shows the top 5 Twitter trends from the selected country
#' with its corresponding average sentiment.
#'
#' @examples
#'
#' trendsentiments(country = "colombia")
#'
#' trendsentiments(country = "canada")
#'
#' @export

trendsentiments = function(country = "united states"){

  #Pull trends from the selected country

  trends = rtweet::get_trends(country)

  #Isolate top 5 trends

  isolated = trends %>% dplyr::select(trend) %>% head(trends, n = 5)

  #Create an object that lists the top 5 trends

  isolated = isolated$trend

  #Create an empty data frame

  average_sent = data.frame()

  #Start a loop to find the average sentiment for the top 5 trends in the country

  for(trend in isolated){

    #Pull tweets from the trend

    tweets1 = rtweet::search_tweets(trend)

    #Separate the sentences in each tweet

    tweet_text_sentences = sentimentr::get_sentences(tweets1$text)

    #Calculate the sentiment average for the sentences from the tweets

    sentiment_scores = sentimentr::sentiment_by(tweet_text_sentences) %>%
      select(-element_id)

    #Average the sentiments from all the sentences

    average_sentiment = mean(sentiment_scores$ave_sentiment)

    #Create a data frame with the name of the trend and its corresponding average
    #sentiment

    tablesentiments = data.frame(trend, average_sentiment)

    #Bind the empty data frame with the one that has the trend and average sentiment

    average_sent = dplyr::bind_rows(average_sent, tablesentiments)

  }

  #Create an object in the environment with the data frame that has each trend
  #with its corresponding sentiment

  assign("sentiment table", average_sent, envir = .GlobalEnv)

  #Visualize the sentiment table

  ggplot2::ggplot(`sentiment table`) +
    aes(x = trend, weight = average_sentiment) +
    geom_bar(fill = "#1fb5ce") +
    labs(x = "Trend", y = "Average sentiment") +
    theme_light()

}
