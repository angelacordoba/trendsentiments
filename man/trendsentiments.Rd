% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trendsentiments.R
\name{trendsentiments}
\alias{trendsentiments}
\title{trendsentiments}
\usage{
trendsentiments(country = "united states")
}
\arguments{
\item{country}{A country the user wants to analyze the Twitter trends from
written in lowercase. united states is the default.}
}
\value{
A table that shows the top 5 Twitter trends from the selected country
with its corresponding average sentiment.
}
\description{
The function visualizes the average sentiment of each of the top 5 Twitter
trends from a country the user selects.
}
\details{
The function pulls Twitter trends from the selected country and isolates the
top 5. Then it pulls 100 tweets from each trend, calculates the sentiment of
those and based on them it calculates the average sentiment of each trend.
Finally it visualizes the top 5 trends with its corresponding average sentiment.
The default country is "united states".
}
\examples{

trendsentiments(country = "colombia")

trendsentiments(country = "canada")

}
\author{
Angela Cordoba Perez
}
