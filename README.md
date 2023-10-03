ST 558: Project 2
================
Chandler Ellsworth
2023-10-02

``` r
library(httr2)
library(jsonlite)
library(tidyverse)
```

``` r
token <- 'L8SLuL2Jzi8KI5g0iHQYaERuCZgEDvyxDpOvDTgSqrLWvU7/8Yd5XuAPCfZJjMCJ'

req <- request("https://api.collegefootballdata.com/talent?year=2022") %>% 
  req_auth_bearer_token(token) %>% 
  req_headers("Accept" = "application/json")

mydata <- req_perform(req)

mydf <- fromJSON(rawToChar(mydata$body))

mytib <- as_tibble(mydf[[1]])

mytib
```

    ## # A tibble: 233 × 1
    ##    value
    ##    <int>
    ##  1  2022
    ##  2  2022
    ##  3  2022
    ##  4  2022
    ##  5  2022
    ##  6  2022
    ##  7  2022
    ##  8  2022
    ##  9  2022
    ## 10  2022
    ## # ℹ 223 more rows

``` r
team_talent_composite_rankings <- function(year) {
  
  token <- 'L8SLuL2Jzi8KI5g0iHQYaERuCZgEDvyxDpOvDTgSqrLWvU7/8Yd5XuAPCfZJjMCJ'
  
  url <- paste0("https://api.collegefootballdata.com/talent?year=",year)
  
  req <- request(url) %>% 
    req_auth_bearer_token(token) %>% 
    req_headers("Accept" = "application/json")
  
  mydata <- req_perform(req)
  
  mydf <- fromJSON(rawToChar(mydata$body))
  
  mytib <- as_tibble(mydf)
  
  return(mytib)

}

team_talent_composite_rankings(2018)
```

    ## # A tibble: 237 × 3
    ##     year school        talent
    ##    <int> <chr>         <chr> 
    ##  1  2018 Ohio State    984.30
    ##  2  2018 Alabama       978.54
    ##  3  2018 Georgia       964.00
    ##  4  2018 USC           933.65
    ##  5  2018 Clemson       893.21
    ##  6  2018 LSU           889.91
    ##  7  2018 Florida State 888.75
    ##  8  2018 Michigan      862.35
    ##  9  2018 Texas         861.20
    ## 10  2018 Notre Dame    847.85
    ## # ℹ 227 more rows

``` r
coaching_history <- function(first_name, last_name) {
  token <- 'L8SLuL2Jzi8KI5g0iHQYaERuCZgEDvyxDpOvDTgSqrLWvU7/8Yd5XuAPCfZJjMCJ'
  
  url <- paste0("https://api.collegefootballdata.com/coaches?firstName=",first_name,"&lastName=",last_name)
  
  req <- request(url) %>% 
    req_auth_bearer_token(token) %>% 
    req_headers("Accept" = "application/json")
  
  mydata <- req_perform(req)
  
  mydf <- fromJSON(rawToChar(mydata$body))$seasons
  
  mytib <- as_tibble(mydf[[1]])
  
  return(mytib)
}

paste0("https://api.collegefootballdata.com/coaches?firstName=","Mack","&lastName=","Brown")
```

    ## [1] "https://api.collegefootballdata.com/coaches?firstName=Mack&lastName=Brown"

``` r
coaching_history("Mack", "Brown")
```

    ## # A tibble: 34 × 12
    ##    school       year games  wins losses  ties preseason_rank postseason_rank
    ##    <chr>       <int> <int> <int>  <int> <int>          <int>           <int>
    ##  1 Tulane       1985    11     1     10     0             NA              NA
    ##  2 Tulane       1986    11     4      7     0             NA              NA
    ##  3 Tulane       1987    12     6      6     0             NA              NA
    ##  4 North Caro…  1988    11     1     10     0             NA              NA
    ##  5 North Caro…  1989    11     1     10     0             NA              NA
    ##  6 North Caro…  1990    11     6      4     1             NA              NA
    ##  7 North Caro…  1991    11     7      4     0             NA              NA
    ##  8 North Caro…  1992    12     9      3     0             NA              19
    ##  9 North Caro…  1993    13    10      3     0             20              19
    ## 10 North Caro…  1994    12     8      4     0             19              NA
    ## # ℹ 24 more rows
    ## # ℹ 4 more variables: srs <chr>, sp_overall <chr>, sp_offense <chr>,
    ## #   sp_defense <chr>

``` r
coaching_history2 <- function(first_name, last_name) {
  
  url <- paste0("https://api.collegefootballdata.com/coaches?firstName=",first_name,"&lastName=",last_name)
  
  return(url)
}

coaching_history2("Mack", "Brown")
```

    ## [1] "https://api.collegefootballdata.com/coaches?firstName=Mack&lastName=Brown"
