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

req <- request("https://api.collegefootballdata.com/venues") %>% 
  req_auth_bearer_token(token) %>% 
  req_headers("Accept" = "application/json")

mydata <- req_perform(req)

mydf <- fromJSON(rawToChar(mydata$body))

mytib <- as_tibble(mydf)

mytib %>%
  select(name, capacity, grass, city, state, elevation, year_constructed, dome)
```

    ## # A tibble: 816 × 8
    ##    name          capacity grass city  state elevation year_constructed dome 
    ##    <chr>            <int> <lgl> <chr> <chr> <chr>                <int> <lgl>
    ##  1 Abbott Memor…    10000 NA    Tusk… AL    122.8                 1925 FALSE
    ##  2 Abel Stadium      2500 NA    Linc… NE    <NA>                    NA FALSE
    ##  3 Ace W. Mumfo…    28500 FALSE Bato… LA    20.24213…             1928 FALSE
    ##  4 ACU Football…        0 NA    Glen… AZ    <NA>                    NA FALSE
    ##  5 Adamson Stad…     6500 NA    Brow… PA    <NA>                    NA FALSE
    ##  6 Aggie Memori…    28853 FALSE Las … NM    1208.201…             1978 FALSE
    ##  7 Aggie Stadium    21500 TRUE  Gree… NC    235.1224…             1981 FALSE
    ##  8 Aggie Stadium    10743 FALSE Davis CA    11.38259…             2007 FALSE
    ##  9 A.J. McClung…        0 NA    Colu… GA    <NA>                    NA FALSE
    ## 10 A.J. Simeon …    15000 NA    High… NC    <NA>                    NA FALSE
    ## # ℹ 806 more rows

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
team_season_stats <- function(year, team) {
  token <- 'L8SLuL2Jzi8KI5g0iHQYaERuCZgEDvyxDpOvDTgSqrLWvU7/8Yd5XuAPCfZJjMCJ'
  
  first_name <- word(team, 1)
  last_name <- word(team, 2)
  
  url <- ifelse(grepl(" ", team),
                paste0("https://api.collegefootballdata.com/stats/season?year=",
                       year,"&team=",first_name,"%20",last_name),
                paste0("https://api.collegefootballdata.com/stats/season?year=",
                       year,"&team=",team))
  
  req <- request(url) %>% 
    req_auth_bearer_token(token) %>% 
    req_headers("Accept" = "application/json")
  
  mydata <- req_perform(req)
  
  mydf <- fromJSON(rawToChar(mydata$body))
  
  mytib <- as_tibble(mydf)
  
  return(mytib)
}

team_season_stats(2019, "Texas")
```

    ## # A tibble: 32 × 5
    ##    season team  conference statName              statValue
    ##     <int> <chr> <chr>      <chr>                     <int>
    ##  1   2019 Texas Big 12     interceptionYards           100
    ##  2   2019 Texas Big 12     turnovers                    14
    ##  3   2019 Texas Big 12     interceptionTDs               0
    ##  4   2019 Texas Big 12     kickReturnTDs                 1
    ##  5   2019 Texas Big 12     penaltyYards                890
    ##  6   2019 Texas Big 12     penalties                    95
    ##  7   2019 Texas Big 12     interceptions                10
    ##  8   2019 Texas Big 12     fourthDownConversions         7
    ##  9   2019 Texas Big 12     thirdDowns                  185
    ## 10   2019 Texas Big 12     puntReturnYards             155
    ## # ℹ 22 more rows

``` r
game_results <- function(year, team) {
  token <- 'L8SLuL2Jzi8KI5g0iHQYaERuCZgEDvyxDpOvDTgSqrLWvU7/8Yd5XuAPCfZJjMCJ'
  
  first_name <- word(team, 1)
  last_name <- word(team, 2)
  
  url <- ifelse(grepl(" ", team),
                paste0("https://api.collegefootballdata.com/games?year=",
                       year,"&seasonType=regular&team=",first_name,"%20",last_name),
                paste0("https://api.collegefootballdata.com/games?year=",
                       year,"&seasonType=regular&team=",team))
  
  req <- request(url) %>% 
  req_auth_bearer_token(token) %>% 
  req_headers("Accept" = "application/json")

  mydata <- req_perform(req)

  mydf <- fromJSON(rawToChar(mydata$body))

  mytib <- as_tibble(mydf)

  mytib <- mytib %>%
    select(season, week, attendance, venue, home_team, home_points, away_team, away_points, excitement_index)
  
  return(mytib)
}

game_results(2019, "Ohio State")
```

    ## # A tibble: 13 × 9
    ##    season  week attendance venue home_team home_points away_team away_points
    ##     <int> <int>      <int> <chr> <chr>           <int> <chr>           <int>
    ##  1   2019     1     103228 Ohio… Ohio Sta…          45 Florida …          21
    ##  2   2019     2     104089 Ohio… Ohio Sta…          42 Cincinna…           0
    ##  3   2019     3      47945 Memo… Indiana            10 Ohio Sta…          51
    ##  4   2019     4     103190 Ohio… Ohio Sta…          76 Miami (O…           5
    ##  5   2019     5      89759 Memo… Nebraska            7 Ohio Sta…          48
    ##  6   2019     6     104797 Ohio… Ohio Sta…          34 Michigan…          10
    ##  7   2019     8      47330 Ryan… Northwes…           3 Ohio Sta…          52
    ##  8   2019     9     102998 Ohio… Ohio Sta…          38 Wisconsin           7
    ##  9   2019    11     101022 Ohio… Ohio Sta…          73 Maryland           14
    ## 10   2019    12      33528 High… Rutgers            21 Ohio Sta…          56
    ## 11   2019    13     104355 Ohio… Ohio Sta…          28 Penn Sta…          17
    ## 12   2019    14     112071 Mich… Michigan           27 Ohio Sta…          56
    ## 13   2019    15      66649 Luca… Wisconsin          21 Ohio Sta…          34
    ## # ℹ 1 more variable: excitement_index <chr>

``` r
team_records <- function(year, team) {
  token <- 'L8SLuL2Jzi8KI5g0iHQYaERuCZgEDvyxDpOvDTgSqrLWvU7/8Yd5XuAPCfZJjMCJ'
  
  first_name <- word(team, 1)
  last_name <- word(team, 2)
  
  url <- ifelse(grepl(" ", team),
                paste0("https://api.collegefootballdata.com/records?year=",
                       year,"&team=",first_name,"%20",last_name),
                paste0("https://api.collegefootballdata.com/records?year=",
                       year,"&team=",team))
  
  req <- request(url) %>% 
    req_auth_bearer_token(token) %>% 
    req_headers("Accept" = "application/json")

  mydata <- req_perform(req)

  mydf <- fromJSON(rawToChar(mydata$body))$total

  mytib <- as_tibble(mydf)
  
  return(mytib)
}

team_records(2022, "Ohio State")
```

    ## # A tibble: 1 × 4
    ##   games  wins losses  ties
    ##   <int> <int>  <int> <int>
    ## 1    13    11      2     0

``` r
venue_info <- function(venue_name = NULL) {
  token <- 'L8SLuL2Jzi8KI5g0iHQYaERuCZgEDvyxDpOvDTgSqrLWvU7/8Yd5XuAPCfZJjMCJ'

  req <- request("https://api.collegefootballdata.com/venues") %>% 
    req_auth_bearer_token(token) %>% 
    req_headers("Accept" = "application/json")

  mydata <- req_perform(req)

  mydf <- fromJSON(rawToChar(mydata$body))

  mytib <- as_tibble(mydf)

  mytib <- mytib %>%
    select(name, capacity, grass, city, state, elevation, year_constructed, dome)
  
  ifelse(!is.null(venue_name),
         mytib <- mytib %>%
           filter(name == venue_name),
         NA)
  
  return(mytib)
}

venue_info("ACU Football Field")
```

    ## # A tibble: 1 × 8
    ##   name           capacity grass city  state elevation year_constructed dome 
    ##   <chr>             <int> <lgl> <chr> <chr> <chr>                <int> <lgl>
    ## 1 ACU Football …        0 NA    Glen… AZ    <NA>                    NA FALSE
