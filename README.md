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

req <- request("https://api.collegefootballdata.com/games?year=2022&seasonType=regular&team=Ohio%20State") %>% 
  req_auth_bearer_token(token) %>% 
  req_headers("Accept" = "application/json")
req
```

    ## <httr2_request>

    ## GET
    ## https://api.collegefootballdata.com/games?year=2022&seasonType=regular&team=Ohio%20State

    ## Headers:

    ## • Authorization: '<REDACTED>'

    ## • Accept: 'application/json'

    ## Body: empty

``` r
mydata <- req_perform(req)
names(mydata)
```

    ## [1] "method"      "url"         "status_code" "headers"     "body"

``` r
mydf <- fromJSON(rawToChar(mydata$body))
mydf
```

    ##           id season week season_type               start_date
    ## 1  401404124   2022    1     regular 2022-09-03T23:30:00.000Z
    ## 2  401405079   2022    2     regular 2022-09-10T16:00:00.000Z
    ## 3  401405088   2022    3     regular 2022-09-17T23:00:00.000Z
    ## 4  401405095   2022    4     regular 2022-09-24T23:30:00.000Z
    ## 5  401405104   2022    5     regular 2022-10-01T19:30:00.000Z
    ## 6  401405110   2022    6     regular 2022-10-08T20:00:00.000Z
    ## 7  401405119   2022    8     regular 2022-10-22T16:00:00.000Z
    ## 8  401405126   2022    9     regular 2022-10-29T16:00:00.000Z
    ## 9  401405129   2022   10     regular 2022-11-05T16:00:00.000Z
    ## 10 401405139   2022   11     regular 2022-11-12T17:00:00.000Z
    ## 11 401405144   2022   12     regular 2022-11-19T20:30:00.000Z
    ## 12 401405153   2022   13     regular 2022-11-26T17:00:00.000Z
    ##    start_time_tbd completed neutral_site conference_game attendance
    ## 1           FALSE      TRUE        FALSE           FALSE     106594
    ## 2           FALSE      TRUE        FALSE           FALSE     100067
    ## 3           FALSE      TRUE        FALSE           FALSE     105398
    ## 4           FALSE      TRUE        FALSE            TRUE     105473
    ## 5           FALSE      TRUE        FALSE            TRUE     104245
    ## 6           FALSE      TRUE        FALSE            TRUE      72809
    ## 7           FALSE      TRUE        FALSE            TRUE     104848
    ## 8           FALSE      TRUE        FALSE            TRUE     108433
    ## 9           FALSE      TRUE        FALSE            TRUE      42774
    ## 10          FALSE      TRUE        FALSE            TRUE     103888
    ## 11          FALSE      TRUE        FALSE            TRUE      41969
    ## 12          FALSE      TRUE        FALSE            TRUE     106787
    ##    venue_id            venue home_id      home_team home_conference
    ## 1      3861     Ohio Stadium     194     Ohio State         Big Ten
    ## 2      3861     Ohio Stadium     194     Ohio State         Big Ten
    ## 3      3861     Ohio Stadium     194     Ohio State         Big Ten
    ## 4      3861     Ohio Stadium     194     Ohio State         Big Ten
    ## 5      3861     Ohio Stadium     194     Ohio State         Big Ten
    ## 6      3936  Spartan Stadium     127 Michigan State         Big Ten
    ## 7      3861     Ohio Stadium     194     Ohio State         Big Ten
    ## 8      3632   Beaver Stadium     213     Penn State         Big Ten
    ## 9      3911       Ryan Field      77   Northwestern         Big Ten
    ## 10     3861     Ohio Stadium     194     Ohio State         Big Ten
    ## 11     3665 Maryland Stadium     120       Maryland         Big Ten
    ## 12     3861     Ohio Stadium     194     Ohio State         Big Ten
    ##    home_division home_points home_line_scores    home_post_win_prob
    ## 1            fbs          21       7, 0, 7, 7     0.739877676265821
    ## 2            fbs          45    14, 10, 21, 0    0.9989693384552342
    ## 3            fbs          77   28, 14, 14, 21    0.9989484674561335
    ## 4            fbs          52    21, 10, 14, 7    0.9989624304932823
    ## 5            fbs          49    14, 14, 14, 7    0.9977842582267297
    ## 6            fbs          20       7, 6, 0, 7 0.0003351660384028238
    ## 7            fbs          54   16, 10, 14, 14    0.9972803153743994
    ## 8            fbs          31     0, 14, 0, 17   0.11008017570023716
    ## 9            fbs           7       7, 0, 0, 0    0.8747394469966221
    ## 10           fbs          56    21, 7, 14, 14     0.998908359428427
    ## 11           fbs          30      6, 7, 0, 17   0.29090745714775934
    ## 12           fbs          23     10, 10, 0, 3    0.0288752552513377
    ##    home_pregame_elo home_postgame_elo away_id      away_team
    ## 1              1932              1944      87     Notre Dame
    ## 2              1944              1957    2032 Arkansas State
    ## 3              1957              2079    2649         Toledo
    ## 4              2079              2132     275      Wisconsin
    ## 5              2132              2150     164        Rutgers
    ## 6              1580              1565     194     Ohio State
    ## 7              2165              2233    2294           Iowa
    ## 8              1838              1844     194     Ohio State
    ## 9              1300              1343     194     Ohio State
    ## 10             2184              2201      84        Indiana
    ## 11             1561              1585     194     Ohio State
    ## 12             2177              2108     130       Michigan
    ##     away_conference away_division away_points away_line_scores
    ## 1  FBS Independents           fbs          10       3, 7, 0, 0
    ## 2          Sun Belt           fbs          12       3, 6, 3, 0
    ## 3      Mid-American           fbs          21       7, 7, 7, 0
    ## 4           Big Ten           fbs          21      0, 7, 0, 14
    ## 5           Big Ten           fbs          10       7, 0, 3, 0
    ## 6           Big Ten           fbs          49    14, 21, 14, 0
    ## 7           Big Ten           fbs          10       7, 3, 0, 0
    ## 8           Big Ten           fbs          44     10, 3, 3, 28
    ## 9           Big Ten           fbs          21       0, 7, 7, 7
    ## 10          Big Ten           fbs          14       7, 0, 0, 7
    ## 11          Big Ten           fbs          43     7, 3, 17, 16
    ## 12          Big Ten           fbs          45     3, 14, 7, 21
    ##       away_post_win_prob away_pregame_elo away_postgame_elo
    ## 1      0.260122323734179             1826              1814
    ## 2  0.0010306615447658318             1262              1249
    ## 3  0.0010515325438664735             1597              1475
    ## 4  0.0010375695067177393             1828              1775
    ## 5    0.00221574177327033             1347              1329
    ## 6     0.9996648339615972             2150              2165
    ## 7  0.0027196846256005713             1700              1632
    ## 8     0.8899198242997628             2233              2227
    ## 9     0.1252605530033779             2227              2184
    ## 10 0.0010916405715729605             1312              1295
    ## 11    0.7090925428522407             2201              2177
    ## 12    0.9711247447486623             2075              2144
    ##    excitement_index highlights notes
    ## 1      6.0088684122         NA    NA
    ## 2      3.6769873049         NA    NA
    ## 3      1.1880100740         NA    NA
    ## 4      1.5054510397         NA    NA
    ## 5      2.9050293707         NA    NA
    ## 6      0.6766191254         NA    NA
    ## 7      1.7768703924         NA    NA
    ## 8      5.7561272962         NA    NA
    ## 9      1.9908780180         NA    NA
    ## 10     1.0969827547         NA    NA
    ## 11     3.6219631302         NA    NA
    ## 12     5.7352697779         NA    NA

``` r
mytib <- as_tibble(mydf)
mytib
```

    ## # A tibble: 12 × 33
    ##           id season  week season_type start_date    start_time_tbd completed
    ##        <int>  <int> <int> <chr>       <chr>         <lgl>          <lgl>    
    ##  1 401404124   2022     1 regular     2022-09-03T2… FALSE          TRUE     
    ##  2 401405079   2022     2 regular     2022-09-10T1… FALSE          TRUE     
    ##  3 401405088   2022     3 regular     2022-09-17T2… FALSE          TRUE     
    ##  4 401405095   2022     4 regular     2022-09-24T2… FALSE          TRUE     
    ##  5 401405104   2022     5 regular     2022-10-01T1… FALSE          TRUE     
    ##  6 401405110   2022     6 regular     2022-10-08T2… FALSE          TRUE     
    ##  7 401405119   2022     8 regular     2022-10-22T1… FALSE          TRUE     
    ##  8 401405126   2022     9 regular     2022-10-29T1… FALSE          TRUE     
    ##  9 401405129   2022    10 regular     2022-11-05T1… FALSE          TRUE     
    ## 10 401405139   2022    11 regular     2022-11-12T1… FALSE          TRUE     
    ## 11 401405144   2022    12 regular     2022-11-19T2… FALSE          TRUE     
    ## 12 401405153   2022    13 regular     2022-11-26T1… FALSE          TRUE     
    ## # ℹ 26 more variables: neutral_site <lgl>, conference_game <lgl>,
    ## #   attendance <int>, venue_id <int>, venue <chr>, home_id <int>,
    ## #   home_team <chr>, home_conference <chr>, home_division <chr>,
    ## #   home_points <int>, home_line_scores <list>, home_post_win_prob <chr>,
    ## #   home_pregame_elo <int>, home_postgame_elo <int>, away_id <int>,
    ## #   away_team <chr>, away_conference <chr>, away_division <chr>,
    ## #   away_points <int>, away_line_scores <list>, away_post_win_prob <chr>, …

``` r
names(mydf)
```

    ##  [1] "id"                 "season"             "week"              
    ##  [4] "season_type"        "start_date"         "start_time_tbd"    
    ##  [7] "completed"          "neutral_site"       "conference_game"   
    ## [10] "attendance"         "venue_id"           "venue"             
    ## [13] "home_id"            "home_team"          "home_conference"   
    ## [16] "home_division"      "home_points"        "home_line_scores"  
    ## [19] "home_post_win_prob" "home_pregame_elo"   "home_postgame_elo" 
    ## [22] "away_id"            "away_team"          "away_conference"   
    ## [25] "away_division"      "away_points"        "away_line_scores"  
    ## [28] "away_post_win_prob" "away_pregame_elo"   "away_postgame_elo" 
    ## [31] "excitement_index"   "highlights"         "notes"

``` r
mytib %>%
  select(season, week, attendance, venue, home_team, home_points, away_team, away_points, excitement_index)
```

    ## # A tibble: 12 × 9
    ##    season  week attendance venue home_team home_points away_team away_points
    ##     <int> <int>      <int> <chr> <chr>           <int> <chr>           <int>
    ##  1   2022     1     106594 Ohio… Ohio Sta…          21 Notre Da…          10
    ##  2   2022     2     100067 Ohio… Ohio Sta…          45 Arkansas…          12
    ##  3   2022     3     105398 Ohio… Ohio Sta…          77 Toledo             21
    ##  4   2022     4     105473 Ohio… Ohio Sta…          52 Wisconsin          21
    ##  5   2022     5     104245 Ohio… Ohio Sta…          49 Rutgers            10
    ##  6   2022     6      72809 Spar… Michigan…          20 Ohio Sta…          49
    ##  7   2022     8     104848 Ohio… Ohio Sta…          54 Iowa               10
    ##  8   2022     9     108433 Beav… Penn Sta…          31 Ohio Sta…          44
    ##  9   2022    10      42774 Ryan… Northwes…           7 Ohio Sta…          21
    ## 10   2022    11     103888 Ohio… Ohio Sta…          56 Indiana            14
    ## 11   2022    12      41969 Mary… Maryland           30 Ohio Sta…          43
    ## 12   2022    13     106787 Ohio… Ohio Sta…          23 Michigan           45
    ## # ℹ 1 more variable: excitement_index <chr>

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
  
  url <- paste0("https://api.collegefootballdata.com/stats/season?year=",year,"&team=",team)
  
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
  
  url <- paste0("https://api.collegefootballdata.com/games?year=",year,"&seasonType=regular&team=",team)
  
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

game_results(2019, "Texas")
```

    ## # A tibble: 12 × 9
    ##    season  week attendance venue home_team home_points away_team away_points
    ##     <int> <int>      <int> <chr> <chr>           <int> <chr>           <int>
    ##  1   2019     1      93418 Darr… Texas              45 Louisian…          14
    ##  2   2019     2      98763 Darr… Texas              38 LSU                45
    ##  3   2019     3      42417 NRG … Rice               13 Texas              48
    ##  4   2019     4      96936 Darr… Texas              36 Oklahoma…          30
    ##  5   2019     6      62069 Moun… West Vir…          31 Texas              42
    ##  6   2019     7      92100 Cott… Texas              27 Oklahoma           34
    ##  7   2019     8      97137 Darr… Texas              50 Kansas             48
    ##  8   2019     9      47660 Amon… TCU                37 Texas              27
    ##  9   2019    11      97833 Darr… Texas              27 Kansas S…          24
    ## 10   2019    12      58946 Jack… Iowa Sta…          23 Texas              21
    ## 11   2019    13      49109 McLa… Baylor             24 Texas              10
    ## 12   2019    14      93747 Darr… Texas              49 Texas Te…          24
    ## # ℹ 1 more variable: excitement_index <chr>
