# IMO-Performance-Analysis
Performance analysis of countries that participated in the IMO over the past 30 years, and factors that contributed to their success.

## Data Dictionary

### `country_results_df.csv`

|variable                  |class     |description                           |
|:-------------------------|:---------|:-------------------------------------|
|year                      |integer   |Year of IMO |
|country                   |character |Participating country |
|team_size_all             |integer   |Participating contestants |
|team_size_male            |integer   |Male contestants |
|team_size_female          |integer   |Female contestants|
|p1                        |integer   |Score on problem 1 |
|p2                        |integer   |Score on problem 2 |
|p3                        |integer   |Score on problem 3 |
|p4                        |integer   |Score on problem 4 |
|p5                        |integer   |Score on problem 5 |
|p6                        |integer   |Score on problem 6 |
|p7                        |integer   |Score on problem 7 |
|awards_gold               |integer   |Number of gold medals |
|awards_silver             |integer   |Number of silver medals |
|awards_bronze             |integer   |Number of bronze medals |
|awards_honorable_mentions |integer   |Number of honorable mentions |
|leader                    |character |Leader of country team |
|deputy_leader             |character |Deputy leader of country team |

### `individual_results_df.csv`

|variable        |class     |description                           |
|:---------------|:---------|:-------------------------------------|
|year            |integer   |Year of IMO  |
|contestant      |character |Participant's name |
|country         |character |Participant's country |
|p1              |integer   |Score on problem 1 |
|p2              |integer   |Score on problem 2 |
|p3              |integer   |Score on problem 3 |
|p4              |integer   |Score on problem 4 |
|p5              |integer   |Score on problem 5 |
|p6              |integer   |Score on problem 6 |
|total           |integer   |Total score on all problems |
|individual_rank |integer   |Individual rank |
|award           |character |Award won |

### `timeline_df.csv`

|variable          |class     |description                           |
|:-----------------|:---------|:-------------------------------------|
|edition           |integer   |Edition of International Mathematical Olympiad (IMO) |
|year              |integer   |Year of IMO |
|country           |character |Host country |
|city              |character |Host city |
|countries         |integer   |Number of participating countries|
|all_contestant    |integer   |Number of participating contestants|
|male_contestant   |integer   |Number of participating male contestants |
|female_contestant |integer   |Number of participating female contestants |
|start_date        |Date      |Start date of IMO |
|end_date          |Date      |End date of IMO |

## Downloading the Data from TidyTuesday repository
> The International Mathematical Olympiad (IMO) is the World Championship Mathematics Competition for High School students and is held annually in a different country. The first IMO was held in 1959 in Romania, with 7 countries participating. It has gradually expanded to over 100 countries from 5 continents. The competition consists of 6 problems and is held over two consecutive days with 3 problems each.


```r
# Option 1: tidytuesdayR package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2024-09-24')
## OR
tuesdata <- tidytuesdayR::tt_load(2024, week = 39)

country_results_df <- tuesdata$country_results_df
individual_results_df <- tuesdata$individual_results_df
timeline_df <- tuesdata$timeline_df

# Option 2: Read directly from GitHub

country_results_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-09-24/country_results_df.csv')
individual_results_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-09-24/individual_results_df.csv')
timeline_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-09-24/timeline_df.csv')
```

### Cleaning Script

```r
# Scraping IMO results data

library(tidyverse)
library(rvest)
library(janitor)
library(httr2)

timeline_df <- read_html("https://www.imo-official.org/organizers.aspx") %>%
  html_table() %>%
  .[[1]] %>%
  clean_names() %>%
  rename(
    "all_contestant" = contestants,
    "male_contestant" = contestants_2,
    "female_contestant" = contestants_3,
    "edition" = number
  ) %>%
  filter(edition != "#") %>%
  mutate(
    start_date = paste0(gsub("(.*)(-)(.*)", "\\1", date),year),
    end_date = paste0(gsub("(.*)(-)(.*)", "\\3", date),year),
    across(
      c(start_date, end_date),
      ~as.Date(.x, format = "%d.%m.%Y")
    ),
    across(
      c(edition, year, countries, all_contestant, male_contestant, female_contestant),
      as.integer
    )
  ) %>%
  select(-date) %>%
  # only keeping records till current year
  filter(year < 2025)

# circulate through country results link and rbind tables
scrape_country <- function(year) {
  paste0("https://www.imo-official.org/year_country_r.aspx?year=", year) %>%
    read_html() %>%
    html_table() %>%
    .[[1]] %>%
    clean_names() %>%
    filter(country != "Country") %>%
    mutate(year = year, .before = "country") 
}

country_results_df <- map_df(timeline_df$year, scrape_country) %>%
  select(
    year,
    country,
    team_size_all = team_size,
    team_size_male = team_size_2,
    team_size_female = team_size_3,
    starts_with("p"),
    awards_gold = awards,
    awards_silver = awards_2,
    awards_bronze = awards_3,
    awards_honorable_mentions = awards_4,
    leader,
    deputy_leader
  ) %>% 
  mutate(
    across(
      c(team_size_all:awards_honorable_mentions),
      as.integer
    )
  )


# circulate through individual results link and rbind tables
scrape_individual <- function(year) {
  # These can time out, so we'll use httr2 to retry.
  paste0("https://www.imo-official.org/year_individual_r.aspx?year=", year) %>%
    httr2::request() %>%
    httr2::req_retry(max_tries = 3) %>%
    httr2::req_perform() %>%
    httr2::resp_body_html() %>%
    html_table() %>%
    .[[1]] %>%
    clean_names() %>%
    mutate(year = year, .before = "contestant") 
}

individual_results_df <- map_df(timeline_df$year, scrape_individual) %>%
  select(
    year:p6, p7, total,
    individual_rank = number_rank,
    award
  ) %>%
  mutate(
    across(
      c(year, p1:individual_rank),
      as.integer
    )
  )
```
