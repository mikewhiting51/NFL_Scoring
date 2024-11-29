# Scoring Trends in the NFL in the Super Bowl Era

## 📚 Table of Contents
- [Initial Questions](#initial-questions)
- [Filtering Data and Creating Tables](#filtering-data-and-creating-tables)
- [Scoring Trends](#scoring-trends)
- [Yardage Trends](#yardage-trends)
- [Other Trends](#other-trends)
- [Conclusions](#conclusions)

***

## Initial Questions
1. Has scoring increased over time, and if so, by how much? 
2. If scoring has increased, in what ways? Are teams simply scoring more offensive touchdowns, or have there been significant increases in other methods of scoring (two-point conversions, field goals, special teams/defensive touchdowns)? 
3. Has offensive innovation outpaced defensive innovation, evidenced by significant increases in yards per game over time? 
4. What impact have trends in other aspects of the game (penalties, turnovers, play count) had on scoring?

***

## Filtering Data and Creating Tables
````r
# Starting to analyze year by year data
yearly <- read.csv("nfl_scoring.csv")

# Filter for super bowl era
yearly_post_sb <- yearly %>%
  filter(Year > 1966)

# Filter DF to exclude 2024
yearly_sbe <- yearly_post_sb %>%
  filter (Year < 2024)
````
