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
I began the project by loading datasets from Pro Football Reference. I then filtered and summarized the data into different frames to be able to analyze data specifically for the Super Bowl Era and aggregated by decade. 
````r
# Starting to analyze year by year data
yearly <- read.csv("nfl_scoring.csv")

# Filter for super bowl era
yearly_post_sb <- yearly %>%
  filter(Year > 1966)

# Filter DF to exclude 2024
yearly_sbe <- yearly_post_sb %>%
  filter (Year < 2024)

# Show scoring by decade
decade_data <- yearly_sbe %>%
  group_by(Decade = floor(Year / 10) * 10) %>%
  summarise(AvgPts = mean(Pts))

# Download CSV of Rushing/Receiving from PFR to see trends in pass vs rush yards
yardage <- read.csv("yardage.csv")

# Filter yardage DF for sb era and modern fg era
yardage_sbe <- yardage %>%
  filter(Year > 1966)
  
yardage_fge <- yardage_sbe %>%
  filter(Year > 1973)

overall <- read.csv("overall.csv")

overall_sbe <- overall %>%
  filter(Year > 1966)
````

***

## Scoring Trends
### Points over Time
After loading and summarizing the data, I created a line chart to plot points scored per team per game over time. I also created a linear model to determine the R² value and the strength of the correlation.

````r
# Fit linear model for points over time
model3 <- lm(Pts ~ Year, data = yearly_sbe)
r_squared_points <- summary(model3)$r.squared

# Plot scoring over time in super bowl era
ggplot(yearly_sbe, aes(x = Year, y = Pts)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red", alpha = 0.2) +
  geom_vline(xintercept = 1974, linetype = "dashed", color = "black") + # Vertical line
  annotate("text", x = 1970, y = 25, label = "<------------------------- Modern FG Era (1974) -------------------------->", hjust = -0.1, color = "black") + # Annotation
  annotate("text", x = 2015, y = 18, label = paste("R² =", round(r_squared_points, 3)), color = "black") +
  scale_x_continuous(
    breaks = seq(1960, 2020, by = 10), # Set breaks for each decade
    labels = seq(1960, 2020, by = 10)  # Use decade labels
  ) +
  labs(
    title = "NFL Scoring Over Time",
    subtitle = "Super Bowl Era (1967-2023)",
    x = "Year",
    y = "Points per Game"
  ) +
  theme_minimal()
````
![pts_over_time_sbe](https://github.com/user-attachments/assets/48c4e8f2-57eb-49fa-a35c-8b9f83b2d8e9)

The result showed only a slight increase in points per game per team over time. Further, the values for 1967 and 2023 (the first and last years in the filtered dataset) were identical, indicating that scoring has not changed significantly in the Super Bowl Era.

### Points by Decade
Although the values for 1967 and 2023 were equal, the points over time visual did have a modestly strong R² value, and the overall trajectory of the line was upwards. This prompted me to examine the data on a decade-to-decade level, in order to reduce the influence of temporary increases or decreases in scoring. 

````r
# Plot scoring by decade
ggplot(decade_data, aes(x = factor(Decade), y = AvgPts, fill = AvgPts)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AvgPts, 1)), vjust = -0.5, size = 3) +
  scale_fill_gradient(low = "blue", high = "red") +
  coord_cartesian(ylim = c(10, NA)) + 
  labs(
    title = "Average NFL Scoring by Decade",
    subtitle = "Super Bowl Era",
    x = "Decade",
    y = "Points per Game per Team"
  ) +
  theme_minimal()
````
![pts_per_decade_sbe](https://github.com/user-attachments/assets/126abde0-6b6b-4a06-ba14-2289e4f294d1)

This visual provided a different perspective on the data, showing a consistent (although still modest) increase in scoring from decade to decade, with exceptions in the 1970s and 1990s. It should be noted that the decline in scoring in the 1970s may have been due in part to a rule change that moved the field goal posts to the back of the endzone, increasing the difficulty of field goals (this is explored in greater detail later). Since the 1970s, scoring per team per game has increased by 3.7 points. 

### Touchdowns over Time
I then moved on to touchdown-specific data, to see how total touchdowns, rushing touchdowns, and passing touchdowns have changed over time. 

````r
# Reshape data into long format to plot rush td, pass td, and total td
yearly_post_sb_long <- yearly_post_sb %>%
  pivot_longer(
    cols = c(RshTD, RecTD, AllTD),
    names_to = "Category",
    values_to = "TDs"
  )

# Calculate R2 values for all td categories and set annotation positions dynamically
annotations <- yearly_post_sb_long %>%
  group_by(Category) %>%
  summarise(
    R2 = summary(lm(TDs ~ Year))$r.squared,
    x_pos = min(Year),
    y_pos = max(TDs)
  )

# Merge R2 value into the long df
yearly_post_sb_long <- yearly_post_sb_long %>%
  left_join(annotations, by = "Category")

# Plot rush, rec, and total Td
ggplot(yearly_post_sb_long, aes(x = Year, y = TDs, color = Category)) +
  geom_line(linewidth = .5) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_wrap(~ Category, scales = "free_y", nrow = 3) +
  geom_text(
    data = annotations,
    aes(x = x_pos, y = y_pos, label = paste("R² =", round(R2, 3))),
        inherit.aes = FALSE,
        hjust = 0,
        vjust = 1
  ) +
  labs(
    title = "NFL Scoring Trends over Time",
    subtitle = "Super Bowl Era",
    x = "Year",
    y = "Touchdowns per Game",
  ) +
  theme_minimal()
````
![touchdown_trends_over_time](https://github.com/user-attachments/assets/cda9eaa7-bb9e-48f3-ab0d-b41c8598e6c6)

The visual demonstrates that despite the increase in points over time, there has been very little change in total touchdowns scored per game per team. While there has been a slight increase in the number of passing touchdowns per game (although even this is not conclusive), it seems to have been somewhat offset by a coinciding decline in rushing touchdowns per game. 

### Field Goal Trends 
With a lack of evidence for an increase in touchdowns over time, the logical next step was to evaluate field goal trends, in order to determine if an increase in field goals has driven the rise in scoring over time. 

````r
# Fit linear model for FGM over time
model4 <- lm(FGM ~ Year, data = yearly_post_sb)
r_squared_fgm <- summary(model4)$r.squared

# Plot FGM over time
ggplot(yearly_post_sb, aes(x = Year, y = FGM)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  annotate("text", x = 1970, y = 1.7, label = paste("R² =", round(r_squared_fgm, 3)), color = "black") +
  labs(
    title = "Field Goals Made Over Time",
    subtitle = "1967-2024 (Note: In 1974, FG posts were moved to back of end zone)",
    x = "Year",
    y = "FGM per Game"
  ) +
  theme_minimal()
````

![fgm_over_time](https://github.com/user-attachments/assets/b3c6a7b6-1019-4fee-a83b-ba6b51ad1e80)

The result was a clear rise in field goals made over time. After the posts were moved back in the early 1970s, teams made around .8 field goals per game; in the modern era, teams are making between 1.6-1.8, which tracks with the 3.7 additional points scored per game. 

Not only are kickers making more field goals, but their accuracy has skyrocketed as well. 

````r
# Add column to show FG% for each year
yearly_post_sb$FGPercentage <- (yearly_post_sb$FGM)/(yearly_post_sb$FGA)

# Fit linear model for FG% over time
model5 <- lm(FGPercentage ~ Year, data = yearly_post_sb)
r_squared_fgperc <- summary(model5)$r.squared

# Plot FG% over time
ggplot(yearly_post_sb, aes(x = Year, y = FGPercentage)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  annotate("text", x = 1970, y = .8, label = paste("R² =", round(r_squared_fgperc, 3)), color = "black") +
  labs(
    title = "Field Goal % Over Time",
    subtitle = "1967-2024 (Note: In 1974, FG posts were moved to back of end zone)",
    x = "Year",
    y = "FG % per Game"
  ) +
  theme_minimal()
````

![fg_percentage_over_time](https://github.com/user-attachments/assets/78723636-608d-418b-b792-ab0ce7d8f90e)

While kickers made 50-65% of their field goals in the early Super Bowl Era, they now consistently make between 80-90% of kicks. 

***

## Yardage Trends
I next moved on to yardage trends to determine if there has been an accompanying increase in offensive efficiency alongside the slight increase in scoring. While the assumption would be yes, the sharp rise in field goal kicker accuracy suggests that kickers may simply be more skilled in the modern era, thus being able to make longer field goals and reducing the amount of yards necessary for offenses to put up points.

### Total Yardage
I began by analyzing total yards from scrimmage over time. 

````r
# Fit linear model for yards over time in super bowl era
model7 <- lm(YScm ~ Year, data = yardage_sbe)
r_squared_yds <- summary(model7)$r.squared

# Plot total yards over time 
ggplot(yardage_sbe, aes(x = Year, y = YScm)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  annotate("text", x = 1976, y = 360, label = paste("R² =", round(r_squared_yds, 3)), color = "black") +
  labs(
    title = "Total Yards Over Time",
    subtitle = "Super Bowl Era (1967-2024)",
    x = "Year",
    y = "Total YPG per Team"
  ) +
  theme_minimal()
````

![yds_over_time_sbe](https://github.com/user-attachments/assets/e4077d97-419d-4139-b78b-0cd8b56f0c1a)

The visual shows a substantial increase in yards per game per team over time. While yardage per game per team hovered between 300-320 for most years prior to 1980, teams now consistently gain over 350 yards per game. 

There has also been a modest but steady increase in yards per play over time, which indicates that the increase in yardage has not been merely a result of an more plays per game:

````r
# Plot yards per play over time 
ggplot(overall_sbe, aes(x = Year, y = Y.P)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = .8) +
  labs(
    title = "Yards per Play Over Time",
    subtitle = "Super Bowl Era",
    x = "Year",
    y = "Yards per Play per Game"
  ) +
  theme_minimal()
````

![yards_per_play_sbe](https://github.com/user-attachments/assets/d0eb3e0e-3a33-4cc2-816f-fa0a05cb92e0)

### Pass Yards and Rush Yards 
I then went on to isolate passing and rushing yards to compare the trends in each of these categories versus the overall yardage trends. 

````r
# Convert to long format to plot rush yds, pass yds, and total yds
yardage_sbe_long <- yardage_sbe %>%
  pivot_longer(
    cols = c(Yds, Yds.1, YScm),
    names_to = "Category",
    values_to = "Yards"
  )
# Plot the data
ggplot(yardage_sbe_long, aes(x = Year, y = Yards, color = Category, group = Category)) +
  geom_line(size = .8) + # Add lines
  scale_color_manual(
    values = c("Yds" = "#0072B2", "Yds.1" = "#E69F00", "YScm" = "#009E73"),
    labels = c("YScm" = "Total Yards", "Yds.1" = "Pass Yards", "Yds" = "Rush Yards")
  ) +
  labs(
    title = "NFL Yardage Over Time",
    subtitle = "Super Bowl Era (1967-2023)",
    x = "Year",
    y = "Yards per Game per Team",
    color = "Category"
  ) +
  theme_minimal()
````

![yardage_over_time_sbe](https://github.com/user-attachments/assets/3163421d-69f9-41c7-b336-8ff8a67b6871)

The result showed that the increase in total yardage over time was driven by a significant increase in passing yards, while rush yards per game have slightly declined. 

A decade-by-decade comparison shows the trend in better focus: 

````r
# Aggregate yardage by decade for each category
decade_data_yards <- yardage_sbe %>%
  group_by(Decade = floor(Year / 10) * 10) %>%
  summarise(
    TotalYards = mean(YScm),
    RushYards = mean(Yds),
    PassYards = mean(Yds.1)
  ) %>%
  pivot_longer(
    cols = c(TotalYards, PassYards, RushYards),
    names_to = "Category",
    values_to = "AvgYds"
  )

# Plot yardage by decade with facet_wrap
ggplot(decade_data_yards, aes(x = factor(Decade), y = AvgYds, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AvgYds, 0)), vjust = -0.5, size = 2.3) +
  facet_wrap(~ Category, scales = "free_y") + # Facet by category
  scale_fill_manual(
      values = c(
        "TotalYards" = "#0072B2",  # Blue
        "PassYards" = "#E69F00",  # Orange
        "RushYards" = "#009E73"   # Green
      )
  ) +
  labs(
    title = "Average Yardage by Decade",
    subtitle = "Super Bowl Era",
    x = "Decade",
    y = "Average Yards per Game per Team",
    fill = "Yards"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Slant x-axis labels at 45 degrees
    legend.position = "none"
  )
````

![yardage_by_decade_sbe](https://github.com/user-attachments/assets/e9741ebd-3c2d-4fbb-893b-f152002e884f)

***

## Other Trends
Before drawing any conclusions, I first examined a couple of other variables that can impact scoring. 

### Penalties
First, I analyzed penalty data during the Super Bowl Era, to see if rule changes and an increased focus on player safety have played a role in the increase in scoring and offensive efficiency. 

````r
# Aggregate penalty data by decade for each category
decade_data_penalties <- overall_sbe %>%
  group_by(Decade = floor(Year / 10) * 10) %>%
  summarise(
    PenaltyYards = mean(Yds.3),
    FirstDownsByPenalty = mean(X1stPy),
    Penalties = mean(Pen)
  ) %>%
  pivot_longer(
    cols = c(PenaltyYards, FirstDownsByPenalty, Penalties),
    names_to = "Category",
    values_to = "Avg"
  )

# Plot penalties by decade with facet_wrap
ggplot(decade_data_penalties, aes(x = factor(Decade), y = Avg, fill = Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Avg, 1)), vjust = -0.5, size = 2.3) +
  facet_wrap(~ Category, scales = "free_y") + # Facet by category
  scale_fill_manual(
    values = c(
      "Penalties" = "#0072B2",  # Blue
      "PenaltyYards" = "#E69F00",  # Orange
      "FirstDownsByPenalty" = "#009E73"   # Green
    )
  ) +
  labs(
    title = "Penalty Data by Decade",
    subtitle = "Super Bowl Era",
    x = "Decade",
    y = "Per Game Per Team Average",
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Slant x-axis labels at 45 degrees
    legend.position = "none"
  )
````

![penalty_data_sbe](https://github.com/user-attachments/assets/daf7d63b-9cc9-44bd-bac8-0965b478ce97)

While teams are gaining first downs by penalty slightly more often in the modern era, the number of penalties per game has remained relatively flat over time, and penalty yards per game have actually declined (aside from a brief spike in the 2010s). 

### Turnovers
I then moved on to turnover data, as turnovers are a decent proxy for overall offensive efficiency and success rates. Changes in turnover rates over time can provide insight into the general effectiveness of offenses over time. Further, turovers inevitably have an impact on scoring, although this relationship can be complicated; while one might intuitively guess that turnovers hinder scoring, turnovers can often lead to better field position for offenses and thus more opportunities for points. 

````r
# Plot turnovers over time 
ggplot(overall_sbe, aes(x = Year, y = TO)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = .8) +
  labs(
    title = "Turnovers Over Time",
    subtitle = "Super Bowl Era",
    x = "Year",
    y = "Turnovers per Game per Team"
  ) +
  theme_minimal()
````

![turnovers_sbe](https://github.com/user-attachments/assets/7f3d76d5-8d05-4669-ae1c-2a791b6143ac)

The data shows a clear, significant, and steady decline in turnovers per team per game over time. While teams consistently turned the ball over more than twice a game until the 1990s, they have been consistently below 1.5 per game since the mid 2010s, and are averageing a historically low 1.2 per game in 2024. 

***

## Conclusions
Based on the above analysis, several general conclusions can be drawn: 

1. The NFL has seen a modest increase in scoring in the Super Bowl era, but this increase has been driven by field goals rather than touchdowns.
  a. Modern-day kickers are far more accurate and effective, evidenced by a significant and         steady increase in both field goals made and field goal percentage over time.
  b. Kickers today make an average of one additional field goal per game compared to kickers        in 1974, when the posts were first moved back.
2. Although teams are not necessarily scoring significantly more touchdowns, they are moving the ball far more effectively than in the early Super Bowl era, evidenced by a steady increase in yards per game over time.
  a. Increase in yardage per game has been driven by a rise in passing yards, while rushing         yards per game has actually declined slightly.
  b. Yards per play has risen in accordance with total yards per game, dispelling the notion        that teams are simply running more offensive plays per game.
3. Offenses have become increasingly efficient and effective over time.
  a. Not only do teams gain more yards per game in the modern era, but they turn the ball over      far fewer times and average more yards per play.
  b. Penalties have not played a major role in this increased efficiency.
