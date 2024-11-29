library(dplyr)

# Starting to analyze year by year data
yearly <- read.csv("nfl_scoring.csv")

# Plot scoring over time
ggplot(yearly, aes(x = Year, y = Pts)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  labs(
    title = "NFL Scoring Over Time",
    x = "Year",
    y = "Average Points"
  ) +
  theme_minimal()

# Filter for super bowl era
yearly_post_sb <- yearly %>%
  filter(Year > 1966)

# Filter DF to exclude 2024
yearly_sbe <- yearly_post_sb %>%
  filter (Year < 2024)

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


# Heat map of scoring per decade in SBE
yearly_sbe$Decade <- floor(yearly_sbe$Year / 10) * 10
ggplot(yearly_sbe, aes(x = Year, y = Decade, fill = Pts)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(
    title = "Heatmap of NFL Scoring Trends",
    subtitle = "Points per Game (1967-2023)",
    x = "Year",
    y = "Decade",
    fill = "Points"
  ) +
  theme_minimal()

# Show scoring by decade
decade_data <- yearly_sbe %>%
  group_by(Decade = floor(Year / 10) * 10) %>%
  summarise(AvgPts = mean(Pts))

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



# Plot scoring over time in super bowl era
ggplot(yearly_post_sb, aes(x = Year, y = RshTD)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  labs(
    title = "Rushing Touchdowns Over Time",
    x = "Year",
    y = "Rush TDs per Game"
  ) +
  theme_minimal()

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
# Visual shows relatively strong evidence for an increase in passing touchdowns over time, but not rushing touchdowns or total touchdowns
# Touchdowns have not actually increased that much over time; maybe it's mostly field goals accounting for the uptick?

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

# Plot FGA over time
ggplot(yearly_post_sb, aes(x = Year, y = FGA)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  labs(
    title = "Field Goals Attempted Over Time",
    subtitle = "1967-2024 (Note: In 1974, FG posts were moved to back of end zone)",
    x = "Year",
    y = "FGM per Game"
  ) +
  theme_minimal()

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

# Filter for modern FG era
yearly_fg_era <- yearly_post_sb %>%
  filter(Year > 1973)

# Fit linear model for Pts over time in modern FG era
model6 <- lm(Pts ~ Year, data = yearly_fg_era)
r_squared_ptsFGE <- summary(model6)$r.squared

# Plot scoring over time in modern field goal era
ggplot(yearly_fg_era, aes(x = Year, y = Pts)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  annotate("text", x = 1976, y = 24, label = paste("R² =", round(r_squared_ptsFGE, 3)), color = "black") +
  labs(
    title = "NFL Scoring Over Time",
    subtitle = "Modern Field Goal Era: 1974-2024",
    x = "Year",
    y = "Points per Game"
  ) +
  theme_minimal()

# Download CSV of Rushing/Receiving from PFR to see trends in pass vs rush yards
yardage <- read.csv("yardage.csv")

# Filter yardage DF for sb era and modern fg era
yardage_sbe <- yardage %>%
  filter(Year > 1966)
  
yardage_fge <- yardage_sbe %>%
  filter(Year > 1973)

# Plot rush yards over time 
ggplot(yardage_sbe, aes(x = Year, y = Yds)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  labs(
    title = "Rush Yards Over Time",
    subtitle = "Super Bowl Era (1967-2024)",
    x = "Year",
    y = "Rush YPG per Team"
  ) +
  theme_minimal()

# Plot rec yards over time 
ggplot(yardage_sbe, aes(x = Year, y = Yds.1)) +
  geom_line(stat = "identity", color = "blue") +
  geom_point(size = 1) +
  labs(
    title = "Receiving Yards Over Time",
    subtitle = "Super Bowl Era (1967-2024)",
    x = "Year",
    y = "Receiving YPG per Team"
  ) +
  theme_minimal()

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

# Convert to long format to plot rush yds, pass yds, and total yds
yardage_sbe_long <- yardage_sbe %>%
  pivot_longer(
    cols = c(Yds, Yds.1, YScm),
    names_to = "Category",
    values_to = "Yards"
  )

# Plot rush yds, pass yds, and total yds over time
library(ggplot2)

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

# Calculate the yearly rate of change
average_rate_of_change <- yardage_sbe_long %>%
  group_by(Category) %>%
  arrange(Year) %>%  # Ensure data is ordered by Year
  mutate(Change = Yards - lag(Yards)) %>%  # Calculate year-over-year change
  summarise(AverageChangePerYear = mean(Change, na.rm = TRUE)) # Average the changes

print(average_rate_of_change)

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

# Plot yardage by decade
ggplot(decade_data_yards, aes(x = factor(Decade), y = AvgYds, fill = AvgYds)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(AvgYds, 1)), vjust = -0.5, size = 3) + # Add data labels
  scale_fill_gradient(low = "blue", high = "red") +
  coord_cartesian(ylim = c(300, NA)) +  # Adjust visible range of y-axis without removing data
  labs(
    title = "Average Total Yardage by Decade",
    subtitle = "Super Bowl Era",
    x = "Decade",
    y = "Total Yards per Game per Team"
  ) +
  theme_minimal()

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

overall <- read.csv("overall.csv")

overall_sbe <- overall %>%
  filter(Year > 1966)

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

# So are kickers better, or are teams better at moving the ball down the field?
# Need to visualize changes in rushing, receiving, and total yards over time to see if there is a
# meaningful increase in yardage over time. But if there is, is this because teams are having more 
# possessions? Or are offenses truly better?
# Has the increase in FGs occurred despite more aggressive playcalling, or because of it?

# Determine and show r for each chart to provide information on strength of linear relationship