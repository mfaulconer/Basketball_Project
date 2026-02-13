library(tidyverse)
library(ggplot2)
library(ggforce)
library(ggimage)

#import data
games <- read.csv("/Users/maryellenfaulconer/Documents/Basketball_Project/games_data.csv")
images <- read.csv("/Users/maryellenfaulconer/Documents/Basketball_Project/bpi_team_logos.csv")


## first + second half scores
games$home_final_score <- games$home_team_first_half_score + games$home_team_second_half_score
games$away_final_score <- games$away_team_first_half_score + games$away_team_second_half_score



## filter to top 100 BPI teams only
bpi_teams <- c(
  "Duke Blue Devils",
  "Michigan Wolverines",
  "Arizona Wildcats",
  "Houston Cougars",
  "Florida Gators",
  "Iowa State Cyclones",
  "Gonzaga Bulldogs",
  "Illinois Fighting Illini",
  "UConn Huskies",
  "Purdue Boilermakers",
  "Louisville Cardinals",
  "Kansas Jayhawks",
  "Tennessee Volunteers",
  "Vanderbilt Commodores",
  "St. John's Red Storm",
  "Alabama Crimson Tide",
  "BYU Cougars",
  "Nebraska Cornhuskers",
  "Texas Tech Red Raiders",
  "Kentucky Wildcats",
  "Arkansas Razorbacks",
  "Virginia Cavaliers",
  "Auburn Tigers",
  "Indiana Hoosiers",
  "NC State Wolfpack",
  "UCLA Bruins",
  "Iowa Hawkeyes",
  "North Carolina Tar Heels",
  "Utah State Aggies",
  "Villanova Wildcats",
  "Wisconsin Badgers",
  "Georgia Bulldogs",
  "Ohio State Buckeyes",
  "SMU Mustangs",
  "Baylor Bears",
  "Miami Hurricanes",
  "Missouri Tigers",
  "West Virginia Mountaineers",
  "Oklahoma Sooners",
  "VCU Rams",
  "Washington Huskies",
  "USC Trojans",
  "Clemson Tigers",
  "Cincinnati Bearcats",
  "Santa Clara Broncos",
  "San Diego State Aztecs",
  "LSU Tigers",
  "TCU Horned Frogs",
  "Seton Hall Pirates",
  "Ole Miss Rebels",
  "New Mexico Lobos",
  "Providence Friars",
  "UCF Knights",
  "Butler Bulldogs",
  "Creighton Bluejays",
  "Wake Forest Demon Deacons",
  "High Point Panthers",
  "Northwestern Wildcats",
  "Syracuse Orange",
  "Virginia Tech Hokies",
  "Mississippi State Bulldogs",
  "McNeese Cowboys",
  "South Florida Bulls",
  "Minnesota Golden Gophers",
  "Nevada Wolf Pack",
  "Tulsa Golden Hurricane",
  "Boise State Broncos",
  "Oklahoma State Cowboys",
  "California Golden Bears",
  "Notre Dame Fighting Irish",
  "George Washington Revolutionaries",
  "Memphis Tigers",
  "George Mason Patriots",
  "Georgetown Hoyas",
  "Dayton Flyers",
  "Akron Zips",
  "Grand Canyon Lopes",
  "Marquette Golden Eagles",
  "Stanford Cardinal",
  "Kansas State Wildcats",
  "Liberty Flames",
  "Oregon Ducks",
  "Miami (OH) RedHawks",
  "South Carolina Gamecocks",
  "Arizona State Sun Devils",
  "Colorado State Rams",
  "Florida State Seminoles",
  "Colorado Buffaloes",
  "Maryland Terrapins",
  "Xavier Musketeers",
  "Wichita State Shockers",
  "Illinois State Redbirds",
  "Northern Iowa Panthers",
  "Pittsburgh Panthers",
  "Belmont Bruins",
  "Michigan State Spartans",
  "Texas A&M Aggies",
  "Saint Mary's Gaels",
  "Texas Longhorns",
  "Saint Louis Billikens"
)


## functions to help determine # of wins and losses 
win <- function(games) {
  games |>
    mutate(
      winner = if_else(home_final_score > away_final_score,
                       home_team, away_team),
      loser  = if_else(home_final_score > away_final_score,
                       away_team, home_team)
    )
}

win_count <- function(bpi_teams, games) {
  results <- data.frame()
  for (team in bpi_teams) {
    games_count <- 0
    wins <- 0
    first_half <- 0
    second_half <- 0
    final_score <- 0
    for (i in 1:nrow(games)) {
      if (team == games$home_team[i] ||
          team == games$away_team[i]) {
        games_count <- games_count + 1
        if(team == games$home_team[i]) {
          first_half <- first_half + games$home_team_first_half_score[i]
          second_half <- second_half + games$home_team_second_half_score[i]
          final_score <- final_score + games$home_final_score[i]
        }
        if (team == games$away_team[i]){
          first_half <- first_half + games$away_team_first_half_score[i]
          second_half <- second_half + games$away_team_second_half_score[i]
          final_score <- final_score + games$away_final_score[i]
        }
        if (team == games$winner[i]) {
          wins <- wins + 1
        }
      }
      
      first_half_avg <- round((first_half/games_count), 1)
      second_half_avg <- round((second_half/games_count), 1)
      overall_avg <- round((final_score/games_count), 1)
    }
    
    results <- rbind(
      results,
      data.frame(team, games_count, wins, first_half_avg, second_half_avg, overall_avg)
    )
  }
  return(results)
}



## use functions
games <- win(games)

results <- win_count(bpi_teams, games)




## only keep games where at least 1 team is in BPI Top 100
games <- games|>
  filter(home_team %in% bpi_teams |
           away_team %in% bpi_teams)





## first half v second half scores of BPI Top 100 Teams
overall_averages <- results |>
  summarise(
    avg_first_half = round(mean(first_half_avg, na.rm=TRUE), 1),
    avg_second_half = round(mean(second_half_avg, na.rm=TRUE), 1),
    avg_overall = round(mean(overall_avg, na.rm = TRUE), 1)
  )



### PLOTS

results <- results|>
  rename(team_name = team)
  
results <- results|>
  left_join(images, by = "team_name")

ggplot(results, aes(x = first_half_avg, y = second_half_avg)) +
  geom_image(aes(image = logo_url), size = 0.05, 
             position = position_jitter(width = 0.1, height = 0.1, seed = 123)) +
  geom_hline(
    yintercept = 41.9,
    linetype = "dashed",
    color = "red",
    linewidth = 0.5
  ) +
  geom_vline(
    xintercept = 38.8,
    linetype = "dashed",
    color = "red",
    linewidth = 0.5
  ) +
  labs(
    title = "First Half v Second Half Scoring",
    subtitle = "Avg Scoring for BPI Top 100",
    x = "First Half Avg",
    y = "Second Half Avg",
    caption = "All Data from ESPN"
  ) 




### currently not using but might
geom_ellipse(
  aes(x0 = 44.5, y0 = 47,   # center
      a = 1.2, b = 2,      # width & height
      angle = 0.5),
  alpha = 0.01,
  color = "lightblue",
  fill = 'lightblue'
) +

  annotate(
    "text",
    x = 45, y = 47,
    label = "Slow starters,\ngreat finishers",
    color = "red"
  )
