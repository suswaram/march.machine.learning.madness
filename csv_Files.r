library(plyr)

regular_season_results <- read.csv("~/Projects/Kaggle/March Madness/regular_season_results.csv")
sample_submission <- read.csv("~/Projects/Kaggle/March Madness/sample_submission.csv")
seasons <- read.csv("~/Projects/Kaggle/March Madness/seasons.csv")
teams <- read.csv("~/Projects/Kaggle/March Madness/teams.csv")
tourney_results <- read.csv("~/Projects/Kaggle/March Madness/tourney_results.csv")
tourney_slots <- read.csv("~/Projects/Kaggle/March Madness/tourney_slots.csv")
pointspreads <- read.csv("~/Projects/Kaggle/March Madness/pointspreads.csv")
ordinal_ranks_non_core <- read.csv("~/Projects/Kaggle/March Madness/ordinal_ranks_non_core.csv")
ordinal_ranks_core_33 <- read.csv("~/Projects/Kaggle/March Madness/ordinal_ranks_core_33.csv")
sagp_weekly_ratings <- read.csv("~/Projects/Kaggle/March Madness/sagp_weekly_ratings.csv")
pct_tourney <- read.csv("~/Projects/Kaggle/March Madness/pct_tourney.csv")
pct_both_tourney <- read.csv("~/Projects/Kaggle/March Madness/pct_both_tourney.csv")
rpi <- read.csv("~/Projects/Kaggle/March Madness/rpi.csv")
rpi_submission <- read.csv("~/Projects/Kaggle/March Madness/rpi_submission.csv")
seed_submission <- read.csv("~/Projects/Kaggle/March Madness/seed_submission.csv")
cm_submission <- read.csv("~/Projects/Kaggle/March Madness/cm_submission.csv")
chessmetrics <- read.csv("~/Projects/Kaggle/March Madness/chessmetrics.csv")

head(regular_season_results)
wteam_diff <- regular_season_results$wscore-regular_season_results$lscore
wteam_diff <- cbind(regular_season_results$season,regular_season_results$wteam,wteam_diff)
lteam_diff <- regular_season_results$lscore-regular_season_results$wscore
lteam_diff <- cbind(regular_season_results$season,regular_season_results$lteam,lteam_diff)
colnames(all_diff) <- c("season","team","diff")

a_train <- regular_season_results[regular_season_results$season=="A",]
a_test <- tourney_results[tourney_results$season=="A",]

## what do we do about neutral sites?
homeaway <- function(wteam,lteam,wloc) {
  hteam <- ifelse(wloc=="H",wteam,lteam)
  ateam <- ifelse(wloc=="H",lteam,wteam)
  loc <- ifelse(wloc=="H",1,0)
  cbind(hteam,ateam,loc)
}
a_train[c("hteam","lteam","loc")] <- homeaway(wteam,lteam,wloc)
head(a_train)
## basic logistic regression based on home and away status of each team
glm(data=a_train,loc~factor(hteam)+factor(ateam))
