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
all_diff <- rbind(wteam_diff,lteam_diff)
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
glm(data=a_train,loc~factor(hteam)+factor(ateam))

library(randomForest)

last_sag <- subset(sagp_weekly_ratings, rating_day_num==133)[,c(1,4,5)]
last_chess <- subset(chessmetrics, rating_day_num==133)[,c(1,3,4)]
tourney_results <- merge(tourney_results, last_chess, by.x=c("season","wteam"), by.y=c("season","team"))
colnames(tourney_results) <- c(colnames(tourney_results)[1:7],"wrating")
tourney_results<- merge(tourney_results, last_chess, by.x=c("season","lteam"), by.y=c("season","team"))
colnames(tourney_results) <- c(colnames(tourney_results)[1:8],"lrating")
tourney_results[,c("grating","brating","gscore","bscore")] <- ifelse(c(tourney_results$wrating>tourney_results$lrating,tourney_results$wrating>tourney_results$lrating,tourney_results$wrating>tourney_results$lrating,tourney_results$wrating>tourney_results$lrating)
                                                           ,c(tourney_results$wrating,tourney_results$lrating,tourney_results$wscore,tourney_results$lscore)
                                                           ,c(tourney_results$lrating,tourney_results$wrating,tourney_results$lscore,tourney_results$wscore))
tourney_results$diff <- tourney_results$gscore-tourney_results$bscore
tourney_results$win <- ifelse(tourney_results$diff>0,1,0)

train <- subset(tourney_results,season %in% c("K","F","I","P","O","B","D","A","L","J"))
cross <- subset(tourney_results,season %in% c("E","G","H","R"))
test <- subset(tourney_results,season %in% c("C","M","Q"))

marchRf <- randomForest(x=train[,10:11], y=train$win,
                                 xtest=cross[,10:11], ytest=cross$win,
                                 keep.forest=T, ntree=5000, norm.votes=T)

results <- cbind(predict(marchRf,test[,c(10:11,15)]),test$win)
colnames(results) <- c("pred","actual")
results <- as.data.frame(results)
table(results$pred>.5 & results$actual==1)

library(Metrics)

logLoss(results$actual, results$pred)

all_results <- cbind(predict(marchRf,tourney_results[,c(10:11,15)]),tourney_results$win)
colnames(all_results) <- c("pred","actual")
all_results <- as.data.frame(all_results)
table(all_results$pred>.5 & all_results$actual==1)
logLoss(all_results$actual, all_results$pred)
