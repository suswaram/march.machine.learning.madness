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

library(randomForest)

last_sag <- subset(sagp_weekly_ratings, rating_day_num==133)[,c(1,4,5)]
last_chess <- subset(chessmetrics, rating_day_num==133)[,c(1,3,4)]
chess_sag <- merge(last_chess, last_sag, by.x=c("season", "team"),
                   by.y=c("season", "team"), all.x=T)
chess_sag_lm <- lm(data=chess_sag, rating.y~rating.x)
pred_sag <- ifelse(chess_sag$season %in% c("A","B","C","D","E"),
                   predict(chess_sag_lm, newdata=subset(chess_sag, season %in% c("A","B","C","D","E"))),
                   chess_sag$rating.y)
chess_sag <- cbind(chess_sag, pred_sag)

tourney_results <- merge(tourney_results, last_chess,
                         by.x=c("season","wteam"), by.y=c("season","team"),
                         all.x=T)
tourney_results <- merge(tourney_results, chess_sag$pred_sag,
                         by.x=c("season","wteam"), by.y=c("season","team"),
                         all.x=T)
colnames(tourney_results) <- c(colnames(tourney_results)[1:7],"wrating_chess","wrating_sag")
tourney_results <- merge(tourney_results, last_chess, by.x=c("season","lteam"),
                         by.y=c("season","team"), all.x=T)
tourney_results <- merge(tourney_results, chess_sag$pred_sag, by.x=c("season","lteam"),
                         by.y=c("season","team"), all.x=T)
colnames(tourney_results) <- c(colnames(tourney_results)[1:9],"lrating_chess","lrating_sag")
tourney_results[,c("grating_chess", "grating_sag", "brating_chess", "brating_sag", "gscore", "bscore")] <-
    ifelse(
      c(tourney_results$wrating_chess>tourney_results$lrating_chess,
        tourney_results$wrating_chess>tourney_results$lrating_chess,
        tourney_results$wrating_chess>tourney_results$lrating_chess,
        tourney_results$wrating_chess>tourney_results$lrating_chess,
        tourney_results$wrating_chess>tourney_results$lrating_chess,
        tourney_results$wrating_chess>tourney_results$lrating_chess),
      c(tourney_results$wrating_chess,tourney_results$wrating_sag,
        tourney_results$lrating_chess,tourney_results$lrating_sag,
        tourney_results$wscore,tourney_results$lscore),
      c(tourney_results$lrating_chess,tourney_results$lrating_sag,
        tourney_results$wrating_chess,tourney_results$wrating_sag,
        tourney_results$lscore,tourney_results$wscore))
tourney_results$diff <- tourney_results$gscore-tourney_results$bscore
tourney_results$win <- ifelse(tourney_results$diff>0,1,0)

train <- subset(tourney_results,season %in% c("K","E","I","P","R","C","D","A","L","J"))
cross <- subset(tourney_results,season %in% c("F","G","H","B"))
test <- subset(tourney_results,season %in% c("O","M","Q"))

marchRf <- randomForest(x=train[,12:15], y=train$diff, keep.forest=T,
                        xtest=cross[,12:15], ytest=cross$diff,
                        ntree=4500, norm.votes=T)

results <- cbind(predict(marchRf,test[,c(12:15,18)]), test$diff, test$win)
colnames(results) <- c("pred","actual","win")
results <- as.data.frame(results)
table(results$pred>0 & results$actual>0)
win_prob <- glm(data=results, win~pred, family=binomial)
results <- cbind(results, win_prob$fitted)
colnames(results)[4] <- "win_pred"

library(Metrics)

logLoss(results$win, results$win_pred)

all_results <- cbind(predict(marchRf,tourney_results[,c(12:15,18)]), tourney_results$diff, tourney_results$win)
colnames(all_results) <- c("pred", "actual", "win")
all_results <- as.data.frame(all_results)
table(all_results$pred>0 & all_results$actual>0)
win_prob_all <- glm(data=all_results, win~pred, family=binomial)
all_results <- cbind(all_results, win_prob_all$fitted.values)
colnames(all_results)[4] <- "win_pred"

logLoss(all_results$win, all_results$win_pred)