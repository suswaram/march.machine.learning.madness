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
## devon's ratings
final_scores <- read.csv("~/Projects/Kaggle/March Madness/final_scores.csv", header=F)
colnames(final_scores) <- c("season", "team", "dev_rating")

library(randomForest)

last_sag <- subset(sagp_weekly_ratings, rating_day_num==133)[,c(1,4,5)]
last_chess <- subset(chessmetrics, rating_day_num==133)[,c(1,3,4)]
last_rpi <- subset(rpi, rating_day_num=133)[,c(1,3,7)]
chess_sag <- merge(last_chess, last_sag, by.x=c("season", "team"),
                   by.y=c("season", "team"), all.x=T)
chess_sag_dev <- merge(chess_sag, final_scores,
                       by.x=c("season", "team"), by.y=c("season", "team"),
                       all.x=T)
chess_sag_dev_lm <- lm(data=chess_sag_dev, rating.y~rating.x*dev_rating+rating.x+dev_rating)
pred_sag <- ifelse(chess_sag_dev$season %in% c("A","B","C","D","E"),
                   predict(chess_sag_dev_lm, newdata=subset(chess_sag_dev, season %in% c("A","B","C","D","E"))),
                   chess_sag$rating.y)
chess_sag_dev <- cbind(chess_sag_dev[,c(1:3,5)], pred_sag)
colnames(chess_sag_dev) <- c("season", "team", "chess", "sag", "dev")

tourney_results <- merge(tourney_results, chess_sag_dev,
                         by.x=c("season","wteam"), by.y=c("season","team"),
                         all.x=T)
colnames(tourney_results) <- c(colnames(tourney_results)[1:7],
                               "wrating_chess", "wrating_sag", "wrating_dev")
tourney_results <- merge(tourney_results, chess_sag_dev, by.x=c("season","lteam"),
                         by.y=c("season","team"), all.x=T)
colnames(tourney_results) <- c(colnames(tourney_results)[1:10],
                               "lrating_chess", "lrating_sag", "lrating_dev")
tourney_results[,c("grating_chess", "grating_sag", "grating_dev",
                   "brating_chess", "brating_sag", "brating_dev",
                   "gscore", "bscore")] <-
  ifelse(
    c(tourney_results$wrating_chess>tourney_results$lrating_chess,
      tourney_results$wrating_chess>tourney_results$lrating_chess,
      tourney_results$wrating_chess>tourney_results$lrating_chess,
      tourney_results$wrating_chess>tourney_results$lrating_chess,
      tourney_results$wrating_chess>tourney_results$lrating_chess,
      tourney_results$wrating_chess>tourney_results$lrating_chess,
      tourney_results$wrating_chess>tourney_results$lrating_chess,
      tourney_results$wrating_chess>tourney_results$lrating_chess),
    c(tourney_results$wrating_chess,tourney_results$wrating_sag,tourney_results$wrating_dev,
      tourney_results$lrating_chess,tourney_results$lrating_sag,tourney_results$lrating_dev,
      tourney_results$wscore,tourney_results$lscore),
    c(tourney_results$lrating_chess,tourney_results$lrating_sag,tourney_results$lrating_dev,
      tourney_results$wrating_chess,tourney_results$wrating_sag,tourney_results$wrating_dev,
      tourney_results$lscore,tourney_results$wscore))
tourney_results$chess_diff <- tourney_results$grating_chess-tourney_results$brating_chess
tourney_results$sag_diff <- tourney_results$grating_sag-tourney_results$brating_sag
tourney_results$dev_diff <- tourney_results$grating_dev-tourney_results$brating_dev

tourney_results$diff <- tourney_results$gscore-tourney_results$bscore
tourney_results$win <- ifelse(tourney_results$diff>0,1,0)
tourney_results$win_factor <- as.factor(ifelse(tourney_results$diff>0,"win","loss"))

sample(LETTERS[1:18])

train <- subset(tourney_results, season %in% c("C", "P", "I", "E", "O", "A", "J", "N", "D", "Q", "H"))
cross <- subset(tourney_results, season %in% c("F", "K", "R", "B"))
test <- subset(tourney_results, season %in% c("M", "G", "L"))

marchRf <- randomForest(x=train[,c(14:19,22:24)], y=train$win, keep.forest=T,
                        xtest=cross[,c(14:19,22:24)], ytest=cross$win, nperm=2,
                        ntree=5000, norm.votes=T, strata=season, corr.bias=T)

results <- cbind(predict(marchRf,
                         newdata=rbind(train, cross)[,c(14:19,22:24,26)]),
                 rbind(train, cross)[,26])
colnames(results) <- c("pred","win")
results <- as.data.frame(results)
table(results$pred>.5 & results$win==1)

library(Metrics)

## for training/cross data
logLoss(results$win, results$pred) ##.2936813

test_results <- cbind(predict(marchRf,test[,c(14:19,22:24,26)]),
                      test[,26])
colnames(test_results) <- c("pred", "win")
test_results <- as.data.frame(test_results)
table(test_results$pred>.5 & test_results$win==1)

logLoss(test_results$win, test_results$pred) ## .4948581

other_march_rf <- randomForest(x=tourney_results[,c(14:19,22:24)],
                               y=tourney_results$win, keep.forest=T,
                               ntree=5000, norm.votes=T, nperm=2,
                               strata=tourney_results$season, corr.bias=T)
other_results <- cbind(predict(other_march_rf),
                       tourney_results[,26])
colnames(other_results) <- c("pred","win")
other_results <- as.data.frame(other_results)
table(other_results$pred>.5 & other_results$win==1)
logLoss(other_results$win, other_results$pred) ## .5610305

other_test_results <- cbind(predict(other_march_rf),
                            tourney_results[,c(1,26)])
other_test_results <- subset(other_test_results, season %in% c("M", "G", "L"))
colnames(other_test_results) <- c("pred","season", "win")
other_test_results <- as.data.frame(other_test_results)
table(other_test_results$pred>.5 & other_test_results$win==1)
logLoss(other_test_results$win, other_test_results$pred)
