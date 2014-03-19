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
final_scores <- read.csv("~/Projects/Kaggle/March Madness/final_scores.csv")
colnames(final_scores) <- c("season", "team", "dev_rating", "opprating")
converted_rankings <- read.csv("~/Projects/Kaggle/March Madness/converted_rankings.csv")

library(randomForest)

## get last rating (day 133)

last_sag <- subset(sagp_weekly_ratings, rating_day_num==133)[,c(1,4,5)]
last_chess <- subset(chessmetrics, rating_day_num==133)[,c(1,3,4)]
last_rpi <- subset(rpi, rating_day_num==133)[,c(1,3,7)]
last_ordinal_core <- subset(ordinal_ranks_core_33, rating_day_num==133)
last_ordinal_core <- ddply(last_ordinal_core, .(season), mutate,
                           rating = 3.6411598 - 0.4401187*log(orank + 0.1179306) - orank/117.2996136)
last_col <- subset(last_ordinal_core, sys_name=="COL")[,c(1,4,6)]
last_dci <- converted_rankings[,c(1,2,33)]
colnames(last_dci) <- c("season", "team", "DCI")

## merge ratings w/data
chess_sag <- merge(last_chess, last_sag, by.x=c("season", "team"),
                   by.y=c("season", "team"), all.x=T)
chess_sag_dev <- merge(chess_sag, final_scores,
                       by.x=c("season", "team"), by.y=c("season", "team"),
                       all.x=T)
chess_sag_dev_lm <- lm(data=chess_sag_dev, rating.y~rating.x*dev_rating+rating.x+dev_rating)
pred_sag <- ifelse(chess_sag_dev$season %in% c("A","B","C","D","E"),
                   predict(chess_sag_dev_lm, newdata=subset(chess_sag_dev, season %in% c("A","B","C","D","E"))),
                   chess_sag$rating.y)
chess_sag_dev <- cbind(chess_sag_dev[,c(1:3,5:6)], pred_sag)
chess_sag_dev <- chess_sag_dev[,c(1:3,6,4,5)]
colnames(chess_sag_dev) <- c("season", "team", "chess", "sag", "dev", "DCI")
##chess_sag_dev_rpi <- merge(chess_sag_dev, last_rpi, by.x=c("season", "team"),
##                           by.y=c("season", "team"), all.x=T)
last_ratings <- merge(chess_sag_dev, last_dci, by.x=c("season", "team"),
                      by.y=c("season", "team"), all.x=T)
dci_lm <- lm(data=last_ratings, DCI~dev)
last_ratings$DCI <- ifelse(last_ratings$season %in% c("A", "B", "C", "D", "E", "F", "G"),
                              predict(dci_lm, newdata=subset(last_ratings, season %in% c("A", "B", "C", "D", "E", "F", "G"))),
                              last_ratings$DCI)
##colnames(last_ratings) <- c(colnames(last_ratings)[1:6], "col")
last_ratings <- chess_sag_dev

tourney_results <- merge(tourney_results, last_ratings,
                         by.x=c("season","wteam"), by.y=c("season","team"),
                         all.x=T)
colnames(tourney_results) <- c(colnames(tourney_results)[1:7],
                               "wrating_chess", "wrating_sag", "wrating_dev", "wrating_DCI")
tourney_results <- merge(tourney_results, last_ratings, by.x=c("season","lteam"),
                         by.y=c("season","team"), all.x=T)
colnames(tourney_results) <- c(colnames(tourney_results)[1:11],
                               "lrating_chess", "lrating_sag", "lrating_dev", "lrating_DCI")
tourney_results[,c("grating_chess", "grating_sag", "grating_dev", "grating_DCI",
                   "brating_chess", "brating_sag", "brating_dev", "brating_DCI",
                   "gscore", "bscore")] <-
  ifelse(
    c(tourney_results$wrating_dev>tourney_results$lrating_dev,
      tourney_results$wrating_dev>tourney_results$lrating_dev,
      tourney_results$wrating_dev>tourney_results$lrating_dev,
      tourney_results$wrating_dev>tourney_results$lrating_dev,
      tourney_results$wrating_dev>tourney_results$lrating_dev,
      tourney_results$wrating_dev>tourney_results$lrating_dev,
      tourney_results$wrating_dev>tourney_results$lrating_dev,
      tourney_results$wrating_dev>tourney_results$lrating_dev,
      tourney_results$wrating_dev>tourney_results$lrating_dev,
      tourney_results$wrating_dev>tourney_results$lrating_dev),
    c(tourney_results$wrating_chess,tourney_results$wrating_sag,tourney_results$wrating_dev,tourney_results$wrating_DCI,
      tourney_results$lrating_chess,tourney_results$lrating_sag,tourney_results$lrating_dev,tourney_results$lrating_DCI,
      tourney_results$wscore,tourney_results$lscore),
    c(tourney_results$lrating_chess,tourney_results$lrating_sag,tourney_results$lrating_dev,tourney_results$lrating_DCI,
      tourney_results$wrating_chess,tourney_results$wrating_sag,tourney_results$wrating_dev,tourney_results$wrating_DCI,
      tourney_results$lscore,tourney_results$wscore))
tourney_results$chess_diff <- tourney_results$grating_chess-tourney_results$brating_chess
tourney_results$sag_diff <- tourney_results$grating_sag-tourney_results$brating_sag
tourney_results$dev_diff <- tourney_results$grating_dev-tourney_results$brating_dev
tourney_results$DCI_diff <- tourney_results$grating_DCI-tourney_results$brating_DCI

tourney_results$diff <- tourney_results$gscore-tourney_results$bscore
tourney_results$win <- ifelse(tourney_results$diff>0,1,0)
tourney_results$win_factor <- as.factor(ifelse(tourney_results$diff>0,"win","loss"))

sample(LETTERS[1:18])

train <- subset(tourney_results, season %in% c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q"))
cross <- subset(tourney_results, season %in% c("R", "D", "H", "N"))
test <- subset(tourney_results, season %in% c("R"))

library(RRF)
# 
# marchRf <- randomForest(x=train[,c(14:19,22:24)], y=train$win, keep.forest=T,
#                         xtest=cross[,c(14:19,22:24)], ytest=cross$win, nperm=2,
#                         ntree=5000, norm.votes=T, strata=season, corr.bias=T)
marchRf <- randomForest(x=train[,c(16:23,26:29)], y=train$win, keep.forest=T,
                        nperm=2, norm.votes=T, strata=season, sampsize=67,
                        corr.bias=T)


results <- cbind(predict(marchRf,
                         newdata=rbind(train, cross)[,c(14:19,22:24,26)]),
                 rbind(train, cross)[,26])
colnames(results) <- c("pred","win")
results <- as.data.frame(results)
table(results$pred>.5 & results$win==1)

library(Metrics)

## for training/cross data
logLoss(results$win, results$pred) ##.2936813

test_results_future <- cbind(predict(marchRf,test[,c(16:23,26:29,31)]),
                      test$win)
colnames(test_results_future) <- c("pred", "win")
test_results_future <- as.data.frame(test_results_future)
table(test_results_future$pred>.5 & test_results_future$win==1)

logLoss(test_results_future$win, test_results_future$pred) ## .4948581

test_results <- rbind(test_results, test_results_future)
logLoss(test_results$win, test_results$pred)

other_march_rf <- RRF(x=tourney_results[,c(16:18,20:22,26:28)], y=tourney_results$win,
                      keep.forest=T, norm.votes=T, nperm=2, data=tourney_results,
                      coefReg=.95, sampsize=67, strata=season, corr.bias=T, ntree=300)
other_results <- cbind(predict(other_march_rf),
                       tourney_results$win)
colnames(other_results) <- c("pred","win")
other_results <- as.data.frame(other_results)
other_results$season <- tourney_results$season
table(other_results$pred>.5 & other_results$win==1)
logLoss(other_results$win, other_results$pred) ## 0.5377948
season_logloss <- ddply(other_results, .(season), summarize, logloss=logLoss(win, pred))
season_logloss

h_r_tourney <- subset(tourney_results, season %in% c("H","I","J","K","L","M","N","O","P","Q","R"))

og_cv <- matrix(0,nrow(h_r_tourney),2)
colnames(og_cv) <- c("pred","win")

for (r in 1:nrow(h_r_tourney)) {
  rf <- RRF(x=h_r_tourney[-r,c(16:18,20:22,26:28)], y=h_r_tourney$win[-r],
            keep.forest=T, norm.votes=T, nperm=2,
            coefReg=.95, sampsize=67, corr.bias=T, ntree=300)
  og_cv[r,c(1,2)] <- cbind(predict(rf, newdata=h_r_tourney[r, c(16:18,20:22,26:28,31)]),
                           h_r_tourney[r,31])
}
logLoss(og_cv[,2], og_cv[,1]) ##.5407909
table(og_cv[,2]==1 & og_cv[,1]>.5)

other_test_results <- cbind(predict(other_march_rf),
                            tourney_results[,c(1,26)])
other_test_results <- subset(other_test_results, season %in% c("P"))
colnames(other_test_results) <- c("pred","season", "win")
other_test_results <- as.data.frame(other_test_results)
table(other_test_results$pred>.5 & other_test_results$win==1)
logLoss(other_test_results$win, other_test_results$pred)

### new data

tourney_results_new <- read.csv("~/Projects/Kaggle/March Madness/tourney_results.csv")

tourney_results_new$ateam <- ifelse(tourney_results_new$wteam<tourney_results_new$lteam,
                                    tourney_results_new$wteam, tourney_results_new$lteam)
tourney_results_new$bteam <- ifelse(tourney_results_new$wteam<tourney_results_new$lteam,
                                    tourney_results_new$lteam, tourney_results_new$wteam)

library(robCompositions)
#converted_imputed <- impKNNa(x=converted_rankings[,3:36], k=18, primitive=T)

tourney_results_new <- merge(tourney_results_new, converted_rankings, by.x=c("season", "ateam"),
                             by.y=c("season", "team"), all.x=T)
tourney_results_new <- merge(tourney_results_new, converted_rankings, by.x=c("season", "bteam"),
                             by.y=c("season", "team"), all.x=T, suffixes=c(".a", ".b"))
tourney_results_new$win <- ifelse(tourney_results_new$ateam==tourney_results_new$wteam,
                                  1, 0)

new_rf_imp <- rrfImpute(x=tourney_results_new[,10:77], y=tourney_results_new$win, ntree=500,
                        keep.forest=T, norm.votes=T, nperm=2, data=tourney_results_new,
                        coefReg=.95, sampsize=67, strata=season, corr.bias=T, iter=18)
colnames(new_rf_imp)[1] <- "win"
new_rf_imp_diff <- cbind(new_rf_imp$win,new_rf_imp[,2:35]-new_rf_imp[,36:69])
colnames(new_rf_imp_diff)[1] <- "win"

new_rf_imp_march <- RRF(x=new_rf_imp_diff[,2:35], y=new_rf_imp_diff$win, keep.forest=T,
                        norm.votes=T, nperm=2, coefReg=.95, sampsize=67, strata=season,
                        corr.bias=T)
new_rf_imp_results <- cbind(predict(new_rf_imp_march), new_rf_imp_diff$win)
colnames(new_rf_imp_results) <- c("pred", "win")
new_rf_imp_results <- as.data.frame(new_rf_imp_results)
table(new_rf_imp_results$pred>.5 & new_rf_imp_results$win==1)
logLoss(new_rf_imp_results$win, new_rf_imp_results$pred)
arrange(as.data.frame(cbind(colnames(new_rf_imp_diff)[2:35], new_rf_imp_march$importance)), IncNodePurity)

#### predictions!!! ####

### first get data ###

sagarin_ratings_season_s <- read.csv("~/Projects/Kaggle/March Madness/sagarin_ratings_season_s.csv")
sagarin_ratings_season_s$team <- sagarin_ratings_season_s$team

devon_season_s <- subset(final_scores, season=="S")

s_ratings <- merge(sagarin_ratings_season_s, devon_season_s[,2:3], by.x="team",
                   by.y="team", all.x=T)
colnames(s_ratings) <- c("team", "name", "sag", "dev")
chess_lm <- lm(data=last_ratings,
               subset=season %in% c("F","G","H","I","J","K","L","M","N","O","P","Q","R"),
               chess~sag+dev)
s_ratings$chess <- predict(chess_lm, s_ratings)
s_ratings <- s_ratings[,c(1,2,5,3,4)]

tourney_ids <- character()
ateam <- data.frame(ateam=character(2278), aname=character(2278),
                    achess=rep(0, 2278), asag=rep(0,2278), adev=rep(0,2278))
bteam <- data.frame(bteam=character(2278), bname=character(2278),
                    bchess=rep(0, 2278), bsag=rep(0,2278), bdev=rep(0,2278))

row=0
for (i in 1:68) {
  j=i+1
  while(j<=68) {
    row=row+1
    tourney_ids[row] <- paste("S", sagarin_ratings_season_s[i,1], sagarin_ratings_season_s[j,1], sep="_")
    ateam[row,] <- s_ratings[i,]
    bteam[row,] <- s_ratings[j,]
    j=j+1
  }
}

tourney_s <- as.data.frame(cbind(tourney_ids, ateam, bteam))
colnames(tourney_s)[1] <- "id"
head(tourney_s)
tourney_s[,c("grating_chess", "grating_sag", "grating_dev",
             "brating_chess", "brating_sag", "brating_dev")] <-
  ifelse(c(tourney_s$adev>tourney_s$bdev,
           tourney_s$adev>tourney_s$bdev,
           tourney_s$adev>tourney_s$bdev,
           tourney_s$adev>tourney_s$bdev,
           tourney_s$adev>tourney_s$bdev,
           tourney_s$adev>tourney_s$bdev),
         c(tourney_s$achess, tourney_s$asag, tourney_s$adev,
           tourney_s$bchess, tourney_s$bsag, tourney_s$bdev),
         c(tourney_s$bchess, tourney_s$bsag, tourney_s$bdev,
           tourney_s$achess, tourney_s$asag, tourney_s$adev))
tourney_s$chess_diff <- tourney_s$grating_chess-tourney_s$brating_chess
tourney_s$sag_diff <- tourney_s$grating_sag-tourney_s$brating_sag
tourney_s$dev_diff <- tourney_s$grating_dev-tourney_s$brating_dev
colnames(tourney_s)

## now predict! ##

tourney_s$prob <- predict(other_march_rf, newdata=tourney_s[,12:20])
tourney_s$pred <- ifelse(tourney_s$achess==tourney_s$grating_chess,
                         tourney_s$prob, 1-tourney_s$prob)
submit_s <- tourney_s[,c(1,22)]

write.csv(tourney_s, "~/Projects/Kaggle/March Madness/tourney_s.csv")
write.csv(submit_s, "~/Projects/Kaggle/March Madness/submit_s.csv")
