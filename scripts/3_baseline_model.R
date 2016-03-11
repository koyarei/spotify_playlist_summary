# library(caret)
# library(grid)
# library(caTools)
setwd('/Users/Koya/projects/spotify_playlist_summary')
load('./data/playlist.update.RData')

# Create a baseline single-variate model, use repeated 10 cv to gauge RMSE

################################### Functions #############################

BestSingleVariate <- function(df, vars) {
    # Evaluate all potential independent variables, create the baseline model
    # based on the variable that gives the best RMSE
    result <- data.frame()
    for (var in vars) {
        if (grepl("\\d", var)) {
            x <- var
        } else {
            x <- paste0("log(", var, ")")
        }
        set.seed(123)
        fitControl <- trainControl(method = "repeatedcv", 
                                   number = 10,
                                   repeats = 1)
        formula <- as.formula(paste('log(kpi) ~ ', x))
        baseline.fit <- train(formula, 
                              data = playlist.update, 
                              method = 'lm', 
                              metric = 'RMSE',
                              trControl = fitControl)
        
        baseline.result <- cbind(var, baseline.fit$results)
        result <- rbind(result, baseline.result)
        print(result)
    
    }
    
    return(result)
    
}

#################################### Main ###################################
vars <- c("n_tracks", "n_artists", "n_albums",
          "genre_1", "genre_2", "genre_3",
          "mood_1", "mood_2", "mood_3")

baseline.model <- BestSingleVariate(playlist.update, vars)
save(baseline.model, file = './data/baseline.model.RData')
