load("data/100_CleanedData.Rdata")

popIndexData <- cleaned_data %>% 
  filter(year == 2018)

train_val <- popIndexData %>% 
  filter(!is.na(touristic_popularity))

write.csv(train_val, file = "data/201_train_val.csv", row.names = FALSE)

h2o.init()
df <- h2o.importFile("data/201_train_val.csv")

splits <- h2o.splitFrame(df, ratios = 0.7, seed = 42)

train <- splits[[1]]
test <- splits[[2]]
test_df <- as.data.frame(test)

y <- "touristic_popularity"

aml <- h2o.automl(y = y,
                  training_frame = train,
                  leaderboard_frame = test,
                  max_runtime_secs = 120,
                  seed = 42,
                  project_name = "tourist")

print(aml@leaderboard)
plot(aml@leader, metric = "mae")

library(DALEX)

LeadModel <- aml@leader

custom_predict <- function(model, newdata)  {
  newdata_h2o <- as.h2o(newdata)
  res <- as.data.frame(h2o.predict(model, newdata_h2o))
  return(as.numeric(res$predict)[-1])
}

explainer_h2o_automl1 <- explain(model = LeadModel, 
                                 data = test_df %>% select(-y),  
                                 y = test_df[,y],
                                 predict_function = custom_predict,
                                 label = "h2o autoML")

plot(explainer_h2o_automl1$y, explainer_h2o_automl1$y_hat)
lines(c(0,3000), c(0,3000))

mp_automl_1 <- model_performance(explainer_h2o_automl1)

plot(mp_automl_1)

#vi_m1 <- variable_importance(explainer_h2o_automl1)
#vi_m2 <- variable_importance(explainer_h2o_automl2)

#plot(vi_m1)

h2o.saveModel(LeadModel, path = "results/201")

save(explainer_h2o_automl1, file = "results/201/automl1.Rdata")


