load("data/100_CleanedData.Rdata")
load("data/100_RegressionData.Rdata")

popIndexData <- regression_data %>% 
  as.data.frame() %>% 
  filter(year == 2018)

train_val <- popIndexData %>% 
  filter(!is.na(touristic_popularity))

write.csv(train_val, file = "data/201_train_val.csv", row.names = FALSE)

h2o.init()
df <- h2o.importFile("data/201_train_val.csv")

splits <- h2o.splitFrame(df, ratios = 0.7, seed = 42)

train <- splits[[1]]
test <- splits[[2]]
test_df <- as.data.frame(df)

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
  return(as.numeric(res$predict))
}

explainer_h2o_automl1 <- explain(model = LeadModel, 
                                 data = test_df %>% select(-y),  
                                 y = test_df[,y],
                                 predict_function = custom_predict,
                                 label = "h2o autoML")

explainer_h2o_automl1$y_hat

plot(explainer_h2o_automl1$y, explainer_h2o_automl1$y_hat)
lines(c(0,3000), c(0,3000))

mp_automl_1 <- model_performance(explainer_h2o_automl1)

plot(mp_automl_1)

vi_m1 <- DALEX::variable_importance(explainer_h2o_automl1, n_sample = 10)
plot(vi_m1)
#vi_m2 <- variable_importance(explainer_h2o_automl2)

#plot(vi_m1)

h2o.saveModel(LeadModel, path = "results/202")

save(explainer_h2o_automl1, file = "results/201_explainerForPopularity.Rdata")

########################################

plot(mp_automl_1, geom = "boxplot")

library(iBreakDown)

#breakDown <- local_attributions(explainer_h2o_automl1, 
          #                      new_observation = test_df[test_df$city == "poznan",])

#breadDown2 <- local_attributions(explainer_h2o_automl1, 
           #                      new_observation = test_df[test_df$city == "poznan",])
#explainer_h2o_automl1$data <- na.omit(explainer_h2o_automl1$data)

############ PDP ###############

exp_data <- explainer_h2o_automl1$data

pdp_ttd_nr_of_above_4 <- ingredients::partial_dependency(explainer_h2o_automl1,
                                           variables = "restaurants_nr_of_medium_expensive", N = 50)

plot(pdp_ttd_nr_of_above_4)

explainer_h2o_automl1$data

pred_data <- popIndexData %>% 
  filter(is.na(touristic_popularity))

write.csv(pred_data, file = "data/pred_data.csv", row.names = FALSE)

pred_data_h2o <- h2o.importFile("data/pred_data.csv")

predictions <- as.data.frame(predict(LeadModel, newdata = pred_data_h2o))

output_df <- cbind(pred_data$city, predictions)
colnames(output_df) <- c("city", "touristic_popularity")
save(output_df, file = "results/201/1_output_df.Rdata")


