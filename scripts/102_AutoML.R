load("data/100_CleanedData.Rdata")

write.csv(cleaned_data, file = "data/Cleaned.csv", row.names = FALSE)

h2o.init()
df <- h2o.importFile("data/Cleaned.csv")

h2o.describe(df)

y <- "TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem"

splits <- h2o.splitFrame(df, ratios = 0.8, seed = 42)

train <- splits[[1]]
test <- splits[[2]]
test_df <- as.data.frame(test)

aml <- h2o.automl(y = y,
                  training_frame = train,
                  leaderboard_frame = test,
                  max_runtime_secs = 120,
                  seed = 42,
                  project_name = "tourist")

print(aml@leaderboard[2])
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

explainer_h2o_automl2 <- explain(model = h2o.getModel("GBM_1_AutoML_20191116_134202"), 
                             data = test_df %>% select(-y),  
                             y = test_df[,y],
                             predict_function = custom_predict,
                             label = "h2o autoML")

mp_automl_1 <- model_performance(explainer_h2o_automl1)

mp_automl_2 <- model_performance(explainer_h2o_automl2)

plot(mp_automl_1, mp_automl_2)
plot(mp_automl_1, mp_automl_2, geom = "boxplot")
#vi_m1 <- variable_importance(explainer_h2o_automl1)
#vi_m2 <- variable_importance(explainer_h2o_automl2)

plot(vi_m1)

h2o.saveModel(LeadModel, path = "results")
h2o.saveModel(h2o.getModel("GBM_1_AutoML_20191116_134202"), path = "results")

save(explainer_h2o_automl1, file = "results/102_automl1.Rdata")
save(explainer_h2o_automl2, file = "results/102_automl2.Rdata")


