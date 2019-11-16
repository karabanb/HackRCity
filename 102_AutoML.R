load("data/100_CleanedData.Rdata")

write.csv(cleaned_data, file = "data/Cleaned.csv", row.names = FALSE)

h2o.init()
df <- h2o.importFile("data/Cleaned.csv")

h2o.describe(df)

y <- "TURYSTYCZNE_OBIEKTY.turysci_zagraniczni_ogolem"

splits <- h2o.splitFrame(df, ratios = 0.8, seed = 42)

train <- splits[[1]]
test <- splits[[2]]

aml <- h2o.automl(y = y,
                  training_frame = train,
                  leaderboard_frame = test,
                  max_runtime_secs = 120,
                  seed = 42,
                  project_name = "tourist")

print(aml@leaderboard)
plot(aml@leader, metric = "mae")

LeadModel <- aml@leader

library(DALEX)

custom_predict <- function(model, newdata)  {
  newdata_h2o <- as.h2o(newdata)
  res <- as.data.frame(h2o.predict(model, newdata_h2o))
  return(as.numeric(res$predict))
}


explainer_h2o_glm <- explain(model = LeadModel, 
                             data = train,  
                             y = train[y],
                             predict_function = custom_predict,
                             label = "h2o autoML")
