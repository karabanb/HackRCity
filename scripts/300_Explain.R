library(ingredients)
load('results/201/automl1.RData')
h2o.init()
popIndexModel <- h2o.loadModel(paste0(getwd(), '/results/201/DeepLearning_grid_1_AutoML_20191116_160212_model_7'))

vi_autoML_h2o <- h2o.varimp(popIndexModel)

vi_autoML <- ingredients::feature_importance(explainer_h2o_automl1, n_sample = 10)

top10 <- vi_autoML_h2o[1:10,]

ggplot(top5, aes(x = reorder(variable, relative_importance), y = relative_importance))+
  geom_col() +
  coord_flip()

