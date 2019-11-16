
load('results/201/automl1.RData')

popIndexModel <- h2o.loadModel(paste0(getwd(), '/results/201/DeepLearning_grid_1_AutoML_20191116_160212_model_7'))

vi_autoML_h2o <- h2o.varimp(popIndexModel)

h2o.varimp_plot(popIndexModel, num_of_features = 10)

vi_autoML <- variable_importance(explainer_h2o_automl1, n_sample = 10)
