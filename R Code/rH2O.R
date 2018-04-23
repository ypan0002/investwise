#install.packages("h2o")
library(h2o)
h2o.init(nthreads = -1)
library(caret)
setwd("C:/Users/Tim/Documents/Masters of Data Science/FIT5120 - Industry Experience/Data Sets/Iteration Two/")

#setwd("//ad.monash.edu/home/User021/tric0003/Desktop/FIT5120 - IE/Iteration Two/")

features2016Df = read.csv(file = "allFeatures2016.csv", header = TRUE, row.names = NULL)
features2016 = as.h2o(features2016Df)

y1 <- "Rating"  #response column: digits 0-9
x1 <- setdiff(names(features2016), y1)  #vector of predictor column names

dl_fit1 <- h2o.deeplearning(x = x1,
                            y = y1,
                            training_frame = features2016,
                            nfolds = 10,
                            model_id = "dl_fit1",
                            hidden = c(25,25),
                            seed = 1)
h2o.confusionMatrix(dl_fit1)

features2016DfNoLGA = features2016Df[,-1]
features2016NoLGA = as.h2o(features2016DfNoLGA)

y2 <- "Rating"  #response column: digits 0-9
x2 <- setdiff(names(features2016NoLGA), y2)  #vector of predictor column namesa

dl_fit2 <- h2o.deeplearning(x = x2,
                            y = y2,
                            training_frame = features2016NoLGA,
                            nfolds = 5,
                            model_id = "dl_fit2",
                            hidden = c(50,50),
                            seed = 1)
h2o.confusionMatrix(dl_fit2)




features2016NoLGASplit.split = h2o.splitFrame(data=features2016NoLGA, ratios=0.9)
features2016NoLGASplit.train <- features2016NoLGASplit.split[[1]]
features2016NoLGASplit.test <- features2016NoLGASplit.split[[2]]

y3 <- "Rating"  #response column: digits 0-9
x3 <- setdiff(names(features2016NoLGA), y3)  #vector of predictor column names


dl_fit3 <- h2o.deeplearning(x = x3,
                            y = y3,
                            training_frame = features2016NoLGASplit.train,
                            nfolds = 5,
                            model_id = "dl_fit3",
                            hidden = c(50,50),
                            seed = 1)

perf = h2o.performance(dl_fit3, features2016NoLGASplit.test)
perf

pred = h2o.predict(dl_fit3, features2016NoLGASplit.test)
predDf = as.data.frame(pred)
testDf = as.data.frame(features2016NoLGASplit.test)
compDf = cbind(predDf, testDf[,77])
write.csv(compDf, file = "compDf.csv", row.names = FALSE, col.names = FALSE)


features2021Df = read.csv(file = "allFeatures2021.csv", header = TRUE, row.names = NULL)
features2021DfNoLGA = features2021Df[,-1]
features2021NoLGA = as.h2o(features2021DfNoLGA)
pred2021 = h2o.predict(dl_fit2, features2021NoLGA)
pred2021Df = as.data.frame(pred2021)


















features2016PerformanceDfNoLGA = features2016Df[,-1]
features2016NoLGA = as.h2o(features2016DfNoLGA)

y2 <- "Rating"  #response column: digits 0-9
x2 <- setdiff(names(features2016NoLGA), y2)  #vector of predictor column namesa

dl_fit2 <- h2o.deeplearning(x = x2,
                            y = y2,
                            training_frame = features2016NoLGA,
                            nfolds = 5,
                            model_id = "dl_fit2",
                            hidden = c(50,50),
                            seed = 1)
h2o.confusionMatrix(dl_fit2)

















features2016DfPerformance = read.csv(file = "allFeatures2016Performance.csv", header = TRUE, row.names = NULL)
features2016Performance = as.h2o(features2016DfPerformance)



features2016DfPerformanceNoLGA = features2016DfPerformance[,-1]
features2016PerformanceNoLGA = as.h2o(features2016DfPerformanceNoLGA)

y5 <- "Performance"  #response column: digits 0-9
x5 <- setdiff(names(features2016PerformanceNoLGA), y5)  #vector of predictor column namesa

dl_fit5 <- h2o.deeplearning(x = x5,
                            y = y5,
                            training_frame = features2016PerformanceNoLGA,
                            nfolds = 5,
                            model_id = "dl_fit5",
                            hidden = c(50,50),
                            seed = 1)


pred2016Performance = h2o.predict(dl_fit5, features2016PerformanceNoLGA)
pred2016PerformanceDf = as.data.frame(pred2016Performance)
pred2016PerformanceDf$Rating = ifelse(pred2016PerformanceDf$predict <= summary(pred2016PerformanceDf$predict)[[2]], "Under Perform", ifelse(pred2016PerformanceDf$predict >= summary(pred2016PerformanceDf$predict)[[5]], "Out Perform", "Average"))
confusionMatrix(pred2016PerformanceDf$Rating, features2016DfNoLGA$Rating)


features2021PerformanceDf = read.csv(file = "allFeatures2021Performance.csv", header = TRUE, row.names = NULL)
features2021DfPerformanceNoLGA = features2021PerformanceDf[,-1]
features2021PerformanceNoLGA = as.h2o(features2021DfPerformanceNoLGA)
pred2021Performance = h2o.predict(dl_fit5, features2021PerformanceNoLGA)
pred2021PerformanceDf = as.data.frame(pred2021Performance)
pred2021PerformanceDf$Rating = ifelse(pred2021PerformanceDf$predict <= summary(pred2021PerformanceDf$predict)[[2]], "Under Perform", ifelse(pred2021PerformanceDf$predict >= summary(pred2021PerformanceDf$predict)[[5]], "Out Perform", "Average"))

suburb2021Performance = as.data.frame(allFeaturesDf$Suburb)
suburb2021Performance$Performance2021 = pred2021PerformanceDf$Rating

write.csv(suburb2021Performance, file = "suburb2021Performance.csv")







features2016DfPerformanceNoLGA = features2016DfPerformance[,-1]
features2016PerformanceNoLGA = as.h2o(features2016DfPerformanceNoLGA)

y6 <- "Performance"  #response column: digits 0-9
x6 <- setdiff(names(features2016PerformanceNoLGA), y6)  #vector of predictor column namesa

dl_fit6 <- h2o.gbm(x = x6,
                            y = y6,
                            training_frame = features2016PerformanceNoLGA,
                            nfolds = 5,
                            model_id = "dl_fit6",
                            seed = 1)


pred2016Performance = h2o.predict(dl_fit6, features2016PerformanceNoLGA)
pred2016PerformanceDf = as.data.frame(pred2016Performance)
pred2016PerformanceDf$Rating = ifelse(pred2016PerformanceDf$predict <= summary(pred2016PerformanceDf$predict)[[2]], "Under Perform", ifelse(pred2016PerformanceDf$predict >= summary(pred2016PerformanceDf$predict)[[5]], "Out Perform", "Average"))
confusionMatrix(pred2016PerformanceDf$Rating, features2016DfNoLGA$Rating)






gbm <- h2o.gbm(
  ## standard model parameters
  x = x6, 
  y = y6, 
  training_frame = features2016PerformanceNoLGA,
  nfolds = 5,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.01,                                                         
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-5, stopping_metric = "MSE", 
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8,                                                   
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                 
)

gbm


pred2016Performance = h2o.predict(gbm, features2016PerformanceNoLGA)
pred2016PerformanceDf = as.data.frame(pred2016Performance)
pred2016PerformanceDf$Rating = ifelse(pred2016PerformanceDf$predict <= summary(pred2016PerformanceDf$predict)[[2]], "Under Perform", ifelse(pred2016PerformanceDf$predict >= summary(pred2016PerformanceDf$predict)[[5]], "Out Perform", "Average"))
confusionMatrix(pred2016PerformanceDf$Rating, features2016DfNoLGA$Rating)


















## Depth 10 is usually plenty of depth for most datasets, but you never know
hyper_params = list( max_depth = seq(1,29,2) )
#hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## full Cartesian hyper-parameter search
  search_criteria = list(strategy = "Cartesian"),
  
  ## which algorithm to run
  algorithm="gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id="depth_grid",
  
  ## standard model parameters
  x = x6, 
  y = y6, 
  training_frame = features2016PerformanceNoLGA,
  nfolds = 5,
  
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  
  ## sample 80% of columns per split
  col_sample_rate = 0.8, 
  
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5,
  stopping_tolerance = 1e-5,
  stopping_metric = "MSE", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                
)

## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
grid                                                                       

## sort the grid models by decreasing AUC
sortedGrid <- h2o.getGrid("depth_grid", sort_by="MSE", decreasing = FALSE)    
sortedGrid

## find the range of max_depth for the top 5 models
topDepths = sortedGrid@summary_table$max_depth[1:5]                       
minDepth = min(as.numeric(topDepths))
maxDepth = max(as.numeric(topDepths))
minDepth
maxDepth











hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                         
  
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train))-1,1),                                 
  
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "AUC",
  stopping_tolerance = 1e-3
)

grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid", 
  
  ## standard model parameters
  x = x6, 
  y = y6, 
  training_frame = features2016PerformanceNoLGA,
  nfolds = 5,
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,                                                 
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-5, stopping_metric = "MSE", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)

## Sort the grid models by AUC
sortedGrid <- h2o.getGrid("final_grid", sort_by = "MSE", decreasing = FALSE)    
sortedGrid

gbm <- h2o.getModel(sortedGrid@model_ids[[1]])



pred2016Performance = h2o.predict(gbm, features2016PerformanceNoLGA)
pred2016PerformanceDf = as.data.frame(pred2016Performance)
pred2016PerformanceDf$Rating = ifelse(pred2016PerformanceDf$predict <= summary(pred2016PerformanceDf$predict)[[2]], "Under Perform", ifelse(pred2016PerformanceDf$predict >= summary(pred2016PerformanceDf$predict)[[5]], "Out Perform", "Average"))
confusionMatrix(pred2016PerformanceDf$Rating, features2016DfNoLGA$Rating)