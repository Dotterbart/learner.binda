library(mlr)
library(binda)

makeRLearner.classif.binda <- function(){
  makeRLearnerClassif(
    cl = "classif.binda",
    package = "binda",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda.freqs")
    ),
    properties = c("twoclass", "multiclass", "numerics", "prob")
  )
}


trainLearner.classif.binda = function(.learner, .task, .subset, .weights = NULL, ...){
  dat <- getTaskData(.task, .subset)
  taN <- getTaskTargetNames(.task, .subset)
  tar <- getTaskTargets(.task, .subset)
  binda::binda(as.matrix(dat[setdiff(names(dat), taN)]), tar)
}

predictLearner.classif.binda <- function(.learner, .model, .newdata, ...){
  erg <- predict(.model$learner.model, newdata = .newdata, ...)
  if (learner$predict.type == "response")
    return(erg$class)
  else
    return(erg$posterior)
}

lrn <- makeLearner("classif.binda")
