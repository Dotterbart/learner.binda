library(mlr)
library(binda)
?binda

makeRLeaner.classif.binda <- function(){
  makeRLearnerClassif(
    cl = "regr.binda",
    package = "binda",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda.freqs")
    ),
    properties = c("twoclass", "multiclass", "prob")
  )
}


trainLearner.classif.binda = function(.learner, .task, .subset, .weights = NULL, ...){
  dat <- getTaskData(.task, .subset)
  taN <- getTaskTargetNames(.task, .subset)
  tar <- getTaskTargets(.task, .subset)
  binda(as.matrix(dat[setdiff(names(dat), taN)]), tar)
}

predictLearner.classif.binda <- function(.learner, .model, .newdata, ...){
  erg <- predict(.model$learner.model, newdata = .newdata, ...)
  if (learner$predict.type == "response")
    return(erg$class)
  else
    return(erg$posterior)
}
