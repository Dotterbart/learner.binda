
library(mlr)
library(binda)
makeRLearner.classif.binda <- function(){
  makeRLearnerClassif(
    cl = "classif.binda",
    package = "binda",
    par.set = makeParamSet(
      makeNumericLearnerParam(id = "lambda.freqs")
    ),
    properties = c("twoclass", "multiclass", "prob", "numerics")
  )
}
trainLearner.classif.binda <- function(.learner, .task, .subset, .weights = NULL, ...){
  dat <- getTaskData(.task, .subset)
  taN <- getTaskTargetNames(.task)
  tar <- getTaskTargets(.task)
  binda::binda(as.matrix(dat[setdiff(names(dat), taN)]), tar)
}
predictLearner.classif.binda <- function(.learner, .model, .newdata, ...){
  erg <- predict(.model$learner.model, newdata = .newdata, ...)
  
  # erg <- predict(.model$learner.model, as.matrix(.newdata), ...)
  
  # dat <- getTaskData(.newdata)
  # taN <- getTaskTargetNames(.newdata)
  # erg <- predict(.model$learner.model, as.matrix(dat[setdiff(names(dat), taN)]), ...)
  if (learner$predict.type == "response")
    return(erg$class)
  else
    return(erg$posterior)
}

lrn <- makeLearner("classif.binda")
tr <- train(lrn, bt)
# Bis dahin funktioniert alles, ich bekomme auch Vorhersagen, wenn ich den klassischen
# "binda-Weg" gehe, der eine Matrix der neuen Daten will, mittels:
# pr <- predict(tr$learner.model, <<Matrix mit Werten>>) (**) <-- predict mit Binda-Modell

# Will ich allerdings den "mlr-Weg" gehen, dann bekomme ich mit
# pr <- predict(tr, <<Task oder Data-Frame>>)
# immer Fehlermeldungen, die darauf hindeuten, dass die Daten für die neuen Vorhersagen
# fehlen bzw. in falschem Format vorliegen.
# Ich habe schon so einige Variationen ausprobiert (zwei davon siehe oben auskommentiert), 
# bekomme aber immer Fehler, auch bei den beiden auskommentierten Varianten, bei denen
# nach meinem Verständins eine entsprechende Matrix wie in (**) übergeben werden sollte.
