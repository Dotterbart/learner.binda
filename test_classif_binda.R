context("classif_binda")

test_that("classif_binda", {
  multiclass.df.binda = normalize(x = multiclass.df, method = "standardize")
  multiclass.df.binda = normalize(x = multiclass.df.binda, method = "range")
  multiclass.df.binda = apply(multiclass.df.binda,2, round)
  multiclass.df.binda = as.data.frame(multiclass.df.binda)
  multiclass.train.binda = multiclass.df.binda[multiclass.train.inds, ]
  multiclass.test.binda = multiclass.df.binda[multiclass.test.inds, ]
  
  requirePackages("binda", default.method = "load")
  set.seed(getOption("mlr.debug.seed"))
  m = binda::binda(as.matrix(multiclass.train.binda), multiclass.df[multiclass.train.inds, multiclass.target])
  set.seed(getOption("mlr.debug.seed"))
  p = predict(m, as.matrix(multiclass.test.binda))
  multiclass.df.binda$Species = multiclass.df$Species
  
  testSimple("classif.binda", multiclass.df.binda, multiclass.target, multiclass.train.inds, p$class)
  testProb  ("classif.binda", multiclass.df.binda, multiclass.target, multiclass.train.inds, p$posterior)
  
  tt = binda::binda
  tp = function(model, newdata) predict(model, newdata)$class
  
  testCV("classif.lda", multiclass.df, multiclass.target, tune.train = tt, tune.predict = tp)
})