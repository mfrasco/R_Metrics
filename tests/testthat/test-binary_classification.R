context('binary classification')

test_that('area under ROC curve is calculated correctly', {
    expect_equal(auc(c(1,0,1,1), c(.32,.52,.26,.86)), 1/3)
    expect_equal(auc(c(1,0,1,0,1),c(.9,.1,.8,.1,.7)), 1)
    expect_equal(auc(c(0,1,1,0),c(.2,.1,.3,.4)), 1/4)
    expect_equal(auc(c(1,1,1,1,0,0,0,0,0,0),0*(1:10)), 0.5)
    expect_warning(auc(c(0, 1, 0), rnorm(2)), regexp = 'longer object')
})

test_that('log loss is calculated correctly', {
    expect_equal(ll(1,1), 0)
    expect_equal(ll(1,0), Inf)
    expect_equal(ll(0,1), Inf)
    expect_equal(ll(1,0.5), -log(0.5))
})

test_that('mean los loss is calculated correctly', {
    expect_equal(logLoss(c(1,1,0,0),c(1,1,0,0)), 0)
    expect_equal(logLoss(c(1,1,0,0),c(1,1,1,0)), Inf)
    expect_equal(logLoss(c(1,1,1,0,0,0),c(.5,.1,.01,.9,.75,.001)), 1.881797068998267)
})

test_that('precision is calculated correctly', {
    expect_equal(precision(c(1,1,0,0),c(1,1,0,0)), 1)
    expect_equal(precision(c(0,0,1,1),c(1,1,0,0)), 0)
    expect_equal(precision(c(1,1,0,0),c(1,1,1,1)), 1/2)
})

test_that('recall is calculated correctly', {
  expect_equal(recall(c(1,1,0,0),c(1,1,0,0)), 1)
  expect_equal(recall(c(0,0,1,1),c(1,1,0,0)), 0)
  expect_equal(recall(c(1,1,1,1),c(1,0,0,1)), 1/2)
})

test_that('f-beta score is calculated correctly',{
  expect_equal(fbeta_score(c(1,1,0,0),c(1,1,0,0)), 1)
  expect_equal(fbeta_score(c(0,0,1,1),c(1,1,1,0)), 2/5)
  expect_equal(fbeta_score(c(1,1,1,1),c(1,0,0,1)), 2/3)
  expect_equal(fbeta_score(c(1,1,0,0),c(1,1,1,1),beta=0), 1/2)
})

test_that(
  "sensitivity is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(sensitivity(a, p), 2/3, tol = 0.01)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(sensitivity(a, p), 2/3, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(sensitivity(a, p), 2/3, tol = 0.01)
  }
)

test_that(
  "specificity is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(specificity(a, p), 0.91, tol = 0.01)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(specificity(a, p), 0.53, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(specificity(a, p), 0.53, tol = 0.01)
  }
)

test_that(
  "false negative rate (fnr) is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(fnr(a, p), 1/3, tol = 0.01)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(fnr(a, p), 1/3, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(fnr(a, p), 1/3, tol = 0.01)
  }
)

test_that(
  "false positive rate (fpr) is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(fpr(a, p), 0.09, tol = 0.01)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(fpr(a, p), 0.47, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(fpr(a, p), 0.47, tol = 0.01)
  }
)

test_that(
  "positive predictive value (ppv) is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(ppv(a, p), 0.1, tol = 0.01)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(ppv(a, p), 0.2, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(ppv(a, p), 0.38, tol = 0.01)
  }
)

test_that(
  "negative predictive value (npv) is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(npv(a, p), 0.995, tol = 0.01)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(npv(a, p), 0.9, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(npv(a, p), 0.787, tol = 0.01)
  }
)

test_that(
  "false discovery rate (fdr) is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(fdr(a, p), 0.9, tol = 0.01)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(fdr(a, p), 0.8, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(fdr(a, p), 0.623, tol = 0.01)
  }
)

test_that(
  "false omission rate (fomr) is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(fomr(a, p), 0.0055, tol = 0.0001)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(fomr(a, p), 0.1, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(fomr(a, p), 0.213, tol = 0.01)
  }
)

test_that(
  "positive likelihood rate (lrp) is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(lrp(a, p), 7.41, tol = 0.01)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(lrp(a, p), 1.42, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(lrp(a, p), 1.41, tol = 0.01)
  }
)

test_that(
  "negative likelihood rate (lrn) is calculated correctly",
  {
    # first dataset
    a <- c(rep(1, 20), rep(1, 180), rep(0, 10), rep(0, 1820)) # actual
    p <- c(rep(1, 20), rep(0, 180), rep(1, 10), rep(0, 1820)) # predicted
    expect_equal(lrn(a, p), 0.366, tol = 0.001)
    # second dataset
    a <- c(rep(1, 10), rep(1, 40), rep(0, 5), rep(0, 45)) # actual
    p <- c(rep(1, 10), rep(0, 40), rep(1, 5), rep(0, 45)) # predicted
    expect_equal(lrn(a, p), 0.63, tol = 0.01)
    # third dataset
    a <- c(rep(1, 20), rep(1, 33), rep(0, 10), rep(0, 37)) # actual
    p <- c(rep(1, 20), rep(0, 33), rep(1, 10), rep(0, 37)) # predicted
    expect_equal(lrn(a, p), 0.631, tol = 0.001)
  }
)
