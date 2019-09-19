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
    expect_equal(ll(1,0), -log(1e-12))
    expect_equal(ll(0,1), -log(1 - (1 - 1e-12)))
    expect_equal(ll(1,0.5), -log(0.5))
})

test_that('mean log loss is calculated correctly', {
    expect_equal(logLoss(c(1,1,0,0),c(1,1,0,0)), 0)
    expect_true(is.finite(logLoss(c(1,1,0,0),c(1,1,1,0))))
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

