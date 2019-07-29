#' @title Inherit Documentation for Binary Classification Metrics
#' @name params_binary
#' @description This object provides the documentation for the parameters of functions
#'              that provide binary classification metrics
#' @param actual The ground truth binary numeric vector containing 1 for the positive
#'               class and 0 for the negative class.
#' @param predicted The predicted binary numeric vector containing 1 for the positive
#'                  class and 0 for the negative class. Each element represents the
#'                  prediction for the corresponding element in \code{actual}.
NULL

#' Area under the ROC curve (AUC)
#'
#' \code{auc} computes the area under the receiver-operator characteristic curve (AUC).
#'
#' \code{auc} uses the fact that the area under the ROC curve is equal to the probability
#' that a randomly chosen positive observation has a higher predicted value than a
#' randomly chosen negative value. In order to compute this probability, we can
#' calculate the Mann-Whitney U statistic. This method is very fast, since we
#' do not need to compute the ROC curve first.
#'
#' @inheritParams params_binary
#' @param predicted A numeric vector of predicted values, where the smallest values correspond
#'                  to the observations most believed to be in the negative class
#'                  and the largest values indicate the observations most believed
#'                  to be in the positive class. Each element represents the
#'                  prediction for the corresponding element in \code{actual}.
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' auc(actual, predicted)
auc <- function(actual, predicted) {
    if (length(actual) != length(predicted)) {
        msg <- "longer object length is not a multiple of shorter object length"
        warning(msg)
    }
    r <- rank(predicted)
    n_pos <- as.numeric(sum(actual == 1))
    n_neg <- length(actual) - n_pos
    return((sum(r[actual == 1]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg))
}

#' Log Loss
#'
#' \code{ll} computes the elementwise log loss between two numeric vectors.
#'
#' @inheritParams params_binary
#' @param predicted A numeric vector of predicted values, where the values correspond
#'                  to the probabilities that each observation in \code{actual}
#'                  belongs to the positive class
#' @export
#' @seealso \code{\link{logLoss}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' ll(actual, predicted)
ll <- function(actual, predicted) {
    score <- -(actual * log(predicted) + (1 - actual) * log(1 - predicted))
    score[actual == predicted] <- 0
    score[is.nan(score)] <- Inf
    return(score)
}

#' Mean Log Loss
#'
#' \code{logLoss} computes the average log loss between two numeric vectors.
#'
#' @inheritParams ll
#' @export
#' @seealso \code{\link{ll}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' logLoss(actual, predicted)
logLoss <- function(actual, predicted) {
    return(mean(ll(actual, predicted)))
}



#' Precision
#'
#' \code{precision} computes proportion of observations predicted to be in the
#'                  positive class (i.e. the element in \code{predicted} equals 1)
#'                  that actually belong to the positive class (i.e. the element
#'                  in \code{actual} equals 1)
#'
#' @inheritParams params_binary
#' @export
#' @seealso \code{\link{recall}} \code{\link{fbeta_score}} \code{\link{ppv}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 1, 1, 1, 1, 1)
#' precision(actual, predicted)
precision <- function(actual, predicted) {
    cm <- confusion_matrix(actual, predicted)
    cm$tp / (cm$tp + cm$fp)
}

#' Recall
#'
#' \code{recall} computes proportion of observations in the positive class
#'               (i.e. the element in \code{actual} equals 1) that are predicted
#'               to be in the positive class (i.e. the element in \code{predicted}
#'               equals 1)
#'
#' @inheritParams params_binary
#' @export
#' @seealso \code{\link{precision}} \code{\link{fbeta_score}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' recall(actual, predicted)
recall <- function(actual, predicted) {
    cm <- confusion_matrix(actual, predicted)
    cm$tp / (cm$tp + cm$fn)
}

#' F-beta Score
#'
#' \code{fbeta_score} computes a weighted harmonic mean of Precision and Recall.
#'                    The \code{beta} parameter controls the weighting.
#'
#' @inheritParams params_binary
#' @param beta A non-negative real number controlling how close the F-beta score is to
#'             either Precision or Recall. When \code{beta} is at the default of 1,
#'             the F-beta Score is exactly an equally weighted harmonic mean.
#'             The F-beta score will weight toward Precision when \code{beta} is less
#'             than one.  The F-beta score will weight toward Recall when \code{beta} is
#'             greater than one.
#' @export
#' @seealso \code{\link{precision}} \code{\link{recall}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' recall(actual, predicted)
fbeta_score <- function(actual, predicted, beta = 1) {
    prec <- precision(actual, predicted)
    rec <- recall(actual, predicted)
    return((1 + beta^2) * prec * rec / ((beta^2 * prec) + rec))
}

#' Binary confusion matrix
#'
#' \code{confusion_matrix} Calculates a binary classification confusion matrix,
#' comparing the predicted with the actual values for the classes.
#' Assumes that 1 is used for the positive class and 0 for the
#' negative class.
#'
#' Returns a \code{data.frame} with columns corresponding to the
#' number of True Positives (\code{tp}), False Positives (\code{fp}),
#' True Negatives (\code{tn}), and False Negatives (\code{fn})
#'
#' @inheritParams params_binary
#' @seealso \code{\link{sensitivity}} \code{\link{specificity}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' confusion_matrix(actual, predicted)
confusion_matrix <- function(actual, predicted) {
    binvals <- c(0, 1)

    # ideally "actual" should be a combination of 0s and 1s,
    # but could be all 0s or all 1s as degenerate cases
    if (!(
        setequal(binvals, unique(actual)) |
        setequal(c(0), unique(actual)) |
        setequal(c(1), unique(actual))
    )) {
        stop(paste("Expecting a vector of 0s and 1s for 'actual'. Got:",
                   paste(actual, collapse = ", ")))
    }

    # "predicted" could be all 0s, all 1s, or a combination
    if (!(
        setequal(binvals, unique(predicted)) |
        setequal(c(0), unique(predicted)) |
        setequal(c(1), unique(predicted))
    )) {
        stop(paste("Expecting a vector of 0s and 1s for 'predicted'. Got:",
                    paste(predicted, collapse = ", ")))
    }

    if (length(actual) != length(predicted)) {
        stop(
            paste(
                "Size of 'actual' and 'predicted' are not the same:",
                length(actual), "!=", length(predicted)
            )
        )
    }

    # explicit comparison
    tp <- sum(actual == 1 & predicted == 1)
    tn <- sum(actual == 0 & predicted == 0)
    fn <- sum(actual == 1 & predicted == 0)
    fp <- sum(actual == 0 & predicted == 1)
    data.frame("tp" = tp, "fn" = fn, "fp" = fp, "tn" = tn)
}

#' Sensitivity
#'
#' \code{sensitivity} calculates the proportion of actual positives (\code{actual} equals 1)
#' that are correctly identified as such. It is also known as
#' \code{true positive rate} or \code{recall}.
#'
#' @inheritParams params_binary
#' @seealso \code{\link{confusion_matrix}} \code{\link{specificity}}
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' sensitivity(actual, predicted)
sensitivity <- function(actual, predicted) {
    recall(actual, predicted)
}

#' Specificity
#'
#' \code{specificity} calculates the proportion of actual negatives (\code{actual} equals 0)
#' that are correctly identified as such. It is also known as
#' \code{true negative rate}.
#'
#' @inheritParams params_binary
#' @seealso \code{\link{confusion_matrix}} \code{\link{sensitivity}}
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' specificity(actual, predicted)
specificity <- function(actual, predicted) {
    cm <- confusion_matrix(actual, predicted)
    cm$tn / (cm$tn + cm$fp)
}

#' False Negative Rate
#'
#' \code{fnr} calculates the proportion of actual positives (\code{actual} equals 1)
#' that are not identified as such.
#'
#' It is defined as \code{1 - sensitivity}
#'
#' @inheritParams params_binary
#' @export
#' @seealso \code{\link{confusion_matrix}} \code{\link{sensitivity}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' fnr(actual, predicted)
fnr <- function(actual, predicted) {
    1 - sensitivity(actual, predicted)
}

#' False Positive Rate
#'
#' \code{fnr} calculates the proportion of actual negative values (\code{actual} equals 0)
#' that are not identified as such.
#'
#' It is defined as \code{1 - specificity}
#'
#' @inheritParams params_binary
#' @seealso \code{\link{confusion_matrix}} \code{\link{specificity}}
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' fpr(actual, predicted)
fpr <- function(actual, predicted) {
    1 - specificity(actual, predicted)
}

#' Positive Predictive Value
#'
#' \code{ppv} calculates the proportion of all predicted positive values (\code{predicted} equals 1) that
#' are true positive results. It is also known as \code{precision}
#'
#' @inheritParams params_binary
#' @seealso \code{\link{confusion_matrix}} \code{\link{npv}} \code{\link{precision}}
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' ppv(actual, predicted)
ppv <- function(actual, predicted) {
    precision(actual, predicted)
}

#' Negative Predictive Value
#'
#' \code{ppv} calculates the proportion all predicted negative values (\code{predicted} equals 0) that
#' are true negative results.
#'
#' @inheritParams params_binary
#' @seealso \code{\link{confusion_matrix}} \code{\link{npv}}
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' npv(actual, predicted)
npv <- function(actual, predicted) {
    cm <- confusion_matrix(actual, predicted)
    cm$tn / (cm$tn + cm$fn)
}

#' False Discovery Rate
#'
#' \code{fdr} computes proportion of observations predicted to be in
#' the positive class (i.e. the element in \code{predicted} equals 1) that
#' actually belong to the negative class (i.e.the element in \code{actual} equals 0).
#'
#' It is implemented as \code{1 - ppv}
#'
#' @inheritParams params_binary
#' @seealso \code{\link{confusion_matrix}} \code{\link{ppv}}
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' fpr(actual, predicted)
fdr <- function(actual, predicted) {
    1 - ppv(actual, predicted)
}

#' False Omission Rate
#'
#' \code{fomr} is the complement of the Negative Predictive
#' Value (\code{npv}), and is the proportion of negative
#' results that are false negatives.
#'
#' @inheritParams params_binary
#' @seealso \code{\link{confusion_matrix}} \code{\link{npv}}
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' fomr(actual, predicted)
fomr <- function(actual, predicted) {
    1 - npv(actual, predicted)
}

#' Positive Likelihood Ratio
#'
#' \code{lrp} is used to assessing the value of performing a
#' diagnostic test, and estimates the ratio of the probability
#' of a true positive result over the probability of a false positive
#' result.
#'
#' It is implemented as the ratio: \code{sensitivity / (1 - specificity)}
#'
#' @inheritParams params_binary
#' @seealso \code{\link{tpr}} \code{\link{fpr}}
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' lrp(actual, predicted)
lrp <- function(actual, predicted) {
    sensitivity(actual, predicted) / (1 - specificity(actual, predicted))
}

#' Negative Likelihood Ratio
#'
#' \code{lrn} is used to assessing the value of performing a
#' diagnostic test, and estimates the ratio of the probability
#' of a true negative result over the probability of a false negative
#' result.
#'
#' It is implemented as the ratio: \code{(1 - sensitivity) / specificity}
#'
#' @inheritParams params_binary
#' @seealso \code{\link{specificity}} \code{\link{sensitivity}}
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' lrn(actual, predicted)
lrn <- function(actual, predicted) {
    (1 - sensitivity(actual, predicted)) / specificity(actual, predicted)
}
