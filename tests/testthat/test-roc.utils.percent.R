library(pROC)
data(aSAH)

context("roc.utils.percent")

test_that("roc_utils_topercent works on full AUC", {
	expect_equal_ignore_call(pROC:::roc_utils_topercent.roc(r.wfns), r.wfns.percent)
})

test_that("roc_utils_unpercent works on full AUC", {
	expect_equal_ignore_call(pROC:::roc_utils_unpercent.roc(r.wfns.percent), r.wfns)
})

test_that("roc_utils_topercent works on partial AUC", {
	expect_equal_ignore_call(pROC:::roc_utils_topercent.roc(r.wfns.partial1), r.wfns.percent.partial1)
})

test_that("roc_utils_unpercent works on partial AUC", {
	expect_equal_ignore_call(pROC:::roc_utils_unpercent.roc(r.wfns.percent.partial1), r.wfns.partial1)
})

test_that_no_progress("roc_utils_topercent works with CI", {
	r <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc_utils_topercent.roc(r), r.percent)
})

test_that_no_progress("roc_utils_unpercent works with CI", {
	r <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc_utils_unpercent.roc(r.percent), r)
})

test_that("roc_utils_topercent works without AUC", {
	r <- roc(aSAH$outcome, aSAH$wfns, auc=FALSE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, auc=FALSE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc_utils_topercent.roc(r), r.percent)
})

test_that("roc_utils_unpercent works without AUC", {
	r <- roc(aSAH$outcome, aSAH$wfns, auc=FALSE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, auc=FALSE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc_utils_unpercent.roc(r.percent), r)
})

test_that_no_progress("roc_utils_topercent works with different types of CI", {
	r <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE)
	r.percent <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, percent = TRUE)
	expect_equal_ignore_call(pROC:::roc_utils_topercent.roc(r), r.percent)
})

test_that_no_progress("roc.utils.to/unpercent works with ci .thresholds, .sp, .se", {
	skip_slow()
	for (of in c("thresholds", "sp", "se")) {
		set.seed(42)
		r <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, of = of)
		set.seed(42)
		r.percent <- roc(aSAH$outcome, aSAH$wfns, ci=TRUE, percent = TRUE, of = of)
		expect_equal_ignore_call(pROC:::roc_utils_unpercent.roc(r.percent), r)
		expect_equal_ignore_call(pROC:::roc_utils_topercent.roc(r), r.percent)
	}
})