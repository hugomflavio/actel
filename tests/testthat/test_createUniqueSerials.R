skip_on_cran()

test_that("createUniqueSerials correctly assigns receivers in the deployments", {
	xdep <- example.deployments
	xdep$Receiver[1] <- 132908
	xdep$Start[1] <- xdep$Start[1] - (3600 * 24 * 78)
	xdep$Stop[1]  <-  xdep$Stop[1] - (3600 * 24 * 78)
	output <- createUniqueSerials(input = xdep)
	expect_true(all(!is.na(match(names(output), unique(xdep$Receiver)))))
	expect_equal(length(output), 16)
	to.check <- c(2, rep(1, 15))
	names(to.check) <- names(output)
	expect_equal(unlist(lapply(output, nrow)), to.check)
	expect_equal(output[[1]]$Receiver, c("132908.dpl.1", "132908.dpl.2"))
})
