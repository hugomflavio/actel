skip_on_cran()

test_that("getSpeeds stops if input is not recognized", {
	expect_error(getSpeeds("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(getSpeeds(list(A = "a")), "Could not recognise the input as an actel results object.", fixed = TRUE)
	xresults <- example.results
	xresults$dist.mat <- NULL
	expect_error(getSpeeds(xresults), "These results do not contain a valid distances matrix.", fixed = TRUE)
})

test_that("getSpeeds is extracting speeds.", {
	x <- getSpeeds(example.results)
	expect_equal(nrow(x), 452)
	expect_equal(colnames(x), c("Fish", "Event", "From.array", "From.station", "To.array", "To.station", "Speed"))
	expect_equal(x$Speed[1:7], example.results$valid.movements[[1]]$Average.speed.m.s[1:7])
	expect_equal(unique(x$Fish), names(example.results$valid.movements))
})

test_that("getSpeeds argument 'direct' is working.", {
	x <- getSpeeds(example.results, direct = TRUE)
	expect_equal(nrow(x), 449)
	expect_true(
		all(
			apply(x, 1, function(i) {
				if (i[3] == "Release")
					i[5] == "A1"
				else
					i[5] %in% example.results$arrays[[i[3]]]$neighbours
				})))
	})
})

test_that("getSpeeds argument 'type' is working", {
	x <- getSpeeds(example.results, type = "forward")
	expect_equal(nrow(x), 448)
	expect_true(
		all(
			apply(x, 1, function(i) {
				if (i[3] == "Release")
					i[5] == "A1"
				else
					i[5] %in% example.results$arrays[[i[3]]]$all.after
				})))

	x <- getSpeeds(example.results, type = "forward", direct = TRUE)
	expect_equal(nrow(x), 445)
	expect_true(
		all(
			apply(x, 1, function(i) {
				if (i[3] == "Release")
					i[5] == "A1"
				else
					i[5] %in% example.results$arrays[[i[3]]]$after
				})))

	x <- getSpeeds(example.results, type = "backward")
	expect_equal(nrow(x), 4)
	expect_true(
		all(
			apply(x, 1, function(i) {
				if (i[3] == "Release")
					i[5] == "A1"
				else
					i[5] %in% example.results$arrays[[i[3]]]$all.before
				})))
})

test_that("getSpeeds argument 'n.events' is working", {
	x <- getSpeeds(example.results, n.events = "first")
	expect_equal(nrow(x), 447)

	check <- paste(x$Fish, x$From.array, x$To.array, sep = "_")
	expect_false(any(duplicated(check)))

	expect_equal(x$Event[259], 15)

	x <- getSpeeds(example.results, n.events = "last")
	expect_equal(nrow(x), 447)

	check <- paste(x$Fish, x$From.array, x$To.array, sep = "_")
	expect_false(any(duplicated(check)))

	expect_equal(x$Event[259], 19)
})

