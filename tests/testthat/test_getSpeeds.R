skip_on_cran()

test_that("getSpeeds stops if input is not recognized", {
	expect_error(getSpeeds("a"), "Could not recognise the input as an actel results object.", fixed = TRUE)
	expect_error(getSpeeds(list(A = "a")), "Could not recognise the input as an actel results object.", fixed = TRUE)
	xresults <- example.results
	attributes(xresults$dist.mat)$valid <- NULL
	expect_error(getSpeeds(xresults), "The input object was not compiled using actel 1.1.0 or higher. Please re-run the analysis with the current version of actel.", fixed = TRUE)
	xresults <- example.results
	xresults$dist.mat <- NULL
	expect_error(getSpeeds(xresults), "These results do not contain a valid distances matrix.", fixed = TRUE)
})

xresults <- example.results
xresults$valid.movements[[1]] <- xresults$valid.movements[[1]][-1, ]
xresults$valid.movements[[3]] <- xresults$valid.movements[[3]][-1, ]
xresults$valid.movements[[5]] <- xresults$valid.movements[[5]][-1, ]
test_that("getSpeeds is extracting speeds.", {
	x <- getSpeeds(xresults, n.events = "all")
	expect_equal(nrow(x), 449)
	expect_equal(colnames(x), c("Tag", "Event", "From.array", "From.station", "To.array", "To.station", "Speed"))
	expect_equal(x$Speed[1:6], xresults$valid.movements[[1]]$Average.speed.m.s[1:6])
	expect_equal(as.character(unique(x$Tag)), as.character(names(xresults$valid.movements)))
})

test_that("getSpeeds argument 'direct' is working.", {
	x <- getSpeeds(xresults, direct = TRUE, n.events = "all")
	expect_equal(nrow(x), 443)
	expect_true(
		all(
			apply(x, 1, function(i) {
				if (i[3] == "Release")
					i[5] == "A1"
				else
					i[5] %in% xresults$arrays[[i[3]]]$neighbours
				})))
})

test_that("getSpeeds argument 'type' is working", {
	x <- getSpeeds(xresults, n.events = "all", type = "forward")
	expect_equal(nrow(x), 445)
	expect_true(all(
			apply(x, 1, function(i) {
				if (i[3] == "Release")
					TRUE # should be equal to A1 but I removed some events at the start, so just skip it
				else
					i[5] %in% xresults$arrays[[i[3]]]$all.after
				})))

	x <- getSpeeds(xresults, n.events = "all", type = "forward", direct = TRUE)
	expect_equal(nrow(x), 439)
	expect_true(
		all(
			apply(x, 1, function(i) {
				if (i[3] == "Release")
					TRUE # should be equal to A1 but I removed some events at the start, so just skip it
				else
					i[5] %in% xresults$arrays[[i[3]]]$after
				})))

	x <- getSpeeds(xresults, n.events = "all", type = "backward")
	expect_equal(nrow(x), 4)
	expect_true(
		all(
			apply(x, 1, function(i) {
				if (i[3] == "Release")
					TRUE # should be equal to A1 but I removed some events at the start, so just skip it
				else
					i[5] %in% xresults$arrays[[i[3]]]$all.before
				})))
})

test_that("getSpeeds argument 'n.events' is working", {
	x <- getSpeeds(xresults, n.events = "first")
	expect_equal(nrow(x), 444)

	check <- paste(x$Tag, x$From.array, x$To.array, sep = "_")
	expect_false(any(duplicated(check)))

	expect_equal(x$Event[259], 2)

	x <- getSpeeds(xresults, n.events = "last")
	expect_equal(nrow(x), 444)

	check <- paste(x$Tag, x$From.array, x$To.array, sep = "_")
	expect_false(any(duplicated(check)))

	expect_equal(x$Event[259], 2)
})
