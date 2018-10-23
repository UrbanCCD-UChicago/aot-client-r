context("ls.projects")

test_that("returns a data frame", {
  df <- ls.projects()
  expect_equal("data.frame", class(df), label="class")
})


context("stat.project")

test_that("returns a list", {
  chi <- stat.project("chicago")
  expect_equal("list", class(chi), label="class")
})


context("ls.nodes")

test_that("returns a data frame", {
  df <- ls.nodes()
  expect_equal("data.frame", class(df), label="class")
})

test_that("apply filters", {
  all <- ls.nodes()
  co <- ls.nodes(filters=list(has_sensor="chemsense.co.concentration"))
  expect_equal(nrow(all) >= nrow(co), TRUE)
})

test_that("unknown filter values return 200 and 0 length arrays", {
  zilch <- ls.nodes(filters=list(has_sensor="barf.o.meter"))
  expect_equal(nrow(zilch), 0)
})


context("stat.node")

test_that("returns a list", {
  node <- stat.node("004")
  expect_equal("list", class(node), label="class")
})


context("ls.sensors")

test_that("returns a data frame", {
  df <- ls.sensors()
  expect_equal("data.frame", class(df), label="class")
})

test_that("apply filters", {
  all <- ls.sensors()
  n004 <- ls.sensors(filters=list(onboard_node="004"))
  expect_equal(nrow(all) >= nrow(n004), TRUE)
})

test_that("unknown filter values return 200 and 0 length arrays", {
  zilch <- ls.sensors(filters=list(onboard_node="666"))
  expect_equal(nrow(zilch), 0)
})


context("stat.sensor")

test_that("returns a list", {
  sensor <- stat.sensor("metsense.bmp180.temperature")
  expect_equal("list", class(sensor), label="class")
})


context("ls.observations")

test_that("returns a data frame", {
  df <- ls.observations()
  expect_equal("data.frame", class(df), label="class")
})

test_that("apply filters", {
  df <- ls.observations(filters=list(by_sensor="metsense.bmp180.temperature"))
  expect_equal("list", class(df), label="class")
})

test_that("unknown filter values return 200 and 0 length arrays", {
  zilch <- ls.observations(filters=list(by_sensor="barf.o.meter"))
  expect_equal(nrow(zilch), 0)
})


context("ls.raw_observations")

test_that("returns a data frame", {
  df <- ls.raw_observations()
  expect_equal("data.frame", class(df), label="class")
})

test_that("apply filters", {
  df <- ls.raw_observations(filters=list(by_sensor="metsense.bmp180.temperature"))
  expect_equal("list", class(df), label="class")
})

test_that("unknown filter values return 200 and 0 length arrays", {
  zilch <- ls.raw_observations(filters=list(by_sensor="barf.o.meter"))
  expect_equal(nrow(zilch), 0)
})
