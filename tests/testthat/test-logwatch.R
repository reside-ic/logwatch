test_that("throttle", {
  a <- 0
  f <- function() {
    a <<- a + 1
  }
  throttled <- throttle(f, 0.05, Inf)
  t1 <- Sys.time() + 0.5
  while (Sys.time() < t1) {
    throttled(f(1))
  }
  expect_lte(a, 11)
  expect_gte(a, 2) # this is hard on the very slow mac runner
})


test_that("throttle can timeout", {
  f <- mockery::mock(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  throttled <- throttle(f, 0.05, 0.2)
  t1 <- Sys.time() + 0.5
  err <- expect_error(
    while (Sys.time() < t1) {
      throttled(f(1))
    }, "timeout")

  expect_s3_class(err, c("timeout", "error", "condition"), exact = TRUE)
})


test_that("collect logs from immediately running job", {
  get_status <- mockery::mock("RUNNING", "RUNNING", "RUNNING", "SUCCESS")
  get_log <- mockery::mock(letters[1], letters[1:2], letters[1:3], letters[1:4])
  res <- evaluate_promise(
    logwatch("job", get_status, get_log, show_log = TRUE, poll = 0,
             status_waiting = "WAITING", status_running = "RUNNING"))
  expect_equal(res$result$status, "SUCCESS")
  expect_equal(res$messages, paste0(letters[1:4], "\n"))
  mockery::expect_called(get_status, 4)
  mockery::expect_called(get_log, 4)
})


test_that("collect logs with uneven log updates", {
  get_status <- mockery::mock(
    "running", "running", "running", "running", "running", "success")
  get_log <- mockery::mock(
    letters[1:2], letters[1:2], letters[1:5], letters[1:5], letters[1:6],
    letters[1:9])
  res <- evaluate_promise(
    logwatch("job", get_status, get_log, show_log = TRUE, poll = 0))
  expect_equal(res$result$status, "success")
  expect_equal(res$messages,
               c(paste0(letters[1:2], "\n", collapse = ""),
                 paste0(letters[3:5], "\n", collapse = ""),
                 paste0(letters[6], "\n", collapse = ""),
                 paste0(letters[7:9], "\n", collapse = "")))
  mockery::expect_called(get_status, 6)
  mockery::expect_called(get_log, 6)
})


test_that("can poll before we start", {
  mock_progress_bar <- mockery::mock()
  mock_progress_update <- mockery::mock()
  mock_progress_done <- mockery::mock()
  mockery::stub(logwatch, "cli::cli_progress_bar", mock_progress_bar)
  mockery::stub(logwatch, "cli::cli_progress_update", mock_progress_update)
  mockery::stub(logwatch, "cli::cli_progress_done", mock_progress_done)

  get_status <- mockery::mock(
    "waiting", "waiting", "waiting", "waiting",
    "running", "running", "running",
    "success")
  get_log <- mockery::mock(
    letters[1], letters[1:2], letters[1:3], letters[1:4])
  res <- evaluate_promise(
    logwatch("job", get_status, get_log, show_log = TRUE, poll = 0))

  expect_equal(res$result$status, "success")
  mockery::expect_called(get_status, 8)
  mockery::expect_called(get_log, 4)
  expect_true(all(paste0(c("a", "b", "c", "d"), "\n") %in% res$messages))

  mockery::expect_called(mock_progress_bar, 1)
  args <- mockery::mock_args(mock_progress_bar)[[1]]
  expect_length(args, 2)
  expect_equal(
    args$format,
    "{cli::pb_spin} Waiting for {what} to start [{cli::pb_elapsed}]")
  expect_equal(
    args$format_done,
    "{.alert-success Waited {cli::pb_elapsed} for {what} to start}")
  mockery::expect_called(mock_progress_update, 4)
  expect_equal(
    mockery::mock_args(mock_progress_update),
    rep(list(list()), 4))
  mockery::expect_called(mock_progress_done, 1)
  expect_equal(
    mockery::mock_args(mock_progress_done),
    list(list()))
})


test_that("can suppress output", {
  get_status <- mockery::mock("running", "running", "running", "success")
  get_log <- mockery::mock(NULL,
                           "a", "a", c("a", "b"), c("a", "b", "c"))
  res <- testthat::evaluate_promise(
    logwatch("job", get_status, get_log, poll = 0, show_log = FALSE))
  expect_equal(res$result$status, "success")
  expect_equal(res$output, "")
  expect_equal(res$messages, character())
  mockery::expect_called(get_status, 4)
  mockery::expect_called(get_log, 0)
  expect_equal(mockery::mock_args(get_status), rep(list(list()), 4))
})


test_that("show log differences", {
  expect_silent(res <- show_new_log(NULL, NULL))
  expect_null(res)

  msg <- capture_messages(
    expect_equal(show_new_log(c("a", "b"), NULL), c("a", "b")))
  expect_equal(msg, c("a\nb\n"))

  msg <- capture_messages(
    expect_equal(show_new_log(c("a", "b"), c("a", "b")), c("a", "b")))
  expect_equal(msg, character())

  msg <- capture_messages(
    expect_equal(show_new_log(c("a", "b", "c"), c("a", "b")), c("a", "b", "c")))
  expect_equal(msg, "c\n")
})


test_that("can cope with timeout", {
  get_status <- mockery::mock("running", cycle = TRUE)
  get_log <- mockery::mock()
  res <- testthat::evaluate_promise(
    logwatch("job", get_status, get_log, poll = .01, timeout = 0.2,
             show_log = FALSE))
  expect_equal(res$result$status, "timeout")
  expect_equal(res$output, "")
  expect_equal(res$messages, character())
})


test_that("can cope with interrupt", {
  get_status <- mockery::mock(
    "running", "running", "running",
    signalCondition(structure(list(), class = c("interrupt", "condition"))))
  get_log <- mockery::mock()
  res <- testthat::evaluate_promise(
    logwatch("job", get_status, get_log, poll = 0, show_log = FALSE))
  expect_equal(res$result$status, "interrupt")
  mockery::expect_called(get_status, 4)
})
