##' Watch for a log of something.
##'
##' @title Watch logs
##'
##' @param what Very short summary of the process underlying the
##'   logs. Examples include "task", "installation", etc.
##'
##' @param get_status Function that will fetch a status. Accepts no
##'   arguments, and returns one of a set of (by default) "waiting",
##'   "running" or any status you like after that.  When status is
##'   "waiting" we show a spinner but do not stream logs. When
##'   "running" we stream logs (but don't show a spinner). Any other
##'   statement returns.  You can change the expected "waiting" or
##'   "running" status values with the `status_waiting` and
##'   `status_running` arguments.
##'
##' @param get_log A callback to read logs of the installation
##'   (something like `function() readLines(filename, warn = FALSE)`
##'   may be sufficient), or `NULL` if log fetching is not possible.
##'
##' @param show_log Logical, indicating if the installation log should
##'   be printed.
##'
##' @param show_spinner Logical, indicating if a spinner should be
##'   shown while waiting for the task to start, and if `show_log` is
##'   `FALSE` for the task to complete.
##'
##' @param skip Optional integer indicating how to handle log content
##'   that exists at the point where we start watching. The default
##'   (0) shows all log contents.  A positive integer skips that many
##'   lines, while a negative integer shows only that many lines (so
##'   -5 shows the first five lines in the log).  You can pass `Inf`
##'   to discard all previous logs, but stream all new ones.
##'
##' @param poll Time, in seconds, used to throttle calls to the status
##'   function. The default is 1 second
##'
##' @param timeout Timeout, in seconds, after which we give up. This
##'   does not cancel the underlying process being watched!  We return
##'   `status_timeout` after a timeout (by default "timeout").
##'
##' @param status_waiting The value of a waiting status (can be a
##'   vector of possible statuses)
##'
##' @param status_running The value of a running status (can be a
##'   vector of possible statuses)
##'
##' @param status_timeout The value to return if we timeout
##'
##' @param status_interrupt The value to return if we are interrupted
##'
##' @param multiple Logical, indicating if `get_status()` is expected
##'   to return multiple values.  This changes the progress slightly
##'   by disabling the logging (even if `get_log` is provided and
##'   `show_log = TRUE`, logs are never fetched or shown), and there
##'   is no longer a distinction between waiting for things to start
##'   vs complete.  Instead, we print a summary of things in the
##'   waiting, running and finished states.
##'
##' @return A list with elements:
##'
##' * status: Your final status call
##' * start: The start time
##' * end: The end time
##'
##' @export
logwatch <- function(what, get_status, get_log, show_log = TRUE, skip = 0,
                     show_spinner = TRUE, poll = 1, timeout = Inf,
                     status_waiting = "waiting", status_running = "running",
                     status_timeout = "timeout",
                     status_interrupt = "interrupt",
                     multiple = FALSE) {
  ## TODO: we should allow for skipping of some amount of log at first
  get_status_throttled <- throttle(get_status, poll, timeout)
  show_log <- !multiple && show_log && !is.null(get_log)
  t0 <- Sys.time()
  logs <- NULL
  status_incomplete <- c(status_waiting, status_running)
  tryCatch({
    status <- get_status_throttled()
    if (multiple) {
      if (show_spinner) {
        summary <- ""
        cli::cli_progress_bar(
          format = paste("{cli::pb_spin} Waiting for {what}",
                         "({summary}) [{cli::pb_elapsed}]"),
          format_done = paste("{.alert-success Waited {cli::pb_elapsed}",
                              "for {what}}"))
      }
      while (any(status %in% status_incomplete)) {
        if (show_spinner) {
          n_waiting <- sum(status %in% status_waiting)
          n_running <- sum(status %in% status_running)
          n_finished <- length(status) - n_waiting - n_running
          summary <- sprintf("%d waiting, %d running, %d finished",
                             n_waiting, n_running, n_finished)
          cli::cli_progress_update()
        }
        status <- get_status_throttled()
      }
      if (show_spinner) {
        cli::cli_progress_done()
      }
    } else {
      if (status %in% status_waiting) {
        if (show_spinner) {
          cli::cli_progress_bar(
            format = paste("{cli::pb_spin} Waiting for {what} to start",
                           "[{cli::pb_elapsed}]"),
            format_done = paste("{.alert-success Waited {cli::pb_elapsed}",
                                "for {what} to start}"))
        }
        while (status %in% status_waiting) {
          if (show_spinner) {
            cli::cli_progress_update()
          }
          status <- get_status_throttled()
        }
        if (show_spinner) {
          cli::cli_progress_done()
        }
      }
      ## TODO: it would be nice to have a reassuring spinner here, but
      ## there's no "cleanup last print" functionality in cli at the
      ## moment (as we've used in the past) so not doing this yet.
      if (status %in% status_running) {
        if (!show_log && show_spinner) {
          cli::cli_progress_bar(
            format = paste("{cli::pb_spin} Waiting for {what} to finish",
                           "[{cli::pb_elapsed}]"),
            format_done = paste("{.alert-success Waited {cli::pb_elapsed}",
                                "for {what} to finish}"))
        }
        while (status %in% status_running) {
          if (show_log) {
            logs <- show_new_log(get_log(), logs, skip)
          } else if (show_spinner) {
            cli::cli_progress_update()
          }
          status <- get_status_throttled()
        }
        if (!show_log && show_spinner) {
          cli::cli_progress_done()
        }
      }
    }
    if (show_log) {
      logs <- show_new_log(get_log(), logs, skip)
    }
  },
  timeout = function(e) {
    status[status %in% status_incomplete] <<- status_timeout
  },
  interrupt = function(e) {
    status[status %in% status_incomplete] <<- status_interrupt
  })

  list(status = status, start = t0, end = Sys.time())
}


show_new_log <- function(curr, prev, skip) {
  if (is.null(prev) && skip != 0) {
    show <- utils::tail(curr, -skip)
  } else if (length(prev) == 0) {
    show <- curr
  } else {
    show <- curr[-seq_along(prev)]
  }
  if (length(show) > 0) {
    message(paste(show, collapse = "\n"))
  }
  curr %||% character()
}


throttle <- function(call, interval, timeout) {
  last <- Sys.time() - interval
  time_end <- Sys.time() + timeout
  function(expr) {
    now <- Sys.time()
    if (now > time_end) {
      stop(structure(list(message = "timeout"),
                     class = c("timeout", "error", "condition")))
    }
    wait <- interval - (now - last)
    if (wait > 0) {
      Sys.sleep(wait)
    }
    last <<- Sys.time()
    call()
  }
}
