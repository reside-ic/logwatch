##' Watch for a log of something.
##'
##' @title Watch logs
##'
##' @param what Very short summary of the process underlying the
##'   logs. Examples include "task", "installation", etc.
##'
##' @param get_status Function that will fetch a status. Accepts no
##'   arguments, and returns one of a set of "waiting", "running" or
##'   any status you like after that.  When status is "waiting" we
##'   show a spinner but do not stream logs. When "running" we stream
##'   logs (but don't show a spinner). Any other statement returns.
##'
##' @param get_log A callback to read logs of the installation
##'   (something like `function() readLines(filename, warn = FALSE)`
##'   may be sufficient)
##'
##' @param show_log Logical, indicating if the installation log should
##'   be printed
##'
##' @param poll Time, in seconds, used to throttle calls to the status
##'   function. The default is 1 second
##'
##' @return A list with elements:
##'
##' * status: Your final status call
##' * start: The start time
##' * end: The end time
##'
##' @export
logwatch <- function(what, get_status, get_log, show_log = TRUE, poll = 1) {
  ## TODO: we should add an interrupt handler here too, and indicate
  ## if we were interrupted; let the caller decide what to do with
  ## that?
  throttled <- throttle(poll)
  t0 <- Sys.time()
  logs <- NULL
  status <- throttled(get_status())
  if (status == "waiting") {
    cli::cli_progress_bar(
      format = "{cli::pb_spin} Waiting for {what} to start [{cli::pb_elapsed}]",
      format_done = paste("{.alert-success Waited {cli::pb_elapsed}",
                          "for {what} to start}"))
    while (status == "waiting") {
      cli::cli_progress_update()
      status <- throttled(get_status())
    }
    cli::cli_progress_done()
  }
  ## TODO: it would be nice to have a reassuring spinner here, but
  ## there's no "cleanup last print" functionality in cli at the
  ## moment (as we've used in the past) so not doing this yet.
  if (status == "running") {
    while (status == "running") {
      if (show_log) {
        logs <- show_new_log(get_log(), logs)
      }
      status <- throttled(get_status())
    }
  }

  if (show_log) {
    logs <- show_new_log(get_log(), logs)
  }

  list(status = status, start = t0, end = Sys.time())
}


show_new_log <- function(curr, prev) {
  if (length(prev) == 0) {
    show <- curr
  } else {
    show <- curr[-seq_along(prev)]
  }
  if (length(show) > 0) {
    message(paste(show, collapse = "\n"))
  }
  curr
}


throttle <- function(interval) {
  last <- Sys.time() - interval
  function(expr) {
    wait <- interval - (Sys.time() - last)
    if (wait > 0) {
      Sys.sleep(wait)
    }
    last <<- Sys.time()
    force(expr)
  }
}
