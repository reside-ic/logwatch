% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logwatch.R
\name{logwatch}
\alias{logwatch}
\title{Watch logs}
\usage{
logwatch(
  what,
  get_status,
  get_log,
  show_log = TRUE,
  skip = 0,
  show_spinner = TRUE,
  poll = 1,
  timeout = Inf,
  status_waiting = "waiting",
  status_running = "running",
  status_timeout = "timeout",
  status_interrupt = "interrupt",
  multiple = FALSE
)
}
\arguments{
\item{what}{Very short summary of the process underlying the
logs. Examples include "task", "installation", etc.}

\item{get_status}{Function that will fetch a status. Accepts no
arguments, and returns one of a set of (by default) "waiting",
"running" or any status you like after that.  When status is
"waiting" we show a spinner but do not stream logs. When
"running" we stream logs (but don't show a spinner). Any other
statement returns.  You can change the expected "waiting" or
"running" status values with the \code{status_waiting} and
\code{status_running} arguments.}

\item{get_log}{A callback to read logs of the installation
(something like \code{function() readLines(filename, warn = FALSE)}
may be sufficient), or \code{NULL} if log fetching is not possible.}

\item{show_log}{Logical, indicating if the installation log should
be printed.}

\item{skip}{Optional integer indicating how to handle log content
that exists at the point where we start watching. The default
(0) shows all log contents.  A positive integer skips that many
lines, while a negative integer shows only that many lines (so
-5 shows the first five lines in the log).  You can pass \code{Inf}
to discard all previous logs, but stream all new ones.}

\item{show_spinner}{Logical, indicating if a spinner should be
shown while waiting for the task to start, and if \code{show_log} is
\code{FALSE} for the task to complete.}

\item{poll}{Time, in seconds, used to throttle calls to the status
function. The default is 1 second}

\item{timeout}{Timeout, in seconds, after which we give up. This
does not cancel the underlying process being watched!  We return
\code{status_timeout} after a timeout (by default "timeout").}

\item{status_waiting}{The value of a waiting status (can be a
vector of possible statuses)}

\item{status_running}{The value of a running status (can be a
vector of possible statuses)}

\item{status_timeout}{The value to return if we timeout}

\item{status_interrupt}{The value to return if we are interrupted}

\item{multiple}{Logical, indicating if \code{get_status()} is expected
to return multiple values.  This changes the progress printing
slightly by disabling the logging (even if \code{get_log} is provided
and \code{show_log = TRUE}, logs are never fetched or shown), and
there is no longer a distinction between waiting for things to
start vs complete.  Instead, we print a summary of things in the
waiting, running and finished states.}
}
\value{
A list with elements:
\itemize{
\item status: Your final status call
\item start: The start time
\item end: The end time
}
}
\description{
Watch for a log of something.
}
