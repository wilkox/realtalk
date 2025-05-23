% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logger.R
\name{Logger}
\alias{Logger}
\title{R6 Class for Unified Logging}
\description{
The Logger class provides a unified logging system with standardized formats,
severity levels, and thread safety. It supports logging at INFO, WARNING, and
ERROR levels with component tagging and structured context data.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-Logger-new}{\code{Logger$new()}}
\item \href{#method-Logger-info}{\code{Logger$info()}}
\item \href{#method-Logger-warning}{\code{Logger$warning()}}
\item \href{#method-Logger-error}{\code{Logger$error()}}
\item \href{#method-Logger-log}{\code{Logger$log()}}
\item \href{#method-Logger-get_log_path}{\code{Logger$get_log_path()}}
\item \href{#method-Logger-get_logs}{\code{Logger$get_logs()}}
\item \href{#method-Logger-clone}{\code{Logger$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Logger-new"></a>}}
\if{latex}{\out{\hypertarget{method-Logger-new}{}}}
\subsection{Method \code{new()}}{
Create a new Logger
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Logger$new(log_path = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{log_path}}{Path to the log file}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new Logger object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Logger-info"></a>}}
\if{latex}{\out{\hypertarget{method-Logger-info}{}}}
\subsection{Method \code{info()}}{
Log an informational message
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Logger$info(component, message, context = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{component}}{The component generating the log entry}

\item{\code{message}}{The message to log}

\item{\code{context}}{Optional list of contextual data}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Invisibly returns NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Logger-warning"></a>}}
\if{latex}{\out{\hypertarget{method-Logger-warning}{}}}
\subsection{Method \code{warning()}}{
Log a warning message
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Logger$warning(component, message, context = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{component}}{The component generating the log entry}

\item{\code{message}}{The message to log}

\item{\code{context}}{Optional list of contextual data}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Invisibly returns NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Logger-error"></a>}}
\if{latex}{\out{\hypertarget{method-Logger-error}{}}}
\subsection{Method \code{error()}}{
Log an error message
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Logger$error(component, message, context = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{component}}{The component generating the log entry}

\item{\code{message}}{The message to log}

\item{\code{context}}{Optional list of contextual data}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Invisibly returns NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Logger-log"></a>}}
\if{latex}{\out{\hypertarget{method-Logger-log}{}}}
\subsection{Method \code{log()}}{
Direct logging implementation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Logger$log(level, component, message, context = list())}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{level}}{The log level (INFO, WARNING, ERROR)}

\item{\code{component}}{The component generating the log entry}

\item{\code{message}}{The message to log}

\item{\code{context}}{Optional list of contextual data}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Invisibly returns NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Logger-get_log_path"></a>}}
\if{latex}{\out{\hypertarget{method-Logger-get_log_path}{}}}
\subsection{Method \code{get_log_path()}}{
Get the path to the log file
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Logger$get_log_path()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
The path to the log file
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Logger-get_logs"></a>}}
\if{latex}{\out{\hypertarget{method-Logger-get_logs}{}}}
\subsection{Method \code{get_logs()}}{
Retrieve filtered log entries
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Logger$get_logs(level = NULL, component = NULL, pattern = NULL, n = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{level}}{Optional level to filter by (INFO, WARNING, ERROR)}

\item{\code{component}}{Optional component name to filter by}

\item{\code{pattern}}{Optional text pattern to search for}

\item{\code{n}}{Optional limit on number of entries to return}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A character vector of log entries
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-Logger-clone"></a>}}
\if{latex}{\out{\hypertarget{method-Logger-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Logger$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
