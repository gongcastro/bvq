% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logs.R
\name{track_progress}
\alias{track_progress}
\title{Track a participant's response progress.}
\usage{
track_progress(response_id, participants = NULL, ...)
}
\arguments{
\item{participants}{Participants data frame, as generated by
\code{\link[=bvq_participants]{bvq_participants()}}.}

\item{...}{Arguments passed to \link{download_surveys}.}

\item{respose_id}{a character string identifying a single response to the questionnaire. This value is always unique for each response to the questionnaire, even for responses from the same participant.}
}
\value{
A logical vector indicating the surveys that the participant has completed.
}
\description{
This function prints some informative messages about a participants progress through the BVQ, and returns a vector of logical values indicating the surveys that the participant has completed.
}
\examples{
\dontrun{
track_progress("1911", participants, verbose=FALSE)
}

}
\author{
Gonzalo Garcia-Castro
}
