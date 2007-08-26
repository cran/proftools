\name{profileCallGraph2Dot}
\alias{profileCallGraph2Dot}
\title{ Write Call Graph for Rprof Profile Data to Graphviz Dot File }
\description{
   Prints a Graphviz .cot file representation of the call graph for profile
   data produced by \code{Rprof}.
}
\usage{
profileCallGraph2Dot(pd, score = c("total", "self"),
                     transfer = function(x) x, colorMap = NULL,
                     filename = "Rprof.dot", landscape = FALSE,
                     mergeCycles = FALSE, edgesColored = TRUE,
                     rankdir = "LR", center = FALSE, size)
}
\arguments{
  \item{pd}{profile data as returned by \code{readProfileData}.}
  \item{score}{ character string specifying whether to use total time
                or self time for coloring nodes/edges; no color used if
                missing. }
  \item{transfer}{ function; maps score values in unit interval to unit
                   interval }
  \item{colorMap}{ character vector of color specifications as produced by
                   \code{rainbow}; \code{transfer} of score is mapped to
                   color }
  \item{filename}{ name of \code{.dot} file }
  \item{landscape}{ logical; whether to ad the \code{rotate=90} option to
                    the \code{.dot} file }
  \item{mergeCycles}{ logical; whether to merge each cycle of recursion into
                      a single node }
  \item{edgesColored}{ logical; whether to color edges }
  \item{rankdir}{ character; value to use for the \code{rankdir=} option to
                  specifythe direction that the plot is laid out using
                  the \code{dot} layout; must be either \code{"TB"} for
                  Top-to-Bottom or \code{"LR"} for Left-to-Right.  The
                  default value is \code{"LR"}.}
  \item{center}{ logical; whether to add the \code{center=1} option to
                 the \code{.dot} file. }
  \item{size}{ character; string to add as \code{size=} option in the
               \code{.dot} file. }
}
\value{Used for side effect.}
\details{
  Writes the call graph as a Graphviz \code{.dot} file. Color is used
  to encode the fraction total or self time spent in each function or
  call.  The scores used correspond to the values in the printed
  representation produced by \code{printProfileCallGraph}.  For now,
  see the \code{gprof} manual for further details.  The color encoding
  for a score \code{s} and a color map \code{m} is
  \code{ceiling(length(m) * transfer(s))}
}
\note{
  Because of lazy evaluation, nested calls like \code{f(g(x))}
  appear in the profile graph as \code{f} or one of its callees
  calling \code{g}.
}

\references{
  User manual for \code{gprof}, the GNU profiler.

  Graphviz: \url{http://www.research.att.com/sw/tools/graphviz/}
}
\author{ Luke Tierney }

\seealso{
  \code{\link{Rprof}},
  \code{\link{summaryRprof}},
  \code{\link{readProfileData}},
  \code{\link{flatProfile}},
  \code{\link{plotProfileCallGraph}},
  \code{\link{printProfileCallGraph}}
}
\examples{
\dontrun{
     ## Rprof() is not available on all platforms
     Rprof(tmp <- tempfile())
     example(glm)
     Rprof()
     profileCallGraph2Dot(readProfileData(tmp), filename = tmp)
     file.show(tmp)
     unlink(tmp)

     ## If you have graphviz installed on a UNIX-like system
     ## then in R do:

     Rprof(tmp <- tempfile())
     example(glm)
     Rprof()
     profileCallGraph2Dot(readProfileData(tmp), score = "total",
                          filename = "foo.dot")

     ## and then in a shell do (to use the interactive dotty):

     dotty foo.dot

     ## or (to create a postscript version and view with gv)

     dot -Tps foo.dot > foo.ps
     gv foo.ps
  }
}
\keyword{programming}
\keyword{utilities}
