\name{plotProfileCallGraph}
\alias{plotProfileCallGraph}
\title{ Plot Call Graph for Rprof Profile Data }
\description{
  Uses the graph and Rgraphviz packages to plot a call graph for
  profile data produced by \code{Rprof}.
}
\usage{
plotProfileCallGraph(pd, layout = "dot", score = c("total", "self"),
                     transfer = function(x) x, colorMap = NULL,
                     mergeCycles = FALSE, edgesColored = TRUE,
                     rankDir = "LR", ...)
}
\arguments{
  \item{pd}{profile data as returned by \code{readProfileData}.}
  \item{layout}{The layout method to use: One of \code{"dot"},
                \code{"neato"}, and \code{"twopi"}.}
  \item{score}{ character string specifying whether to use total time
                or self time for coloring nodes/edges; no color used if
                missing. }
  \item{transfer}{ function; maps score values in unit interval to unit
                   interval }
  \item{colorMap}{ character vector of color specifications as produced by
                   \code{rainbow}; \code{transfer} of score is mapped to
                   color }
  \item{mergeCycles}{ logical; whether to merge each cycle of recursion into
                      a single node }
  \item{edgesColored}{ logical; whether to color edges }
  \item{rankDir}{ The direction that the plot is laid out in, one of
                  either \code{"TB"} for Top-to-Bottom or \code{"LR"} for
                  Left-to-Right.  The default value is \code{"LR"}.  This
                  argument is only useful for \code{dot} layouts.}
  \item{\dots}{ additional arguments for the \code{graphNEL} \code{plot}
                method.}
}
\value{Used for side effect.}
\details{
  Color is used to encode the fraction of total or self time spent in
  each function or call.  The scores used correspond to the values in
  the printed representation produced by \code{printProfileCallGraph}.
  For now, see the \code{gprof} manual for further details.  The color
  encoding for a score \code{s} and a color map \code{m} is
  \code{m[ceiling(length(m) * transfer(s))]}
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
  \code{\link{printProfileCallGraph}},
  \code{\link{profileCallGraph2Dot}}
}
\examples{
\dontrun{
     ## Rprof() is not available on all platforms
     Rprof(tmp <- tempfile())
     example(glm)
     Rprof()
     plotProfileCallGraph(readProfileData(tmp))
     plotProfileCallGraph(readProfileData(tmp), score = "total")
     unlink(tmp)
  }
}
}
\keyword{programming}
\keyword{utilities}
