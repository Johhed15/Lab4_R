\name{linreg-class}
\Rdversion{1.1}
\docType{class}
\alias{linreg-class}
\alias{linreg}

\title{Class \code{"linreg"}}
\description{
%%  ~~ A concise (1-5 lines) description of what the class is. ~~
}
\section{Extends}{

All reference classes extend and inherit methods from \code{"\linkS4class{envRefClass}"}.

}
\references{
%%  ~~put references to the literature/web site here~~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
}
\examples{
showClass("linreg")
}
\keyword{classes}
\section{Fields}{
  \describe{
    \item{\code{formula}:}{Object of class \code{formula} ~~ }
    \item{\code{data}:}{Object of class \code{data.frame} ~~ }
    \item{\code{Y}:}{Object of class \code{numeric} ~~ }
    \item{\code{X}:}{Object of class \code{matrix} ~~ }
    \item{\code{beta_hat}:}{Object of class \code{matrix} ~~ }
    \item{\code{y_hat}:}{Object of class \code{array} ~~ }
    \item{\code{e}:}{Object of class \code{array} ~~ }
    \item{\code{df}:}{Object of class \code{numeric} ~~ }
    \item{\code{sigma_2}:}{Object of class \code{matrix} ~~ }
    \item{\code{VAR_B}:}{Object of class \code{matrix} ~~ }
    \item{\code{t_b}:}{Object of class \code{array} ~~ }
    \item{\code{p_values}:}{Object of class \code{array} ~~ }
    \item{\code{df_name}:}{Object of class \code{character} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{\code{summary()}:}{ ~~ }
    \item{\code{coef()}:}{ ~~ }
    \item{\code{pred()}:}{ ~~ }
    \item{\code{resid()}:}{ ~~ }
    \item{\code{print()}:}{ ~~ }
    \item{\code{plot()}:}{ ~~ }
    \item{\code{initialize(formula, data)}:}{ ~~ }
  }
}
