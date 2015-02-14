\name{tr.rules}
\alias{tr.rules}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Reguły decyzyjne na podstawie drzew decyzyjnych
}
\description{
 Tworzy zbiór reguł decyzyjnych na podstawie modelu z pakietu rpart
}
\usage{
tr.rules(model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
	model - model wygenerowany za pomocą funkci rpart z pakietu rpart (Zobacz \code{\link{rpart}}, \code{\link{rpart.object}})
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
	Obiekt klasy S4 \code{tr.rules.set}.
}

\examples{
	model <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
	rules <- tr.rules(model)
}
